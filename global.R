# ==============================================================================
# global.R
# Optimisation de Portefeuille International sous contraintes ESG
# ==============================================================================
# Rôle : Initialisation de l'application Shiny.
#   - Chargement de tous les packages requis
#   - Import du fond de carte géographique mondial
#   - Définition des fonctions mathématiques "hors-réactivité"
# ==============================================================================


# ------------------------------------------------------------------------------
# ÉTAPE 1 : Chargement des librairies
# ------------------------------------------------------------------------------

library(shiny)
library(httr)
library(jsonlite)

library(sf)
library(leaflet)
library(plotly)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(quadprog)


# ------------------------------------------------------------------------------
# ÉTAPE 2 : Import du fond de carte géographique mondial
# ------------------------------------------------------------------------------

monde_sf <- rnaturalearth::ne_countries(
  scale       = "medium",
  type        = "countries",
  returnclass = "sf"
)

# Correction spécifique pour la France et la Norvège : Natural Earth utilise la valeur "-99"
# pour le code iso_a3. On force le code ISO correct pour permettre la jointure.
monde_sf$iso_a3[monde_sf$admin == "France"] <- "FRA"
monde_sf$iso_a3[monde_sf$admin == "Norway"] <- "NOR"

# Vérification de l'intégrité du fond de carte
stopifnot(
  inherits(monde_sf, "sf"),
  nrow(monde_sf) > 0
)

# Normalisation : conserver uniquement les colonnes utiles pour l'application
# iso_a3  : code ISO 3 lettres du pays (clé de jointure avec les données financières/ESG)
# name    : nom du pays (affiché dans les tooltips de la carte)
# geometry: géométries des polygones (obligatoire pour leaflet)
monde_sf <- monde_sf[, c("iso_a3", "name", "geometry")]

# Suppression des territoires sans code ISO valide (ex : Antarctique, territoires disputés)
# Ces entités ont iso_a3 == "-99" dans Natural Earth et ne peuvent pas être
# jointes avec des données financières. On les exclut dès l'import.
monde_sf <- monde_sf[monde_sf$iso_a3 != "-99", ]


# ------------------------------------------------------------------------------
# ÉTAPE 3 : Fonctions mathématiques "hors-réactivité"
# ------------------------------------------------------------------------------

# --- 3.1 Calcul des rentabilités logarithmiques ---
calculer_rendements_log <- function(prix_df) {

  stopifnot(is.data.frame(prix_df), "date" %in% colnames(prix_df), nrow(prix_df) >= 2)

  cols_actifs <- setdiff(colnames(prix_df), "date")

  mat_prix <- as.matrix(prix_df[, cols_actifs])

  # Calcul des log-rendements : diff(log(P)) = log(P_t) - log(P_{t-1})
  mat_rendements <- diff(log(mat_prix))

  # Reconstruction du data.frame avec les dates (on perd la première date)
  rendements_df <- data.frame(
    date = prix_df$date[-1],
    mat_rendements
  )
  colnames(rendements_df) <- c("date", cols_actifs)

  return(rendements_df)
}


# --- 3.2 Calcul de la matrice de variance-covariance ---
calculer_matrice_cov <- function(rendements_df, facteur_annuel = 1) {

  stopifnot(is.data.frame(rendements_df), nrow(rendements_df) >= 2)

  cols_actifs <- setdiff(colnames(rendements_df), "date")

  mat_rendements <- as.matrix(rendements_df[, cols_actifs])

  # Matrice de variance-covariance, optionnellement annualisée
  # facteur_annuel = 252 pour des rendements journaliers
  # facteur_annuel = 1   pour des rendements annuels (Banque Mondiale)
  sigma <- cov(mat_rendements) * facteur_annuel

  return(sigma)
}


# --- 3.3 Résolveur d'optimisation quadratique (Markowitz + contrainte ESG) ---
# Résout l'optimisation de Markowitz via programmation quadratique (quadprog::solve.QP).
# Minimise (1/2) * w' * Sigma * w sous les contraintes :
#   sum(w) = 1, w_i >= 0, w' * mu >= rendement_cible, w' * esg >= esg_minimum.
optimiser_portefeuille <- function(sigma, mu, esg, rendement_cible, esg_minimum) {

  n <- length(mu)
  stopifnot(nrow(sigma) == n, ncol(sigma) == n, length(esg) == n)

  # Régularisation de Sigma pour garantir la définie-positivité numérique
  # (ajout d'une petite constante sur la diagonale)
  sigma_reg <- sigma + diag(1e-8, n)

  # --- Construction de la matrice de contraintes pour solve.QP ---
  # solve.QP résout : min(-d'w + 1/2 w'Dw) sous A'w >= b0

  # d = vecteur nul (on minimise uniquement le risque, pas un trade-off linéaire)
  dvec <- rep(0, n)

  # Contraintes (en colonnes dans Amat, valeurs correspondantes dans bvec) :
  # 1. sum(w) = 1  →  on impose sum(w) >= 1 et -sum(w) >= -1 (égalité via meq=1)
  # 2. w_i >= 0    →  n contraintes de non-négativité
  # 3. w' * mu >= rendement_cible
  # 4. w' * esg >= esg_minimum

  # Colonne pour sum(w) = 1
  A_somme <- matrix(rep(1, n), nrow = n)

  # Colonnes pour w_i >= 0 (matrice identité)
  A_positif <- diag(n)

  # Colonne pour le rendement minimum
  A_rendement <- matrix(mu, nrow = n)

  # Colonne pour le score ESG minimum
  A_esg <- matrix(esg, nrow = n)

  # Assemblage final : Amat = [A_somme | A_positif | A_rendement | A_esg]
  Amat <- cbind(A_somme, A_positif, A_rendement, A_esg)
  bvec <- c(1, rep(0, n), rendement_cible, esg_minimum)

  # Appel du solveur (meq=1 : la première contrainte est une égalité)
  solution <- tryCatch(
    expr = {
      quadprog::solve.QP(
        Dmat = sigma_reg,
        dvec = dvec,
        Amat = Amat,
        bvec = bvec,
        meq  = 1   # 1 contrainte d'égalité (sum(w) = 1)
      )
    },
    error = function(e) {
      # Problème infaisable : ignoré silencieusement (cas fréquent aux extrêmes de la frontière)
      return(NULL)
    }
  )

  if (is.null(solution)) return(NULL)

  # Extraction et nettoyage des poids (valeurs numériquement proches de 0 → 0)
  poids <- solution$solution
  poids[poids < 1e-6] <- 0

  # Renormalisation pour corriger les erreurs d'arrondi numériques
  poids <- poids / sum(poids)

  # Nommage des poids avec les noms des actifs
  names(poids) <- names(mu)

  return(poids)
}


# --- 3.4 Simulation de la Frontière Efficiente ---
simuler_frontiere_efficiente <- function(sigma, mu, esg, esg_min, n_points = 50) {

  # Plage de rendements cibles à explorer
  mu_min <- min(mu) * 1.01
  mu_max <- max(mu) * 0.99
  cibles <- seq(mu_min, mu_max, length.out = n_points)

  resultats <- lapply(cibles, function(cible) {
    poids <- optimiser_portefeuille(sigma, mu, esg, cible, esg_min)
    if (is.null(poids)) return(NULL)

    rendement_reel <- sum(poids * mu)
    # Volatilité annualisée = sqrt(w' * Sigma * w)
    risque_reel    <- sqrt(as.numeric(t(poids) %*% sigma %*% poids))
    score_esg_reel <- sum(poids * esg)

    data.frame(
      rendement  = rendement_reel,
      risque     = risque_reel,
      score_esg  = score_esg_reel,
      as.data.frame(t(poids))
    )
  })

  # Suppression des cas infaisables (NULL) et assemblage
  resultats_valides <- Filter(Negate(is.null), resultats)
  if (length(resultats_valides) == 0) return(data.frame())

  do.call(rbind, resultats_valides)
}
