# ==============================================================================
# server.R
# Optimisation de Portefeuille International sous contraintes ESG
# ==============================================================================

# Table de correspondance zones géographiques → codes ISO 3 lettres
zones_iso <- list(
  AMN = c("USA", "CAN"),
  AML = c("BRA", "MEX", "CHL", "COL", "ARG", "PER"),
  EUR = c("GBR", "FRA", "DEU", "CHE", "NLD", "SWE", "NOR", "DNK", "FIN", "ESP", "ITA"),
  EUE = c("POL", "CZE", "HUN", "ROU", "TUR"),
  ASD = c("JPN", "KOR", "SGP", "HKG", "TWN"),
  ASE = c("CHN", "IND", "IDN", "THA", "MYS", "PHL"),
  AFR = c("ZAF", "NGA", "KEN", "EGY"),
  MOY = c("SAU", "ARE", "QAT", "KWT"),
  OCE = c("AUS", "NZL")
)

# Noms lisibles des pays (associés aux codes ISO)
noms_pays <- c(
  USA = "États-Unis", CAN = "Canada",
  BRA = "Brésil", MEX = "Mexique", CHL = "Chili", COL = "Colombie",
  ARG = "Argentine", PER = "Pérou",
  GBR = "Royaume-Uni", FRA = "France", DEU = "Allemagne", CHE = "Suisse",
  NLD = "Pays-Bas", SWE = "Suède", NOR = "Norvège", DNK = "Danemark",
  FIN = "Finlande", ESP = "Espagne", ITA = "Italie",
  POL = "Pologne", CZE = "Rép. tchèque", HUN = "Hongrie",
  ROU = "Roumanie", TUR = "Turquie",
  JPN = "Japon", KOR = "Corée du Sud", SGP = "Singapour",
  HKG = "Hong Kong", TWN = "Taïwan",
  CHN = "Chine", IND = "Inde", IDN = "Indonésie", THA = "Thaïlande",
  MYS = "Malaisie", PHL = "Philippines",
  ZAF = "Afrique du Sud", NGA = "Nigéria", KEN = "Kenya", EGY = "Égypte",
  SAU = "Arabie Saoudite", ARE = "Émirats arabes unis", QAT = "Qatar",
  KWT = "Koweït", AUS = "Australie", NZL = "Nouvelle-Zélande"
)

# Dictionnaire de correspondance (Code ISO -> Ticker ETF MSCI Yahoo Finance)
tickers_yahoo <- c(
  USA = "SPY", CAN = "EWC", BRA = "EWZ", MEX = "EWW", CHL = "ECH",
  COL = "GXG", ARG = "ARGT", PER = "EPU",
  GBR = "EWU", FRA = "EWQ", DEU = "EWG", CHE = "EWL", NLD = "EWN",
  SWE = "EWD", NOR = "ENOR", DNK = "EDEN",
  FIN = "EFNL", ESP = "EWP", ITA = "EWI", POL = "EPOL", TUR = "TUR",
  JPN = "EWJ", KOR = "EWY", SGP = "EWS", HKG = "EWH", TWN = "EWT",
  CHN = "MCHI", IND = "INDA",
  IDN = "EIDO", THA = "THD", MYS = "EWM", PHL = "EPHE", ZAF = "EZA",
  NGA = "NGE", EGY = "EGPT",
  SAU = "KSA", ARE = "UAE", QAT = "QAT", AUS = "EWA", NZL = "ENZL"
)


# --- Fonction utilitaire : appel API Banque Mondiale -------------------------
# Encapsule les appels REST vers l'API publique (v2) de la Banque Mondiale.
# Réutilisable pour tout indicateur disponible sur l'endpoint /country/all/indicator/.

appeler_api_banque_mondiale <- function(indicateur, date_range = "2010:2022",
                                        per_page = 300) {
  # --- 1. Construction de l'URL de l'endpoint REST ---
  url_api <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/", indicateur,
    "?format=json",
    "&per_page=", per_page,
    "&date=", date_range
  )

  tryCatch(
    expr = {
      # --- 2. Requête HTTP GET via httr ---
      response <- httr::GET(
        url = url_api,
        httr::add_headers(
          "Accept"     = "application/json",
          "User-Agent" = "ESG-Portfolio-Optimizer/1.0"
        ),
        httr::timeout(30)
      )

      # --- 3. Vérification du code HTTP ---
      httr::stop_for_status(response)

      # --- 4. Extraction du contenu brut en texte ---
      contenu_texte <- httr::content(response, as = "text", encoding = "UTF-8")

      # --- 5. Désérialisation JSON → liste R ---
      # simplifyVector=FALSE préserve la structure [metadata, data] de la Banque Mondiale
      donnees_json <- jsonlite::fromJSON(contenu_texte, simplifyVector = FALSE)

      # --- 6. Vérification de la structure de la réponse ---
      # L'API Banque Mondiale retourne un array JSON à 2 éléments :
      # [[1]] = métadonnées de pagination, [[2]] = liste d'observations
      if (!is.list(donnees_json) || length(donnees_json) < 2 || is.null(donnees_json[[2]])) {
        stop("Structure JSON inattendue (indicateur introuvable ou archivé).")
      }

      # --- 7. Conversion de la liste imbriquée en data.frame ---
      df <- do.call(rbind, lapply(donnees_json[[2]], function(obs) {
        data.frame(
          iso_a3 = obs$countryiso3code %||% NA_character_,
          pays = obs$country$value %||% NA_character_,
          date = as.integer(obs$date %||% NA),
          value = as.numeric(obs$value %||% NA),
          stringsAsFactors = FALSE
        )
      }))

      # Nettoyage : exclure agrégats régionaux (garder uniquement les codes ISO 3 valides)
      pays_valides <- unlist(zones_iso, use.names = FALSE)
      df <- df[!is.na(df$value) & df$iso_a3 %in% pays_valides, ]
      rownames(df) <- NULL

      df
    },
    error = function(e) {
      # Erreur réseau ou structure inattendue : retourne un data.frame vide
      data.frame(
        iso_a3 = character(), pays = character(),
        date = integer(), value = numeric()
      )
    }
  )
}

# --- Fonction utilitaire : appel API Yahoo Finance ---------------------------
# Récupère l'historique des prix de clôture ajustés d'un ticker ETF sur 10 ans
# en fréquence mensuelle via l'endpoint v8/finance/chart de Yahoo Finance.

appeler_api_yahoo <- function(ticker) {
  # --- 1. Construction de l'URL de l'endpoint REST ---
  # interval=1mo : cotations mensuelles (adapté à l'optimisation Markowitz)
  # range=10y    : fenêtre de 10 ans pour l'estimation des paramètres historiques
  url_api <- paste0(
    "https://query1.finance.yahoo.com/v8/finance/chart/", ticker,
    "?interval=1mo&range=10y"
  )

  tryCatch(
    {
      # --- 2. Requête HTTP GET via httr ---
      # User-Agent recommandé : sans identification, Yahoo Finance peut retourner
      # un 429 (rate-limit) ou un contenu vide selon la politique d'accès.
      response <- httr::GET(
        url_api,
        httr::add_headers("User-Agent" = "ESG-Portfolio-Optimizer/1.0"),
        httr::timeout(10)
      )

      # --- 3. Vérification du code HTTP ---
      if (httr::status_code(response) != 200) {
        return(NULL)
      }

      # --- 4. Extraction du contenu brut en texte ---
      contenu <- httr::content(response, as = "text", encoding = "UTF-8")

      # --- 5. Désérialisation JSON → liste R ---
      # simplifyVector=FALSE : indispensable pour préserver la structure
      # imbriquée chart$result[[1]], que fromJSON simplifierait sinon en data.frame.
      json <- jsonlite::fromJSON(contenu, simplifyVector = FALSE)

      # --- 6. Vérification de la structure de la réponse ---
      # Structure : json$chart$result[[1]]
      #   $timestamp          : vecteur de dates UNIX (secondes depuis 1970-01-01)
      #   $indicators$adjclose: prix de clôture ajustés (dividendes et splits inclus)
      if (is.null(json$chart$result[[1]])) {
        return(NULL)
      }

      # --- 7. Extraction et conversion en data.frame ---
      res <- json$chart$result[[1]]
      timestamps <- res$timestamp
      adjclose <- res$indicators$adjclose[[1]]$adjclose

      # Filtrer les observations nulles (ticker non encore coté sur certaines périodes)
      valid <- sapply(adjclose, function(x) !is.null(x))
      if (sum(valid) == 0) {
        return(NULL)
      }

      data.frame(
        date  = as.Date(as.POSIXct(unlist(timestamps[valid]), origin = "1970-01-01")),
        price = unlist(adjclose[valid])
      )
    },
    error = function(e) NULL # Ticker invalide ou API indisponible
  )
}


server <- function(input, output, session) {
  # --- Gestion de la sidebar fixe en R pur via shinyjs ---
  # Parité du compteur de clics : impair = fermer, pair = réouvrir
  observeEvent(input$sidebar_toggle, {
    if (input$sidebar_toggle %% 2 == 1) {
      shinyjs::addClass(id = "sidebar", class = "collapsed")
      shinyjs::addClass(selector = "body", class = "sidebar-collapsed")
      updateActionButton(session, "sidebar_toggle", label = HTML("<span class='chevron-icon'>\u203A</span>")) # Chevron →
    } else {
      shinyjs::removeClass(id = "sidebar", class = "collapsed")
      shinyjs::removeClass(selector = "body", class = "sidebar-collapsed")
      updateActionButton(session, "sidebar_toggle", label = HTML("<span class='chevron-icon'>\u2039</span>")) # Chevron ←
    }
    # Correction Leaflet : déclencher le redimensionnement après la fin de l'animation CSS (280 ms)
    shinyjs::delay(300, shinyjs::runjs("window.dispatchEvent(new Event('resize'));"))
    shinyjs::delay(350, shinyjs::runjs("window.dispatchEvent(new Event('resize'));"))
  })

  # ==============================================================================
  # MOTEUR RÉACTIF ET APPELS API
  # ==============================================================================

  # --- Appel 1 : Données ESG (énergie renouvelable) ---
  donnees_esg <- appeler_api_banque_mondiale(
    indicateur = "EG.FEC.RNEW.ZS",
    date_range = "2021",
    per_page   = 300
  )

  # Part d'énergie renouvelable par pays (proxy ESG, en %)
  esg_scores <- setNames(donnees_esg$value, donnees_esg$iso_a3)

  # --- Appel 2 : Données de prix (Yahoo Finance) ---
  # Construction d'un data.frame global prix_df avec une colonne "date" et
  # une colonne par code ISO de pays.

  liste_prix <- list()
  iso_dispos <- names(tickers_yahoo)

  withProgress(message = "Chargement des données de marché...", value = 0, {
    n_tot <- length(iso_dispos)
    for (i in seq_along(iso_dispos)) {
      iso <- iso_dispos[i]
      ticker <- tickers_yahoo[iso]
      incProgress(1 / n_tot, detail = paste("Pays :", iso))

      df_ticker <- appeler_api_yahoo(ticker)
      if (!is.null(df_ticker) && nrow(df_ticker) > 20) {
        # Renommer la colonne price avec le code ISO du pays
        colnames(df_ticker)[2] <- iso
        liste_prix[[iso]] <- df_ticker
      }
    }
  })

  # Jointure externe complète de toutes les séries de prix sur la colonne date
  if (length(liste_prix) > 0) {
    prix_df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), liste_prix)
    # Trier chronologiquement et supprimer les lignes avec trop de NA
    prix_df <- prix_df[order(prix_df$date), ]
    # Ne conserver que les dates où au moins 50% des actifs cotent
    seuil_na <- (ncol(prix_df) - 1) * 0.5
    prix_df <- prix_df[rowSums(!is.na(prix_df)) >= seuil_na, ]

    # Imputation "forward-fill" basique des NA persistants pour éviter les ruptures
    for (j in 2:ncol(prix_df)) {
      for (i in 2:nrow(prix_df)) {
        if (is.na(prix_df[i, j])) prix_df[i, j] <- prix_df[i - 1, j]
      }
    }
  } else {
    prix_df <- data.frame(date = Sys.Date())
  }

  pays_complets <- colnames(prix_df)[-1]
  pays_communs <- intersect(pays_complets, names(esg_scores))

  if (length(pays_communs) < 5) {
    showNotification(
      paste0(
        "Attention : seulement ", length(pays_communs),
        " pays avec données complètes (Prix + ESG)."
      ),
      type = "warning", duration = 10
    )
  }


  # ==========================================================================
  # MOTEUR D'OPTIMISATION RÉACTIF
  # ==========================================================================

  portefeuille_optimal <- reactive({
    cible_rend <- input$rendement_cible / 100 # Conversion % → décimal
    seuil_esg <- input$esg_minimum
    zones <- input$zones_geo

    # Validation : au moins une zone doit être sélectionnée
    req(length(zones) > 0, cancelOutput = TRUE)

    withProgress(message = "Optimisation en cours…", value = 0, {
      # --- Filtrage par zones géographiques ---
      incProgress(0.2, detail = "Filtrage des actifs par zone")
      iso_zones <- unlist(zones_iso[zones], use.names = FALSE)
      iso_filtres <- intersect(iso_zones, pays_communs)
      iso_filtres <- intersect(iso_filtres, colnames(prix_df)[-1])

      req(length(iso_filtres) >= 2)

      # --- Calcul des rendements et de la matrice de covariance ---
      incProgress(0.2, detail = "Calcul des rendements logarithmiques")
      prix_filtre <- prix_df[, c("date", iso_filtres)]
      rendements <- calculer_rendements_log(prix_filtre)

      # Matrice de covariance annualisée (*12 mois)
      sigma <- calculer_matrice_cov(rendements, facteur_annuel = 12)

      # Rendements espérés annualisés (moyenne arithmétique mensuelle * 12)
      mu_annualise <- colMeans(rendements[, -1, drop = FALSE], na.rm = TRUE) * 12

      esg_filtre <- esg_scores[iso_filtres]

      # --- Optimisation quadratique (Markowitz + ESG) ---
      incProgress(0.3, detail = "Résolution de l'optimisation quadratique")
      poids <- optimiser_portefeuille(
        sigma, mu_annualise, esg_filtre,
        cible_rend, seuil_esg
      )

      if (is.null(poids)) {
        showNotification(
          "Contraintes incompatibles : aucun portefeuille ne satisfait ces paramètres. Essayez de réduire le rendement cible ou le seuil ESG.",
          type = "warning", duration = 8
        )
        return(NULL)
      }

      # --- Construction du data.frame de sortie ---
      incProgress(0.2, detail = "Construction du tableau de résultats")
      noms <- ifelse(names(poids) %in% names(noms_pays), noms_pays[names(poids)], names(poids))

      df_resultat <- data.frame(
        iso_a3 = names(poids),
        pays = noms,
        poids_pct = round(poids * 100, 2),
        rendement = round(mu_annualise[names(poids)] * 100, 2),
        risque = round(sqrt(diag(sigma)[names(poids)]) * 100, 2),
        score_esg = round(esg_filtre[names(poids)], 1),
        stringsAsFactors = FALSE
      )
      rownames(df_resultat) <- NULL

      # Supprimer les actifs à poids nul pour la lisibilité
      df_resultat <- df_resultat[df_resultat$poids_pct > 0.01, ]

      # Métriques globales (en attribut pour les commentaires et graphiques)
      vol_port <- sqrt(as.numeric(t(poids) %*% sigma %*% poids)) * 100
      rend_port <- sum(poids * mu_annualise) * 100
      esg_port <- sum(poids * esg_filtre)

      attr(df_resultat, "rendement_global") <- round(rend_port, 2)
      attr(df_resultat, "volatilite_globale") <- round(vol_port, 2)
      attr(df_resultat, "esg_global") <- round(esg_port, 1)
      attr(df_resultat, "sigma") <- sigma
      attr(df_resultat, "mu") <- mu_annualise
      attr(df_resultat, "esg_vec") <- esg_filtre
      attr(df_resultat, "poids_complets") <- poids

      attr(df_resultat, "prix") <- prix_filtre

      incProgress(0.1, detail = "Terminé")
      df_resultat
    })
  })


  # Frontière efficiente (réactive partagée entre les deux graphiques)
  frontiere_reactive <- reactive({
    df <- portefeuille_optimal()
    req(df)
    sigma <- attr(df, "sigma")
    mu <- attr(df, "mu")
    esg_vec <- attr(df, "esg_vec")
    seuil <- input$esg_minimum
    simuler_frontiere_efficiente(sigma, mu, esg_vec, seuil, n_points = 40)
  })


  # ==========================================================================
  # CARTE CHOROPLÈTHE
  # ==========================================================================
  # Rendu initial de la carte (une seule fois)
  output$carte_portefeuille <- renderLeaflet({
    leaflet(monde_sf, options = leafletOptions(
      minZoom = 2,
      maxZoom = 5
    )) |>
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
        minZoom = 2,
        maxZoom = 5
      )) |>
      setView(lng = 10, lat = 25, zoom = 2) |>
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)
  })

  # Mise à jour réactive des polygones via leafletProxy()
  observe({
    df <- portefeuille_optimal()
    req(df)

    # Jointure pondérations → fond de carte
    carte_data <- merge(monde_sf, df[, c("iso_a3", "poids_pct")],
      by = "iso_a3", all.x = TRUE
    )
    carte_data$poids_pct[is.na(carte_data$poids_pct)] <- 0

    pal <- colorNumeric(
      palette  = c("#f0f4f8", "#1a3a5c"),
      domain   = c(0, max(carte_data$poids_pct, 1)),
      na.color = "#eeeeee"
    )

    leafletProxy("carte_portefeuille", session) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        data = carte_data,
        fillColor = ~ pal(poids_pct),
        fillOpacity = 0.75,
        color = "#ffffff",
        weight = 0.8,
        label = ~ paste0(name, " : ", round(poids_pct, 1), " %"),
        highlightOptions = highlightOptions(
          weight       = 2,
          color        = "#2ecc71",
          fillOpacity  = 0.9,
          bringToFront = TRUE
        )
      ) |>
      leaflet::addLegend(
        position = "bottomleft",
        pal      = pal,
        values   = carte_data$poids_pct,
        title    = "Poids (%)",
        opacity  = 0.8
      )
  })


  # ==========================================================================
  # GRAPHIQUES PLOTLY
  # ==========================================================================
  # --- 4a. Frontière Efficiente (nuage de points scatter) ---
  # source = "frontiere" permet la liaison croisée avec le graphique de performance
  output$graphique_frontiere <- renderPlotly({
    df <- portefeuille_optimal()
    req(df)

    frontiere <- frontiere_reactive()
    req(nrow(frontiere) > 0)

    rend_opt <- attr(df, "rendement_global")
    vol_opt <- attr(df, "volatilite_globale")

    plot_ly(source = "frontiere") |>
      add_trace(
        data = frontiere,
        x = ~ risque * 100,
        y = ~ rendement * 100,
        type = "scatter",
        mode = "markers",
        marker = list(size = 6, color = "#a8c4e0", opacity = 0.7),
        name = "Frontière Efficiente",
        hovertemplate = "Volatilité: %{x:.2f}%<br>Rendement: %{y:.2f}%<extra></extra>"
      ) |>
      add_trace(
        x = vol_opt,
        y = rend_opt,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 14, color = "#2ecc71", symbol = "star",
          line = list(color = "#1a3a5c", width = 2)
        ),
        name = "Portefeuille optimal",
        hovertemplate = paste0(
          "\u2605 Optimal<br>Volatilité: ", round(vol_opt, 2),
          "%<br>Rendement: ", round(rend_opt, 2), "%<extra></extra>"
        )
      ) |>
      layout(
        xaxis  = list(title = "Volatilité annualisée (%)", zeroline = FALSE),
        yaxis  = list(title = "Rendement espéré annualisé (%)"),
        legend = list(orientation = "h", y = -0.25),
        margin = list(t = 10, b = 60)
      )
  })

  # --- 4b. Rendements cumulés historiques (courbe linéaire) ---
  # Liaison croisée : cliquer sur un point de la frontière affiche
  # la performance du portefeuille correspondant en comparaison.
  output$graphique_rendements <- renderPlotly({
    df <- portefeuille_optimal()
    req(df)

    poids_complets <- attr(df, "poids_complets")
    prix_filtre <- attr(df, "prix")
    req(!is.null(poids_complets), !is.null(prix_filtre))

    cols_actifs <- names(poids_complets)
    mat_prix_loc <- as.matrix(prix_filtre[, cols_actifs])
    rend_jour <- diff(log(mat_prix_loc)) %*% poids_complets
    rend_cumules <- cumsum(rend_jour) * 100

    df_perf <- data.frame(
      date      = prix_filtre$date[-1],
      rendement = as.numeric(rend_cumules)
    )

    p <- plot_ly() |>
      add_trace(
        data = df_perf,
        x = ~date,
        y = ~rendement,
        type = "scatter",
        mode = "lines",
        line = list(color = "#1a3a5c", width = 2),
        fill = "tozeroy",
        fillcolor = "rgba(26, 58, 92, 0.08)",
        name = "Portefeuille optimal",
        hovertemplate = "%{x|%Y}<br>%{y:.2f}%<extra></extra>"
      )

    # Liaison croisée avec la frontière efficiente
    click <- event_data("plotly_click", source = "frontiere")
    frontiere <- frontiere_reactive()

    if (!is.null(click) && !is.null(frontiere) && click$curveNumber == 0) {
      idx <- click$pointNumber + 1
      if (idx >= 1 && idx <= nrow(frontiere)) {
        cols_poids <- setdiff(colnames(frontiere), c("rendement", "risque", "score_esg"))
        poids_alt <- as.numeric(frontiere[idx, cols_poids])
        names(poids_alt) <- cols_poids
        cols_communs <- intersect(cols_poids, cols_actifs)
        poids_alt_f <- poids_alt[cols_communs]
        poids_alt_f <- poids_alt_f / sum(poids_alt_f)

        rend_alt <- diff(log(as.matrix(prix_filtre[, cols_communs]))) %*% poids_alt_f
        rend_cum_alt <- cumsum(rend_alt) * 100

        df_alt <- data.frame(
          date      = prix_filtre$date[-1],
          rendement = as.numeric(rend_cum_alt)
        )

        p <- p |>
          add_trace(
            data = df_alt,
            x = ~date,
            y = ~rendement,
            type = "scatter",
            mode = "lines",
            line = list(color = "#a8c4e0", width = 2, dash = "dash"),
            name = "Sélection frontière",
            hovertemplate = "%{x|%Y}<br>%{y:.2f}%<extra></extra>"
          )
      }
    }

    p |>
      layout(
        xaxis = list(
          title = "",
          rangeslider = list(
            visible   = TRUE,
            thickness = 0.02,
            bgcolor   = "rgba(26, 58, 92, 0.04)",
            pad       = list(t = 20)
          )
        ),
        yaxis = list(title = "Rendement cumulé (%)"),
        margin = list(t = 10, b = 10)
      )
  })


  # ==========================================================================
  # TABLEAU DT
  # ==========================================================================
  output$tableau_portefeuille <- renderDT({
    df <- portefeuille_optimal()
    req(df)

    DT::datatable(
      df[, c("pays", "iso_a3", "poids_pct", "rendement", "risque", "score_esg")],
      colnames = c("Pays", "Code ISO", "Poids (%)", "Rendement ann. (%)", "Risque ann. (%)", "Score ESG"),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        dom = "Bfrtip",
        buttons = c("csv", "excel", "pdf"),
        order = list(list(2, "desc")), # Tri par poids décroissant
        language = list(
          search   = "Rechercher :",
          info     = "Affichage de _START_ à _END_ sur _TOTAL_ actifs",
          paginate = list(previous = "Précédent", `next` = "Suivant")
        )
      )
    ) |>
      formatRound(columns = c("poids_pct", "rendement", "risque"), digits = 2) |>
      formatRound(columns = "score_esg", digits = 1)
  })


  # ==========================================================================
  # COMMENTAIRES RÉACTIFS
  # ==========================================================================
  texte_financier <- reactive({
    df <- portefeuille_optimal()
    req(df)
    rend <- attr(df, "rendement_global")
    vol <- attr(df, "volatilite_globale")
    paste0(
      "Le rendement espéré du portefeuille est de ", rend,
      " % pour une volatilité de ", vol, " %."
    )
  })

  texte_esg <- reactive({
    df <- portefeuille_optimal()
    req(df)
    esg <- attr(df, "esg_global")
    paste0(
      "La note ESG globale du portefeuille atteint un score de ",
      esg, " / 100, respectant votre filtre."
    )
  })

  output$commentaire_financier <- renderText({
    texte_financier()
  })

  output$commentaire_esg <- renderText({
    texte_esg()
  })
}
