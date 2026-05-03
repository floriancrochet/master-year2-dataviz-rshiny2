# ==============================================================================
# ui.R
# Optimisation de Portefeuille International sous contraintes ESG
# ==============================================================================


navbarPage(

  title  = "Optimisateur de Portefeuille ESG",
  id     = "nav_principal",
  footer = tags$div(
    id = "app-footer",
    "Florian CROCHET \u2014 Dataviz\u00a0: R-Shiny\u00a02 \u2014 sous la direction de Fabien MORINEAU"
  ),

  # --------------------------------------------------------------------------
  # En-tête global : CSS (style.css) + shinyjs + Sidebar fixe repliable
  # --------------------------------------------------------------------------
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      includeCSS("www/style.css"),
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),

    # ------------------------------------------------------------------
    # Enveloppe de la sidebar fixe (positionnement fixe côté gauche)
    # #sidebar-wrapper contient :
    #   - #sidebar       : corps scrollable avec widgets et commentaires
    #   - #sidebar-toggle : bouton chevron pour plier/déplier
    # ------------------------------------------------------------------
    tags$div(
      id = "sidebar-wrapper",

      tags$div(
        id = "sidebar",

        tags$div(class = "sidebar-header",
          tags$h4("PARAMÈTRES")
        ),

        # ----------------------------------------------------------------
        # Rendement cible
        # ----------------------------------------------------------------
        numericInput(
          inputId = "rendement_cible",
          label   = "Rendement cible annualisé (%)",
          value   = 8,     # Valeur par défaut : 8 % (rendement actions)
          min     = 0.5,   # Minimum : 0.5 %
          max     = 30,    # Maximum : 30 %
          step    = 0.5    # Pas de 0,5 %
        ),

        tags$hr(class = "sidebar-sep"),

        # ----------------------------------------------------------------
        # Score ESG minimum
        # ----------------------------------------------------------------
        sliderInput(
          inputId = "esg_minimum",
          label   = "Score ESG minimum",
          min     = 0,
          max     = 100,
          value   = 15,    # Valeur par défaut : 15/100 (médiane pays développés)
          step    = 5,
          ticks   = TRUE,
          post    = " / 100"
        ),

        tags$hr(class = "sidebar-sep"),

        # ----------------------------------------------------------------
        # Zones géographiques
        # ----------------------------------------------------------------
        shinyWidgets::pickerInput(
          inputId  = "zones_geo",
          label    = "Zones géographiques",
          choices  = list(
            "Amérique"  = c("Amérique du Nord" = "AMN",
                            "Amérique Latine"  = "AML"),
            "Europe"    = c("Europe Développée" = "EUR",
                            "Europe Émergente"  = "EUE"),
            "Asie"      = c("Asie Développée"   = "ASD",
                            "Asie Émergente"    = "ASE"),
            "Autres"    = c("Afrique"            = "AFR",
                            "Moyen-Orient"       = "MOY",
                            "Océanie"            = "OCE")
          ),
          selected = c("AMN", "EUR", "ASD"),
          multiple = TRUE,
          options  = shinyWidgets::pickerOptions(
            actionsBox         = TRUE,
            liveSearch         = TRUE,
            selectedTextFormat = "count > 2",
            countSelectedText  = "{0} zones sélectionnées",
            noneSelectedText   = "Aucune zone sélectionnée",
            size               = 8
          )
        ),

        tags$hr(class = "sidebar-sep"),

        # ----------------------------------------------------------------
        # Synthèse financière
        # ----------------------------------------------------------------
        tags$div(
          class = "commentaire-reactif commentaire-financier",
          tags$div(class = "commentaire-label", "Synthèse financière"),
          tags$div(
            class = "commentaire-texte",
            textOutput(outputId = "commentaire_financier", inline = TRUE)
          )
        ),

        tags$div(style = "height: 10px;"),

        # ----------------------------------------------------------------
        # Synthèse ESG
        # ----------------------------------------------------------------
        tags$div(
          class = "commentaire-reactif commentaire-esg",
          tags$div(class = "commentaire-label", "Synthèse ESG"),
          tags$div(
            class = "commentaire-texte",
            textOutput(outputId = "commentaire_esg", inline = TRUE)
          )
        )
      ), # fin #sidebar

      # ---- Bouton toggle : chevron positionné sur le bord droit de la sidebar ----
      actionButton(
        inputId = "sidebar_toggle",
        label   = HTML("<span class='chevron-icon'>\u2039</span>"),
        title   = "Plier / Déplier le panneau"
      )
    ) # fin #sidebar-wrapper
  ),


  # ==========================================================================
  # ONGLET 1 — CARTE
  # Carte choroplèthe mondiale des pondérations du portefeuille
  # ==========================================================================
  tabPanel(
    title = "\U0001f5fa\ufe0f Carte",
    value = "tab-carte",

    # Pas de padding pour que la carte occupe toute la largeur disponible
    tags$div(
      style = "position: relative;",


      leafletOutput(
        outputId = "carte_portefeuille",
        width    = "100%",
        height   = "85vh"
      )
    )   # fin tags$div position:relative
  ),    # fin tabPanel Carte


  # ==========================================================================
  # ONGLET 2 — ANALYSE
  # Deux graphiques Plotly réactifs :
  #   • Frontière Efficiente (nuage de points Risque/Rendement)
  #   • Rendements cumulés historiques du portefeuille (courbe temporelle)
  # ==========================================================================
  tabPanel(
    title = "\U0001f4c8 Analyse",
    value = "tab-analyse",

    fluidPage(

      fluidRow(

        column(
          width = 12,
          tags$div(
            class = "graphique-container",
            tags$div(class = "section-titre", "Frontière Efficiente de Markowitz"),
            plotlyOutput(
              outputId = "graphique_frontiere",
              height   = "420px"
            )
          )
        ),


        column(
          width = 12,
          tags$div(
            class = "graphique-container",
            tags$div(class = "section-titre", "Performance historique cumulée"),
            plotlyOutput(
              outputId = "graphique_rendements",
              height   = "420px"
            )
          )
        )
      ), # fin fluidRow graphiques


    )   # fin fluidPage
  ),    # fin tabPanel Analyse


  # ==========================================================================
  # ONGLET 3 — PORTEFEUILLE
  # Tableau DT interactif de la composition du portefeuille optimal
  # ==========================================================================
  tabPanel(
    title = "\U0001f4bc Portefeuille",
    value = "tab-portefeuille",

    fluidPage(

      fluidRow(
        column(
          width = 12,


          tags$div(
            class = "tableau-container",
            tags$div(
              class = "section-titre",
              style = "padding-bottom: 12px;",
              "Composition du portefeuille optimal"
            ),
            DT::DTOutput(
              outputId = "tableau_portefeuille",
              width    = "100%"
            )
          )
        )
      ), # fin fluidRow tableau

    )   # fin fluidPage
  )     # fin tabPanel Portefeuille

) # fin navbarPage
