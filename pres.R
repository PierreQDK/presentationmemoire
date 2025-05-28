library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plm)
library(broom)
library(DT)
library(synthdid)
library(dplyr)
library(broom)

# 📁 Données
prices_data <- read_excel("daily_futures_prices_1920s.xlsx", sheet = "panel7") %>%
  mutate(
    Indiv_raw = gsub("_", "-", Indiv),
    Date = as.Date(paste(Year, Month, Day, sep = "-"))
  ) %>%
  separate(Indiv_raw, into = c("City", "Commodity", "Echeance"), sep = "-") %>%
  mutate(
    Indiv = paste(City, Commodity, Echeance, sep = "-"),
    Treatment = ifelse(City == "CHI", 1, 0),
    Post = ifelse(Date >= as.Date("1926-02-01"), 1, 0),
    Treat_Post = Treatment * Post
  ) %>%
  filter(!is.na(Close))

# Charger les données et vérifier les colonnes
df <- read_excel("Dataset1920_ln.xlsx", sheet = "Sheet4") %>%
  filter(!is.na(RECEIPTS)) %>%
  mutate(MONTH = as.Date(MONTH)) %>%
  arrange(INDIVIDU, MONTH)

names(flux_data) <- toupper(names(flux_data))

# NB : Assure-toi d'avoir déjà chargé le fichier `df` (Dataset1920_ln.xlsx Sheet4)
library(dplyr)
library(synthdid)

df <- df %>%
  filter(MONTH >= as.Date("1925-01-01") & MONTH <= as.Date("1926-12-31"))  # fenêtre réduite

nb_periods <- length(unique(df$MONTH))

# 📅 Transformation des dates et filtrage
df <- df %>%
  mutate(MONTH = as.Date(MONTH)) %>%
  filter(MONTH >= as.Date("1924-01-01") & MONTH <= as.Date("1927-12-31"))

# 🎯 Création des variables DiD
df <- df %>%
  mutate(
    POST = ifelse(MONTH >= as.Date("1926-02-01"), 1, 0),
    Treat_Post = TREATMENT * POST
  )

# ⚠️ Détection des outliers via RosnerTest
rosner_receipts <- rosnerTest(na.omit(df$RECEIPTS), k = 10)
rosner_shipments <- rosnerTest(na.omit(df$SHIPMENTS), k = 10)

rosner_receipts
rosner_shipments
# 🔍 Extraire les valeurs à exclure
receipts_outliers <- rosner_receipts$all.stats %>%
  filter(Outlier == TRUE) %>%
  pull(Value)

shipments_outliers <- rosner_shipments$all.stats %>%
  filter(Outlier == TRUE) %>%
  pull(Value)

# 🧼 Nettoyage de la base (exclusion des outliers)
df_clean <- df %>%
  filter(!RECEIPTS %in% receipts_outliers,
         !SHIPMENTS %in% shipments_outliers)


# === Estimations SDiD pré-calculées (hors serveur) ===
library(synthdid)

# --- RECEIPTS ---
panel_receipts <- df_clean %>%
  dplyr::select(INDIVIDU, MONTH, RECEIPTS, TREATMENT) %>%
  filter(!is.na(RECEIPTS)) %>%
  arrange(INDIVIDU, MONTH)

# ⚖️ Identifier les unités avec données complètes
nb_periods <- length(unique(panel_receipts$MONTH))

valid_units <- panel_receipts %>%
  group_by(INDIVIDU) %>%
  summarise(n_obs = n()) %>%
  filter(n_obs == nb_periods) %>%
  pull(INDIVIDU)

# 🧼 Garder panel équilibré
panel_receipts_balanced <- panel_receipts %>%
  filter(INDIVIDU %in% valid_units)

# 🧪 Matrice synthdid
Ymat_receipts <- panel.matrices(as.data.frame(panel_receipts_balanced),
                                unit = 1, time = 2, outcome = 3, treatment = 4)

# 📈 Estimation SDiD
tau_sdid_receipts <- synthdid_estimate(Ymat_receipts$Y, Ymat_receipts$N0, Ymat_receipts$T0)

# 📝 Résumé
#summary(tau_sdid_receipts)
plot(tau_sdid_receipts)

# --- SHIPMENTS ---
panel_shipments <- df_clean %>%
  dplyr::select(INDIVIDU, MONTH, SHIPMENTS, TREATMENT) %>%
  filter(!is.na(SHIPMENTS)) %>%
  arrange(INDIVIDU, MONTH)

nb_periods_ship <- length(unique(panel_shipments$MONTH))

valid_units_ship <- panel_shipments %>%
  group_by(INDIVIDU) %>%
  summarise(n_obs = n()) %>%
  filter(n_obs == nb_periods_ship) %>%
  pull(INDIVIDU)

panel_shipments_balanced <- panel_shipments %>%
  filter(INDIVIDU %in% valid_units_ship)

Ymat_shipments <- panel.matrices(as.data.frame(panel_shipments_balanced),
                                 unit = 1, time = 2, outcome = 3, treatment = 4)

tau_sdid_ship <- synthdid_estimate(Ymat_shipments$Y, Ymat_shipments$N0, Ymat_shipments$T0)




# Open commitment



# 📁 Charger les données
oc_data <- read_excel("open_commitment_1920s.xlsx", sheet = "OC3")

# 🧼 Nettoyage et construction de la date
oc_data <- oc_data %>%
  mutate(
    Date = as.Date(paste(Year, Month, Day, sep = "-")),
    Treatment = ifelse(City == "Chicago", 1, 0)
  ) %>%
  filter(!is.na(Open_Commitment))  # garde seulement les lignes valides

## Vérification des tendances parallèles


# 📆 Données pré-traitement uniquement
oc_pre <- oc_data %>%
  filter(Date < as.Date("1926-02-01")) %>%
  mutate(time_to_treat = as.numeric(Date - as.Date("1925-01-01")))

# ✅ Vérification de la distribution des groupes
print(table(oc_pre$City))
print(table(oc_pre$Treatment))

# 📊 Régression pour tester les tendances parallèles
model_parallel <- lm(Open_Commitment ~ time_to_treat + Treatment + time_to_treat*Treatment, data = oc_pre)
result <- tidy(model_parallel) %>% filter(term == "time_to_treat:Treatment")
print(result)

# 🔍 Interprétation simple
if (nrow(result) > 0) {
  pval <- result$p.value
  cat("\n--- Interprétation ---\n")
  if (pval < 0.05) {
    cat("⚠️ Tendance non parallèle détectée (p =", round(pval, 3), ")\n")
  } else {
    cat("✅ Hypothèse de tendances parallèles acceptable (p =", round(pval, 3), ")\n")
  }
}

# 📈 Visualisation des tendances pré-traitement par ville
plot_data <- oc_pre %>%
  group_by(Date, City) %>%
  summarise(mean_oc = mean(Open_Commitment, na.rm = TRUE), .groups = "drop")

ggplot(plot_data, aes(x = Date, y = mean_oc, color = City)) +
  geom_line(size = 1) +
  labs(
    x = "Date", y = "Open Commitments") +
  theme_minimal() +
  theme(legend.position = "bottom")


# 🖼 UI
ui <- navbarPage(
  title = "Analyse mémoire",
  windowTitle = "Mémoire - Contrats à terme",
  
  header = tags$head(
    tags$style(HTML("
      /* --- NAVBAR --- */
      .navbar {
        background-color: #002b5c !important;
        border-bottom: 3px solid #00509e;
      }
      .navbar .navbar-brand,
      .navbar .navbar-nav > li > a {
        color: white !important;
        font-weight: bold;
      }
      .navbar .navbar-nav > .active > a {
        background-color: #004080 !important;
        color: white !important;
      }

      /* --- TABLEAUX DT --- */
      table.dataTable {
        background-color: #f9f9f9;
        font-size: 14px;
        border: none;
      }
      .dataTables_wrapper .dataTables_filter input {
        border: 1px solid #ccc;
        border-radius: 4px;
      }
      .dataTables_wrapper .dataTables_length select {
        border-radius: 4px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        background: #004080;
        color: white !important;
        border-radius: 4px;
        margin: 2px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: #005cbf;
      }

      /* --- BOUTONS --- */
      .btn {
        background-color: #004080;
        color: white;
        font-weight: bold;
        border-radius: 5px;
        border: none;
      }
      .btn:hover {
        background-color: #005cbf;
      }
    "))
  ),
  tabPanel("Accueil",
           fluidPage(
             titlePanel("L’impact de l’introduction de la novation sur les marchés à terme agricoles : une analyse économétrique du cas du Chicago Board of Trade en 1926"),
             
             br(),
             p("Ce mémoire analyse l'effet de l’introduction de la novation en février 1926 sur les marchés à terme agricoles américains, en s’appuyant sur des données historiques provenant des bourses de Chicago, Kansas City et St. Louis. À travers une approche économétrique combinant modèles à effets fixes (TWFE), différences-en-différences synthétiques (SDiD) et tests de tendances parallèles, l’étude évalue l’impact de cette réforme juridique sur trois dimensions essentielles : les prix à terme, les flux physiques (réceptions et expéditions) et les engagements ouverts. L’application interactive développée permet de visualiser ces effets, de comparer les dynamiques entre marchés, et de mieux comprendre les mécanismes institutionnels à l’œuvre dans les années 1920."),
             
             br(), br(),
             
             div(style = "display: flex; justify-content: center; align-items: center; margin-top: 40px;",
                 div(style = "background-color: #002b5c; padding: 25px 40px; border-radius: 12px; max-width: 600px; width: 100%; text-align: left;",
                     h3("📘 Sommaire de l'application", style = "color: white; text-align: center; font-size: 26px; font-weight: bold; margin-bottom: 20px;"),
                     tags$ul(style = "list-style-type: none; padding-left: 0; color: white; font-size: 18px;",
                             tags$li(HTML("<b>5.1 - Prix à terme</b>"),
                                     tags$ul(style = "list-style-type: disc; padding-left: 20px;",
                                             tags$li("Évolution des prix"),
                                             tags$li("Volatilité"),
                                             tags$li("Tendances parallèles")
                                     )
                             ),
                             tags$li(HTML("<b>5.2 - Flux physiques</b>"),
                                     tags$ul(style = "list-style-type: disc; padding-left: 20px;",
                                             tags$li("Évolution globale"),
                                             tags$li("Tendances parallèles"),
                                             tags$li("Effet du traitement (SDiD)")
                                     )
                             ),
                             tags$li(HTML("<b>5.3 - Engagements ouverts</b>"),
                                     tags$ul(style = "list-style-type: disc; padding-left: 20px;",
                                             tags$li("Évolution des engagements"),
                                             tags$li("Tendances parallèles")
                                     )
                             )
                     )
                 )
             ),
             br(), br(),  br(), 
             div(style = "text-align: center; margin-top: 100px;",
                 tags$h2("Auteur : Pierre QUINTIN de KERCADIO"),
                 tags$h2("Date : JUIN 2025")
             )
           )
  ), # ← fin du tabPanel "Accueil"
  
  
  
  tabPanel("5.1 - Prix à terme",
           tabsetPanel(
             tabPanel("Évolution des prix",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("commodity", "Matière première :", choices = NULL),
                          selectInput("echeance", "Échéance :", choices = NULL),
                          p("Cette visualisation présente l’évolution des prix de clôture des contrats à terme pour une matière première et une échéance données, sur les marchés de Chicago (CHI), Kansas City (KC) et St. Louis (STL). La ligne verticale en pointillés marque le début de la période post-réforme (février 1926), afin de comparer visuellement l'effet du Grain Futures Act sur les niveaux de prix."),
                          
                        ),
                        mainPanel(
                          plotOutput("price_plot_all", height = "600px")
                        )
                      )
             ),
             tabPanel("Volatilité",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("vol_type", "Choisir la maturité :",
                                      choices = c("Mai", "Juillet", "Général"), selected = "Mai"), 
                          p("Ce tableau compare la volatilité moyenne des contrats à terme avant et après la réforme de 1926 pour différentes combinaisons ville–matière–échéance. La volatilité est mesurée par l’écart-type des prix de clôture. L’objectif est de détecter un éventuel effet stabilisateur de la novation sur les marchés traités."),
                          
                        ),
                        mainPanel(
                          h5("Volatilité moyenne par contrat"),
                          DTOutput("volatility_summary", height = "300px")
                        )
                      )
             ),
             
             
             tabPanel("Tendances parallèles",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("trend_echeance", "Choisir l'échéance :",
                                      choices = c("MAY", "JULY"), selected = "MAY"),
                          p("Ce graphique compare les tendances pré-traitement des prix à terme entre Chicago (groupe traité) et les autres villes (groupe contrôle) pour l’échéance sélectionnée. Il permet de vérifier visuellement l’hypothèse des tendances parallèles avant la réforme de 1926, condition nécessaire à l’estimation crédible d’un effet causal."),
                          
                        ),
                        mainPanel(
                          plotOutput("trend_plot", height = "600px"),
                          
                        )
                      )
             )
           ) # ← fin du tabsetPanel
  ),    # ← fin du tabPanel "5.1"
  
  tabPanel("5.2 - Flux physiques",
           tabsetPanel(
             
             tabPanel("Évolution globale",
                      fluidPage(
                        titlePanel("Flux physiques à Chicago vs autres villes"),
                        p("Cette visualisation présente l’évolution mensuelle moyenne des flux physiques — entrées (RECEIPTS) et sorties (SHIPMENTS) — pour le blé dans différentes villes du Midwest. Elle permet de comparer la dynamique de Chicago (ville traitée) avec celle des villes témoins (Duluth, Kansas City, Milwaukee, Minneapolis) entre 1924 et 1928. Ces courbes sont utiles pour identifier les ruptures ou anomalies potentielles autour de l’introduction de la novation en février 1926."),
                        
                        plotOutput("flux_plot"),
                        
                       
                      )
             ),
             
             tabPanel("Tendances parallèles",
                      fluidPage(
                        titlePanel("Tendances pré-traitement des flux physiques"),
                        p("Ce graphique examine les tendances des flux physiques (RECEIPTS et SHIPMENTS) avant l’introduction de la novation en février 1926. Il permet de tester visuellement l’hypothèse des tendances parallèles, essentielle pour toute estimation crédible de type Diff-in-Diff ou SDiD."),
                        p("En comparant l’évolution à Chicago (groupe traité) avec celle d’un groupe de contrôle composé d’autres villes, on cherche à s’assurer que les différences observées après le traitement ne sont pas dues à des dynamiques antérieures divergentes."),
                        
                        plotOutput("flux_trend_plot"),
                          )
             ),
             
             tabPanel("Effet du traitement (SDiD)",
                      fluidPage(
                        titlePanel("Effet du traitement sur les flux physiques (SDiD)"),
                        p("Cette section présente les résultats de l'estimation de l'effet causal de la réforme (novation) sur les flux physiques à l'aide de la méthode Synthetic Difference-in-Differences (SDiD)."),
                        p("Les graphiques comparent le groupe traité (Chicago) avec une version synthétique de contrôle construite à partir des autres villes. La distance verticale entre les deux courbes après février 1926 mesure l'effet estimé."),
                        p("L'utilisation de SDiD permet de renforcer la validité des inférences causales en combinant les forces des méthodes synthétiques et des modèles à effets fixes."),
                        
                        fluidRow(
                          column(6, plotOutput("sdid_receipts_plot")),
                          column(6, plotOutput("sdid_shipments_plot"))
                        ),
               
                      )
             )
           )
  ),
  
  
  tabPanel("5.3 - Engagements ouverts",
           tabsetPanel(
             
             tabPanel("Évolution des engagements ouverts",
                      fluidPage(
                        titlePanel("Évolution des engagements ouverts"),
                        p("Ce graphique illustre l'évolution des engagements ouverts sur les marchés à terme pour le blé et le maïs entre 1924 et 1928."),
                        p("L’analyse distingue Chicago, principal marché traité, des autres places comme Kansas City et Minneapolis."),
                        p("Une ligne verticale marque la date de mise en place de la réforme (février 1926), ce qui permet de visualiser les changements structurels potentiels."),
                        
                        plotOutput("oc_plot", height = "600px"),
               
                      )
             ),
             
             tabPanel("Tendances parallèles",
                      fluidPage(
                        titlePanel("Tendances parallèles sur les Open Commitments (pré-traitement)"),
                        p("Ce graphique explore les tendances pré-traitement des engagements ouverts sur les marchés à terme."),
                        p("Il compare l’évolution des positions ouvertes à Chicago (traité) et dans les villes de contrôle (Kansas City et Minneapolis) avant février 1926."),
                        p("L'objectif est de valider l'hypothèse des tendances parallèles, condition essentielle à l'identification causale dans les modèles de différence-en-différences."),
                        
                        plotOutput("trend_plot_oc", height = "600px"),
                        verbatimTextOutput("trend_text_oc")
                      )
             )
           )
  
  )
)



# Serveur
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "commodity", choices = unique(prices_data$Commodity))
  })
  
  observeEvent(input$commodity, {
    echeances <- prices_data %>%
      filter(Commodity == input$commodity) %>%
      pull(Echeance) %>%
      unique()
    updateSelectInput(session, "echeance", choices = echeances)
  })
  
  output$price_plot_all <- renderPlot({
    req(input$commodity, input$echeance)
    
    df_plot <- prices_data %>%
      filter(Commodity == input$commodity, Echeance == input$echeance) %>%
      mutate(Date = as.Date(as.character(Date)))
    
    validate(need(nrow(df_plot) > 0, "Aucune donnée disponible."))
    
    ggplot(df_plot, aes(x = Date, y = Close, color = City)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = as.Date("1926-02-01"), linetype = "dashed") +
      labs(title = paste("Prix pour", input$commodity, input$echeance),
           x = "Date", y = "Prix de clôture") +
      theme_minimal()
  })
  
  output$did_result <- renderTable({
    req(input$commodity, input$echeance)
    
    df <- prices_data %>%
      filter(Commodity == input$commodity, Echeance == input$echeance) %>%
      mutate(Date = as.Date(as.character(Date)))
    
    validate(need(nrow(df) > 0, "Pas de données pour ce sous-échantillon."))
    
    model <- tryCatch(
      plm(Close ~ Treat_Post, data = pdata.frame(df, index = c("Indiv", "Date")),
          model = "within", effect = "twoways"),
      error = function(e) return(NULL)
    )
    
    if (is.null(model)) return(data.frame(Message = "Modèle non estimable"))
    
    broom::tidy(coeftest(model, vcovHC(model, type = "HC1"))) %>%
      filter(term == "Treat_Post")
  })
  
  output$volatility_summary <- renderDT({
    vol_data <- prices_data %>%
      mutate(
        Période = ifelse(Date < as.Date("1926-02-01"), "Avant", "Après")
      ) %>%
      group_by(Indiv, Période) %>%
      summarise(Volatilité = sd(Close, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = Période, values_from = Volatilité) %>%
      mutate(Différence = Après - Avant)
    
    # Sélection selon l'input
    filtered <- switch(input$vol_type,
                       "Mai" = vol_data %>% filter(grepl("MAY$", Indiv)),
                       "Juillet" = vol_data %>% filter(grepl("JULY$", Indiv)),
                       "Général" = vol_data)
    
    datatable(filtered, rownames = FALSE,
              options = list(pageLength = 10, dom = 'tip')) %>%
      formatRound(columns = c("Avant", "Après", "Différence"), digits = 4)
  })
  
  
  
output$spread_summary <- renderDT({
  df <- prices_data %>%
    mutate(Date = as.Date(Date))

  spread_filtered <- switch(input$vol_type,
                            "Mai" = df %>% filter(Echeance == "MAY"),
                            "Juillet" = df %>% filter(Echeance == "JULY"),
                            "Général" = df
  )

  chicago <- spread_filtered %>%
    filter(City == "CHI") %>%
    select(Date, Commodity, Echeance, Close) %>%
    rename(Chicago_Close = Close)

  autres <- spread_filtered %>%
    filter(City != "CHI")

  spread_data <- merge(autres, chicago, by = c("Date", "Commodity", "Echeance")) %>%
    mutate(
      Spread = Close - Chicago_Close,
      Période = ifelse(Date < as.Date("1926-02-01"), "Avant", "Après")
    ) %>%
    group_by(City, Période) %>%
    summarise(Spread_Moyen = mean(Spread, na.rm = TRUE), .groups = "drop") %>%
    mutate_if(is.numeric, round, 4)  # ⚠️ Important pour JSON compatibilité

  datatable(as.data.frame(spread_data), rownames = FALSE,
            options = list(pageLength = 10, dom = 'tip'))
})

  
  
  
  output$flux_plot <- renderPlot({
    flux_data %>%
      group_by(MONTH, CITY) %>%
      summarise(RECEIPTS = mean(RECEIPTS), SHIPMENTS = mean(SHIPMENTS), .groups = "drop") %>%
      pivot_longer(cols = c(RECEIPTS, SHIPMENTS), names_to = "Type", values_to = "Valeur") %>%
      ggplot(aes(x = MONTH, y = Valeur, color = CITY)) +
      geom_line() +
      facet_wrap(~Type, scales = "free_y") +
      theme_minimal()
  })
  
  output$flux_stats <- renderTable({
    flux_data %>%
      group_by(CITY) %>%
      summarise(Moy_Receipts = mean(RECEIPTS), Moy_Shipments = mean(SHIPMENTS), .groups = "drop")
  })
  
  output$oc_plot <- renderPlot({
    df <- oc_data %>%
      mutate(Date = as.Date(as.character(Date)))
    
    ggplot(df, aes(x = Date, y = Open_Commitment, color = City)) +
      geom_line() +
      geom_vline(xintercept = as.Date("1926-02-01"), linetype = "dashed") +
      labs(x = "Date", y = "Engagements ouverts") +
      theme_minimal()
  })
  
  output$oc_stats <- renderTable({
    oc_data %>%
      group_by(City) %>%
      summarise(Moyenne = mean(Open_Commitment), .groups = "drop")
  })
  
  output$oc_did_result <- renderTable({
    df <- oc_data %>%
      mutate(Date = as.Date(as.character(Date)))
    
    model <- tryCatch(
      plm(Open_Commitment ~ Treat_Post, data = pdata.frame(df, index = c("Indiv", "Date")),
          model = "within", effect = "twoways"),
      error = function(e) return(NULL)
    )
    
    if (is.null(model)) return(data.frame(Message = "Modèle non estimable"))
    
    broom::tidy(coeftest(model, vcovHC(model, type = "HC1"))) %>%
      filter(term == "Treat_Post")
  })

  output$trend_plot <- renderPlot({
    req(input$trend_echeance)
    
    df <- prices_data %>%
      filter(Echeance == input$trend_echeance, 
             Commodity %in% c("WHEAT", "CORN"), 
             Date < as.Date("1926-02-01"))
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    trend_data <- df %>%
      mutate(Groupe = ifelse(City == "CHI", "Traitement", "Contrôle")) %>%
      group_by(Date, Commodity, Groupe) %>%
      summarise(Prix = mean(Close, na.rm = TRUE), .groups = "drop")
    
    ggplot(trend_data, aes(x = Date, y = Prix, color = Groupe)) +
      geom_line(linewidth = 1.2) +
      facet_wrap(~Commodity, scales = "free_y") +
      labs(
        title = paste("Tendances pré-traitement - Échéance", input$trend_echeance),
        x = "Date", y = "Prix de clôture"
      ) +
      theme_minimal()
  })
  
  output$flux_trend_plot <- renderPlot({
    flux_data %>%
      filter(MONTH < as.Date("1926-02-01")) %>%
      mutate(Groupe = ifelse(CITY == "Chicago", "Traitement", "Contrôle")) %>%
      group_by(MONTH, Groupe) %>%
      summarise(Receipts = mean(RECEIPTS),
                Shipments = mean(SHIPMENTS),
                .groups = "drop") %>%
      pivot_longer(cols = c(Receipts, Shipments), names_to = "Type", values_to = "Valeur") %>%
      ggplot(aes(x = MONTH, y = Valeur, color = Groupe)) +
      geom_line(linewidth = 1.1) +
      facet_wrap(~Type, scales = "free_y") +
      labs(x = "Date", y = "Valeur", title = "Tendances pré-traitement des flux physiques") +
      theme_minimal()
  })
  
  output$sdid_receipts_plot <- renderPlot({
    plot(tau_sdid_receipts)
  })
  
  output$sdid_shipments_plot <- renderPlot({
    plot(tau_sdid_ship)
  })
  # 📁 Charger et préparer les données si pas déjà fait (à mettre globalement si tu veux éviter doublons)
  oc_data <- read_excel("open_commitment_1920s.xlsx", sheet = "OC3") %>%
    mutate(
      Date = as.Date(paste(Year, Month, Day, sep = "-")),
      Treatment = ifelse(City == "Chicago", 1, 0)
    ) %>%
    filter(!is.na(Open_Commitment))
  
  # 📅 Données pré-traitement
  oc_pre <- oc_data %>%
    filter(Date < as.Date("1926-02-01")) %>%
    mutate(time_to_treat = as.numeric(Date - as.Date("1925-01-01")))
  
  # 📈 Graphique
  output$trend_plot_oc <- renderPlot({
    plot_data <- oc_pre %>%
      group_by(Date, City) %>%
      summarise(mean_oc = mean(Open_Commitment, na.rm = TRUE), .groups = "drop")
    
    ggplot(plot_data, aes(x = Date, y = mean_oc, color = City)) +
      geom_line(size = 1) +
      labs(x = "Date", y = "Open Commitments") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # ✅ Texte interprétatif
  output$trend_text_oc <- renderText({
    model <- lm(Open_Commitment ~ time_to_treat + Treatment + time_to_treat * Treatment, data = oc_pre)
    result <- broom::tidy(model) %>% filter(term == "time_to_treat:Treatment")
    
    if (nrow(result) > 0 && !is.na(result$p.value)) {
      pval <- result$p.value
      if (pval < 0.05) {
        return(paste("⚠️ Tendance non parallèle détectée (p =", round(pval, 3), ")"))
      } else {
        return(paste("Hypothèse de tendances parallèles acceptable (p =", round(pval, 3), ")"))
      }
    } else {
      return("⚠️ Résultat non interprétable : interaction non estimée.")
    }
  })
  
  
  
  
  
}

# 🚀 Lancer
shinyApp(ui = ui, server = server)
