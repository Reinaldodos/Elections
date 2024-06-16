library(shiny)
library(arrow)
library(dplyr)
library(DT)
library(dendextend)
library(tidyr)

# Chemin vers le fichier Parquet
file_path <- "data/donnees.parquet"

source(file = "Fonctions.R")

# Lire le fichier Parquet et extraire les circonscriptions
circonscriptions_data <- read_parquet(file_path)

circonscriptions <-
  circonscriptions_data %>%
  distinct(code_du_departement, code_de_la_circonscription) %>%
  tidyr::unite(col = circo,
               code_du_departement,
               code_de_la_circonscription,
               sep = "") %>%
  pull(circo)

# UI de l'application
ui <- fluidPage(
  titlePanel("Choisir une circonscription"),
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Réduire la largeur du side panel
      selectInput("circonscription",
                  "Circonscription:",
                  choices = circonscriptions),
      actionButton("apply", "Appliquer"),
      sliderInput("Nb_clusters",
                  "Nombre de clusters:",
                  min = 1, max = 10, value = 1)
    ),
    mainPanel(
      width = 9,  # Ajuster la largeur du main panel pour combler l'espace
      tabsetPanel(
        tabPanel("Dendrogramme", plotOutput("hc_KOR")),
        tabPanel("Scores", plotOutput("scores")),
        tabPanel("Groupes", DT::DTOutput("Groupes"))
      )
    )
  )
)

# Serveur de l'application
server <- function(input, output, session) {
  observeEvent(input$apply, {
    # Appeler la fonction de traitement avec les données et la circonscription sélectionnée
    input_circo <-
      get_data_circo(input = circonscriptions_data,
                     circo = input$circonscription)

    data <-
      input_circo %>%
      unite_data()

    hc_KOR <-
      data %>%
      get_CAH()

    output$hc_KOR <- renderPlot({
      toto <-
        hc_KOR %>%
        as.dendrogram() %>%
        dendextend::set("branches_k_color",
                        k = input$Nb_clusters) %>%
        dendextend::plot_horiz.dendrogram()

      print(toto)
    })

    Groupes <-
      hc_KOR %>%
      get_clusters(Nb_clusters = input$Nb_clusters)

    output$Groupes <- DT::renderDataTable(expr = {
      Groupes %>%
        get_clusters_libelles(input_circo = input_circo)
    }, filter = "top")

    output$scores <- renderPlot({
      data %>%
        get_data_score(Groupes = Groupes) %>%
        get_plot_groupes()
    })
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
