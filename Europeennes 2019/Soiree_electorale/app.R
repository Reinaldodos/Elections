

library(shiny)
source(file = "Fetch.R")

# Define UI for application that draws a histogram -----

ui <- fluidPage(# Application title
  titlePanel(textOutput(outputId = "time")),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "Selection",
        label = "Listes sélectionnées",
        selected = as.character(SAFE),
        choices = levels(Results$Listes)
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "graphique"),
      plotOutput(outputId = "tableau")
    )
  ))

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # source(file = "Fetch.R")

  output$time = renderText({
    paste("Elections européennes",
          Sys.time(),
          sep = " ")
  })

  output$graphique <- renderPlot({
    require(tidyverse)
    plot =
      Results %>% data.table() %>%
      filter(Listes %in% input$Selection) %>%
      mutate(DOM = case_when(DOM ~ "DOM",
                             TRUE ~ "Métropole")) %>%
      ggplot(mapping = aes(y = Score, x = Inscrits, colour = Listes)) +
      geom_smooth() +
      scale_x_log10() +
      theme(legend.position = "none") +
      ylim(0,NA)+
      facet_wrap(DOM ~ ., scales = "free")

    print(plot)
  })

  output$tableau <- renderPlot({
    require(tidyverse)
    TOP =
      Tableau %>%
      filter(Listes %in% input$Selection)

    TOP$Listes = factor(x = TOP$Listes,
                        levels = intersect(levels(TOP$Listes), TOP$Listes))

    TOP =
      TOP %>%
      ggplot(mapping = aes(x = Listes, y = TOT, fill = Listes)) +
      geom_bar(stat = "identity") +
      theme(legend.position = "none") +
      geom_hline(yintercept = c(0.03 * Cut, 0.05 * Cut)) +
      scale_x_discrete(limits = rev(levels(TOP$Listes))) +
      ylab("Total des voix") +
      coord_flip() +
      theme(axis.text.y = element_blank())

    print(TOP)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
