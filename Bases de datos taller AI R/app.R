library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

data_candidates <- c(
  "Rendimiento.xlsx",
  file.path("Bases de datos taller AI R", "Rendimiento.xlsx"),
  "Rendimientos_decada.xlsx",
  file.path("Bases de datos taller AI R", "Rendimientos_decada.xlsx")
)

data_file <- data_candidates[file.exists(data_candidates)][1]

if (is.na(data_file)) {
  stop("No se encontró Rendimiento.xlsx ni Rendimientos_decada.xlsx.")
}

rendimientos <- read_excel(
  data_file,
  sheet = "rendimiento",
  na = c("", "NA", "N/A", "SD")
)

required_columns <- c(
  "Cultivo", "Campaña", "Sup. Sembrada", "Sup. Cosechada"
)
missing_columns <- setdiff(required_columns, names(rendimientos))

if (length(missing_columns) > 0) {
  stop(
    "Faltan columnas requeridas en la planilla: ",
    paste(missing_columns, collapse = ", ")
  )
}

rendimientos <- rendimientos %>%
  filter(!is.na(Cultivo), !is.na(Campaña))

cultivos <- sort(unique(rendimientos$Cultivo))
campanias <- unique(as.character(rendimientos$Campaña))

sum_observed <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

ui <- fluidPage(
  titlePanel("Superficie sembrada y cosechada por campaña"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "cultivo",
        "Seleccioná un cultivo:",
        choices = cultivos,
        selected = cultivos[1]
      )
    ),
    mainPanel(
      plotOutput("superficies", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  datos_cultivo <- reactive({
    rendimientos %>%
      filter(Cultivo == input$cultivo) %>%
      group_by(Campaña) %>%
      summarise(
        `Sup. Sembrada` = sum_observed(`Sup. Sembrada`),
        `Sup. Cosechada` = sum_observed(`Sup. Cosechada`),
        .groups = "drop"
      ) %>%
      mutate(Campaña = factor(as.character(Campaña), levels = campanias)) %>%
      pivot_longer(
        cols = c(`Sup. Sembrada`, `Sup. Cosechada`),
        names_to = "Superficie",
        values_to = "Hectáreas"
      ) %>%
      group_by(Superficie) %>%
      mutate(Segmento = cumsum(is.na(Hectáreas))) %>%
      ungroup()
  })

  output$superficies <- renderPlot({
    ggplot(
      datos_cultivo(),
      aes(
        x = Campaña,
        y = Hectáreas,
        color = Superficie,
        group = interaction(Superficie, Segmento)
      )
    ) +
      geom_line(na.rm = TRUE) +
      geom_point(na.rm = TRUE) +
      labs(
        x = "Campaña",
        y = "Superficie (hectáreas)",
        color = NULL
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
