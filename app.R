#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(openintro)) install.packages("openintro")
if (!require(plotly)) install.packages("plotly")
if (!require(rnaturalearth)) install.packages("rnaturalearth")
if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
library(tidyverse)
library(openintro)
library(plotly)
library(readxl)
library(dplyr)
library("rnaturalearth")
library("rnaturalearthdata")
scholarly_publications_origin <- read_csv("scholarly-publications-on-artificial-intelligence-per-million-people.csv")
ai_performance <- read_csv("ai-performance-knowledge-tests-vs-training-computation.csv")
annual_patent_origin <- read_csv("artificial-intelligence-patents-submitted-per-million.csv")

my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

filter_map <- function(year) {
  scholarly_publications <- scholarly_publications_origin |>
    filter(Year == year) |>
    rename(nPublications = `Number of articles per 1 million people - Field: All`) |>
    mutate(text1 = paste("<b>", Entity, "</b>\nNumber of Articles:", ifelse(round(nPublications) == 0, "<1", round(nPublications)),
                         "per 1 million people")) |>
    mutate(nPublications = as.numeric(nPublications))
  
  annual_patent <- annual_patent_origin |>
    filter(Year == year) |>
    rename(patents = `Patent applications per 1 million people - Field: All`) |>
    mutate(text2 = paste("<b>", Entity, "</b>\nNumber of Patents:", ifelse(round(patents) == 0, "<1", round(patents)),
                         "per 1 million people")) |>
    mutate(patents = as.numeric(patents))
  
  result <- left_join(scholarly_publications, annual_patent, by = c("Entity", "Code", "Year"))
  return(result)
}

tabRender <- function(tab, variable, year) {
  if (tab == "1") {
    result <- map_plot(variable, year)
  } else {
    result <- scatter_plot()
  }
  return(result)
}

map_plot <- function(var_name, year) {
  my_var <- enquo(var_name)
  rhs <- filter_map(year)
  rhs <- rhs |>  
    mutate(text = case_when(
      var_name == "nPublications" ~ text1,
      var_name == "patents" ~ text2
    )) |>
    select(Entity, Code, Year, var = !!my_var, text)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  world_publications <- left_join(world, rhs, by = c("iso_a3_eh" = "Code"))
  
  if (var_name == "nPublications") {
    plot_world_publications <- ggplot(world_publications) +
      geom_sf(aes(fill = var + runif(nrow(world_publications), min = 0, max = 0.001), text = text), color = "black") +
      scale_fill_continuous(
        low = "#f5b0cc",
        high = "red",
        trans = "log10",
        limits = c(0.1, 1000),
        breaks = c(0.1, 1, 10, 100, 1000),
        labels = c("0", "1", "10", "100", "1000"),
        na.value = "gray",
      ) +
      my_map_theme() +
      labs(fill = "Number of Articles", title = paste("Scholarly Publications on Artificial Intelligence\nPer Million People", year))
    
    ggplotly(plot_world_publications, tooltip = "text") |>
      style(hoveron = "fill")
  } else {
    plot_world_publications <- ggplot(world_publications) +
      geom_sf(aes(fill = var + runif(nrow(world_publications), min = 0, max = 0.001), text = text), color = "black") +
      scale_fill_continuous(
        low = "#f5b0cc",
        high = "red",
        na.value = "gray",
      ) +
      my_map_theme() +
      labs(fill = "Number of Articles", title = paste("Annual Patent Applications Related To AI Per Million People", year))
    
    ggplotly(plot_world_publications, tooltip = "text") |>
      style(hoveron = "fill")
  }
}

scatter_plot <- function() {
  ai_performance <- ai_performance |>
    mutate(text = paste("<b>", Entity, "</b>\nTraining computation (petaFLOP):", 
                        format(`Training computation (petaFLOP)`, scientific = FALSE, big.mark = ","),
                        "\nKnowledge Test (MMLU):", `MMLU avg`, "%",
                        "\nTraining Dataset Size:", format(`Training dataset size`, scientific = FALSE, big.mark = ",")))
  
  ai_performance_interactive <- ggplot(ai_performance, aes(x = `Training computation (petaFLOP)`, y = `MMLU avg`)) +
    geom_point(aes(size = `Training dataset size`, color = Organization, text = text), alpha = 0.6, na.rm = TRUE) +
    scale_y_continuous(
      name = "Knowledge Test (MMLU)",
      limits = c(0, 100),
      breaks = c(0, 20, 40, 60, 80, 100),
      labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
      minor_breaks = NULL,
    ) +
    scale_x_log10(name = "Training Computation (petaFLOP)",
                  limits = c(100000, 100000000000),
                  minor_breaks = NULL,
                  breaks = c(100000, 1000000, 10000000, 100000000, 1000000000, 10000000000, 100000000000),
                  labels = c("100,000", "1 million", "10 million", "100 million", "1 billion", "10 billion", "100 billion")) +
    theme(
      legend.box.margin = margin(t = 20)  # Adjust margin to move legend box down
    ) +
    guides(color = guide_legend(override.aes = list(size = 3)), size = FALSE) +
    labs(title = "Artificial intelligence:\nPerformance on knowledge tests vs. training computation") +
    scale_size(
      name = "Training Dataset Size",
      labels = scales::number_format(scale = 1, accuracy = 1),
      range = c(1, 10),
    )
  
  ggplotly(ai_performance_interactive, tooltip = "text") |>
    style(hoveron = "fill")
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Final Project: Artificial Intelligence"),
    
    tabsetPanel(id = "tabs",
                tabPanel(title = "Publications/Patents", value = "1"),
                sidebarPanel(
                  selectInput("variable",
                              "Statistic to Plot:",
                              choices = list("Number of Scholarly Publications on Artificial Intelligence" = "nPublications",
                                             "Artificial Intelligence Patents" = "patents"), 
                              selected = "nPublications")),
                sidebarPanel(sliderInput("year",
                                         "Timelapse:",
                                         min = 2010,
                                         max = 2021,
                                         value = 2021,
                                         sep = "")),
                tabPanel(title = "AI Performance", value = "2"),
    ),
            

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot"),
           h5("Data source: Artificial Intelligence",
              tags$a(href="https://cat.eto.tech/",
                     "Learn more about this data.")),
           h5(tags$a(href="https://ourworldindata.org/artificial-intelligence",
                     "Ourworldindata.org/artificial-intelligence")),
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        tabRender(tab = input$tabs, variable = input$variable, year = as.integer(input$year))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
