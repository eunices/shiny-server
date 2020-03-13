# Shiny App: Visualising resource flow - from taxonomist country of residence to type locality

# Libraries
library(shiny)
library(data.table)
library(networkD3)


# UI component of shiny app
shinyUI(fluidPage(
  verticalLayout(
    inputPanel(
      sliderInput("thresholdInput", "Minimum threshold", 
                  min=0, max=floor(sort(t$N)[length(t$N)-1]/10)*10, 
                  value=DEFAULT_THRESHOLD, post=" N spp.",
                  step=5),
      selectInput("nodeColourInput", "Node colour by",
                  c("Country", "Colonialism", "Economic status"), selected=NULL, multiple=F),
      selectInput("countryInput", "Country", # sorted by Discover Life codes
                  c("All", nodes_lab), selected=NULL, multiple=F),
      conditionalPanel(
        condition = "input.countryInput != 'All'",
        checkboxGroupInput("typeInput", "Type (Source or Target)", 
                          choices = list("Source" = 1, "Target" = 2),
                          selected = c(1, 2))
      )
    ),
    mainPanel(
      sidebarLayout(
        sidebarPanel(tableOutput("networkTable"),
                     "Legend:", 
                     "Src = Source i.e., country of taxonomist", 
                     "Tgt = Target i.e. country of type species", 
                     "N = N species described",
                     width=2), 
        mainPanel(forceNetworkOutput("network", height="500px"), width=10)
      ),
      width=12
    )
  )
))
