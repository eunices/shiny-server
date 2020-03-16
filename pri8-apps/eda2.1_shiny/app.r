# Shiny App: Visualising resource flow - from taxonomist country of residence to type locality

# Libraries
library(shiny)
library(data.table)
library(networkD3)

# Constants / parameters
DEFAULT_THRESHOLD <- 0

# Read data (residing in data folder)
# setwd('2019-06-19-ascher-type-data/eda2.1_shiny')
t <- fread("data/7.0-author-networks.csv", encoding="UTF-8", na.strings="")
auth <- fread("data/authors.csv", encoding="UTF-8", na.strings="")
t <- merge(t, auth[, c('last.name', 'full.name.of.describer.n')], by.x="p1", by.y="full.name.of.describer.n", 
           all.x=T, all.y=F)
t <- merge(t, auth[, c('last.name', 'full.name.of.describer.n')], by.x="p2", by.y="full.name.of.describer.n", 
           all.x=T, all.y=F, suffixes=c("_p1", "_p2"))
t$p1 <- NULL; t$p2 <- NULL
names(t) <- gsub("last.name_", "", names(t))

# Create labels for authors
nodes_lab <- sort(auth[last.name %in% unique(c(t$p1, t$p2))]$last.name)

# Update data function
update <- function(threshold=DEFAULT_THRESHOLD, author=NA) {
  
  t2 <- t[N>threshold]

  # Subset dataset
  if (!(is.na(author) | author == "All")) { t2 <- t2[(p1 == author | p2 == author)] }

  # Create network dataset - indexing from 0
  if (dim(t2)[1] == 0) {

    nodes <- data.frame(idx=integer(), label=character())
    t2 <- data.frame(ori=character(), des=character())

  } else {

    nodes <- unique(data.frame(label=c(t2$p1, t2$p2)))
    nodes <- data.table(nodes); nodes <- nodes[order(label)]
    nodes$idx <- seq(0, dim(nodes)[1]-1, 1)
    nodes <- merge(nodes, auth, by.x="label", by.y="last.name")

    t2 <- merge(t2, nodes[, c("label", "idx")], by.x="p1", by.y="label", all.x=T, all.y=F)
    t2 <- merge(t2, nodes[, c("label", "idx")], by.x="p2", by.y="label", all.x=T, all.y=F, suffixes=c("_p1", "_p2"))

  }

  list(t2=t2, nodes=nodes)

}


# UI component of shiny app
ui <- fluidPage(
  verticalLayout(
    inputPanel(
      sliderInput("thresholdInput", "Minimum threshold", 
                  min=0, max=floor(sort(t$N)[length(t$N)-1]/10)*10, 
                  value=DEFAULT_THRESHOLD, post=" N spp.",
                  step=5),
      selectInput("nodeColourInput", "Node colour by",
                  c("Gender", "Country of residence"), selected=NULL, multiple=F),
      selectInput("authorInput", "Authors", # sorted by Discover Life codes
                  c("All", nodes_lab), selected=NULL, multiple=F)
    ),
    mainPanel(
      sidebarLayout(
        sidebarPanel(tableOutput("networkTable"),
                     width=3), 
        mainPanel(forceNetworkOutput("network", height="500px"), width=9)
      ),
      width=12
    )
  )
)

# Server component of shiny app
server <- function(input, output) {

  # Update data
  update_data <- reactive({
    if(input$authorInput == "All") {
      updated <- update(input$thresholdInput)
    } else {
      updated <- update(input$thresholdInput, input$authorInput)
    }
    updated
  })

  # Output plots
  clickJS <- 'd3.selectAll(".node").on("click", function(d){ alert(d.name + ": " + d.nodesize + "spp."); })'
  output$network <- renderForceNetwork({

    updated <- update_data()

    if (dim(updated$t2)[1] == 0) {

    } else {
      
      grouping <- ifelse(input$nodeColourInput == "Country of residence", "residence.country.describer.first", 
        ifelse(input$nodeColourInput == "Gender", "describer.gender.n", ""))

      forceNetwork(
        Links = updated$t2, Nodes = updated$nodes,
        Source = "idx_p1", Target = "idx_p2", Value = "N", 
        colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
        NodeID = "label", Group=grouping, Nodesize="spp_N", zoom=T, opacity=.8, 
        radiusCalculation = JS("Math.sqrt(d.nodesize)"),
        linkWidth = JS("function(d) { return d.value == 1 ? 1 : Math.log(d.value, 2); }"),
        charge=-2000/((input$thresholdInput/200+1)), fontFamily="Calibri", 
        fontSize=40, opacityNoHover = 1, clickAction=clickJS, legend=TRUE, arrows=FALSE)
    }
  })

  output$networkTable <- renderTable({
    
    updated <- update_data()

    if(dim(updated$t2)[1] >=1) {
      tableshow <- updated$t2[,c("p1", "p2", "N")]
      names(tableshow) <- c("Author1", "Author2", "N spp.")
      tableshow
    } else { data.frame(Comment=c("No data")) }

  })

}

# Run app
shinyApp(ui = ui, server = server)
