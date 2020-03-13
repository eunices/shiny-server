# Shiny App: Visualising resource flow - from taxonomist country of residence to type locality

# Libraries
library(shiny)
library(data.table)
library(networkD3)

# TODO: changes with time

# Constants / parameters
DEFAULT_THRESHOLD <- 0

# Read data (residing in data folder)
lu <- fread("data/lookup-country-codes.csv", encoding="UTF-8", na.strings="")
t <- fread("data/2019-09-22-flow-map-type-loc-des-country.csv", encoding="UTF-8", na.strings="")

# Create labels for countryInput
nodes_lab <- sort(paste0(lu$Country, ifelse(is.na(lu$Col_status), "", paste0(" (", lu$Col_status, ")")), " - ", lu$DL))

# Update data function
update <- function(threshold=DEFAULT_THRESHOLD, country=NA, typeInput=c()) {
  
  t2 <- t[no_flow == "FALSE"]; no_flow <- t[no_flow=="TRUE"]

  # Subset dataset
  if (is.na(country) | country == "All") {

    t2 <- t2[N>threshold]

  } else {

    if (all(c(1,2) %in% typeInput)) {
      t2 <- t2[N>threshold & (ori == country | des == country)]
    } else if (1 %in% typeInput) {
      t2 <- t2[N>threshold & (ori == country)]
    } else if (2 %in% typeInput) {
      t2 <- t2[N>threshold & (des == country)]
    } else if (length(typeInput) <= 0) {
      nodes <- data.frame(idx=integer(), label=character())
      t2 <- data.frame(ori=character(), des=character())
    }

  }

  # Create network dataset - indexing from 0
  if (dim(t2)[1] == 0) {

    nodes <- data.frame(idx=integer(), label=character())
    t2 <- data.frame(ori=character(), des=character())

  } else {

    nodes <- unique(data.frame(label=c(t2$ori, t2$des)))
    nodes <- data.table(nodes); nodes <- nodes[order(label)]
    nodes$idx <- seq(0, dim(nodes)[1]-1, 1)
    nodes <- merge(nodes, no_flow[, c("ori", "N")], by.x="label", by.y="ori", all.x=T, all.y=F)
    nodes <- merge(nodes, lu, by.x="label", by.y="DL")
    nodes$Econ_status <- factor(nodes$Econ_status, levels=c("High income", 
                                                            "Upper middle income",
                                                            "Lower middle income",
                                                            "Low income", 
                                                            "Unclassed"))

    t2 <- merge(t2, nodes[, c("label", "idx")], by.x="ori", by.y="label", all.x=T, all.y=F)
    t2 <- merge(t2, nodes[, c("label", "idx")], by.x="des", by.y="label", all.x=T, all.y=F, suffixes=c("_from", "_to"))
    # t2$Nodesize <- t2$N/threshold

  }

  list(t2=t2, nodes=nodes)

}

# Server component of shiny app
shinyServer(function(input, output) {

  # Update data
  update_data <- reactive({
    parsed_country <- unlist(strsplit(input$countryInput, " - "))[2]
    if(input$countryInput == "All") {
      updated <- update(input$thresholdInput, parsed_country)
    } else {
      updated <- update(input$thresholdInput, parsed_country, input$typeInput)
    }
    updated
  })

  # Output plots
  clickJS <- 'd3.selectAll(".node").on("click", function(d){ alert(d.name + ": " + d.nodesize + "spp."); })'
  output$network <- renderForceNetwork({

    updated <- update_data()

    if (dim(updated$t2)[1] == 0) {

    } else {
      
      grouping <- ifelse(input$nodeColourInput == "Country", "label", 
        ifelse(input$nodeColourInput == "Colonialism", "Col_status", 
          ifelse(input$nodeColourInput == "Economic status", "Econ_status", "")))

      legend_status <- ifelse(input$nodeColourInput == "Country", FALSE, TRUE)

      forceNetwork(
        Links = updated$t2, Nodes = updated$nodes,
        Source = "idx_from", Target = "idx_to", Value = "N", 
        colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
        NodeID = "label", Group=grouping, Nodesize="N", arrows=T, zoom=T, opacity=.8, 
        radiusCalculation = JS("Math.sqrt(d.nodesize)"),
        linkWidth = JS("function(d) { return d.value == 1 ? 1 : Math.log(d.value, 2); }"),
        charge=-10000/((input$thresholdInput/200+1)), fontFamily="Calibri", 
        fontSize=40, opacityNoHover = 1, clickAction=clickJS, legend=legend_status)
    }
  })

  output$networkTable <- renderTable({
    
    updated <- update_data()

    if(dim(updated$t2)[1] >=1) {
      tableshow <- updated$t2[,c("ori", "des", "N")]
      names(tableshow) <- c("Src", "Tgt", "N")
      tableshow
    } else { data.frame(Comment=c("No data")) }

  })

})
