# Visualising resource flow - from taxonomist country of residence to type locality
library(shiny)
library(data.table)
library(networkD3)

# Default settings
default_threshold <- 50

# Read data (residing in data folder)
lu <- fread("2019-06-19-ascher-type-data/eda1.1_shiny/data/lookup-country-codes.csv",
           encoding="UTF-8", na.strings="")
t <- fread("2019-06-19-ascher-type-data/eda1.1_shiny/data/2019-09-22-flow-map-type-loc-des-country.csv",
           encoding="UTF-8", na.strings="")

# Parameters
threshold <- default_threshold

# Create labels
nodes_lab <- paste0(lu$DL, " - ", lu$Country)

# Update data function

parsed_country <- unlist(strsplit("AU - asdfasdf", " - "))[1]

t2 <- t[no_flow == "FALSE"]; no_flow <- t[no_flow=="TRUE"]

if (is.na(parsed_country) | parsed_country == "All") {
t2 <- t2[N>threshold]
} else {
t2 <- t2[N>threshold & (ori == parsed_country | des == parsed_country)]
par}

nodes <- unique(data.frame(label=c(t2$ori, t2$des)))
nodes <- data.table(nodes); nodes <- nodes[order(label)]
nodes$idx <- seq(0, dim(nodes)[1]-1, 1)
nodes <- merge(nodes, no_flow[, c("ori", "N")], by.x="label", by.y="ori", all.x=T, all.y=F)

t2 <- merge(t2, nodes[, c("label", "idx")], by.x="ori", by.y="label", all.x=T, all.y=F)
t2 <- merge(t2, nodes[, c("label", "idx")], by.x="des", by.y="label", all.x=T, all.y=F, suffixes=c("_from", "_to"))
# t2$Nodesize <- t2$N/threshold

forceNetwork(
    Links = updated$t2, Nodes = updated$nodes,
    Source = "idx_from", Target = "idx_to", Value = "N", 
    NodeID = "label", Group="label", Nodesize="N", arrow=T, zoom=T, opacity=1, 
    radiusCalculation = JS(" Math.sqrt(d.nodesize, 2)"),
    linkWidth = JS("function(d) { return Math.log(d.value, 2); }"),
    charge=-30000/input$thresholdInput, fontFamily="Calibri", fontSize=20, opacityNoHover = 1, clickAction=clickJS)