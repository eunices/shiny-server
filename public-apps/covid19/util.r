library(ggplot2)

camelcase_labeller <- function(variable, value){
    words = strsplit(value, split="[[:space:][:punct:]]")
    words = lapply(words, tolower)
    capitalize = function(x) paste0(toupper(substring(x,1,1)), substring(x,2))
    words = lapply(words, capitalize)
    sapply(words, paste, collapse=" ")
}

breaks_f <- function(v) {
  min = floor(min(v))
  max = ceiling(max(v))
  inc = ifelse(max > 50, 10, 2)
  seq(0, max, inc)
}

blank_theme <- theme_minimal() +
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=14, face="bold")
)