library(ggplot2)

camelcase_labeller <- function(variable, value){
    words = strsplit(value, split="[[:space:][:punct:]]")
    words = lapply(words, tolower)
    capitalize = function(x) paste0(toupper(substring(x,1,1)), substring(x,2))
    words = lapply(words, capitalize)
    sapply(words, paste, collapse=" ")
}

breaks_f <- function(v) {
  max = ceiling(max(v, na.rm=T))
  inc = ifelse(max>300, 100, ifelse(max>100, 50, ifelse(max>80, 20, ifelse(max>20, 10, ifelse(max>10, 5,  2)))))
  min = floor(min(v, na.rm=T)/inc)*inc
  seq(min, max, inc)
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