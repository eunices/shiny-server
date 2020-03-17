camelcase_labeller <- function(variable, value){
    words = strsplit(value, split="[[:space:][:punct:]]")
    words = lapply(words, tolower)
    capitalize = function(x) paste0(toupper(substring(x,1,1)), substring(x,2))
    words = lapply(words, capitalize)
    sapply(words, paste, collapse=" ")
}
