filename = "public-apps/covid19/9999-01-01-data.csv"
d = fread(filename)

get_li = function(text) {
  text = gsub(" and ", ", ", text)
  text = strsplit(text, ", ")[[1]]
  print(paste0("There are ",  length(text), " recovered."))
  text[order(text)]
}

recovered = "187, 270, 315, 326, 328, 332, 371, 402, 438, 451, 510, 514, 518, 531, 561, 572, 577, 580, 584, 588, 648, 665, 698, 710, 725, 733, 835, 844, 870, 880, 882, 929, 995, 1034 and 1233"
recovered = get_li(recovered)

d[case %in% recovered]$recovered.at.date = as.character(Sys.Date())


x = rbind(c("Westlite Woodlands dormitory at 2 Woodlands Sector 2", 
            "1399, 1921, 1721, 2008, 2047, 2075, 2083 and 2089"),
          c("North Coast Lodge at 51 North Coast Avenue",
            "2262 and 2263, 1429, 1464, 2080 and 2081"))

x = data.frame(x, stringAsFactors=F)
names(x) = c("cluster", "cases")
x$cluster = as.character(x$cluster)
x$cases = lapply(x$cases, function(x) get_li(x))

for(i in 1:dim(x)[1]) {
  cluster = x[i,]$cluster
  cases = x[i,]$cases[[1]]
  for(j in 1:length(cases)) {
    if(d[case==cases[j], ]$cluster == "") {
      d[case==cases[j], ]$cluster = cluster
    } else {
      d[case==cases[j], ]$cluster = paste0(d[case==cases[j], ]$cluster, "; ", cluster)
    }
  }
}

write.csv(d, filename, row.names=F)
