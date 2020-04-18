
library(data.table)
library(tidyr)

fdir = "public-apps/covid19/"
filename = paste0(fdir, "9999-01-01-data.csv")
cluster_filename = paste0(fdir, "clusters.csv")
d = fread(filename)
ddir = "C:\\Users\\ejysoh\\Desktop\\sg-covid-cases\\"

get_li = function(text) {
  text = trimws(text)
  text = gsub(" and ", ", ", text)
  text = strsplit(text, ", ")[[1]]
  print(paste0("There are ",  length(text), " recovered."))
  text[order(text)]
}

today = "2020-04-18"
recovered = "190, 305, 324, 344, 387, 490, 500, 507, 508, 517, 521, 587, 603, 660, 676, 705, 741, 845, 873, 907, 1136, 1272, 1335, 1386, 1398, 1431, 1548, 2212, 2504, 2535, 2537, 2563, 3332, 3364, 3468, 3657, 3970 and 4754"
text_filename = paste0(ddir, today, "-contact.txt")
links = readChar(text_filename, file.info(text_filename)$size)


text = links

# Recovered
recovered = get_li(recovered)
d[case %in% recovered]$recovered.at.date = as.character(today)


# Links/ clusters
links = unlist(strsplit(text, "\\n"))
links = data.frame(text=links)
links$text = as.character(links$text)
links = links[links$text != "",]
links = data.frame(text=links)
links$text = as.character(links$text)
links_de = data.frame(do.call('rbind', strsplit(as.character(links$text),': ',fixed=TRUE)))
names(links_de) = c("cluster", "cases")
links_de$cluster = gsub(".*\\t","",links_de$cluster)
links_de$cases = as.character(links_de$cases)
links_de$cases = gsub("\\r", "", links_de$cases)
links_de = data.table(links_de)
links_de = links_de[cases!=""]

cls = as.character(read.csv(cluster_filename)[,1])
links_de$full = ""

for (i in 1:dim(links_de)[1]) {
  for (j in 1:length(cls)) {
    if(grepl(tolower(links_de[i,]$cluster), tolower(cls[j]))) {
      links_de[i,]$full = cls[j]
    }
  }
}

# Manual 
links_filename = paste0(ddir, "clean.csv")
links_de = links_de[,c("cluster", "full", "cases")]
# write.csv(links_de, links_filename, row.names=F)

links_de = fread(links_filename)
links_de$cases = gsub("Cases |\\.", "", links_de$cases)
links_de$cases = gsub(" and ", ", ", links_de$cases)
links_de$cases = lapply(links_de$cases, function(text) strsplit(text, ",|, ")[[1]])
links_de = data.table(unnest(links_de, cols=c("cases")))
links_de$cases = as.integer(links_de$cases)
links_de = links_de[!is.na(cases)]
links_de = unique(links_de)

links_de[duplicated(links_de$cases)]
# i = which(links_de$case == 1104)[1]

for(i in 1:dim(links_de)[1]) {
  idx = as.integer(links_de[i,]$cases)
  clust = as.character(links_de[i,]$full)
  if(idx %in% d$case) {
    if(d[case==idx,]$cluster == "") {
      d[case==idx,]$cluster = clust
    } else if (grepl(clust, d[case==idx,]$cluster)){
      # don't do anything
    } else {
      d[case==idx,]$cluster = paste0(d[case==idx,]$cluster, "; ", clust)
    }
  } else {
    print(paste0(i, " / case ", idx, " : Not in index"))  
  } 
}

write.csv(d, paste0(gsub(".csv", "", filename), "-edit.csv"), row.names=F)

# Manually change -edit to actual