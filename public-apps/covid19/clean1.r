library(data.table)
ddir = "C:\\Users\\ejysoh\\Desktop\\sg-covid-cases\\"
filename = "2020-04-11.csv"
type = 1
# type 1: without country of origin
# type 2: with country of origin

nms = names(fread("public-apps/covid19/9999-01-01-data.csv"))

d = fread(paste0(ddir, filename))

if (type==1) {
  names(d) = c("case", "confirmed.at.date", "hospital", "age", "gender", 
               "nat", "type", "X", "clust")
  d$country.of.origin = ""
}

if (type==2) {
  names(d) = c("case", "confirmed.at.date", "hospital", "age", "gender", 
               "nat", "country.of.origin", "type", "X", "clust")
  d[grepl("uk", tolower(country.of.origin))]$country.of.origin = "United Kingdom"
  d[grepl("us", tolower(country.of.origin))]$country.of.origin = "United States of America"
}


d$hospital = gsub('[0-9]+', '', d$hospital)

d$patient.nationality[] = gsub("\\n", "", 
                               lapply(d$nat, function(text) strsplit(text, "\\(")[[1]][1]))
d$patient.citizen = "visitor"
d[grepl("long", tolower(nat)),]$patient.citizen = "lp"
d[grepl("work", tolower(nat)),]$patient.citizen = "wp"
d[grepl("permanent", tolower(nat)),]$patient.citizen = "pr"
d[grepl("citizen", tolower(nat)),]$patient.citizen = "citizen"
d$nat = NULL


d$infection.source = "Local transmission"
d[grepl("local", tolower(type))]$infection.source = "Local transmission"
d[grepl("import", tolower(type))]$infection.source = "Imported case"
d$type = NULL

d$linked.to.family = ""
d[grepl("family", tolower(X))]$linked.to.family = tolower(d[grepl("family", tolower(X))]$X)
text = "family member of cases |family member of case "
d[grepl("family", tolower(X))]$linked.to.family = gsub(" and ", ", ", gsub(text, "", 
  d[grepl("family", tolower(X))]$linked.to.family))

d$linked.to = ""
d[grepl("contact", tolower(X))]$linked.to = tolower(d[grepl("contact", tolower(X))]$X)
text = "contact of cases |contact of case "
d[grepl("contact", tolower(X))]$linked.to = gsub(" and ", ", ", gsub(text, "", 
  d[grepl("contact", tolower(X))]$linked.to))

d$X = NULL

# Manual
clust = data.frame(clust=unique(d$clust), full="")
write.csv(clust, paste0(ddir, "clust.csv"), row.names=F))

lp = fread(paste0(ddir, "clust.csv"))
d = merge(d, lp, by="clust", all.x=T, all.y=F)
names(d)[which(names(d)== "full")] = "cluster"
d[is.na(cluster)]$cluster = ""
d$clust = NULL

d$displayed.symptoms = TRUE
d$symptomatic.at.date = NA
d$recovered.at.date = NA
d$deceased.at.date = NA

d = d[, ..nms]
d = d[order(as.numeric(case))]

write.csv(d, paste0(ddir, gsub(".csv", "", filename), "-edit.csv"), row.names=F)

# copy and paste into main file