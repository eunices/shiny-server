library(data.table)
ddir = "C:\\Users\\ejysoh\\Desktop\\sg-covid-cases\\"
date = "2020-04-18"
filename = paste0(date, ".csv")
type = 1 
# type 1: without country of origin
# type 2: with country of origin

nms = names(fread("public-apps/covid19/9999-01-01-data.csv"))
cls = unique(fread("public-apps/covid19/9999-01-01-data.csv")$cluster)
cls = lapply(cls, function(x) strsplit(x, "; "))
cls = unique(unlist(cls))
cls = cls[order(cls)]
cls = cls[cls!=""]

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

d[age=="Pending"]$age = "unknown"
d[grepl("mth", age)]$age = as.numeric(gsub('[A-z]+|\\r|\\n', '', d[grepl("mth", age)]$age))/12

d[gender=="Pending"]$gender = "unknown"

d$confirmed.at.date = as.character(date)

d$hospital = gsub('[0-9]+', '', d$hospital)
d[hospital=="Pending"]$hospital = 'unknown'
d[hospital=="CIF"]$hospital = 'Community Isolation Facility'
d[hospital=="MEN"]$hospital = 'Mount Elizabeth Novena Hospital'
d[hospital=="MAH"]$hospital = 'Mount Alvernia Hospital'
d[hospital=="GH"]$hospital = 'Gleneagles Hospital'
d[hospital=="PEH"]$hospital = 'Parkway East Hospital'
d[hospital=="FPH"]$hospital = 'Farrer Park Hospital'

d$patient.nationality = trimws(gsub("\\n", "", 
                               lapply(d$nat, function(text) strsplit(text, "\\(")[[1]][1])))
d[grepl("singapore permanent", tolower(nat)),]$patient.nationality = "Singapore"
d[grepl("singapore citizen", tolower(nat)),]$patient.nationality = "Singapore"
d[grepl("US", nat),]$patient.nationality = "United States of America"
d[grepl("UK", nat),]$patient.nationality = "United Kingdom"
d[grepl("pending", tolower(nat)),]$patient.nationality = "unknown"

d$patient.citizen = "visitor"
d[grepl("citizen", tolower(nat)),]$patient.citizen = "citizen"
d[grepl("permanent", tolower(nat)),]$patient.citizen = "pr"
d[grepl("employment pass", tolower(nat)),]$patient.citizen = "ep"
d[grepl("s pass", tolower(nat)),]$patient.citizen = "sp"
d[grepl("work", tolower(nat)),]$patient.citizen = "wp"
d[grepl("long", tolower(nat)),]$patient.citizen = "lp"
d[grepl("pending", tolower(nat)),]$patient.citizen = "unknown"
# https://www.paulhypepage.com/guide-faq/singapore-working-visa-guide/

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
clust = data.table(clust=unique(d$clust), full="")
clust = clust[clust != ""]
for (i in 1:dim(clust)[1]) {
  for (j in 1:length(cls)) {
    if(grepl(tolower(clust[i,]$clust), tolower(cls[j]))) {
      clust[i,]$full = cls[j]
    }
  }
}
# write.csv(clust[clust != "",][order(clust),], paste0(ddir, "clust.csv"), row.names=F)

lp = fread(paste0(ddir, "clust.csv"))
d = merge(d, lp, by="clust", all.x=T, all.y=F)
names(d)[which(names(d)== "full")] = "cluster"
d[is.na(cluster)]$cluster = ""
d$clust = NULL

d$displayed.symptoms = TRUE
d$symptomatic.at.date = NA
d$recovered.at.date = NA
d$deceased.at.date = NA
d$deceased.at.date.other = NA

d = d[, ..nms]
d = d[order(as.numeric(case))]

write.csv(d, paste0(ddir, gsub(".csv", "", filename), "-edit.csv"), row.names=F)

# copy and paste into main file