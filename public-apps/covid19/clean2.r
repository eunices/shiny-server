filename = "public-apps/covid19/9999-01-01-data.csv"
d = fread(filename)

recovered = "187, 270, 315, 326, 328, 332, 371, 402, 438, 451, 510, 514, 518, 531, 561, 572, 577, 580, 584, 588, 648, 665, 698, 710, 725, 733, 835, 844, 870, 880, 882, 929, 995, 1034 and 1233"
recovered = gsub(" and ", ", ", recovered)
recovered = strsplit(recovered, ", ")

d[case %in% recovered[[1]]]$recovered.at.date = as.character(Sys.Date())
write.csv(d, filename, row.names=F)
