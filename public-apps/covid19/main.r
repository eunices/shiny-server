get_data = function(url) {
    html = read_html(url)
    print(paste0(Sys.time(), ": Read data from URL."))
    df = html %>% 
        html_nodes("#casesTable") %>%
        html_table()
    if (exists("df")) {
        df = df[[1]]
        names(df) = gsub("\\?", "", gsub("\\ ", ".", tolower(names(df))))
        df
    } else {
        stop(paste0(Sys.time(), ": Did not download data successfully from URL."))
    }
}

clean_data = function(df, write=F) {
    print(paste0(Sys.time(), ": Cleaning patient field."))

    # Split patient field
    # df$patient.age = sub("(.*)(year-old|year old).*", "\\1", df$patient)
    # df[grepl("month", df$patient),]$patient.age = 
    #   as.numeric(sub("(.*)(month-old|month old).*", "\\1", df[grepl("month", df$patient),]$patient))/12
    # df$patient.age = as.numeric(sub("-|\\ ", "", df$patient.age))
    # df$patient.gender = "U"
    # df[grepl("male|man|bangladesh", tolower(df$patient)),]$patient.gender = "M"
    # df[grepl("female", df$patient),]$patient.gender = "F"

    df$patient.info = sub(".*male (.*)", "\\1", df$patient)
    df$patient.citizen = "visitor"
    text = "singapore pr|(singapore|singaporea) (citizen|resident)|singapore permanent|singaporean|permanent resident"
    df[grepl(text, tolower(df$patient.info)),]$patient.citizen = "citizen"
    df[grepl("work pass", tolower(df$patient.info)),]$patient.citizen = "wp"

    df$patient.nationality = df$nationality
    df$gender = tolower(df$gender)
    df$infection.source = tolower(df$infection.source)

    # Remove unnecessary columns
    df$patient.info = NULL
    df$nationality = NULL
    df$patient = NULL

    # patient.citizen = Singaporean, Work Pass, Visitor
    # patient.nationality only filled in if not Singaporean

    print(paste0(Sys.time(), ": Cleaning date fields."))

    df$confirmed.at.date = as.Date(gsub("st|th|rd|nd", "", df$confirmed.at), "%d, %b %Y")
    df$symptomatic.at.date = as.Date(gsub("st|th|rd|nd", "", df$symptomatic.at), "%d, %b %Y")
    df$recovered.at.date = as.Date(gsub("st|th|rd|nd", "", df$recovered.at), "%d, %b %Y")

    df$days.to.recover.symptomatic = df$recovered.at.date - df$symptomatic.at.date
    df$days.to.recover.resources = df$recovered.at.date - df$confirmed.at.date
    df$days.symptomatic.to.confirm = df$confirmed.at.date - df$symptomatic.at.date

    df$age = as.numeric(df$age)

    # Remove columns not required
    df$symptomatic.toconfirmation = NULL
    df$days.torecover = NULL
    df$confirmed.at = NULL
    df$recovered.at = NULL
    df$symptomatic.at = NULL

    print(paste0(Sys.time(), ": Finishing up."))
    df = df[order(as.numeric(df$case)),]

    if (write==T) {
        # Write data just in case it's gone
        app_dir = "public-apps/covid19/"
        print(paste0(Sys.time(), ": Persist file to ", app_dir, "."))
        write.csv(df, paste0(Sys.Date(), "-data.csv"), 
                  fileEncoding="UTF-8", na="NA", row.names=F)
    }

    df
    
}


get_alternative_data = function() {
    filenames = list.files()[grepl("-data.csv", list.files())]
    df = read.csv(filenames[length(filenames)], encoding="UTF-8", na.strings="NA", 
                stringsAsFactors=FALSE)
    df$confirmed.at.date = as.Date(df$confirmed.at.date, "%Y-%m-%d")
    df$recovered.at.date = as.Date(df$recovered.at.date, "%Y-%m-%d")
    df$symptomatic.at.date = as.Date(df$symptomatic.at.date, "%Y-%m-%d")
    df$age = as.numeric(df$age)
    df
    print(filenames)
}