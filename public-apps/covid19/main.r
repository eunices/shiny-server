library(data.table)

get_data = function(url) {
    html = read_html(url)
    print(html)
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

get_df_cases_day = function() {
    filename = 'cases.csv'
    print(paste0(Sys.time(), ": Read case summary data from ", filename, "."))
    cases = fread(filename)
    cases$date = as.Date(cases$date)
    cases$case_load = cases$I1 + cases$I2 + cases$I3
    cases$cfr = cases$D / (cases$Recovered + cases$D) *100
    cases[is.na(cases)] = 0
    cases
}

get_events = function() {
    events_cols = c("date", "short", "type", "related")
    filename = "events.csv"
    print(paste0(Sys.time(), ": Read event data from ", filename, "."))
    events = fread(filename)[, ..events_cols]
    events$date = as.Date(events$date)
    events$no = 1:dim(events)[1]
    events
}

get_clusters = function() {
    filename = 'clusters.csv'
    print(paste0(Sys.time(), ": Read clusters data from ", filename, "."))
    clusters = fread(filename)
    clusters
}

get_df_cases_day_hosp = function(df) {
    today = max(df$confirmed.at.date)
    df = df[!tolower(hospital) %in% c("unknown", "not admitted")]
    df$I = mapply(function(min, max) if(is.na(max)) seq(min, today, by="day") else seq(min, max, by="day"),
    min=df$confirmed.at.date, max=df$recovered.at.date)
    df$R = mapply(function(min, max) { if(is.na(min)) NA else seq(min, max, by="day") }, 
        min=df$recovered.at.date, max=Sys.Date())
    df$D = mapply(function(min, max) { if(is.na(min)) NA else seq(min, max, by="day") }, 
        min=df$deceased.at.date, max=Sys.Date())
    I = data.table(unnest(df, cols=c(I)))[, c("I", "hospital")][, list(N_I=.N), by=c("I", "hospital")]
    R = data.table(unnest(df, cols=c(R)))[, c("R", "hospital")][, list(N_R=.N), by=c("R", "hospital")]
    D = data.table(unnest(df, cols=c(D)))[, c("D", "hospital")][, list(N_D=.N), by=c("D", "hospital")]

    IRD = merge(merge(I, R, by.x=c("I", "hospital"), by.y=c("R", "hospital"), all.x=T, all.y=T), 
            D, by.x=c("I", "hospital"), by.y=c("D", "hospital"), all.x=T, all.y=T)
    names(IRD)[which(names(IRD)=="I")] = "date"
    dates = seq(min(df$confirmed.at.date), max(df$confirmed.at.date), by="day")
    IRD = data.table(merge(data.frame(date=dates), IRD, by="date", all.x=T, all.y=T))
    IRD = IRD[!is.na(date),]
    IRD = IRD[date<=today,]
    IRD[is.na(IRD)] = 0
    names(IRD) = c("date", "hospital", "I", "R", "D")
    IRD
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
    text = "(singapore|singaporea) (citizen|resident)|singaporean"
    df[grepl(text, tolower(df$patient.info)),]$patient.citizen = "citizen"
    text = "singapore pr|permanent resident|singapore permanent"
    df[grepl(text, tolower(df$patient.info)),]$patient.citizen = "pr"
    df[grepl("work pass", tolower(df$patient.info)),]$patient.citizen = "wp"
    df[grepl("long term", tolower(df$patient.info)),]$patient.citizen = "lp"

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
    df$status = NULL

    df$patient.citizen = factor(df$patient.citizen,
        levels=c("citizen", "pr", "sp", "wp", "lp", "visitor", "unknown"),
        labels=c("Citizen", "PR", "SP", "WP", "LP", "Visitor", "Unknown"))   

    print(paste0(Sys.time(), ": Finishing up."))
    df = df[order(as.numeric(df$case)),]

    if (write==T) {
        # Write data just in case it's gone
        app_dir = "public-apps/covid19/"
        df = df[, c("case", "age", "gender", "infection.source", 
                    "country.of.origin", "displayed.symptoms", 
                    "patient.citizen", "patient.nationality", 
                    "confirmed.at.date", "symptomatic.at.date",
                    "recovered.at.date")]
        print(paste0(Sys.time(), ": Persist file to ", app_dir, "."))
        write.csv(df, paste0(Sys.Date(), "-data.csv"), 
                  fileEncoding="UTF-8", na="NA", row.names=F)
    }

    df
    
}


get_alternative_data = function() {
    filenames = list.files()[grepl("-data.csv", list.files())]
    filename = filenames[length(filenames)]
    print(paste0(Sys.time(), ": Alternative data used from ", filename))
    df = read.csv(filename, encoding="UTF-8", na.strings="NA", 
                stringsAsFactors=FALSE)
    df$confirmed.at.date = as.Date(df$confirmed.at.date, "%Y-%m-%d")
    df$recovered.at.date = as.Date(df$recovered.at.date, "%Y-%m-%d")
    df$symptomatic.at.date = as.Date(df$symptomatic.at.date, "%Y-%m-%d")
    if("deceased.at.date" %in% names(df)) df$deceased.at.date = as.Date(df$deceased.at.date, "%Y-%m-%d")

    # Calculated in days
    df$days.to.recover.symptomatic = as.numeric(df$recovered.at.date - df$symptomatic.at.date)
    df$days.to.recover.resources = as.numeric(df$recovered.at.date - df$confirmed.at.date)
    df$days.symptomatic.to.confirm = as.numeric(df$confirmed.at.date - df$symptomatic.at.date)
    if("deceased.at.date" %in% names(df)) df$days.to.deceased = as.numeric(df$deceased.at.date - df$symptomatic.at.date)

    # Status
    df$patient.status = ifelse(!is.na(df$deceased.at.date), "Deceased", 
        ifelse(is.na(df$recovered.at.date), "Infected", "Recovered"))

    # Age
    df$age = as.numeric(df$age)

    df = data.table(df)

    # Infection source
    df$infection.type = df$infection.source

    df[grepl("local", tolower(infection.type)) & (cluster == "" & linked.to == "" & linked.to.family == "")]$infection.type =
        "Local unlinked"
    df[grepl("local", tolower(infection.type)) & !(cluster == "" & linked.to == "" & linked.to.family == "")]$infection.type =
        "Local linked"
   
    df

}