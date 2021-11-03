#libraries needed

library(tidyverse)
library(RSQLite)

# Functions needed

#Rename
#' 
#' Rename first few columns
#' 
#' @param df Sheet.
#' 
#' @keywords internal
rename_sheets <- function(df){
  names(df)[1:4] <- c(
    "state",
    "country",
    "lat", 
    "lon"
  )
  return(df)
}

#' Pivot
#' 
#' Change data from wide to long.
#' 
#' @param df Sheet.
#' 
#' @keywords internal
pivot <- function(df){
  tidyr::pivot_longer(
    df, 
    tidyselect::contains("/"),
    names_to = c("date"),
    values_to = c("cases")
  )
}


#' Convert
#' 
#' Convert dates.
#' 
#' @keywords internal
as_date <- function(date){
  date <- lubridate::mdy(date, "%m/%d/%Y")
  date[!is.na(date)]
}

# jhu data
confirmed_sheet <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
deaths_sheet <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
# confirmed cases
confirmed <- readr::read_csv(confirmed_sheet, col_types = readr::cols())

# deaths
deaths <- readr::read_csv(deaths_sheet, col_types = readr::cols()) 

# add col
confirmed$type <- "confirmed"
deaths$type <- "death"

# rename
confirmed <- rename_sheets(confirmed)
deaths <- rename_sheets(deaths)  

# pivot longer
confirmed <- pivot(confirmed)
deaths <- pivot(deaths)    

suppressWarnings({    
  df <- dplyr::bind_rows(confirmed, deaths) %>% 
    dplyr::mutate(
      date = as_date(date),
      cases = trimws(cases),
      cases = as.numeric(cases),
      cases = dplyr::case_when(
        is.na(cases) ~ 0,
        TRUE ~ cases
      ),
      country_iso2c = countrycode::countrycode(country, "country.name", "iso2c")
    )
})
df$state <- ifelse(is.na(df$state), df$country,df$state)


# Connect to SQLite and save the data
con <- dbConnect(SQLite(), "/srv/shiny-server/CoronaOutbreak/covid.db")
log <- tibble::tibble(last_updated = Sys.time())
DBI::dbWriteTable(con, "jhu", df, overwrite =TRUE, append = FALSE)
DBI::dbWriteTable(con, "log", log, append = TRUE)
DBI::dbDisconnect(con)
