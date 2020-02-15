# libraries needed

library(tidyverse)

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
    values_to = c("cases"),
    values_ptypes = list(cases = "character")
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
confirmed_sheet <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
deaths_sheet <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
recovered_sheet <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
# confirmed cases
confirmed <- readr::read_csv(confirmed_sheet, col_types = readr::cols())

# recovered cases
recovered <- readr::read_csv(recovered_sheet, col_types = readr::cols())

# deaths
deaths <- readr::read_csv(deaths_sheet, col_types = readr::cols()) 

# add col
confirmed$type <- "confirmed"
recovered$type <- "recovered"
deaths$type <- "death"

# rename
confirmed <- rename_sheets(confirmed)
recovered <- rename_sheets(recovered)
deaths <- rename_sheets(deaths)  

# pivot longer
confirmed <- pivot(confirmed)
recovered <- pivot(recovered)
deaths <- pivot(deaths)    

suppressWarnings({    
  df <- dplyr::bind_rows(confirmed, recovered, deaths) %>% 
    dplyr::mutate(
      date = as_date(date),
      cases = trimws(cases),
      cases = as.numeric(cases),
      cases = dplyr::case_when(
        is.na(cases) ~ 0,
        TRUE ~ cases
      ),
      country = dplyr::case_when(
        country == "US" ~ "United States of America",
        TRUE ~ country
      ),
      country_iso2c = countrycode::countrycode(country, "country.name", "iso2c")
    )
})
df$state <- ifelse(is.na(df$state), df$country,df$state)

# Connect to database

config <- yaml::read_yaml("/etc/skconfig")

con <- pool::dbPool(
  RPostgres::Postgres(),
  host = config$database$host,
  user = config$database$user,
  password = config$database$password,
  dbname = config$database$name,
  port = 25060)

# Save to database

log <- tibble::tibble(last_updated = Sys.time())
DBI::dbWriteTable(con, "jhu", df, overwrite =TRUE, append = FALSE)
DBI::dbWriteTable(con, "log", log, append = TRUE)
