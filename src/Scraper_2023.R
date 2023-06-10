library(tidyverse)
library(rvest)
library(purrr)
library(naniar)
library(stringi)
library(lubridate)

get_names_parties <- . %>%
    html_node("tr") %>%
    html_nodes("th a") %>%
    html_attr("href")

# 2023 April elections

url_2321 <- 'https://en.wikipedia.org/wiki/Opinion_polling_for_the_2023_Spanish_general_election'
url_2119 <- 'https://en.wikipedia.org/wiki/Nationwide_opinion_polling_for_the_2023_Spanish_general_election_(2019%E2%80%932021)'

# 2023, first tranche (2019 to 2021)

raw_tables <- url_2119 %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    html_table(header = NA,
    na.strings = c("?","–", "—", "")) %>%
    head(3)

parties <- url_2119 %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    head(3) %>%
    map(get_names_parties) %>%
    map(str_remove,"/wiki/")

for (i in 1:3) {
   names(raw_tables[[i]]) <- c("Firm","Date","Sample","Turnout",parties[[i]],"Lead")
   raw_tables[[i]] <- raw_tables[[i]] %>% mutate_at(parties[[i]],as.character)
}

get_percentage <- function(myvalue) {
    if(is.na(myvalue)){return(NA)}
    if(!stri_detect(myvalue, fixed = ".")){return(NA)}
    if (str_detect(myvalue, "–")) {
        low <- myvalue %>% str_extract("([0-9]+.\\d)–", group = 1) %>% as.numeric
        upp <- myvalue %>% str_extract("–([0-9]+.\\d)", group = 1) %>% as.numeric
        mid <- (low+upp)/2
        return(mid)
    } else {
        mid <- myvalue %>% str_extract("[0-9]+.\\d") %>% as.numeric
        return(mid)
    }
}

get_date <- function(myvalue){
    myvalue %>% str_extract("\\d{1,2}\\s\\w{3}$")
}

table_1921 <- raw_tables %>%
    map(filter, is.na(Sample) | Sample != "Sample size") %>%
    map(filter, !is.na(Firm)) %>%
    bind_rows(.id = "Year") %>%
    rowwise() %>%
    mutate(across(parties[[1]], get_percentage)) %>%
    mutate(day_month = Date %>% get_date) %>%
    mutate(year = 2022 - as.numeric(Year)) %>%
    unite("date", day_month, year, sep = " ") %>%
    mutate(date = dmy(date),
    Sample = parse_number(Sample),
    Turnout = parse_number(Turnout)) %>%
    select(date,!c(Year,Date))

table_1921 %>% write_csv("./Data/2019_2021_national_polls.csv")

# 2023, first tranche (2019 to 2021)

raw_tables <- url_2321 %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    html_table(header = NA,
    na.strings = c("?","–", "—", "")) %>%
    head(3)

parties <- url_2321 %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    head(3) %>%
    map(get_names_parties) %>%
    map(str_remove,"/wiki/")

for (i in 1:3) {
   names(raw_tables[[i]]) <- c("Firm","Date","Sample","Turnout",parties[[i]],"Lead")
   raw_tables[[i]] <- raw_tables[[i]] %>% mutate_at(parties[[i]],as.character)
}

get_percentage <- function(myvalue) {
    if(is.na(myvalue)){return(NA)}
    if(!stri_detect(myvalue, fixed = ".")){return(NA)}
    if (str_detect(myvalue, "–")) {
        low <- myvalue %>% str_extract("([0-9]+.\\d)–", group = 1) %>% as.numeric
        upp <- myvalue %>% str_extract("–([0-9]+.\\d)", group = 1) %>% as.numeric
        mid <- (low+upp)/2
        return(mid)
    } else {
        mid <- myvalue %>% str_extract("[0-9]+.\\d") %>% as.numeric
        return(mid)
    }
}

get_date <- function(myvalue){
    myvalue %>% str_extract("\\d{1,2}\\s\\w{3}$")
}

case_year <- function(year) {
  case_when(
    year == "3" ~ "2022",
    .default = "2023"
  )
}

table_2321 <- raw_tables %>%
    map(filter, is.na(Sample) | Sample != "Sample size") %>%
    map(filter, !is.na(Firm)) %>%
    bind_rows(.id = "Year") %>%
    rowwise() %>%
    mutate(across(parties[[1]], get_percentage)) %>%
    mutate(day_month = Date %>% get_date) %>%
    mutate(year = Year %>% case_year ) %>%
    unite("date", day_month, year, sep = " ") %>%
    mutate(date = dmy(date),
    Sample = parse_number(Sample),
    Turnout = parse_number(Turnout)) %>%
    select(date,!c(Year,Date))

table_2321 %>% write_csv("./Data/2021_2023_national_polls.csv")
