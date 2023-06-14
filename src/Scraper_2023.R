library(tidyverse)
library(rvest)
library(purrr)
library(naniar)
library(stringi)
library(lubridate)
library(janitor)


source("./src/functions.R")
source("./src/dictionary.R")


# 2023 April elections

url_2321 <- 'https://en.wikipedia.org/wiki/Opinion_polling_for_the_2023_Spanish_general_election'
url_2119 <- 'https://en.wikipedia.org/wiki/Nationwide_opinion_polling_for_the_2023_Spanish_general_election_(2019%E2%80%932021)'

# 2023, first tranche (2019 to 2021)

# we set the renaming table for the this period
dict_2021 <- party_dictionary %>% filter(table_year == "2019_2021")
rename_list <- setNames(dict_2021$long, dict_2021$short)

this_many <- 3 # the parameter determines the number of tables to consider


# scrape the row table
raw_tables <- url_2119 %>% get_tables(this_many) 
parties <- url_2119 %>% get_parties(this_many)
raw_tables <- set_raw_names(this_many,raw_tables,parties)

table_1921 <- raw_tables %>%
    map(filter, is.na(Sample) | Sample != "Sample size") %>%
    map(filter, !is.na(Firm)) %>%
    bind_rows(.id = "Year") %>%
    rowwise() %>%
    mutate(across(parties[[1]], get_percentage)) %>%
    mutate(day_month = Date %>% get_date) %>%
    mutate(year = 2022 - as.numeric(Year)) %>%
    unite("date", day_month, year, sep = " ") %>%
    mutate(
        date = dmy(date),
        Sample = parse_number(Sample),
        Turnout = parse_number(Turnout)) %>%
    select(date,!c(Year,Date)) %>%
    rename(!!!rename_list)

table_1921 %>% write_csv("./Data/2019_2021_national_polls.csv")

# 2023, first tranche (2019 to 2021)

this_many <- 3
dict_2021 <- party_dictionary %>% filter(table_year == "2021_2023")
rename_list <- setNames(dict_2021$long, dict_2021$short)

raw_tables <- url_2321 %>% get_tables(3)

parties <- url_2321 %>% get_parties(3)

raw_tables <- set_raw_names(3, raw_tables, parties)

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
    select(date,!c(Year,Date)) %>%
    rename(!!!rename_list)

table_2321 %>% write_csv("./Data/2021_2023_national_polls.csv")
