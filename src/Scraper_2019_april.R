library(tidyverse)
library(rvest)
library(purrr)
library(naniar)
library(stringi)
library(lubridate)

source("./src/functions.R")
source("./src/dictionary.R")

# 2019 April elections

url <- 'https://en.wikipedia.org/wiki/Opinion_polling_for_the_April_2019_Spanish_general_election'
this_many <- 4
StartYear <- 2020

raw_tables <- url %>% get_tables(this_many) 
parties <- url %>% get_parties(this_many)
raw_tables <- set_raw_names(this_many,raw_tables,parties)


table <- raw_tables %>%
    map(filter, is.na(Sample) | Sample != "Sample size") %>%
    map(filter, !is.na(Firm)) %>%
    bind_rows(.id = "Year")

change_names <- names_to_change(names(table))
parties_vec <- change_names
names(parties_vec) <- NULL
 
main_results <- table %>%
    rowwise() %>%
    mutate(across(all_of(parties_vec), get_percentage)) %>%
    mutate(day_month = Date %>% get_date) %>%
    mutate(year = StartYear - as.numeric(Year)) %>%
    mutate(Firm = Firm %>% clean_up_names) %>%
    unite("date", day_month, year, sep = " ") %>%
    mutate(date = dmy(date),
    Sample = parse_number(Sample),
    Turnout = parse_number(Turnout)) %>%
    select(date,!c(Year,Date)) %>%
    pivot_longer(all_of(parties_vec)) %>%
    mutate(name = as_factor(name),
           name = fct_recode(name, !!!change_names))
    



main_results %>% write_csv("./Data/2019_April_national_polls.csv")
