library(tidyverse)
library(rvest)
library(purrr)
library(naniar)
library(stringi)
library(lubridate)

source("./src/dictionary.R")
source("./src/functions.R")


# 2019 November elections

url <- 'https://en.wikipedia.org/wiki/Opinion_polling_for_the_November_2019_Spanish_general_election'
this_many <- 1
raw_tables <- url %>% get_tables(this_many) 
parties <- url %>% get_parties(this_many)
raw_tables <- set_raw_names(this_many,raw_tables,parties)

change_names <- names_to_change(names(raw_tables[[1]]))
parties_vec <- change_names
names(parties_vec) <- NULL

main_results <- raw_tables[[1]] %>%
    filter(is.na(Sample) | Sample != "Sample size") %>%
    filter(!is.na(Firm)) %>%
    rowwise() %>%
    mutate(across(all_of(parties_vec), get_percentage)) %>%
    mutate(Firm = Firm %>% clean_up_names) %>%
    mutate(date = Date %>% get_date %>% paste("2019") %>% dmy) %>%
    mutate(
        Sample = parse_number(Sample),
        Turnout = parse_number(Turnout)) %>%
        select(date,!Date) %>%
    pivot_longer(-c(Firm,date,Sample,Turnout,Lead)) %>%
    mutate(name = as_factor(name),
           name = fct_recode(name, !!!change_names))


main_results %>% write_csv("./Data/2019_November_national_polls.csv")
