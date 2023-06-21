library(tidyverse)
library(rvest)
library(purrr)
library(naniar)
library(stringi)
library(lubridate)

source("./src/dictionary.R")
source("./src/functions.R")

# 2011 elections

url <- 'https://en.wikipedia.org/wiki/Opinion_polling_for_the_2011_Spanish_general_election'
this_many <- 1

raw_tables <- url %>% get_tables(this_many) 
parties <- url %>% get_parties(this_many)
raw_tables <- set_raw_names(this_many,raw_tables,parties)

date_pattern <- "\\d{1,2}\\s\\w{3}\\s\\d{4}$"

change_names <- names_to_change(names(raw_tables[[1]]))
parties_vec <- change_names
names(parties_vec) <- NULL

main_results <- raw_tables[[1]] %>%
    filter(is.na(Sample) | Sample != "Sample size") %>%
    filter(!is.na(Firm)) %>%
    rowwise() %>%
    mutate(across(all_of(parties_vec), get_percentage)) %>%
    mutate(date = Date %>% get_date(pattern = date_pattern) %>% dmy) %>%
    mutate(Firm = Firm %>% clean_up_names) %>%
    mutate(Firm = Firm %>% clean_up_names) %>%
    mutate(
        Sample = parse_number(Sample),
        Turnout = parse_number(Turnout)) %>%
        select(date,!Date) %>%
    pivot_longer(all_of(parties_vec)) %>%
    mutate(name = as_factor(name),
           name = fct_recode(name, !!!change_names))

main_results %>% write_csv("./Data/2011_national_polls.csv")
