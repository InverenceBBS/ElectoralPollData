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

# 2008 elections

url <- 'https://en.wikipedia.org/wiki/Opinion_polling_for_the_2008_Spanish_general_election'

raw_tables <- url %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    html_table(header = NA,
    na.strings = c("?","–", "—", "")) %>%
    head(1)

parties <- url %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    head(1) %>%
    map(get_names_parties) %>%
    map(str_remove,"/wiki/")

for (i in 1) {
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
    myvalue %>% str_extract("\\d{1,2}\\s\\w{3}\\s\\d{4}$")
}

main_results <- raw_tables[[1]] %>%
    filter(is.na(Sample) | Sample != "Sample size") %>%
    filter(!is.na(Firm)) %>%
    rowwise() %>%
    mutate(across(parties[[1]], get_percentage)) %>%
    mutate(date = Date %>% get_date %>% dmy) %>%
    mutate(
        Sample = parse_number(Sample),
        Turnout = parse_number(Turnout)) %>%
        select(date,!Date)

main_results %>% write_csv("./Data/2008_national_polls.csv")
