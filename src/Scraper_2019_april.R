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

# 2019 April elections

url <- 'https://en.wikipedia.org/wiki/Opinion_polling_for_the_April_2019_Spanish_general_election'

raw_tables <- url %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    html_table(header = NA,
    na.strings = c("?","–", "—", "")) %>% head(4)

parties <- url %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    head(4) %>%
    map(get_names_parties) %>%
    map(str_remove,"/wiki/")

for (i in 1:4) {
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

main_results <- raw_tables %>%
    map(filter,is.na(Sample) | Sample != "Sample size") %>%
    map(filter,!is.na(Firm)) %>%
    bind_rows(.id = "Year")

parties <- names(main_results)[!names(main_results) %in%
           c("Year", "Firm", "Date", "Sample", "Turnout", "Lead")]

change_names <- names_to_change(parties)
    
main_results <- main_results %>%
    rowwise() %>%
    mutate(across(all_of(parties), get_percentage)) %>%
    mutate(day_month = Date %>% get_date) %>%
    mutate(year = 2020 - as.numeric(Year)) %>%
    mutate(Firm = Firm %>% clean_up_names) %>%
    unite("date", day_month, year, sep = " ") %>%
    mutate(date = dmy(date),
    Sample = parse_number(Sample),
    Turnout = parse_number(Turnout)) %>%
    select(date,!c(Year,Date)) %>%
    pivot_longer(all_of(parties)) %>%
    mutate(name = as_factor(name),
           name = fct_recode(name, !!!change_names))
    



main_results %>% write_csv("./Data/2019_April_national_polls.csv")
