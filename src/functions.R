get_names_parties <- function(html){
    party_names <- html %>%
    html_node("tr") %>%
    html_nodes("th a") %>%
    html_attr("href")

    return(party_names)
}

get_tables <- function(url, head_n){
tables <- url %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    html_table(header = NA,
    na.strings = c("?","–", "—", ""))  %>%
    head(head_n)

    return(tables)
}

get_parties <- function(url, head_n){
   parties <- url %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    head(head_n) %>%
    map(get_names_parties) %>%
    map(str_remove,"/wiki/")

    return(parties)
}

set_raw_names <- function(head_n, rawtabs, parts){
    for (i in 1:head_n) {
    names(rawtabs[[i]]) <- c("Firm","Date","Sample","Turnout",parts[[i]],"Lead")
    rawtabs[[i]] <- rawtabs[[i]] %>% mutate_at(parts[[i]],as.character)
    }

    return(rawtabs)
}


get_percentage <- function(myvalue, pattern_low_date = "([0-9]+.\\d)–", pattern_up_date = "([0-9]+.\\d)–", pattern_one_date = "[0-9]+.\\d") {
    if(is.na(myvalue)){return(NA)}
    if(!stri_detect(myvalue, fixed = ".")){return(NA)}
    if (str_detect(myvalue, "–")) {
        low <- myvalue %>% str_extract(pattern_low_date, group = 1) %>% as.numeric
        upp <- myvalue %>% str_extract(pattern_up_date, group = 1) %>% as.numeric
        mid <- (low+upp)/2
        return(mid)
    } else {
        mid <- myvalue %>% str_extract(pattern_one_date) %>% as.numeric
        return(mid)
    }
}

get_date <- function(myvalue, pattern = "\\d{1,2}\\s\\w{3}$"){
    myvalue %>% str_extract(pattern)
}
