library(tidyverse)

party_names_df <- read_csv("./src/party_names.csv")

names_to_change <- function(current_names, names_df = party_names_df){
    this_names <- names_df %>% filter(party %in% current_names)
    change_names <- setNames(this_names$party, this_names$name)
    return(change_names)
}
