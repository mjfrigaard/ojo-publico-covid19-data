

library(tidyverse)
DrugNames <- readr::read_delim(file = "data/drug_names.tsv", 
                               delim = "\t", 
                               escape_double = FALSE, 
                               col_names = FALSE, 
                               trim_ws = FALSE) %>% 
  # set names 
             purrr::set_names(x = ., 
                              nm = "atc_name", "drug_name")

DrugAtc <- read_delim(file = "data/drug_atc.tsv", 
                      delim = "\t", 
                      escape_double = FALSE,
                      col_names = FALSE, 
                      trim_ws = FALSE) %>% 
            purrr::set_names(x = ., 
                              nm = "atc_name", "drug_name")
