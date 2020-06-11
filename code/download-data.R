
library(tidyverse)

download.file(url = "http://sideeffects.embl.de/media/download/README", 
              destfile = "README.md")



# raw data folder ---------------------------------------------------------
fs::dir_create(path = "data/raw")


# drug names --------------------------------------------------------------
download.file(url = "http://sideeffects.embl.de/media/download/drug_names.tsv", 
              destfile = "data/raw/drug_names.tsv")


# drug atc ----------------------------------------------------------------
download.file(url = "http://sideeffects.embl.de/media/download/drug_atc.tsv", 
              destfile = "data/raw/drug_atc.tsv")


# meddra all indications --------------------------------------------------

download.file(url = "http://sideeffects.embl.de/media/download/meddra_all_indications.tsv.gz", 
              destfile =  "data/raw/meddra_all_indications.tsv.gz")


# meddra all se -----------------------------------------------------------
download.file("http://sideeffects.embl.de/media/download/meddra_all_se.tsv.gz", 
              destfile =  "data/raw/meddra_all_se.tsv.gz")


# meddra freq -------------------------------------------------------------

download.file(url = "http://sideeffects.embl.de/media/download/meddra_freq.tsv.gz", 
              destfile = "data/raw/meddra_freq.tsv.gz")


# meddra all label indications --------------------------------------------

download.file(url = "http://sideeffects.embl.de/media/download/meddra_all_label_indications.tsv.gz", 
              destfile = "data/raw/meddra_freq.tsv.gz")


# meddta all label se -----------------------------------------------------

download.file(url = "http://sideeffects.embl.de/media/download/meddra_all_label_se.tsv.gz", 
              destfile = "data/raw/meddra_all_label_se.tsv.gz")


# meddra ------------------------------------------------------------------

download.file(url = "http://sideeffects.embl.de/media/download/meddra.tsv.gz", 
              destfile = "data/raw/meddra.tsv.gz")
