



## ----packages --------------------------------------------------------------------------------------------
library(knitr)
library(rmdformats)
library(tidyverse)
library(devtools)
library(hrbrthemes)
library(remedy)


## ----import - googlesheets-data, -----------------------------------------------------------------
# devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
gs4_deauth()
CovidDrugs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/12_4fGi2UNnnMiWvhn8VNTGi0mXhl5bf_h1Swo8xG2Sc/edit#gid=0" ) %>% janitor::clean_names()
CovidDrugs %>% glimpse(78)


## ----import SIDER data -------------------------------------------------------
DrugNames <- readr::read_delim(file = "data/sider-data/drug_names.tsv",
                               delim = "\t",
                               escape_double = FALSE,
                               col_names = FALSE,
                               trim_ws = FALSE) %>%
  # set names
             purrr::set_names(x = .,
                              nm = "drug_cid", "drug_name")

DrugAtc <- read_delim(file = "data/sider-data/drug_atc.tsv",
                      delim = "\t",
                      escape_double = FALSE,
                      col_names = FALSE,
                      trim_ws = FALSE) %>%
            purrr::set_names(x = .,
                              nm = "drug_cid", "drug_atc")


## DrugCIDATC -----------------------------------------------------------------
DrugCIDATC <- DrugAtc %>%
  left_join(x = ., y = DrugNames, by = "drug_cid")
DrugCIDATC %>% glimpse()


## -- MeddraAllSe -------------------------------------------------------------
MeddraAllSe <- readr::read_delim("data/sider-data/meddra_all_se.tsv", delim = "\t",
                                 escape_double = FALSE,
                                 col_names = FALSE,
                                 trim_ws = TRUE) %>%
              purrr::set_names(x = .,
                            nm = "STITCH_compound_id01", "STITCH_compound_id02",
                            "UMLS_concept_id", "MedDRA_concept_type",
                            "UMLS_concept_id_for_MedDRA_term",
                            "side_effect_name")

head(MeddraAllSe)


## ----import clinical-trials-gov data -------------------------------------------------------------------
IvermectinRCT <- readr::read_csv(file = "data/clinical-trials-gov/2020-06-05-ivermectin.csv")
IvermectinRCT <- IvermectinRCT %>% janitor::clean_names()
HCQAzithromycinRCT <- readr::read_csv(file = "data/clinical-trials-gov/2020-06-05-hydroxychloroquine-azithromycin .csv")
HCQAzithromycinRCT <- HCQAzithromycinRCT %>% janitor::clean_names()
RemdesivirRCT <- readr::read_csv(file = "data/clinical-trials-gov/2020-06-05-remdesivir.csv")
RemdesivirRCT <- RemdesivirRCT %>% janitor::clean_names()
TocilizumabRCT <- readr::read_csv(file = "data/clinical-trials-gov/2020-06-05-tocilizumab.csv")
TocilizumabRCT <- TocilizumabRCT %>% janitor::clean_names()
# bind these together
AllCovidTrials <- bind_rows("Ivermectin" = IvermectinRCT,
                        "HCQ + Azithromycin" = HCQAzithromycinRCT,
                        "Remdesivir" = RemdesivirRCT,
                        "Tocilizumab" = TocilizumabRCT,
                        .id = "drug")


## ----IvermectinSE------------------------------------------------------------
# look for ATC
DrugCIDATC %>%
  dplyr::filter(drug_atc == "P02CF01")

IvermectinSE <- MeddraAllSe %>%
  dplyr::filter(STITCH_compound_id01 == "CID106435110" |
                  STITCH_compound_id02 == "CID106435110")
IvermectinSE %>%
  count(side_effect_name, sort = TRUE) %>%
  utils::head()


## ----RemdesivirSE----------------------------------------------------------------------------------------------------
RemdesivirSE <- CovidDrugs %>%
  dplyr::filter(drug == "Remdesivir") %>%
  dplyr::mutate(side_effect =
    # 1) split this on the "updated" pattern
                  stringr::str_split(string = side_effect,
                                pattern = ",")) %>%
    # convert the output from split into multiple rows
        tidyr::unnest(side_effect) %>%
    # remove or from side_effect
    dplyr::mutate(side_effect = str_remove_all(string = side_effect,
                                               pattern = "or "))
RemdesivirSE %>% glimpse()


## ----tocilizumab-----------------------------------------------------------------------------------------------------
DrugCIDATC %>%
  dplyr::filter(drug_atc == "L04AC07")


## ----Hydroxychloroquine----------------------------------------------------------------------------------------------
DrugCIDATC %>%
  dplyr::filter(drug_atc == "P01BA02")


## ----HydroxychloroquineSE--------------------------------------------------------------------------------------------
HydroxychloroquineSE <- MeddraAllSe %>%
  dplyr::filter(STITCH_compound_id01 == "CID100003652" |
                  STITCH_compound_id02 == "CID100003652")
HydroxychloroquineSE %>%
  dplyr::count(side_effect_name, sort = TRUE) %>%
  utils::head()


## ----Azithromycin----------------------------------------------------------------------------------------------------
DrugCIDATC %>%
  dplyr::filter(stringr::str_detect(string = drug_atc,
                                    pattern = "J01FA10|S01AA26|J01RA07"))


## ----AzithromycinSE--------------------------------------------------------------------------------------------------
AzithromycinSE <- MeddraAllSe %>%
  dplyr::filter(STITCH_compound_id01 == "CID100002269" |
                  STITCH_compound_id02 == "CID100002269")
AzithromycinSE %>%
  dplyr::count(side_effect_name, sort = TRUE) %>%
  utils::head()


## ----Ivermectin-clinical-trial-data----------------------------------------------------------------------------------
IvermectinTrials <- AllCovidTrials %>%
  filter(drug == "Ivermectin") %>%  
  filter(str_detect(string = title, pattern = "covid|COVID")) %>%
  dplyr::select(title, enrollment, status, locations, drug)
head(IvermectinTrials)


## ----RemdesivirTrials------------------------------------------------------------------------------------------------
RemdesivirTrials <- AllCovidTrials %>%
  dplyr::filter(drug == "Remdesivir") %>%  
  dplyr::filter(stringr::str_detect(string = title, pattern = "COVID")) %>%
  dplyr::select(title, enrollment, status, locations, drug)
head(RemdesivirTrials)


## ----create-TocilizumabTrials----------------------------------------------------------------------------------------
TocilizumabTrials <- AllCovidTrials %>%
  dplyr::filter(drug == "Tocilizumab") %>%  
  dplyr::filter(stringr::str_detect(string = title, pattern = "COVID")) %>%
  dplyr::select(title, enrollment, status, locations, drug)
head(TocilizumabTrials)


## ----AllCovidTrials--------------------------------------------------------------------------------------------------
AllCovidTrials %>%
  dplyr::filter(drug == "HCQ + Azithromycin") %>%  
  dplyr::filter(stringr::str_detect(string = title, pattern = "COVID")) %>%
  dplyr::select(title, enrollment, status, locations, drug) -> HCQAzithromycinTrials
head(HCQAzithromycinTrials)


## ----COVID19Trials---------------------------------------------------------------------------------------------------
COVID19Trials <- dplyr::bind_rows(IvermectinTrials,
                                  RemdesivirTrials,
                                  TocilizumabTrials,
                                  HCQAzithromycinTrials)
COVID19TrialsExport <- COVID19Trials %>%
  dplyr::select(-drug) %>%
  dplyr::distinct(title, .keep_all = TRUE)
head(COVID19TrialsExport %>%
  dplyr::count(title, sort = TRUE))


## ----COVID19TrialsExport---------------------------------------------------------------------------------------------
# COVID19TrialsExport
# export
readr::write_excel_csv(x = COVID19TrialsExport,
                       path = paste0("data/clinical-trials-gov/",
                                     base::noquote(lubridate::today()),
                                     "-COVID19TrialsExport.csv"))


## ----TidyTrialLocations----------------------------------------------------------------------------------------------
TidyTrialLocations <- COVID19Trials %>%
  # remove missing locations
  dplyr::filter(!is.na(locations)) %>%
    dplyr::mutate(locations =
    # 1) split this on the "updated" pattern
                  stringr::str_split(string = locations,
                                pattern = "\\|")) %>%
    # convert the output from split into multiple rows
        tidyr::unnest(locations) %>%
    # remove or from side_effect
    dplyr::mutate(locations = str_remove_all(string = locations,
                                               pattern = "\\|"))
TidyTrialLocations %>%
  dplyr::select(title, locations)


## ----ggmap, -----------------------------------------------------------------------------
library(ggmap)


## ----google-geocoding-api-key.R--------------------------------------------------------------------------------------
source("code/google-geocoding-api-key.R")


## ----example-geocoding-----------------------------------------------------------------------------------------------
ggmap::geocode("Clinica Universidad de Navarra, Pamplona, Navarra, Spain")


## ----GeoCodeTrialLoc-------------------------------------------------------------------------------------------------
# GeoCodeTrialLoc <- purrr::map(TidyTrialLocations$locations, ggmap::geocode)
# saveRDS(object = GeoCodeTrialLoc,
#         file = "data/clinical-trials-gov/GeoCodeTrialLoc.rds")
# import
GeoCodeTrialLoc <- base::readRDS("data/clinical-trials-gov/GeoCodeTrialLoc.rds")
GeoCodeTrialLoc


## ----TidyGeoCodeTrials-----------------------------------------------------------------------------------------------
purrr::map_df(.x = GeoCodeTrialLoc, bind_rows) %>%
  dplyr::bind_cols(TidyTrialLocations) %>%
  dplyr::select(-c("gloc_key","gloc_value")) %>%
  dplyr::distinct() %>%
  dplyr::arrange(drug) %>%
  dplyr::select(title, dplyr::everything())


## ----qmplot-basic----------------------------------------------------------------------------------------------------
ggmap::qmplot(lon, lat,
       data = TidyGeoCodeTrials,
       maptype = "toner-lite",
       color = I("red"))


## --------------------------------------------------------------------------------------------------------------------
#  Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.


## --------------------------------------------------------------------------------------------------------------------
TidyTrialLocations <- TidyTrialLocations %>%
  tidyr::separate(col = locations,
                  # itemize this into specific location components
                  into = c("location_01", "location_02", "location_03", "location_04",
                           "location_05", "location_06", "location_07"),
                  sep = ", ",
                  remove = FALSE) %>%
  # tidy these up
  tidyr::pivot_longer(names_to = "gloc_key",
                      values_to = "gloc_value",
                      cols = location_01:location_07) %>%
  # remove missing values
  dplyr::filter(!is.na(gloc_value))


## --------------------------------------------------------------------------------------------------------------------
# install.packages("countrycode")
library(countrycode)
CountryCodeList <- countrycode::codelist %>%
  janitor::clean_names()
# install.packages("maps")
library(maps)
WorldCities <- maps::world.cities %>%
  janitor::clean_names()

