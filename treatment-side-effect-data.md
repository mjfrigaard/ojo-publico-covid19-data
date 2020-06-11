Ojo publico - drug side effects
================
Martin Frigaard
2020-06-09

# Motivation

This document outlines how we found side effects, adverse events,
clinical trials, and other information for the following covid-19
treatments: Ivermectin, Remdesivir, Tocilizumab, and Hydroxychloroquine
+ Azithromycin

## Medlineplus Googlesheet data

The code below imports data from a Google Drive spreadsheet of drugs and
side effects collected from
[medlineplus.gov](https://medlineplus.gov/druginformation.html) and
[medscape](https://reference.medscape.com/).

``` r
# devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
gs4_deauth()
CovidDrugs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/12_4fGi2UNnnMiWvhn8VNTGi0mXhl5bf_h1Swo8xG2Sc/edit#gid=0" ) %>% janitor::clean_names()
```

    #>  Reading from "covid-19-trt-side-effects"

    #>  Range "side effects"

    #>  New names:
    #>  * `` -> ...9

``` r
CovidDrugs <- CovidDrugs %>% 
  dplyr::select(-x9) %>% 
  glimpse(78)
```

    #>  Rows: 83
    #>  Columns: 8
    #>  $ drug                    <chr> "Hydroxychloroquine", "Hydroxychloroquine",…
    #>  $ route_of_administration <chr> "tablet (orally)", "tablet (orally)", "tabl…
    #>  $ side_effect             <chr> "headache", "dizziness", "loss of appetite"…
    #>  $ side_effect_severity    <chr> "call your doctor if severe or do not go aw…
    #>  $ primary_comorbidity     <chr> "heart disease", "heart disease", "heart di…
    #>  $ secondary_comorbidity   <chr> "liver disease", "liver disease", "liver di…
    #>  $ drug_class              <chr> "antimalarial", "antimalarial", "antimalari…
    #>  $ source                  <chr> "https://medlineplus.gov/druginfo/meds/a601…

Get a quick count of the side effects by drugs,

``` r
CovidDrugs %>%  
  dplyr::count(side_effect, sort = TRUE) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n > 1)
```

<div class="kable-table">

| side\_effect                 | n |
| :--------------------------- | -: |
| diarrhea                     | 3 |
| dizziness                    | 3 |
| headache                     | 3 |
| hives                        | 3 |
| itching                      | 3 |
| loss of appetite             | 3 |
| nausea                       | 3 |
| rash                         | 3 |
| vomiting                     | 3 |
| stomach pain                 | 2 |
| unusual bleeding or bruising | 2 |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "diarrhea")
```

<div class="kable-table">

| drug               | route\_of\_administration | side\_effect | side\_effect\_severity                       | primary\_comorbidity | secondary\_comorbidity        | drug\_class           | source                                               |
| :----------------- | :------------------------ | :----------- | :------------------------------------------- | :------------------- | :---------------------------- | :-------------------- | :--------------------------------------------------- |
| Hydroxychloroquine | tablet (orally)           | diarrhea     | call your doctor if severe or do not go away | heart disease        | liver disease                 | antimalarial          | <https://medlineplus.gov/druginfo/meds/a601240.html> |
| Azithromycin       | tablet (orally)           | diarrhea     | call your doctor if severe or do not go away | heart disease        | irregular heartbeat           | macrolide antibiotics | <https://medlineplus.gov/druginfo/meds/a697037.html> |
| Ivermectin         | tablet (orally)           | diarrhea     | call your doctor if severe or do not go away | meningitis           | human African trypanosomiasis | anthelmintics         | <https://medlineplus.gov/druginfo/meds/a607069.html> |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "dizziness")
```

<div class="kable-table">

| drug               | route\_of\_administration | side\_effect | side\_effect\_severity                       | primary\_comorbidity | secondary\_comorbidity        | drug\_class           | source                                               |
| :----------------- | :------------------------ | :----------- | :------------------------------------------- | :------------------- | :---------------------------- | :-------------------- | :--------------------------------------------------- |
| Hydroxychloroquine | tablet (orally)           | dizziness    | call your doctor if severe or do not go away | heart disease        | liver disease                 | antimalarial          | <https://medlineplus.gov/druginfo/meds/a601240.html> |
| Azithromycin       | tablet (orally)           | dizziness    | call your doctor immediately                 | heart disease        | irregular heartbeat           | macrolide antibiotics | <https://medlineplus.gov/druginfo/meds/a697037.html> |
| Ivermectin         | tablet (orally)           | dizziness    | call your doctor if severe or do not go away | meningitis           | human African trypanosomiasis | anthelmintics         | <https://medlineplus.gov/druginfo/meds/a607069.html> |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "headache")
```

<div class="kable-table">

| drug               | route\_of\_administration | side\_effect | side\_effect\_severity                       | primary\_comorbidity | secondary\_comorbidity | drug\_class                              | source                                               |
| :----------------- | :------------------------ | :----------- | :------------------------------------------- | :------------------- | :--------------------- | :--------------------------------------- | :--------------------------------------------------- |
| Hydroxychloroquine | tablet (orally)           | headache     | call your doctor if severe or do not go away | heart disease        | liver disease          | antimalarial                             | <https://medlineplus.gov/druginfo/meds/a601240.html> |
| Azithromycin       | tablet (orally)           | headache     | call your doctor if severe or do not go away | heart disease        | irregular heartbeat    | macrolide antibiotics                    | <https://medlineplus.gov/druginfo/meds/a697037.html> |
| Tocilizumab        | injection                 | headache     | call your doctor if severe or do not go away | cancer               | diverticulitis         | interleukin-6 (IL-6) receptor inhibitors | <https://medlineplus.gov/druginfo/meds/a611004.html> |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "hives")
```

<div class="kable-table">

| drug         | route\_of\_administration | side\_effect | side\_effect\_severity       | primary\_comorbidity | secondary\_comorbidity        | drug\_class                              | source                                               |
| :----------- | :------------------------ | :----------- | :--------------------------- | :------------------- | :---------------------------- | :--------------------------------------- | :--------------------------------------------------- |
| Azithromycin | tablet (orally)           | hives        | call your doctor immediately | heart disease        | irregular heartbeat           | macrolide antibiotics                    | <https://medlineplus.gov/druginfo/meds/a697037.html> |
| Ivermectin   | tablet (orally)           | hives        | call your doctor immediately | meningitis           | human African trypanosomiasis | anthelmintics                            | <https://medlineplus.gov/druginfo/meds/a607069.html> |
| Tocilizumab  | injection                 | hives        | call your doctor immediately | cancer               | diverticulitis                | interleukin-6 (IL-6) receptor inhibitors | <https://medlineplus.gov/druginfo/meds/a611004.html> |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "itching")
```

<div class="kable-table">

| drug         | route\_of\_administration | side\_effect | side\_effect\_severity       | primary\_comorbidity | secondary\_comorbidity        | drug\_class                              | source                                               |
| :----------- | :------------------------ | :----------- | :--------------------------- | :------------------- | :---------------------------- | :--------------------------------------- | :--------------------------------------------------- |
| Azithromycin | tablet (orally)           | itching      | call your doctor immediately | heart disease        | irregular heartbeat           | macrolide antibiotics                    | <https://medlineplus.gov/druginfo/meds/a697037.html> |
| Ivermectin   | tablet (orally)           | itching      | call your doctor immediately | meningitis           | human African trypanosomiasis | anthelmintics                            | <https://medlineplus.gov/druginfo/meds/a607069.html> |
| Tocilizumab  | injection                 | itching      | call your doctor immediately | cancer               | diverticulitis                | interleukin-6 (IL-6) receptor inhibitors | <https://medlineplus.gov/druginfo/meds/a611004.html> |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "loss of appetite")
```

<div class="kable-table">

| drug               | route\_of\_administration | side\_effect     | side\_effect\_severity                       | primary\_comorbidity | secondary\_comorbidity        | drug\_class           | source                                               |
| :----------------- | :------------------------ | :--------------- | :------------------------------------------- | :------------------- | :---------------------------- | :-------------------- | :--------------------------------------------------- |
| Hydroxychloroquine | tablet (orally)           | loss of appetite | call your doctor if severe or do not go away | heart disease        | liver disease                 | antimalarial          | <https://medlineplus.gov/druginfo/meds/a601240.html> |
| Azithromycin       | tablet (orally)           | loss of appetite | call your doctor immediately                 | heart disease        | irregular heartbeat           | macrolide antibiotics | <https://medlineplus.gov/druginfo/meds/a697037.html> |
| Ivermectin         | tablet (orally)           | loss of appetite | call your doctor if severe or do not go away | meningitis           | human African trypanosomiasis | anthelmintics         | <https://medlineplus.gov/druginfo/meds/a607069.html> |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "nausea")
```

<div class="kable-table">

| drug               | route\_of\_administration | side\_effect | side\_effect\_severity                       | primary\_comorbidity | secondary\_comorbidity        | drug\_class           | source                                               |
| :----------------- | :------------------------ | :----------- | :------------------------------------------- | :------------------- | :---------------------------- | :-------------------- | :--------------------------------------------------- |
| Hydroxychloroquine | tablet (orally)           | nausea       | call your doctor if severe or do not go away | heart disease        | liver disease                 | antimalarial          | <https://medlineplus.gov/druginfo/meds/a601240.html> |
| Azithromycin       | tablet (orally)           | nausea       | call your doctor if severe or do not go away | heart disease        | irregular heartbeat           | macrolide antibiotics | <https://medlineplus.gov/druginfo/meds/a697037.html> |
| Ivermectin         | tablet (orally)           | nausea       | call your doctor if severe or do not go away | meningitis           | human African trypanosomiasis | anthelmintics         | <https://medlineplus.gov/druginfo/meds/a607069.html> |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "rash")
```

<div class="kable-table">

| drug               | route\_of\_administration | side\_effect | side\_effect\_severity                       | primary\_comorbidity | secondary\_comorbidity        | drug\_class                              | source                                               |
| :----------------- | :------------------------ | :----------- | :------------------------------------------- | :------------------- | :---------------------------- | :--------------------------------------- | :--------------------------------------------------- |
| Hydroxychloroquine | tablet (orally)           | rash         | call your doctor if severe or do not go away | heart disease        | liver disease                 | antimalarial                             | <https://medlineplus.gov/druginfo/meds/a601240.html> |
| Ivermectin         | tablet (orally)           | rash         | call your doctor immediately                 | meningitis           | human African trypanosomiasis | anthelmintics                            | <https://medlineplus.gov/druginfo/meds/a607069.html> |
| Tocilizumab        | injection                 | rash         | call your doctor immediately                 | cancer               | diverticulitis                | interleukin-6 (IL-6) receptor inhibitors | <https://medlineplus.gov/druginfo/meds/a611004.html> |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "vomiting")
```

<div class="kable-table">

| drug               | route\_of\_administration | side\_effect | side\_effect\_severity                       | primary\_comorbidity | secondary\_comorbidity        | drug\_class           | source                                               |
| :----------------- | :------------------------ | :----------- | :------------------------------------------- | :------------------- | :---------------------------- | :-------------------- | :--------------------------------------------------- |
| Hydroxychloroquine | tablet (orally)           | vomiting     | call your doctor if severe or do not go away | heart disease        | liver disease                 | antimalarial          | <https://medlineplus.gov/druginfo/meds/a601240.html> |
| Azithromycin       | tablet (orally)           | vomiting     | call your doctor if severe or do not go away | heart disease        | irregular heartbeat           | macrolide antibiotics | <https://medlineplus.gov/druginfo/meds/a697037.html> |
| Ivermectin         | tablet (orally)           | vomiting     | call your doctor if severe or do not go away | meningitis           | human African trypanosomiasis | anthelmintics         | <https://medlineplus.gov/druginfo/meds/a607069.html> |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "stomach pain")
```

<div class="kable-table">

| drug               | route\_of\_administration | side\_effect | side\_effect\_severity                       | primary\_comorbidity | secondary\_comorbidity | drug\_class           | source                                               |
| :----------------- | :------------------------ | :----------- | :------------------------------------------- | :------------------- | :--------------------- | :-------------------- | :--------------------------------------------------- |
| Hydroxychloroquine | tablet (orally)           | stomach pain | call your doctor if severe or do not go away | heart disease        | liver disease          | antimalarial          | <https://medlineplus.gov/druginfo/meds/a601240.html> |
| Azithromycin       | tablet (orally)           | stomach pain | call your doctor if severe or do not go away | heart disease        | irregular heartbeat    | macrolide antibiotics | <https://medlineplus.gov/druginfo/meds/a697037.html> |

</div>

``` r
CovidDrugs %>% 
  filter(side_effect == "unusual bleeding or bruising")
```

<div class="kable-table">

| drug               | route\_of\_administration | side\_effect                 | side\_effect\_severity       | primary\_comorbidity | secondary\_comorbidity | drug\_class           | source                                               |
| :----------------- | :------------------------ | :--------------------------- | :--------------------------- | :------------------- | :--------------------- | :-------------------- | :--------------------------------------------------- |
| Hydroxychloroquine | tablet (orally)           | unusual bleeding or bruising | call your doctor immediately | heart disease        | liver disease          | antimalarial          | <https://medlineplus.gov/druginfo/meds/a601240.html> |
| Azithromycin       | tablet (orally)           | unusual bleeding or bruising | call your doctor immediately | heart disease        | irregular heartbeat    | macrolide antibiotics | <https://medlineplus.gov/druginfo/meds/a697037.html> |

</div>

## Datawrapper table format

This format will include “Drug, Class, Adverse effects, Comorbidities”

``` r
# adverse events
CovidDrugAdverseEvents <- CovidDrugs %>%
  dplyr::select(Drug = drug, 
                side_effect) %>% 
  dplyr::group_by(Drug) %>%
  dplyr::summarize(
    `Adverse effects` = paste(side_effect, collapse = ', ')) %>% 
  dplyr::mutate(`Adverse effects` = stringr::str_to_sentence(`Adverse effects`))
```

    #>  `summarise()` ungrouping output (override with `.groups` argument)

``` r
CovidDrugAdverseEvents
```

<div class="kable-table">

| Drug               | Adverse effects                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| :----------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Azithromycin       | Nausea, diarrhea, vomiting, stomach pain, headache, fast, pounding, or irregular heartbeat, dizziness, fainting, rash with or without a fever, blisters or peeling, fever and pus-filled, blister-like sores, redness, and swelling of the skin, hives, itching, wheezing or difficulty breathing or swallowing, swelling of the face, throat, tongue, lips, eyes, hands, feet, ankles, or lower legs, hoarseness, vomiting or irritability while feeding (in infants less than 6 weeks old), severe diarrhea (watery or bloody stools) that may occur with or without fever and stomach cramps (may occur up to 2 months or more after your treatment), yellowing of the skin or eyes, extreme tiredness, unusual bleeding or bruising, lack of energy, loss of appetite, pain in the upper right part of the stomach, flu-like symptoms, dark-colored urine, unusual muscle weakness or difficulty with muscle control, pink and swollen eyes |
| Hydroxychloroquine | Headache, dizziness, loss of appetite, nausea, diarrhea, stomach pain, vomiting, rash, difficulty reading or seeing (words, letters, or parts of objects missing), sensitivity to light, blurred vision, changes in vision, seeing light flashes or streaks, difficulty hearing, ringing in ears, muscle weakness, unusual bleeding or bruising, bleaching or loss of hair, mood or mental changes, irregular heartbeat, drowsiness, convulsions, decreased consciousness or loss of consciousness, thinking about harming or killing yourself                                                                                                                                                                                                                                                                                                                                                                                                  |
| Ivermectin         | Dizziness, loss of appetite, nausea, vomiting, stomach pain or bloating, diarrhea, constipation, weakness, sleepiness, uncontrollable shaking of a part of the body, chest discomfort, fever, blistering or peeling skin, rash, hives, itching                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Remdesivir         | Pain, bleeding, bruising of the skin, soreness, or swelling near the place where the medication was injected, chills or shivering, nausea, vomiting, sweating, or dizziness upon standing up                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Tocilizumab        | Headache, runny nose or sneezing, redness, itching, pain, or swelling in the place where tocilizumab was injected, rash, flushing, hives, itching, swelling of the eyes, face, lips, tongue, throat, arms, hands, feet, ankles, or lower legs, difficulty breathing or swallowing, chest pain, dizziness or fainting, fever, ongoing stomach-area pain, or change in bowel habits, yellow eyes or skin; right upper abdominal pain; unexplained bruising or bleeding; loss of appetite; confusion; yellow or brown-colored urine; or pale stools                                                                                                                                                                                                                                                                                                                                                                                                |

</div>

``` r
# Comorbidities
CovidDrugComorbidities <- CovidDrugs %>% 
  dplyr::select(Drug = drug, 
                primary_comorbidity, 
                secondary_comorbidity) %>% 
  tidyr::unite(primary_comorbidity, secondary_comorbidity, 
               col = "Comorbidities", sep = ", ") %>% 
  dplyr::mutate(Comorbidities = stringr::str_to_sentence(Comorbidities)) %>%
  dplyr::distinct() 
CovidDrugComorbidities
```

<div class="kable-table">

| Drug               | Comorbidities                             |
| :----------------- | :---------------------------------------- |
| Hydroxychloroquine | Heart disease, liver disease              |
| Azithromycin       | Heart disease, irregular heartbeat        |
| Ivermectin         | Meningitis, human african trypanosomiasis |
| Remdesivir         | Liver disease, kidney disease             |
| Tocilizumab        | Cancer, diverticulitis                    |

</div>

``` r
# Drug and class
CovidDrugClass <- CovidDrugs %>% 
  dplyr::select(Drug = drug, 
                Class = drug_class) %>% 
  dplyr::mutate(Class = stringr::str_to_sentence(Class)) %>%
  dplyr::distinct()
# join
DataWrapperCovidDrugs <- dplyr::inner_join(CovidDrugClass, 
           CovidDrugAdverseEvents, by = "Drug") %>% 
  dplyr::inner_join(x = ., y = CovidDrugComorbidities, by = "Drug")
knitr::kable(DataWrapperCovidDrugs)
```

| Drug               | Class                                    | Adverse effects                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Comorbidities                             |
| :----------------- | :--------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :---------------------------------------- |
| Hydroxychloroquine | Antimalarial                             | Headache, dizziness, loss of appetite, nausea, diarrhea, stomach pain, vomiting, rash, difficulty reading or seeing (words, letters, or parts of objects missing), sensitivity to light, blurred vision, changes in vision, seeing light flashes or streaks, difficulty hearing, ringing in ears, muscle weakness, unusual bleeding or bruising, bleaching or loss of hair, mood or mental changes, irregular heartbeat, drowsiness, convulsions, decreased consciousness or loss of consciousness, thinking about harming or killing yourself                                                                                                                                                                                                                                                                                                                                                                                                  | Heart disease, liver disease              |
| Azithromycin       | Macrolide antibiotics                    | Nausea, diarrhea, vomiting, stomach pain, headache, fast, pounding, or irregular heartbeat, dizziness, fainting, rash with or without a fever, blisters or peeling, fever and pus-filled, blister-like sores, redness, and swelling of the skin, hives, itching, wheezing or difficulty breathing or swallowing, swelling of the face, throat, tongue, lips, eyes, hands, feet, ankles, or lower legs, hoarseness, vomiting or irritability while feeding (in infants less than 6 weeks old), severe diarrhea (watery or bloody stools) that may occur with or without fever and stomach cramps (may occur up to 2 months or more after your treatment), yellowing of the skin or eyes, extreme tiredness, unusual bleeding or bruising, lack of energy, loss of appetite, pain in the upper right part of the stomach, flu-like symptoms, dark-colored urine, unusual muscle weakness or difficulty with muscle control, pink and swollen eyes | Heart disease, irregular heartbeat        |
| Ivermectin         | Anthelmintics                            | Dizziness, loss of appetite, nausea, vomiting, stomach pain or bloating, diarrhea, constipation, weakness, sleepiness, uncontrollable shaking of a part of the body, chest discomfort, fever, blistering or peeling skin, rash, hives, itching                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Meningitis, human african trypanosomiasis |
| Remdesivir         | Antivirals                               | Pain, bleeding, bruising of the skin, soreness, or swelling near the place where the medication was injected, chills or shivering, nausea, vomiting, sweating, or dizziness upon standing up                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | Liver disease, kidney disease             |
| Tocilizumab        | Interleukin-6 (il-6) receptor inhibitors | Headache, runny nose or sneezing, redness, itching, pain, or swelling in the place where tocilizumab was injected, rash, flushing, hives, itching, swelling of the eyes, face, lips, tongue, throat, arms, hands, feet, ankles, or lower legs, difficulty breathing or swallowing, chest pain, dizziness or fainting, fever, ongoing stomach-area pain, or change in bowel habits, yellow eyes or skin; right upper abdominal pain; unexplained bruising or bleeding; loss of appetite; confusion; yellow or brown-colored urine; or pale stools                                                                                                                                                                                                                                                                                                                                                                                                | Cancer, diverticulitis                    |

Export these data as .csv

``` r
fs::dir_create("data/medline-side-effects")
readr::write_csv(as.data.frame(DataWrapperCovidDrugs), 
                 path = "data/medline-side-effects/DataWrapperCovidDrugs.csv")
```
