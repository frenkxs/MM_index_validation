# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT 2: Data cleaning -----------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# .This script prepares individual disease data for all indices.

# For all questions, contact Premysl Velek at p.velek@erasmusmc.nl



# ------------------------ Preliminaries ---------------------------------------

# libraries needed in this script
packages_needed <- c("foreign", "tidyverse", "lubridate", "here")

# install missing libraries
install.packages(setdiff(packages_needed, rownames(installed.packages())))
rm(packages_needed)

# load libraries
require(foreign)
require(tidyverse)
require(lubridate)
require(gtsummary)
require(survival)
library(gtsummary)
library(ggsurvfit)
library(colorspace)
library(haven)

# start here
setwd(here::here())




# ----------------------- Load and compile basic SHIFT data --------------------------

load(here::here("data", "shift_1", "shift_data_2024-04-22.Rdata"))

ergo_basic <- shift_data |>
  dplyr::select(ergoid, rs_cohort, birthd, sex)

# add mortality data
mortality <- here::here("data", "mortality", "fp_VitalStatus_2022-42.sav") %>%
  haven::read_sav(.) %>%
  select(ergoid, fp_vitalstatus, fp_censordate) %>%
  rename(died = fp_vitalstatus) %>%
  mutate(ergoid = factor(ergoid))

ergo_basic <- left_join(ergo_basic, mortality, by = "ergoid")
ergo_basic$died <- as.numeric(ergo_basic$died)

write_rds(ergo_basic, file = here::here("datasets", "ergo_basic.RData"))



# . ------------------------------------------------------------------------
# ----------------- Diseases -----------------------------------------------
# . ------------------------------------------------------------------------


## -----------------------COPD + asthma ----------------------------------------------------------

lung <- shift_data %>%
  mutate(
    prev_lung = case_when(
      prev_COPD == "no" & prev_asthma == "no" ~ "no",
      is.na(prev_COPD) | is.na(prev_asthma) ~ NA_character_,
      TRUE ~ "yes"
    ),
    inc_lung = case_when(
      prev_lung == "yes" ~ "prev",
      inc_COPD == "no" & inc_asthma == "no" ~ "no",
      inc_COPD == "yes" & inc_asthma == "yes" ~ "yes",

      # We need to make sure that all participants are at risk of both asthma and COPD. Hence
      # if follow-up for lung disease ends at the earliest end of follow up (either COPD or asthma).
      # Se we check if the incident case took place before the end of follow up of the other disease
      inc_COPD == "yes" & inc_asthma == "no" ~ if_else(endd_COPD > endd_asthma, "no", "yes"),
      inc_COPD == "no" & inc_asthma == "yes" ~ if_else(endd_asthma > endd_COPD, "no", "yes"),
      is.na(inc_COPD) | is.na(inc_asthma) ~ NA_character_
    )
  ) %>%
  rowwise() %>%
  mutate(endd_lung = min(endd_COPD, endd_asthma)) %>%
  ungroup() %>%

  # remove data on COPD and asthma
  select(c(
    prev_lung, endd_lung, inc_lung,
    startd_lung, ergoid, ic_ok_lung)
    )

readr::write_rds(lung, file = here::here("datasets", "lung.RData"))



## ------------- Stroke, diabetes, dementia, heart failure, cancer, COPD --------

stroke <- shift_data |>
  select(prev_stroke, inc_stroke, startd_stroke, endd_stroke, ic_ok_stroke, ergoid) |>
  mutate(across(prev_stroke:endd_stroke, ~ replace(., ic_ok_stroke != "yes", NA))) |>
  select(-ic_ok_stroke)

readr::write_rds(stroke, file = here::here("datasets", "stroke.RData"))


dia <- shift_data |>
  select(prev_dia, inc_dia, startd_dia, endd_dia, ergoid)

readr::write_rds(dia, file = here::here("datasets", "dia.RData"))


dem <- shift_data |>
  select(prev_dem, inc_dem, startd_dem, endd_dem, ic_ok_dem, ergoid) |>
  mutate(across(prev_dem:endd_dem, ~ replace(., ic_ok_dem != "yes", NA))) |>
  select(-ic_ok_dem)

readr::write_rds(dem, file = here::here("datasets", "dem.RData"))


hf <- shift_data |>
  select(prev_hf, inc_hf, startd_hf, endd_hf, ic_ok_hf, ergoid) |>
  mutate(across(prev_hf:endd_hf, ~ replace(., ic_ok_hf != "yes", NA))) |>
  select(-ic_ok_hf)

readr::write_rds(hf, file = here::here("datasets", "hf.RData"))


can <- shift_data |>
  select(prev_can1, inc_can1, startd_can1, endd_can1, ic_ok_can, ergoid) |>
  rename(prev_can = prev_can1,
         inc_can = inc_can1,
         startd_can = startd_can1,
         endd_can = endd_can1) |>
  mutate(across(prev_can:endd_can, ~ replace(., ic_ok_can != "yes", NA))) |>
  select(-ic_ok_can)

readr::write_rds(can, file = here::here("datasets", "can.RData"))


COPD <- shift_data |>
  select(prev_COPD, inc_COPD, startd_lung, endd_COPD, ic_ok_lung, ergoid) |>
  rename(startd_COPD = startd_lung) |>
  mutate(across(prev_COPD:endd_COPD, ~ replace(., ic_ok_lung != "yes", NA))) |>
  select(-ic_ok_lung)

readr::write_rds(COPD, file = here::here("datasets", "COPD.RData"))


## -----------------------Anemia --------------------------------------------------------------

# RS-I-3 -

hemo_rs_i_3_p <- here::here("data", "indices", "tooth", "hemoglobin", "e3_(3)_COULTER_(29-apr-2013).sav")
hemo_rs_i_3 <- haven::read_sav(hemo_rs_i_3_p) |>
  select(ergoid, e3_2880) |>
  mutate(ergoid = as.factor(ergoid)) %>%
  left_join(., ergo_basic, by = "ergoid") |>
  rename(hgb = e3_2880)
hemo_rs_i_3$sex <- factor(hemo_rs_i_3$sex, labels = c("male", "female"))
hemo_rs_i_3$rs_cohort <- "rs-i"

# Anemia is defined as having hemoglobin levels
#  - <8.1 mmol/L (13 g/dL) for men
#  - <7.5 mmol/L (12 g/dL) for women

hemo_rs_i_3 <- hemo_rs_i_3 %>%
  dplyr::mutate(anemia = case_when(sex == "male" & hgb < 8.1 ~ TRUE,
                            sex == "female" & hgb < 7.5 ~ TRUE,
                            is.na(hgb) ~ NA,
                            TRUE ~ FALSE))

hemo_rs_i_3 <- hemo_rs_i_3[, c(1, 5, 4, 3, 6, 7, 8)]

# add date of the blood sample
hemo_rs_i_3_date_p <- here::here("data", "indices", "tooth", "hemoglobin", "dates",
                                 "e3_(3)_BLDAFNAM_(10-jul-2017).sav")
haven::read_sav(hemo_rs_i_3_date_p) |>
  select(ergoid, e3_2686) |>
  mutate(ergoid = as.factor(ergoid)) %>%
  left_join(hemo_rs_i_3, ., by = "ergoid") |>
  rename(date = e3_2686) -> hemo_rs_i_3

# RS-II-1-

hemo_rs_ii_1_p <- here::here("data", "indices", "tooth", "hemoglobin", "ep_(1)_COULTER_(02-jun-2010).sav")
hemo_rs_ii_1 <- haven::read_sav(hemo_rs_ii_1_p) %>%
  mutate(ergoid = as.factor(ergoid)) %>%
  select(ergoid, ep_2880) %>%
  left_join(., ergo_basic, by = "ergoid") %>%
  rename(hgb = ep_2880)
hemo_rs_ii_1$sex <- factor(hemo_rs_ii_1$sex, labels = c("male", "female"))
hemo_rs_ii_1$rs_cohort <- "rs-ii"

# Anemia is defined as having hemoglobin levels
#  - <8.1 mmol/L (13 g/dL) for men
#  - <7.5 mmol/L (12 g/dL) for women

hemo_rs_ii_1 <- hemo_rs_ii_1 %>%
  mutate(anemia = case_when(sex == "male" & hgb < 8.1 ~ TRUE,
                            sex == "female" & hgb < 7.5 ~ TRUE,
                            is.na(hgb) ~ NA,
                            TRUE ~ FALSE))

hemo_rs_ii_1 <- hemo_rs_ii_1[, c(1, 5, 4, 3, 6, 7, 8)]

# add date of the blood sample
hemo_rs_ii_1_date_p <- here::here("data", "indices", "tooth", "hemoglobin", "dates",
                                  "ep_(1)_BLDAFNAM_(10-jul-2017).sav")
haven::read_sav(hemo_rs_ii_1_date_p) %>%
  mutate(ergoid = as.factor(ergoid)) %>%
  select(ergoid, ep_2686) %>%
  left_join(hemo_rs_ii_1, ., by = "ergoid") %>%
  rename(date = ep_2686) -> hemo_rs_ii_1


# RS-III-1 ---

hemo_rs_iii_1_p <- here::here("data", "indices", "tooth", "hemoglobin", "ej_(1)_LAB_(11-jun-2009).sav")
hemo_rs_iii_1 <- haven::read_sav(hemo_rs_iii_1_p) %>%
  mutate(ergoid = as.factor(ergoid)) %>%
  select(ergoid, ej_12849) %>%
  left_join(., ergo_basic, by = "ergoid") %>%
  rename(hgb = ej_12849)
hemo_rs_iii_1$sex <- factor(hemo_rs_iii_1$sex, labels = c("male", "female"))
hemo_rs_iii_1$rs_cohort <- "rs-iii"

# Anemia is defined as having hemoglobin levels
#  - <8.1 mmol/L (13 g/dL) for men
#  - <7.5 mmol/L (12 g/dL) for women

hemo_rs_iii_1 <- hemo_rs_iii_1 %>%
  mutate(anemia = case_when(sex == "male" & hgb < 8.1 ~ TRUE,
                            sex == "female" & hgb < 7.5 ~ TRUE,
                            is.na(hgb) ~ NA,
                            TRUE ~ FALSE))

hemo_rs_iii_1 <- hemo_rs_iii_1[, c(1, 5, 4, 3, 6, 7, 8)]

# add date of the blood sample
hemo_rs_iii_1_date_p <- here::here("data", "indices", "tooth", "hemoglobin", "dates",
                                   "ej_(1)_BLDAFNAM_(10-jul-2017).sav")
haven::read_sav(hemo_rs_iii_1_date_p) %>%
  mutate(ergoid = as.factor(ergoid)) %>%
  select(ergoid, ej_2686) %>%
  left_join(hemo_rs_iii_1, ., by = "ergoid") %>%
  rename(date = ej_2686) -> hemo_rs_iii_1

# put it all together

anemia <- bind_rows(hemo_rs_i_3, hemo_rs_ii_1, hemo_rs_iii_1)
anemia <- anemia[, c("ergoid", "anemia", "date")]

names(anemia)[3] <- "startd_anemia"


readr::write_rds(anemia, file = here::here("datasets", "anemia.RData"))


## ----------------------- Coronary heart disease + heart failure ------------------------------
hd <- shift_data %>%
  rowwise() %>%
  mutate(startd_hd = max(startd_chd, startd_hf),
         prev_hd = case_when(prev_hf == "no" & prev_chd == "no" ~ "no",
                             is.na(prev_hf) | is.na(prev_chd) ~ NA_character_,
                             TRUE ~ "yes"),

         inc_hd = case_when(inc_hf == "no" & inc_chd == "no" ~ "no",
                            inc_hf == "yes" & inc_chd == "yes" ~ "yes",
                            prev_hd == "yes" ~ "prev",

                            #' This is to account for the possibility that the incident case
                            #' takes place after the follow up end of the non-incident case. In that
                            #' case we don't have a incident heart disease as the follow up for HD ended
                            #' before the incident case happened
                            inc_hf == "yes" & inc_chd == "no" ~ if_else(endd_hf > endd_chd, "no", "yes"),
                            inc_hf == "no" & inc_chd == "yes" ~ if_else(endd_chd > endd_hf, "no", "yes"),

                            is.na(inc_hf) | is.na(inc_chd) ~ NA_character_),

         endd_hd = min(endd_chd, endd_hf),

         ic_ok_hd = case_when(ic_ok_hf == "yes" & ic_ok_chd == "yes" ~ "yes",
                              is.na(ic_ok_hf) | is.na(ic_ok_chd) ~ NA_character_,
                              TRUE ~ "no")
  ) |>

  select(prev_hd, inc_hd, startd_hd, endd_hd, ic_ok_hd, ergoid) |>
  mutate(across(prev_hd:endd_hd, ~ replace(., ic_ok_hd != "yes", NA))) |>
  select(-ic_ok_hd)

readr::write_rds(hd, file = here::here("datasets", "hd.RData"))



## --------------  Chronic renal failure (aka Chronic Kidney disease) ------------------


# load dataset
ckd <- here::here("data", "CKD", "CKD_set.sav") %>%
  haven::read_sav(.)

ckd <- ckd %>%
  dplyr::rename(startd_ckd = first_eGFRdate,
                endd_ckd = last_eGFRdate,
                inc_ckd = incident_CKD,
                prev_ckd = prevalent_CKD) %>%
  mutate(ergoid = as.factor(ergoid),
         inc_ckd = if_else(inc_ckd == 1, "yes", "no"),
         inc_ckd = if_else(prev_ckd == 1, "prev", inc_ckd),
         prev_ckd = if_else(prev_ckd == 1, "yes", "no"))

# if incident case, the endd_ckd will be the date of the diagnosis, rather than the date of the
# last measurement. Similarly, for prevalent cases, the end date and start date will be equal
ckd <- ckd %>%
  mutate(endd_ckd = if_else(inc_ckd == "yes", date_CKD, endd_ckd),
         endd_ckd = if_else(prev_ckd == "yes", startd_ckd, endd_ckd)) %>%
  select(-date_CKD)

# add the remaining data (participants who are not in the CKD dataset)
ckd <- ckd %>%
  left_join(ergo_basic %>% select(ergoid), ., by = "ergoid")

readr::write_rds(ckd, file = here::here("datasets", "ckd.RData"))


##  -----------------------Major vs minor stroke -------------------------------------
# load dataset
nihss <- here::here("data", "stroke", "Cases_clean_20222305.sav") %>%
  haven::read_sav(.) %>%

  # Now, since we don't have data on major/minor stroke for prevalent cases, we'd
  # have to move the baseline so that there are some cases of major strokes. Since
  # the age at baseline of the original cohort is close to 80, we'd have to do it
  # anyway.
  mutate(ergoid = as.factor(ergoid)) %>%
  left_join(., shift_data %>% select(ergoid, endd_stroke, startd_stroke,
                                     fp_censordate)) %>%
  dplyr:: rename(startd_mastroke = startd_stroke) %>%

  # We will also remove participants from RS-IV (those with ergoid not included in the
  # ERGO basic data)
  dplyr::filter(ergoid %in% ergo_basic$ergoid) %>%

  # only select those with major stroke
  dplyr::filter(TIA_NIHSSgt3 == 3) %>%

  # To make it consistent with other datasets, I still include the prevalent column, but it'll be
  # negative for all participants. The start date will be the start date for stroke. We will
  # also assume that all recurrent strokes were mini (a somewhat bold assumption), and that the
  # end of follow up is the ergo censor date (which may not be always correct)
  mutate(prev_mastroke = "no",
         inc_mastroke = "yes",
         endd_mastroke = Eventdate) %>%

  dplyr::select(ergoid, prev_mastroke, inc_mastroke, endd_mastroke)

#' add all the other participants to the dataset. In this case we need to see
#' whether a participant is free of major stroke or follow-up data are missing.
#' We need to compare major stroke data with stroke data. All those with missing
#' data don't give informed consent, therefore we set those erogids to missing

mastroke <- left_join(shift_data %>% select(ergoid, endd_stroke,
                                         startd_stroke, ic_ok_stroke,
                                         prev_stroke), nihss) %>%


  dplyr::rename(ic_ok_mastroke = ic_ok_stroke,
                startd_mastroke = startd_stroke) %>%
  mutate(prev_mastroke = if_else(ic_ok_mastroke == "yes", "no", NA_character_),

         # We have to remove those with prevalent stroke as we don't have the NIHSS scale
         # for them and cannot establish whether they had minor or major stroke. Those
         # cases will be removed at later stage when merging with the rest of the data.
         prev_mastroke = if_else(prev_stroke == "yes", NA_character_, prev_mastroke),
         endd_mastroke = if_else(ic_ok_mastroke == "yes" & is.na(inc_mastroke),
                                 endd_stroke, endd_mastroke),
         inc_mastroke = if_else(ic_ok_mastroke == "yes" & is.na(inc_mastroke),
                                "no", inc_mastroke)) %>%

  dplyr::select(-c(endd_stroke, prev_stroke, ic_ok_mastroke))

# Update 22 April 2024: There are some mismatches between the stroke severity data and the stroke
# data. The stroke data are the reference data here, so all deviations from stroke data will be
# excluded
temp <- left_join(mastroke, stroke, by = "ergoid") |>
  dplyr::filter(inc_mastroke == "yes" & inc_stroke != "yes") |>

  # those with incident major stroke and no incident or prevalent stoke will be set as stroke-free
  dplyr::mutate(status = case_when(
    inc_mastroke == "yes" & inc_stroke == "no" ~ "stroke_free",

  # those with incident major stroke and prevalent stroke will be set as with prevalent major stroke
    inc_mastroke == "yes" & inc_stroke == "prev" ~ "mastroke_prev"
  ))

mastroke <- mastroke %>%
  dplyr::left_join(stroke |> dplyr::select(ergoid, endd_stroke), by = "ergoid") |>
  dplyr::mutate(prev_mastroke = if_else(ergoid %in% temp$ergoid[temp$status == "mastroke_prev"],
                                        "yes", prev_mastroke),
                inc_mastroke = case_when(ergoid %in% temp$ergoid[temp$status == "stroke_free"] ~
                                       "no",
                                       ergoid %in% temp$ergoid[temp$status == "mastroke_prev"] ~
                                         "prev",
                                       .default = inc_mastroke),
                endd_mastroke = if_else(ergoid %in% temp$ergoid[temp$status == "stroke_free"],
                                        endd_stroke, endd_mastroke),

                endd_mastroke = if_else(ergoid %in% temp$ergoid[temp$status == "mastroke_prev"],
                                        NA, endd_mastroke)
                )

readr::write_rds(mastroke, file = here::here("datasets", "mastroke.RData"))


## ----------------------- stroke + TIA------------------------------

cvd <- shift_data %>%
  rowwise() %>%
  mutate(startd_cvd = max(startd_stroke, startd_tia),
         prev_cvd = case_when(prev_tia == "no" & prev_stroke == "no" ~ "no",
                             is.na(prev_tia) | is.na(prev_stroke) ~ NA_character_,
                             TRUE ~ "yes"),

         inc_cvd = case_when(inc_tia == "no" & inc_stroke == "no" ~ "no",
                             inc_tia == "yes" & inc_stroke == "yes" ~ "yes",
                             prev_cvd == "yes" ~ "prev",

                            #' This is to account for the possibility that the incident case takes
                            #' place after the follow up end of the non-incident case. In that case
                            #' we don't have a incident heart disease as the follow up for HD ended
                            #' before the incident case happened
                            inc_tia == "yes" & inc_stroke == "no" ~ if_else(endd_tia > endd_stroke, "no", "yes"),
                            inc_tia == "no" & inc_stroke == "yes" ~ if_else(endd_stroke > endd_tia, "no", "yes"),

                            is.na(inc_tia) | is.na(inc_stroke) ~ NA_character_),

         endd_cvd = min(endd_stroke, endd_tia),

         ic_ok_cvd = case_when(ic_ok_tia == "yes" & ic_ok_stroke == "yes" ~ "yes",
                              is.na(ic_ok_tia) | is.na(ic_ok_stroke) ~ NA_character_,
                              TRUE ~ "no")
  ) |>

  select(prev_cvd, inc_cvd, startd_cvd, endd_cvd, ic_ok_cvd, ergoid) |>
  mutate(across(prev_cvd:endd_cvd, ~ replace(., ic_ok_cvd != "yes", NA))) |>
  select(-ic_ok_cvd)

readr::write_rds(cvd, file = here::here("datasets", "cvd.RData"))

## ---------------- Myocardial infarction --------------------------------------

mi <- read_sav(here::here("results", "Fan", "fan_data_final.sav")) |>
  select(ergoid, contains("_mi"), startd_chd) |>
  mutate(startd_mi = lubridate::ymd(startd_chd)) |>
  mutate(ergoid = as.factor(ergoid),
    prev_mi = case_when(prev_mi == 1 ~ "yes",
                        prev_mi == 0 ~ "no",
                        .default = NA_character_),
    inc_mi = case_when(inc_mi == 2 ~ "yes",
                      inc_mi == 1 ~ "prev",
                      inc_mi == 0 ~ "no",
                      .default = NA_character_)) |>

  mutate(across(c(prev_mi, endd_mi,
                  inc_mi, prev_mi), ~ replace(., ic_ok_mi != 1, NA))) |>
  select(-c(ic_ok_mi, startd_chd))



readr::write_rds(mi, file = here::here("datasets", "mi.RData"))


## ---------------- Ankle-brachial index --------------------------------------

abi <- read_sav(here::here("results", "Charlson Ghali 1996", "Final_data_charlson.sav")) |>
  select(ergoid, contains("_abi")) |>
  rename(abi = prev_abi) |>
  mutate(ergoid = as.factor(ergoid))


readr::write_rds(abi, file = here::here("datasets", "abi.RData"))


## ------------ Cancer data -----------------------------------------


#' we have:
#' 1. solid tumor localised, ICD-9:	140-195.8
#' 2. metastatic cancer, ICD-9: 196-199.0
#' 3. Lymphoma/leukemia, ICD-9: 200-208.91
#' We need to go back to the original data

# We map ICD-9 codes to ICD-10 codes and only select the relevant cancer types


can <- here::here("data", "Cancer",
                  "ONCOLOGY_prevalenceANDincidence_morbidityANDmortality_29.02.2020.sav") %>%
  haven::read_sav(.)



# rename informed consent column to keep track of consent per conditions
names(can)[5] <- "ic_ok_can"

# keep only relevant columns and convert labelled colummns to factors
can <- can[, c(1, 5, 6, 9, 10, 13)] %>%
  haven::as_factor(.)

# remove all possible cases. Only probable and certain cases are considered
can <- can[can$fupsure != "possible", ]

# ------ Read this
# We keep all data in, including those with unclear follow up status (n.a. - dead).
# there are 35 participants who do have cancer according to the data but it's not
# clear if its prevalent or incident case

# !!! Need to ask about this !!
# Here's how I found out:

# temp <- can %>%
#   group_by(ergoid, fupstat) %>%
#   summarise(n = n())
#
# nas <- temp$ergoid[temp$fupstat == "n.a. - dead"] %>% unique()
# non_nas <- temp$ergoid[temp$fupstat != "n.a. - dead"] %>% unique()
# unclear_fupstat <- nas[!(nas %in% non_nas)]

# ------ End


# load the ICD-9 to ICD-10 conversion tables
icd_convert <- here::here("R", "desai2002", "ICD_mapping", "icd9toicd10cmgem.csv") %>%
  readr::read_csv2(.)

# Since the coding in ERGO data is not consistent (sometimes 3 characters are used, sometimes 4)
# We'll match the codes based on the first three characters (ie. C83.0 becomes C83)

#' 1. solid tumor localised, ICD-9:	140-195.8
solid_tumor_codes <- icd_convert %>%
  dplyr::filter(icd9cm >= 1400 & icd9cm <= 1958,
                grepl("C", icd10cm, fixed = TRUE)) %>%
  mutate(icd10cm = substring(icd10cm, 1, 3)) %>%
  pull(icd10cm)

#' 2. metastatic cancer, ICD-9: 196-199.0
metastatic_codes <- icd_convert %>%
  dplyr::filter(icd9cm >= 1960 & icd9cm <= 1990,
                grepl("C", icd10cm, fixed = TRUE)) %>%
  mutate(icd10cm = substring(icd10cm, 1, 3)) %>%
  pull(icd10cm)

#' 3. Lymphoma/leukemia, ICD-9: 200.00-208.91
leukemia_codes <- icd_convert %>%
  dplyr::filter(icd9cm >= 20000 & icd9cm <= 20891,
                grepl("C", icd10cm, fixed = TRUE)) %>%
  mutate(icd10cm = substring(icd10cm, 1, 3)) %>%
  pull(icd10cm)

# only use the first three characters in the ICD-10 codes in the ERGO data
can <- can %>%
  mutate(fupicd10 = substring(fupicd10, 1, 3),
         ergoid = as.factor(ergoid))


# create variables for incidence and prevalence
# We assume that all participants with unclear fu status (n.a. - dead) are
# incident cases
can$fupstat[can$fupstat == "n.a. - dead"] <- "incident"

can$prev_can[can$fupstat == "prevalent"] <- "yes"
can$prev_can[can$fupstat == "incident"] <- "no"
can$prev_can[can$fupstat == "n.a. - dead"] <- "no"

can$inc_can[can$fupstat == "incident"] <- "yes"
can$inc_can[can$fupstat == "n.a. - dead"] <- "yes"
can$inc_can[can$fupstat == "prevalent"] <- "prev"



# we assume that 1) all cancer free participants give informed consent for cancer
# and 2) that the end date for cancer is the same as the end date and start date in the
# vital status dataset (fp_censordate, fp_startdate).
#
# !!! Importantly, we assume that participants who are not included in the cancer
# dataset are cancer free, so we disregard the possibility that someone has cancer and
# did not give informed consent for cancer data. !!!

### Solid tumor ---------------------------------------------------------------------------------

# we take the most recent prevalent case and the earliest incidence case for each participant
can_solid_t <- can %>%
  dplyr::filter(fupicd10 %in% solid_tumor_codes) %>%
  group_by(ergoid, fupstat) %>%
  mutate(order = row_number(eventdat)) %>%
  arrange(order) %>%
  ungroup()


# select the observation with the earliest prevalent case, any later case will
# be disregarded
temp_prev <- can_solid_t %>%
  dplyr::filter(fupstat == "prevalent",
                order == 1)

# select the observation with the earliest incident case, any later case will
# be disregarded.
# We only select those with no prevalent case
temp_inc <- can_solid_t %>%
  dplyr::filter(fupstat  == "incident",
                !(ergoid %in% temp_prev$ergoid)) %>%
  dplyr::filter(order == 1)

# Now we have a table with exactly one row per participant. The event date is
# the earliest event date if participant has a prevalent case, irrespective of any
# potential later prevalent or incident cases. For pariticipants with only incident
# cases, the event date will be the earliest incident date
can_solid_t <- bind_rows(temp_inc, temp_prev) %>%
  dplyr::rename(endd_can_solid_t = eventdat,
                prev_can_solid_t = prev_can,
                inc_can_solid_t = inc_can,
                ic_ok_can_solid_t = ic_ok_can)


can_solid_t <- shift_data %>%
  select(ergoid, fp_startdate, fp_censordate, startd_can1) %>%
  left_join(., can_solid_t, by = "ergoid") %>%
  mutate(ic_ok_can_solid_t = replace_na(ic_ok_can_solid_t, "yes"),
         endd_can_solid_t = if_else(is.na(endd_can_solid_t), fp_censordate,
                                    endd_can_solid_t),
         startd_can_solid_t = startd_can1,
         prev_can_solid_t = replace_na(prev_can_solid_t, "no"),
         inc_can_solid_t = replace_na(inc_can_solid_t, "no")) %>%
  select(-c(fupsure, fupstat, order, fupicd10, fp_startdate, fp_censordate, startd_can1)) |>
  mutate(across(c(prev_can_solid_t, endd_can_solid_t,
                  inc_can_solid_t, prev_can_solid_t), ~ replace(., ic_ok_can_solid_t != "yes", NA))) |>
  select(-ic_ok_can_solid_t)

readr::write_rds(can_solid_t, file = here::here("datasets", "can_solid_t.RData"))

### metastatic --------------------------------------------------------------

can_metastatic <- can %>%
  dplyr::filter(fupicd10 %in% metastatic_codes) %>%
  group_by(ergoid, fupstat) %>%
  mutate(order = row_number(eventdat)) %>%
  ungroup()



# select the observation with the latest prevalent case, any earlier case will be disregarded
temp_prev <- can_metastatic %>%
  dplyr::filter(fupstat == "prevalent",
                order == 1)

# select the observation with the earliest incident case, any later case will
# be disregarded.
# We only select those with no prevalent case
temp_inc <- can_metastatic %>%
  dplyr::filter(fupstat == "incident",

                # we only select those with no prevalent case
                !(ergoid %in% temp_prev$ergoid)) %>%
  dplyr::filter(order == 1)

can_metastatic <- bind_rows(temp_inc, temp_prev) %>%
  dplyr::rename(endd_can_metastatic = eventdat,
                prev_can_metastatic = prev_can,
                inc_can_metastatic = inc_can,
                ic_ok_can_metastatic = ic_ok_can)

can_metastatic <- shift_data %>%
  select(ergoid, fp_startdate, fp_censordate, startd_can1) %>%
  left_join(., can_metastatic, by = "ergoid") %>%
  mutate(ic_ok_can_metastatic = replace_na(ic_ok_can_metastatic, "yes"),
         endd_can_metastatic = if_else(is.na(endd_can_metastatic), fp_censordate,
                                       endd_can_metastatic),
         startd_can_metastatic = startd_can1,
         prev_can_metastatic = replace_na(prev_can_metastatic, "no"),
         inc_can_metastatic = replace_na(inc_can_metastatic, "no")) %>%
  select(-c(fupsure, fupstat, fp_startdate, fp_censordate, order, fupicd10, startd_can1)) |>
  mutate(across(c(prev_can_metastatic, endd_can_metastatic,
                  inc_can_metastatic, prev_can_metastatic), ~ replace(., ic_ok_can_metastatic != "yes", NA))) |>
  select(-ic_ok_can_metastatic)

readr::write_rds(can_metastatic, file = here::here("datasets", "can_metastatic.RData"))


### leukemia ---------
can_leukemia <- can %>%
  dplyr::filter(fupicd10 %in% leukemia_codes) %>%
  group_by(ergoid, fupstat) %>%
  mutate(order = row_number(eventdat)) %>%
  ungroup()

# select the observation with the latest prevalent case, any earlier case will
# be disregarded
temp_prev <- can_leukemia %>%
  dplyr::filter(fupstat == "prevalent",
                order == 1)

# select the observation with the earliest incident case, any later case will be
# disregarded
temp_inc <- can_leukemia %>%
  dplyr::filter(fupstat == "incident",

                # we only select those with no prevalent case
                !(ergoid %in% temp_prev$ergoid)) %>%
  dplyr::filter(order == 1)

can_leukemia <- bind_rows(temp_inc, temp_prev) %>%
  dplyr::rename(endd_can_leukemia = eventdat,
                prev_can_leukemia = prev_can,
                inc_can_leukemia = inc_can,
                ic_ok_can_leukemia = ic_ok_can)


can_leukemia <- shift_data %>%
  select(ergoid, fp_startdate, fp_censordate, startd_can1) %>%
  left_join(., can_leukemia, by = "ergoid") %>%
  mutate(ic_ok_can_leukemia = replace_na(ic_ok_can_leukemia, "yes"),
         endd_can_leukemia = if_else(is.na(endd_can_leukemia), fp_censordate,
                                     endd_can_leukemia),
         startd_can_leukemia = startd_can1,
         prev_can_leukemia = replace_na(prev_can_leukemia, "no"),
         inc_can_leukemia = replace_na(inc_can_leukemia, "no")) %>%
  select(-c(fupsure, fupstat, fp_startdate, fp_censordate,
            order, fupicd10, startd_can1)) |>
  mutate(across(c(prev_can_leukemia, endd_can_leukemia,
                  inc_can_leukemia, prev_can_leukemia),
                ~ replace(., ic_ok_can_leukemia != "yes", NA))) |>
  select(-ic_ok_can_leukemia)

readr::write_rds(can_leukemia, file = here::here("datasets", "can_leukemia.RData"))


# . ------------------------------------------------------------------------
# --------------- Life style covariates -------------------------------------
# . ------------------------------------------------------------------------

## -------------------- Smoking --------------------------------------------------------

load(here::here("results", "fan" , "smoking.RData"))


# this is the smoking data taken at RS-I-4, RS-II-2 and RS-III-1 for RS-I cohort
smoking <- smoking |>
  dplyr::mutate(ergoid = as.factor(ergoid)) %>%
  left_join(., ergo_basic, by = "ergoid") |>

  # Now, we need the fourth interview for RS-I cohort, second interview for RS-II and first
  # interview for RS-III
  dplyr::mutate(smoking = case_when(rs_cohort == "RS-I" ~ smoker.4,
                                    rs_cohort == "RS-II" ~ smoker.2,
                                    rs_cohort == "RS-III" ~ smoker.1),
                startd_smoking = case_when(rs_cohort == "RS-I" ~ int4_d,
                                           rs_cohort == "RS-II" ~ int2_d,
                                           rs_cohort == "RS-III" ~ int1_d)
                ) |>


  # we code it as follows:
  # 0: never
  # 1: former
  # 2: current
  dplyr::mutate(smoking = case_when(smoking == "never" ~ 0,
                                    smoking == "former" ~ 1,
                                    smoking == "current" ~ 2)
  ) |>
  dplyr::select(ergoid, smoking, startd_smoking)

readr::write_rds(smoking, file = here::here("datasets", "smoking.RData"))


##  -------------------- ADL ----------------------------------------

lee <- haven::read_sav("results/Lee/ADL_Lee_final.sav")

# difficulty bathing
bathing <- lee |>
  dplyr::select(ergoid, Bathing, Afspraak_interview) |>
  mutate(ergoid = as.factor(ergoid),

         # since this is codes as 1 vs 2, rather than 0 and 1
         Bathing = Bathing - 1) |>
  rename(startd_bathing = Afspraak_interview,
         bathing = Bathing)


finances <- lee |>
  dplyr::select(ergoid, Finances, Afspraak_interview) |>
  mutate(ergoid = as.factor(ergoid),
         Finances = Finances - 1) |>
  rename(startd_finances = Afspraak_interview,
         finances = Finances)


walking <- lee |>
  dplyr::select(ergoid, Flat_terrain, Afspraak_interview) |>
  mutate(ergoid = as.factor(ergoid),
         Flat_terrain = Flat_terrain - 1) |>
  rename(startd_walking = Afspraak_interview,
         walking = Flat_terrain)


push_pull <- lee |>
  dplyr::select(ergoid, Bed_pushing_pulling, Afspraak_interview) |>
  mutate(ergoid = as.factor(ergoid),
         Bed_pushing_pulling = Bed_pushing_pulling - 1) |>
  rename(startd_push_pull = Afspraak_interview,
         push_pull = Bed_pushing_pulling)


adl <- list(bathing, finances, walking, push_pull) |>
  reduce(left_join, by = "ergoid")


readr::write_rds(adl, file = here::here("datasets", "adl.RData"))


##  -------------------- BMI ----------------------------------------

lee <- haven::read_sav("results/Lee/ADL_Lee_final.sav")

# difficulty bathing
bmi <- lee |>
  dplyr::select(ergoid, bmi_binary) |>
  mutate(ergoid = as.factor(ergoid))


visits_e4 <- haven::read_sav(here::here("data", "visits", "e4_(4)_RESPONS_(12-mar-2018)_excerpt.sav"))
visits_ej <- haven::read_sav(here::here("data", "visits", "ej_(1)_RESPONS_(04-apr-2016)_excerpt.sav"))


visits_e4 <- visits_e4[, c(1, 9)]
names(visits_e4) <- c("ergoid", "visit_d")
visits_ej <- visits_ej[, c(1, 9)]
names(visits_ej) <- c("ergoid", "visit_d")


visits <- bind_rows(visits_e4, visits_ej)
visits$ergoid <- as.factor(visits$ergoid)


bmi <- left_join(bmi, visits, by = "ergoid")
bmi <- bmi |>
  dplyr::rename(bmi = bmi_binary,
                startd_bmi = visit_d)


readr::write_rds(bmi, file = here::here("datasets", "bmi.RData"))


