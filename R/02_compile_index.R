# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT 2: Compiling intex data ----------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# .This script prepares individual disease data for all indices. It builds on the series of
# functions in the cohort builder (get_cohorts.R)

# For all questions, contact Premysl Velek at p.velek@erasmusmc.nl

# start here
setwd(here::here())
source(here::here("R", "99_get_cohorts.R"))

# . ----------
# Tooth, 2008 index -------------------------------------------------------------

# heart disease = 1
# stroke = 2
# low iron = 1
# lung disease = 2
# diabetes = 1
# cancer = 3
# ad = 4

d <- c("hd", "stroke", "anemia", "lung", "dia", "can", "dem")

w <- c(1, 2, 1, 2, 1, 3, 4)

tooth <- compile_cohort(diseases = d)


tooth_index <- get_index_cohort(index_data = tooth,
                                mean_age_index = NULL,
                                sex_ratio_index = "women_only",
                                longitudinal_diseases = c("hd", "stroke", "lung",
                                                          "dia", "can", "dem"),
                                cross_sectional_diseases = c("anemia"),
                                fu_time_index = 6)

tooth_count <- get_count_cohort(index_cohort_data = tooth_index)

tooth_index <- tooth_index[tooth_index$ergoid %in% tooth_count$ergoid, ]

tooth_index_values <- get_index_values(index_cohort_data = tooth_index,
                                      diseases = d,
                                      weights = w)

save(tooth_index_values, tooth_count, tooth_index,
     file = here::here("RESULTS_FINAL", "final_data", "tooth2008_data.RData"))

## Data for sensitivity analyses -------------
tooth_index_all_age_sex <- get_index_cohort(index_data = tooth,
                                mean_age_index = NULL,
                                sex_ratio_index = "both",
                                longitudinal_diseases = c("hd", "stroke", "lung",
                                                          "dia", "can", "dem"),
                                cross_sectional_diseases = c("anemia"),
                                fu_time_index = 6)

tooth_count_all_age_sex <- get_count_cohort(index_cohort_data = tooth_index_all_age_sex)

tooth_index_all_age_sex <- tooth_index_all_age_sex[tooth_index_all_age_sex$ergoid %in% tooth_count_all_age_sex$ergoid, ]

tooth_index_values_all_age_sex <- get_index_values(
  index_cohort_data = tooth_index_all_age_sex,
                                       diseases = d,
                                       weights = w)

save(tooth_index_values_all_age_sex, tooth_count_all_age_sex, tooth_index_all_age_sex,
     file = here::here("RESULTS_FINAL", "final_data", "tooth2008_data_all_age_sex.RData"))

# . ----------
# Desai, 2002 index -------------------------------------------------------------

#     CHF/cardiomyopathy = 2
#     Pneumonia = 1 (not present)
#     COPD/chronic lung disease = 2
#     Cancer (solid tumor, localized) = 3
#     Cancer (metastatic) = 3
#     Lymphoma/leukemia = 6
#     Major stroke (hemiplegia) = 2
#     Acute renal failure = 5 (not present)
#     Chronic renal failure = 2
#     Diabetes mellitus with end-organ damage = 1

d <- c("hf", "lung", "can_solid_t", "can_metastatic",
       "can_leukemia", "mastroke", "ckd", "dia")

w <- c(2, 2, 3, 3, 6, 2, 2, 1)

desai <- compile_cohort(diseases = d)


desai_index <- get_index_cohort(index_data = desai,
                                sex_ratio_index = "both",
                                longitudinal_diseases = d,
                                mean_age_index = 79,
                                fu_time_index = 1)

desai_count <- get_count_cohort(index_cohort_data = desai_index)

desai_index <- desai_index[desai_index$ergoid %in% desai_count$ergoid, ]

desai_index_values <- get_index_values(index_cohort_data = desai_index,
                                       diseases = d,
                                       weights = w)

save(desai_index_values, desai_count, desai_index,
     file = here::here("RESULTS_FINAL", "final_data", "desai2002_data.RData"))

## Data for sensitivity analyses -------------

# Now compile the data with diabetes only assigned to participants who died
desai_index_dm <- desai_index |>
  dplyr::mutate(dia = if_else(dia == 1 & died == 1, 1, 0))

desai_index_values_dm <- get_index_values(index_cohort_data = desai_index_dm,
                                       diseases = d,
                                       weights = w)

save(desai_index_values_dm, desai_count, desai_index_dm,
     file = here::here("RESULTS_FINAL", "final_data", "desai2002_data_dm.RData"))


# Now compile the index data for population without age restriction
desai_index_all_age <- get_index_cohort(index_data = desai,
                                sex_ratio_index = "both",
                                longitudinal_diseases = d,
                                fu_time_index = 1)

desai_count_all_age <- get_count_cohort(index_cohort_data = desai_index_all_age)

desai_index_all_age <- desai_index_all_age[desai_index_all_age$ergoid %in% desai_count_all_age$ergoid, ]

desai_index_values_all_age <- get_index_values(index_cohort_data = desai_index_all_age,
                                       diseases = d,
                                       weights = w)

save(desai_index_values_all_age, desai_count_all_age, desai_index_all_age,
     file = here::here("RESULTS_FINAL", "final_data", "desai2002_data_all_age.RData"))

# . ----------
# Lee, 2006 index -------------------------------------------------------------

#     age:
#     below 65 = 1
#     65-69 = 2
#     70-74 = 3
#     75-79 = 4
#     80-84 = 5
#     85+ = 7
#
#     Male = 2 (Female 0)

#     BMI > 25 = 1
#     Diabetes = 1
#     Cancer = 2
#     Heart failure = 2
#     Lung disease = 2
#     Smoke = 2 (current only)
#     Bathing = 2
#     Finances = 2
#     Walking = 2
#     Push-Pull = 1

d <- c("bmi", "dia", "can", "lung", "hf", "smoking", "adl")

w <- c(1, 1, 2, 2, 2, 2, 2, 2, 2, 1)

lee <- compile_cohort(diseases = d)


lee_index <- get_index_cohort(index_data = lee,
                              sex_ratio_index = "both",
                              longitudinal_diseases = c("dia", "can",
                                                        "hf", "lung"),
                              cross_sectional_diseases = c("bmi", "smoking",
                                                           "bathing", "walking",
                                                           "finances",
                                                           "push_pull"),
                              fu_time_index = 4)

lee_count <- get_count_cohort(index_cohort_data = lee_index)

lee_index <- lee_index[lee_index$ergoid %in% lee_count$ergoid, ]


# we need to recalculate smoking as only current smokers get weights
lee_index <- lee_index |>
  dplyr::mutate(smoking = if_else(smoking == 2, 1, 0))

lee_index_values <- get_index_values(index_cohort_data = lee_index,
                                       diseases = c("bmi", "dia", "can",
                                                    "hf", "lung", "smoking",
                                                    "bathing", "walking",
                                                    "finances", "push_pull"),
                                       weights = w)

# add the extra weights for age
lee_index_values <- lee_index_values |>
  dplyr::mutate(age_cat = cut(age, breaks = c(0, seq(from = 65, 85, by = 5), Inf),
                              ordered_results = TRUE),
                index_value = case_when(as.numeric(age_cat) == 1 ~ index_value + 1,
                                        as.numeric(age_cat) == 2 ~ index_value + 2,
                                        as.numeric(age_cat) == 3 ~ index_value + 3,
                                        as.numeric(age_cat) == 4 ~ index_value + 4,
                                        as.numeric(age_cat) == 5 ~ index_value + 5,
                                        as.numeric(age_cat) == 6 ~ index_value + 7
                                        ),
                index_value = if_else(sex == "male", index_value + 2, index_value)
                )

save(lee_index_values, lee_count, lee_index,
     file = here::here("RESULTS_FINAL", "final_data", "lee2006_data.RData"))

# . ----------
# Fan, 2002 index -----------------------------------------------------
#     55-59 = 1
#     60-64 = 2
#     65-69 = 3
#     70-74 = 4
#     75-79 = 5
#     80-84 = 6
#     85-89 = 7
#     90-94 = 8
#     95-99 = 9

#     MI = 1
#     Cancer = 2
#     Lung = 1
#     HF = 2
#     Diabetes = 2
#     Stroke = 2
#     Past smoker = 2
#     Current smoker = 4

d <- c("mi", "can", "lung", "hf", "dia", "stroke", "smoking")
w <- c(1, 2, 1, 2, 2, 2, 2)

fan <- compile_cohort(diseases = d)

fan_index <- get_index_cohort(index_data = fan,
                              sex_ratio_index = "men_only",
                              longitudinal_diseases = c("mi", "can", "lung",
                                                        "hf", "dia", "stroke"),
                              cross_sectional_diseases = c("smoking"),
                              fu_time_index = 2)

fan_count <- get_count_cohort(index_cohort_data = fan_index)

fan_index <- fan_index[fan_index$ergoid %in% fan_count$ergoid, ]


# Adjust: make sure smoking gets appropriate weights. Former = 2, current = 4.
fan_index_values <- get_index_values(index_cohort_data = fan_index,
                                     diseases = c("mi", "can", "lung", "hf",
                                                  "dia", "stroke", "smoking"),
                                     weights = c(1, 2, 1, 2, 2, 2, 2))

# add the extra weights for age
fan_index_values <- fan_index_values |>
  dplyr::mutate(age_cat = cut(age, breaks = c(0, seq(from = 60, 100, by = 5)),
                               ordered_results = TRUE),
                index_value = case_when(as.numeric(age_cat) == 1 ~ index_value + 1,
                                        as.numeric(age_cat) == 2 ~ index_value + 2,
                                        as.numeric(age_cat) == 3 ~ index_value + 3,
                                        as.numeric(age_cat) == 4 ~ index_value + 4,
                                        as.numeric(age_cat) == 5 ~ index_value + 5,
                                        as.numeric(age_cat) == 6 ~ index_value + 6,
                                        as.numeric(age_cat) == 7 ~ index_value + 7,
                                        as.numeric(age_cat) == 8 ~ index_value + 8,
                                        as.numeric(age_cat) == 9 ~ index_value + 9
                                        )
                )

fan_index_values <- fan_index_values[, c(1, 2, 3, 5, 4, 6)]


save(fan_index_values, fan_count, fan_index,
     file = here::here("RESULTS_FINAL", "final_data", "fan2002_data.RData"))


# data for sensitivity analysis - all sex
fan_index_all_sex <- get_index_cohort(index_data = fan,
                              sex_ratio_index = "both",
                              longitudinal_diseases = c("mi", "can", "lung",
                                                        "hf", "dia", "stroke"),
                              cross_sectional_diseases = c("smoking"),
                              fu_time_index = 2)

fan_count_all_sex <- get_count_cohort(index_cohort_data = fan_index_all_sex)

fan_index_all_sex <- fan_index_all_sex[fan_index_all_sex$ergoid %in% fan_count_all_sex$ergoid, ]


# Adjust: make sure smoking gets appropriate weights. Former = 2, current = 4.
fan_index_values_all_sex <- get_index_values(index_cohort_data = fan_index_all_sex,
                                     diseases = c("mi", "can", "lung", "hf",
                                                  "dia", "stroke", "smoking"),
                                     weights = c(1, 2, 1, 2, 2, 2, 2))

# add the extra weights for age
fan_index_values_all_sex <- fan_index_values_all_sex |>
  dplyr::mutate(age_cat = cut(age, breaks = c(0, seq(from = 60, 100, by = 5)),
                              ordered_results = TRUE),
                index_value = case_when(as.numeric(age_cat) == 1 ~ index_value + 1,
                                        as.numeric(age_cat) == 2 ~ index_value + 2,
                                        as.numeric(age_cat) == 3 ~ index_value + 3,
                                        as.numeric(age_cat) == 4 ~ index_value + 4,
                                        as.numeric(age_cat) == 5 ~ index_value + 5,
                                        as.numeric(age_cat) == 6 ~ index_value + 6,
                                        as.numeric(age_cat) == 7 ~ index_value + 7,
                                        as.numeric(age_cat) == 8 ~ index_value + 8,
                                        as.numeric(age_cat) == 9 ~ index_value + 9
                )
  )

fan_index_values_all_sex <- fan_index_values_all_sex[, c(1, 2, 3, 5, 4, 6)]


save(fan_index_values_all_sex, fan_count_all_sex, fan_index_all_sex,
     file = here::here("RESULTS_FINAL", "final_data", "fan2002_data_all_sex.RData"))


# . ----------
# Charlson- Ghali, 1987 index -------------------------------------------------------------

#     Renal disease = 3
#     Myocardial infarction = 1
#     Peripheral vascular diseases (abi) = 2
#     Cerebrovascular disease = 1
#     Coronary Heart failure = 4


d <- c("ckd", "mi", "abi", "cvd", "hf")

w <- c(3, 1, 2, 1, 4)

ghali <- compile_cohort(diseases = d)


ghali_index <- get_index_cohort(index_data = ghali,
                                sex_ratio_index = "both",
                                longitudinal_diseases = c("ckd", "mi", "cvd", "hf"),
                                cross_sectional_diseases = c("abi"),
                                fu_time_index = 1)

ghali_count <- get_count_cohort(index_cohort_data = ghali_index)

ghali_index <- ghali_index[ghali_index$ergoid %in% ghali_count$ergoid, ]


ghali_index_values <- get_index_values(index_cohort_data = ghali_index,
                                     diseases = d,
                                     weights = w)


save(ghali_index_values, ghali_count, ghali_index,
     file = here::here("RESULTS_FINAL", "final_data", "ghali1996_data.RData"))



# . ----------
# Charlson- Quan, 2011 index -------------------------------------------------------------

# Congestive heart failure  = 1
# Dementia = 2
# Chronic pulmonary disease = 1
# Rheumatologic disease  = 1 (not present)
# Mild liver disease  = 2 (not present)
# Diabetes with chronic complications = 1
# Hemiplegia or paraplegia = 2
# Renal disease  = 1
# Any malignancy, including leukemia and lymphoma  = 2
# Moderate or severe liver disease  = 4 (not present)
# Metastatic solid tumor  = 4
# (this was originally 6 but since all cases of metastatic tumor also have
# any malignancy, we reduce it to 4 to avoid double counting. That way all
# metastatic tumor cases recieve the value of 6 (2 for any malignancy and 2 for
# metastatic tumor))
# AIDS/HIV  = 4 (not present)



d <- c("hf", "dem", "COPD", "dia", "mastroke", "ckd", "can", "can_metastatic")

w <- c(1, 2, 1, 1, 2, 1, 2, 4)

quan <- compile_cohort(diseases = d)


quan_index <- get_index_cohort(index_data = quan,
                                sex_ratio_index = "both",
                                longitudinal_diseases = d,
                                fu_time_index = 1)

quan_count <- get_count_cohort(index_cohort_data = quan_index)

quan_index <- quan_index[quan_index$ergoid %in% quan_count$ergoid, ]


quan_index_values <- get_index_values(index_cohort_data = quan_index,
                                       diseases = d,
                                       weights = w)


save(quan_index_values, quan_count, quan_index,
     file = here::here("RESULTS_FINAL", "final_data", "quan2011_data.RData"))

## Data for sensitivity analyses----------------------------

# Now compile the data with diabetes only assigned to participants who died
quan_index_dm <- quan_index |>
  dplyr::mutate(dia = if_else(dia == 1 & died == 1, 1, 0))

quan_index_values_dm <- get_index_values(index_cohort_data = quan_index_dm,
                                          diseases = d,
                                          weights = w)

save(quan_index_values_dm, quan_count, quan_index_dm,
     file = here::here("RESULTS_FINAL", "final_data", "quan2011_data_dm.RData"))


# Robusto 2016 index -------------------------------------------------------------

load(here::here("datasets", "robusto_data.Rdata"))

# add censor date
robusto_index_cohort <- robusto_index_cohort |>
  dplyr::mutate(index_censord = case_when(
    died == 1 ~ fp_censordate,
    died == 0 ~ fu_end,
    .default = NA
  ))

robusto_count_cohort <- get_count_cohort(index_cohort_data = robusto_index_cohort)
robusto_index_cohort <- robusto_index_cohort[robusto_index_cohort$ergoid %in% robusto_count_cohort$ergoid, ]

robusto_count_cohort$index_censord <- robusto_index_cohort$index_censord

robusto_index_values <- robusto_index_cohort[, c(1, 8:10, 4, 2, 5)]

save(robusto_index_values, robusto_count_cohort, robusto_index_cohort,
     file = here::here("RESULTS_FINAL", "final_data", "robusto2016_data.RData"))

# . ----------
# von Korff 1992 index -------------------------------------------------------------

load(here::here("datasets", "vonkorff1992_data.RData"))

#  get the count cohort, based on index data
vonkorff_count <- get_count_cohort(index_cohort_data = vonkorff_index)

vonkorff_index <- vonkorff_index[vonkorff_index$ergoid %in% vonkorff_count$ergoid, ]

save(vonkorff_index, vonkorff_count,
     file = here::here("RESULTS_FINAL", "final_data",
                                       "vonkorff1992_data.RData"))
