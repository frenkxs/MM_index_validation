# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT 2: Index validation: sensitivity -------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# .This script validates all indices. It builds on the series of functions in the cohort builder
# (get_cohorts.R) and uses data produced by clean_index.R and compile_index.R

# For all questions, contact Premysl Velek at p.velek@erasmusmc.nl

# start here
setwd(here::here())
source(here::here("R", "99_get_cohorts.R"))

# .-------
# Tooth, 2008 index -------------------------------------------------------------

# heart disease = 1
# stroke = 2
# low iron = 1
# lung disease = 2
# diabetes = 1
# cancer = 3
# ad = 4

# fu time = 6

load(here::here("RESULTS_FINAL", "final_data", "tooth2008_data.RData"))


# sensitivity analysis with age
df <- left_join(tooth_count,
                tooth_index_values |> dplyr::select(ergoid, index_value),
                by = "ergoid")

results_tooth_all_age <- validate_index_v02(dat = df, index_name = "Tooth 2008",
                                    include_age = TRUE, include_sex = FALSE)

save(results_tooth_all_age,
     file = here::here("RESULTS_FINAL", "validation", "tooth2008_all_age.RData"))

# sensitivity analysis with age and sex
load(here::here("RESULTS_FINAL", "final_data", "tooth2008_data_all_age_sex.RData"))

df <- left_join(tooth_count_all_age_sex,
                tooth_index_values_all_age_sex |> dplyr::select(ergoid, index_value),
                by = "ergoid")

results_tooth_all_age_sex <- validate_index_v02(dat = df, index_name = "Tooth 2008",
                                            include_age = TRUE, include_sex = TRUE)

save(results_tooth_all_age_sex,
     file = here::here("RESULTS_FINAL", "validation", "tooth2008_all_age_sex.RData"))



# .--------
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

# sensitivity analysis with age
load(here::here("RESULTS_FINAL", "final_data", "desai2002_data_all_age.RData"))

df <- left_join(desai_count_all_age,
                desai_index_values_all_age |> dplyr::select(ergoid, index_value),
                by = "ergoid")


results_desai_all_age <- validate_index_v02(dat = df, index_name = "Desai 2002",
                                    include_age = TRUE)

save(results_desai_all_age,
     file = here::here("RESULTS_FINAL", "validation", "desai2002_all_age.RData"))


# sensitivity analysis with diabetes severity
load(here::here("RESULTS_FINAL", "final_data", "desai2002_data_dm.RData"))

df <- left_join(desai_count,
                desai_index_values_dm |> dplyr::select(ergoid, index_value),
                by = "ergoid")


results_desai_dm <- validate_index_v02(dat = df, index_name = "Desai 2002",
                                            include_age = TRUE)

save(results_desai_dm,
     file = here::here("RESULTS_FINAL", "validation", "desai2002_dm.RData"))




# .--------
# Charlson- Quan, 2011 index --------------------------- -------------------------------------------------------------

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
# metastatic tumor cases receive the value of 6 (2 for any malignancy and 2 for
# metastatic tumor))
# AIDS/HIV  = 4 (not present)



load(here::here("RESULTS_FINAL", "final_data", "quan2011_data_dm.RData"))

df <- left_join(quan_count,
                quan_index_values_dm |> dplyr::select(ergoid, index_value),
                by = "ergoid")


results_quan_dm <- validate_index_v02(dat = df, index_name = "Quan 2011",
                                   include_age = TRUE)

save(results_quan_dm,
     file = here::here("RESULTS_FINAL", "validation", "quan2011_dm.RData"))


# .--------
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


load(here::here("RESULTS_FINAL", "final_data", "fan2002_data_all_sex.RData"))

df <- left_join(fan_count_all_sex,
                fan_index_values_all_sex |> dplyr::select(ergoid, index_value),
                by = "ergoid")

results_fan_all_sex <- validate_index_v02(dat = df, index_name = "Fan 2002",
                                  include_age = FALSE, include_sex = TRUE)

save(results_fan_all_sex,
     file = here::here("RESULTS_FINAL", "validation", "fan2002_all_sex.RData"))

