# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT 2: Index validation --------------------------------------
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


df <- left_join(tooth_count,
                tooth_index_values |> dplyr::select(ergoid, index_value),
                by = "ergoid")

# only those older than 64 will be included
df <- df |>
  dplyr::filter(age > 64)

results_tooth <- validate_index_v02(dat = df, index_name = "Tooth 2008",
                                    include_age = TRUE, include_sex = FALSE)

save(results_tooth,
     file = here::here("RESULTS_FINAL", "validation", "tooth2008.RData"))



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


load(here::here("RESULTS_FINAL", "final_data", "desai2002_data.RData"))

df <- left_join(desai_count,
                desai_index_values |> dplyr::select(ergoid, index_value),
                by = "ergoid")


results_desai <- validate_index_v02(dat = df, index_name = "Desai 2002",
                                    include_age = TRUE)

save(results_desai,
     file = here::here("RESULTS_FINAL", "validation", "desai2002.RData"))

# .--------
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


load(here::here("RESULTS_FINAL", "final_data", "lee2006_data.RData"))

df <- left_join(lee_count,
                lee_index_values |> dplyr::select(ergoid, index_value),
                by = "ergoid")

results_lee <- validate_index_v02(dat = df, index_name = "Lee 2008",
                                  include_age = FALSE, include_sex = FALSE)

save(results_lee,
     file = here::here("RESULTS_FINAL", "validation", "lee2006.RData"))

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


load(here::here("RESULTS_FINAL", "final_data", "fan2002_data.RData"))

df <- left_join(fan_count,
                fan_index_values |> dplyr::select(ergoid, index_value),
                by = "ergoid")

results_fan <- validate_index_v02(dat = df, index_name = "Fan 2002",
                                  include_age = FALSE, include_sex = FALSE)

save(results_fan,
     file = here::here("RESULTS_FINAL", "validation", "fan2002.RData"))



# .--------
# Charlson- Ghali, 1987 index -------------------------------------------------------------

#     Renal disease = 3
#     Myocardial infarction = 1
#     Peripheral vascular diseases (abi) = 2
#     Cerebrovascular disease = 1
#     Coronary Heart failure = 4


# load(here::here("RESULTS_FINAL", "final_data", "ghali1996_data.RData"))
#
# df <- left_join(ghali_count,
#                 ghali_index_values |> dplyr::select(ergoid, index_value),
#                 by = "ergoid")
#
#
# results_ghali <- validate_index_v02(dat = df, index_name = "Ghali 1996",
#                                     include_age = TRUE)
#
# save(results_ghali,
#      file = here::here("RESULTS_FINAL", "validation", "ghali1996.RData"))
#

# .--------
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
# metastatic tumor cases receieve the value of 6 (2 for any malignancy and 2 for
# metastatic tumor))
# AIDS/HIV  = 4 (not present)



load(here::here("RESULTS_FINAL", "final_data", "quan2011_data.RData"))

df <- left_join(quan_count,
                quan_index_values |> dplyr::select(ergoid, index_value),
                by = "ergoid")


results_quan <- validate_index_v02(dat = df, index_name = "Quan 2011",
                                   include_age = TRUE)

save(results_quan,
     file = here::here("RESULTS_FINAL", "validation", "quan2011.RData"))


# .--------
# Robusto 2016 index -------------------------------------------------------------


load(here::here("RESULTS_FINAL", "final_data", "robusto2016_data.RData"))

df <- left_join(robusto_count_cohort,
                robusto_index_values |> dplyr::select(ergoid, index_value),
                by = "ergoid")


results_robusto <- validate_index_v02(dat = df, index_name = "Robusto 2016",
                                      include_age = TRUE)

save(results_robusto,
     file = here::here("RESULTS_FINAL", "validation", "robusto2016.RData"))


# .--------
# Von Korff 1992 index -------------------------------------------------------------


load(here::here("RESULTS_FINAL", "final_data", "vonkorff1992_data.RData"))

df <- left_join(vonkorff_count,
                vonkorff_index |> dplyr::select(ergoid, index_value),
                by = "ergoid")


results_vonkorff <- validate_index_v02(dat = df, index_name = "Von Korff 1992",
                                       include_age = TRUE, include_sex = TRUE)

save(results_vonkorff,
     file = here::here("RESULTS_FINAL", "validation", "vonkorff1992.RData"))


# .--------
# ------------ Get table with all performance metrics -------


load(here::here("RESULTS_FINAL", "validation", "robusto2016.RData"))
load(here::here("RESULTS_FINAL", "validation", "quan2011.RData"))
load(here::here("RESULTS_FINAL", "validation", "fan2002.RData"))
load(here::here("RESULTS_FINAL", "validation", "lee2006.RData"))
load(here::here("RESULTS_FINAL", "validation", "desai2002.RData"))
load(here::here("RESULTS_FINAL", "validation", "tooth2008.RData"))
load(here::here("RESULTS_FINAL", "validation", "vonkorff1992.RData"))

# AIC(results_desai[['fits']][[1]])
# AIC(results_lee[['fits']][[1]])
# AIC(results_tooth[['fits']][[1]])
# AIC(results_tooth[['fits']][[2]])
# AIC(results_tooth[['fits']][[3]])

res <- bind_rows(
  results_desai[['model_performance']],
  results_fan[['model_performance']],
  results_lee[['model_performance']],
  results_quan[['model_performance']],
  results_robusto[['model_performance']],
  results_tooth[['model_performance']],
  results_vonkorff[['model_performance']]
)





indices <- c("desai2002", "fan2002", "lee2006", "quan2011",
             "robusto2016", "tooth2008", "vonkorff1992")
res$index <- rep(indices, each = 18)

res$model <- rep(rep(c("base", "index", "count"), 6), 7)
res$metric2 <- substring(res$metric, 1, 2)

res_delta <- res |>
  group_by(index, metric2, .drop = FALSE) %>%
  summarise(base_stat = first(value),
         index_stat = nth(value, 2),
         count_stat = nth(value, 3)) |>
  mutate(delta_stat_count = if_else(metric2 == "B_", round(1 - (count_stat / base_stat), 3),
                                    round((count_stat / base_stat) - 1, 3)),

         delta_stat_index = if_else(metric2 == "B_", round(1 - (index_stat / base_stat), 3),
                                    round((index_stat / base_stat) - 1, 3)),

         delta_stat =       if_else(metric2 == "B_", round(1 - (index_stat / count_stat), 3),
                                    round((index_stat / count_stat) - 1, 3)),
  )



write.csv(res_delta, here::here("RESULTS_FINAL", "final_metrics.csv"))
write.csv(res, here::here("RESULTS_FINAL", "final_metrics_long.csv"))
