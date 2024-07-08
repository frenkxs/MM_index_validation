# check what baseline to get to meaningfully validate the model, namely check the number of
# events we get for different baseline dates
#
library(tidyverse)

# difference baseline dates -------------------------------------------------------------------
# get difference baseline dates
load(here::here("datasets", "centre_visits.RData"))
source(here::here("R", "99_get_cohorts.R"))
basic <- read_rds(here::here("datasets", "ergo_basic.RData"))

# e1, ep, ej -----------

rs_i <- rs_i_visits |>
  dplyr::select(ergoid, e1_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = e1_visit2d) |>

  # since we need one year of monitoring the prescription use, the baseline will
  # be at ergo visit centre + 1
  dplyr::mutate(fu_start = fu_start + years(1))

rs_ii <- rs_ii_visits |>
  dplyr::select(ergoid, ep_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = ep_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

rs_iii <- rs_iii_visits |>
  dplyr::select(ergoid, ej_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = ej_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

baseline_e1_ep_ej <- bind_rows(rs_i, rs_ii, rs_iii) |>
  drop_na()


# now count then number of deaths after one year of follow up

# this is the follow up time for von Korff
baseline_e1_ep_ej$fu_end <- baseline_e1_ep_ej$fu_start + lubridate::years(1)

# this is for Robusto
baseline_e1_ep_ej$fu_end <- baseline_e1_ep_ej$fu_start + lubridate::years(6)


baseline_e1_ep_ej <- left_join(baseline_e1_ep_ej, basic |> dplyr::select(-rs_cohort), by = "ergoid")

baseline_e1_ep_ej <- baseline_e1_ep_ej |>
  assess_vital_status() |>
  dplyr::filter(died <= 1)

baseline_e1_ep_ej <- baseline_e1_ep_ej |>
  dplyr::mutate(age = birthd %--% fu_start / years(1),
                index_censord = case_when(
                  died == 1 ~ fp_censordate,
                  died == 0 ~ fu_end,
                  .default = NA
                ))


baseline_e1_ep_ej_count <- get_count_cohort(index_cohort_data = baseline_e1_ep_ej)


sum(baseline_e1_ep_ej_count$died)
nrow(baseline_e1_ep_ej_count)
# 26 events with sample size of 5586 (von Korff)
# 283 events with sample size of 5556 (Robusto)

# e2, ep, ej -----------

rs_i <- rs_i_visits |>
  dplyr::select(ergoid, e2_visitd, rs_cohort) |>
  dplyr::rename(fu_start = e2_visitd) |>
  dplyr::mutate(fu_start = fu_start + years(1))

rs_ii <- rs_ii_visits |>
  dplyr::select(ergoid, ep_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = ep_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

rs_iii <- rs_iii_visits |>
  dplyr::select(ergoid, ej_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = ej_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

baseline_e2_ep_ej <- bind_rows(rs_i, rs_ii, rs_iii) |>
  drop_na()


# now count then number of deaths after one year of follow up

baseline_e2_ep_ej$fu_end <- baseline_e2_ep_ej$fu_start + lubridate::years(1)
baseline_e2_ep_ej$fu_end <- baseline_e2_ep_ej$fu_start + lubridate::years(6)

baseline_e2_ep_ej <- left_join(baseline_e2_ep_ej, basic |> dplyr::select(-rs_cohort), by = "ergoid")

baseline_e2_ep_ej <- baseline_e2_ep_ej |>
  assess_vital_status() |>
  dplyr::filter(died <= 1)

baseline_e2_ep_ej <- baseline_e2_ep_ej |>
  dplyr::mutate(age = birthd %--% fu_start / years(1),
                index_censord = case_when(
                  died == 1 ~ fp_censordate,
                  died == 0 ~ fu_end,
                  .default = NA
                ))


baseline_e2_ep_ej_count <- get_count_cohort(index_cohort_data = baseline_e2_ep_ej)

bb <- left_join(basic, baseline_e2_ep_ej_count |> select(-c(sex, count)), by = "ergoid",
                suffix = c(".basic", ".index"))
bb <- left_join()

sum(baseline_e2_ep_ej_count$died)
# still 26 events with sample size of 8723
# still 386 events with sample size of 8693



# e3, ep, ej -----------

rs_i <- rs_i_visits |>
  dplyr::select(ergoid, e3_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = e3_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

rs_ii <- rs_ii_visits |>
  dplyr::select(ergoid, ep_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = ep_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

rs_iii <- rs_iii_visits |>
  dplyr::select(ergoid, ej_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = ej_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

baseline_e3_ep_ej_all <- bind_rows(rs_i, rs_ii, rs_iii)

baseline_e3_ep_ej_non_missing <- bind_rows(rs_i, rs_ii, rs_iii) |>
  drop_na()

save(baseline_e3_ep_ej_all, baseline_e3_ep_ej_non_missing, file = "baseline_e3_ep_ej.RData")


# now count then number of deaths after one year of follow up

baseline_e3_ep_ej$fu_end <- baseline_e3_ep_ej$fu_start + lubridate::years(1)
baseline_e3_ep_ej$fu_end <- baseline_e3_ep_ej$fu_start + lubridate::years(6)

baseline_e3_ep_ej <- left_join(baseline_e3_ep_ej, basic |> dplyr::select(-rs_cohort), by = "ergoid")

baseline_e3_ep_ej <- baseline_e3_ep_ej |>
  assess_vital_status() |>
  dplyr::filter(died <= 1)

baseline_e3_ep_ej <- baseline_e3_ep_ej |>
  dplyr::mutate(age = birthd %--% fu_start / years(1),
                index_censord = case_when(
                  died == 1 ~ fp_censordate,
                  died == 0 ~ fu_end,
                  .default = NA
                ))

baseline_e3_ep_ej_count <- get_count_cohort(index_cohort_data = baseline_e3_ep_ej)


bb <- left_join(basic, baseline_e3_ep_ej_count |> select(-c(sex, died, count)), by = "ergoid")

bb <- bb |>
  filter(died == 1) |>
  dplyr::mutate(is_between = between(fp_censordate, fu_start, index_censord))

sum(baseline_e3_ep_ej_count$died)
# 82 events with sample size of 9153



# e4, ep, ej -----------

rs_i <- rs_i_visits |>
  dplyr::select(ergoid, e4_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = e4_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

rs_ii <- rs_ii_visits |>
  dplyr::select(ergoid, ep_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = ep_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

rs_iii <- rs_iii_visits |>
  dplyr::select(ergoid, ej_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = ej_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

baseline_e4_ep_ej <- bind_rows(rs_i, rs_ii, rs_iii) |>
  drop_na()


# now count then number of deaths after one year of follow up

baseline_e4_ep_ej$fu_end <- baseline_e4_ep_ej$fu_start + lubridate::years(1)
baseline_e4_ep_ej$fu_end <- baseline_e4_ep_ej$fu_start + lubridate::years(6)

baseline_e4_ep_ej <- left_join(baseline_e4_ep_ej, basic |> dplyr::select(-rs_cohort), by = "ergoid")

baseline_e4_ep_ej <- baseline_e4_ep_ej |>
  assess_vital_status() |>
  dplyr::filter(died <= 1)

baseline_e4_ep_ej <- baseline_e4_ep_ej |>
  dplyr::mutate(age = birthd %--% fu_start / years(1),
                index_censord = case_when(
                  died == 1 ~ fp_censordate,
                  died == 0 ~ fu_end,
                  .default = NA
                ))

baseline_e4_ep_ej_count <- get_count_cohort(index_cohort_data = baseline_e4_ep_ej)


bb <- left_join(basic, baseline_e4_ep_ej_count |> select(-c(sex, died, count)), by = "ergoid")

bb <- bb |>
  filter(died == 1) |>
  dplyr::mutate(is_between = between(fp_censordate, fu_start, index_censord))

sum(baseline_e4_ep_ej_count$died)
# 69 events with sample size of 8174 (von Korff)
# 762 events with sample size of 8143 (Robusto)


# e4, e4, ej -----------

rs_i <- rs_i_visits |>
  dplyr::select(ergoid, e4_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = e4_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

rs_ii <- rs_ii_visits |>
  dplyr::select(ergoid, e4_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = e4_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

rs_iii <- rs_iii_visits |>
  dplyr::select(ergoid, ej_visit2d, rs_cohort) |>
  dplyr::rename(fu_start = ej_visit2d) |>
  dplyr::mutate(fu_start = fu_start + years(1))

baseline_e4_e4_ej <- bind_rows(rs_i, rs_ii, rs_iii) |>
  drop_na()


# now count then number of deaths after one year of follow up

baseline_e4_e4_ej$fu_end <- baseline_e4_e4_ej$fu_start + lubridate::years(1)
baseline_e4_e4_ej$fu_end <- baseline_e4_e4_ej$fu_start + lubridate::years(6)

baseline_e4_e4_ej <- left_join(baseline_e4_e4_ej, basic |> dplyr::select(-rs_cohort), by = "ergoid")

baseline_e4_e4_ej <- baseline_e4_e4_ej |>
  assess_vital_status() |>
  dplyr::filter(died <= 1)

baseline_e4_e4_ej <- baseline_e4_e4_ej |>
  dplyr::mutate(age = birthd %--% fu_start / years(1),
                index_censord = case_when(
                  died == 1 ~ fp_censordate,
                  died == 0 ~ fu_end,
                  .default = NA
                ))

baseline_e4_e4_ej_count <- get_count_cohort(index_cohort_data = baseline_e4_e4_ej)


sum(baseline_e4_e4_ej_count$died)
# 73 events with sample size of 7867 (von Korff)
# 792 events with sample size of 7840 (Robusto)



# All numbers together ------------------------------------------------------------------------


sum(baseline_e1_ep_ej_count$died)
nrow(baseline_e1_ep_ej_count)

sum(baseline_e2_ep_ej_count$died)
nrow(baseline_e1_ep_ej_count)

sum(baseline_e3_ep_ej_count$died)
nrow(baseline_e1_ep_ej_count)

sum(baseline_e4_ep_ej_count$died)
nrow(baseline_e1_ep_ej_count)

sum(baseline_e4_e4_ej_count$died)
nrow(baseline_e1_ep_ej_count)
