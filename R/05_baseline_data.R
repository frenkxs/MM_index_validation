
get_baselines <- function(index) {

  path <- paste0("RESULTS_FINAL/final_data/", index, "_data.RData")
  e <- new.env()
  df <- load(path, envir = e)[1]

  df <- e[[df]]

  ethnicity <- haven::read_sav(here::here(
    "data", "ERGO_ethnic_backgrounddef.sav")) |>
    # dplyr::select(ergoid, ASCCEG) |>
    dplyr::mutate(ergoid = factor(ergoid))


  load(here::here("Datasets", "Education.RData"))
  education$ergoid <- factor(education$ergoid)
  df <- dplyr::left_join(df, education, by = "ergoid")
  df <- dplyr::left_join(df, ethnicity, by = "ergoid")

  counts_edu <- table(df$level, useNA = "ifany")
  proportions_edu <- round(prop.table(counts_edu), 3)

  count_eth <- table(df$ASCCEG, useNA = "ifany")
  proportion_eth <- round(prop.table(count_eth), 3)

  list(counts_edu, proportions_edu, count_eth, proportion_eth)
}

get_baselines("vonkorff1992")
get_baselines("fan2002")
get_baselines("desai2002")
get_baselines("lee2006")
# get_baselines("tooth2008")
get_baselines("quan2011")
get_baselines("robusto2016")


# this has to be done specifically for tooth as the data are before exlucing young participants
index = "tooth2008"
path <- paste0("RESULTS_FINAL/final_data/", index, "_data.RData")
e <- new.env()
df <- load(path, envir = e)[1]

df <- e[[df]]

df <- df |>
  dplyr::filter(age > 64)

ethnicity <- haven::read_sav(here::here(
  "data", "ERGO_ethnic_backgrounddef.sav")) |>
  # dplyr::select(ergoid, ASCCEG) |>
  dplyr::mutate(ergoid = factor(ergoid))


load(here::here("Datasets", "Education.RData"))
education$ergoid <- factor(education$ergoid)
df <- dplyr::left_join(df, education, by = "ergoid")
df <- dplyr::left_join(df, ethnicity, by = "ergoid")

counts_edu <- table(df$level, useNA = "ifany")
proportions_edu <- round(prop.table(counts_edu), 3)

count_eth <- table(df$ASCCEG, useNA = "ifany")
proportion_eth <- round(prop.table(count_eth), 3)

list(counts_edu, proportions_edu, count_eth, proportion_eth)








# all data
ethnicity <- haven::read_sav(here::here(
  "data", "ERGO_ethnic_backgrounddef.sav")) |>
  dplyr::mutate(ergoid = factor(ergoid))

x <- table(ethnicity$ASCCEG[ethnicity$rs_cohort != 4], useNA = "ifany")
round(prop.table(x), 2)

load(here::here("Datasets", "Education.RData"))
education$ergoid <- factor(education$ergoid)

x <- table(education$level, useNA = "ifany")
x
round(prop.table(x), 3)
