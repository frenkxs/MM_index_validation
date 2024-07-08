packages_needed <- c(
  "haven",
  "tidyverse",
  "lubridate",
  "here",
  "gtsummary",
  "colorspace",
  "ggsurvfit"
  )

# install missing libraries
install.packages(setdiff(packages_needed, rownames(installed.packages())))

# load libraries
lapply(packages_needed, library, character.only = TRUE)


# start here
setwd(here::here())

# .----------------------------------------------------------------------
# ------------- Format basics ------------------------------------------
# .----------------------------------------------------------------------
basic_p <- here::here("data", "basic", "RoterdamStudy_Basics2014.sav")
basic <- haven::read_sav(basic_p)

basic <- basic %>%
  dplyr::rename(birthd = date_of_birth) %>%
  dplyr::mutate(ergoid = factor(ergoid))

rm(basic_p)

# .----------------------------------------------------------------------
# ------------- Load and define drug data -----------------------------------
# .----------------------------------------------------------------------

drugs <- haven::read_sav(here::here("data", "robusto2016", "Robusto.sav"))

# This is the list of ATC codes we need to look at
# Each vector had three elements:

# 1. ATC code,
# 2. he number of prescriptions needed to be considered a
#    user of drugs defined by the ATC code
# 3. weight of this drug(s) for the index score

# This is based on Robusto 2016, Table 2
# (https://doi.org/10.1371/journal.pone.0149203)

drugs_definition <- new.env()

local({
# simple cases
  a <- list("C01B", 3, 1)
  b <- list("L04", 3, -1)
  c <- list("B01AC", 3, 2)
  d <- list(c("B01AB", "B01AX"), 3, 1)
  e <- list("B01AA", 3, 1)
  f <- list("L01", 3, 3)
  g <- list(c("R03A", "R03BB", "R03DA"), 3, 2)
  i <- list("A10", 2, 2)
  k <- list("A02", 3, 1)
  l <- list("C10", 3, -2)
  m <- list("H02AB", 3, 2)
  n <- list("N02A", 3, 6)
  o <- list("N04", 1, 4)
  p <- list("N05A", 1, 3)
  q <- list("N06A", 1, 4)

  # more complex cases
  h <- list(c("C02A", "C02C", "C02LA",
         "C02LB", "C03A", "C03BA",
         "C03EA01", "C07AA",
         "C07AG", "C07BB", "C07C", "C08",
         "C09AA", "C09BA", "C09CA", "C09DA"), 3, 1)

  # there is an excetion: all of drugs in C07AB but excluding C07AB09
  remove_from_h <- list(c("C07AB", "C07AB09"), 3, 1)


  # this has presence within 45 days of any combination of the drugs with
  # these ATC codes
  j1 <- list(c("C01AA05", "C03CA01", "C03DA01",
         "C07AG02", "C07AB07", "C07AB03",
         "C09"), NA, 3)

  #  OR at least two prescriptions of C01AA05
  j2 <- list("C01AA05", 2, 3)},

  envir = drugs_definition
)

# .----------------------------------------------------------------------
# ------------- Define drug use according to Robusto --------------------
# .----------------------------------------------------------------------

# generic function to extract the usage for simple cases
# it takes the vectors defined above and the drugmatrix and returns
# a data frame with two colums: the ergoid and the usage indicator
# with 0 if the participant is not this drug user and the weight corresponding
# to this drug if participant is this drug user.

# the names of the columns also come from RObusto 2016, Table 2
get_drug_users <- function(medicine, df = drugs, name){
  atc <- tolower(medicine[[1]])
  threshold <- as.numeric(medicine[[2]])
  weight <- as.numeric(medicine[[3]])

  drugs %>%
    dplyr::select(ergoid, start, atc) %>%

  # now we sum the values across the different drugs
  # and determine whether the number of prescriptions exceed the threshold defined by Robusto
    rowwise() %>%
    dplyr::mutate(n_prescription = sum(across(all_of(atc))),
                  {{ name }} := if_else(n_prescription >= threshold, weight, 0)) %>%
    dplyr::select({{ name }}, ergoid)
}

local({
  # get the prescription data
  cat_a <- get_drug_users(a, name = "antiarrhythmics")
  cat_b <- get_drug_users(b, name = "immunosuppressants")
  cat_c <- get_drug_users(c, name = "platelet_inhibitors")
  cat_d <- get_drug_users(d, name = "par_anticoagulants")
  cat_e <- get_drug_users(e, name = "oral_anticoagulants")
  cat_f <- get_drug_users(f, name = "antineoplastic_a")
  cat_g <- get_drug_users(g, name = "inhal_bronchodilators")
  cat_i <- get_drug_users(i, name = "antihyperglycemic_ther")


  # j category needs to be deal with separately, this is just one condition
  cat_j2 <- get_drug_users(j2, name = "drugs_antihypertensive_heart_disorders2")
  cat_k <- get_drug_users(k, name = "drugs_acid_related_disorders")
  cat_l <- get_drug_users(l, name = "lipid_modifiers")
  cat_m <- get_drug_users(m, name = "corticosteroids")
  cat_n <- get_drug_users(n, name = "opioids")
  cat_o <- get_drug_users(o, name = "anti_parkinson_drugs")
  cat_p <- get_drug_users(p, name = "antipsychotics")
  cat_q <- get_drug_users(q, name = "antidementia_drugs")

# get the usage of h case
  cat_h <- drugs %>%
    dplyr::select(ergoid, start, tolower(h[[1]]),
                  tolower(remove_from_h[[1]])) %>%

    # now we need to substract the c07ab09 drug from the c07ab category
    dplyr::mutate(c07ab_clean = c07ab - c07ab09) %>%
    dplyr::select(-tolower(remove_from_h[[1]])) %>%
    rowwise() %>%

    #  now we sum the values across the different drugs
    # and determine whether the number of prescriptions exceed the threshold defined by Robusto
    dplyr::mutate(n_prescription = sum(
      c_across(sym(tolower(h[[1]][1])):c07ab_clean)),
                  drugs_arterial_ht = if_else(
                    n_prescription >= h[[2]], h[[3]], 0)) %>%
    dplyr::select(drugs_arterial_ht, ergoid)
  },
  envir = drugs_definition
)



  # left join all the specific dataframes into one
robusto <- purrr::reduce(list(
  drugs_definition$cat_a,
  drugs_definition$cat_b,
  drugs_definition$cat_c,
  drugs_definition$cat_d,
  drugs_definition$cat_e,
  drugs_definition$cat_f,
  drugs_definition$cat_g,
  drugs_definition$cat_h,
  drugs_definition$cat_i,
  drugs_definition$cat_j2,
  drugs_definition$cat_k,
  drugs_definition$cat_l,
  drugs_definition$cat_m,
  drugs_definition$cat_n,
  drugs_definition$cat_o,
  drugs_definition$cat_p,
  drugs_definition$cat_q
  ),
  dplyr::left_join, by = "ergoid")



# .----------------------------------------------------------------------
# ------------- Calculate index value -----------------------------------
# .----------------------------------------------------------------------

# get the index values and add follow up start and end (fu_start will be the
# same as start date in the drugmatrix)

# follow up time is 7 years, the same as in Robusto 2016
fu_time <- 7

robusto <- robusto %>%
  dplyr::relocate(ergoid) %>%
  rowwise() %>%
  dplyr::mutate(index_value = sum(across(antiarrhythmics:antidementia_drugs))) %>%
  ungroup() %>%
  dplyr::left_join(., drugs %>% dplyr::select(ergoid, start)) %>%
  dplyr::mutate(fu_start = lubridate::ymd(start),
                fu_end = fu_start + lubridate::years(fu_time)) %>%
  dplyr::select(ergoid, fu_start, fu_end, index_value) %>%
  dplyr::mutate(ergoid = factor(ergoid))

# add mortality data
mortality_p <- here::here("data", "mortality", "fp_VitalStatus_2022-42.sav")

mortality <- haven::read_sav(mortality_p) %>%
    select(ergoid, fp_vitalstatus, fp_censordate) %>%
    rename(died = fp_vitalstatus) %>%
    dplyr::mutate(ergoid = factor(ergoid))

# we also get the event indicator with respect to the follow-up, using the
# assess_vital_status function form the get_cohort script
source(here::here("R", "99_get_cohorts.R"))

robusto_index_cohort <- robusto %>%
  left_join(., mortality, by = "ergoid") %>%

  # we only select those with complete follow up
  assess_vital_status() %>%
  dplyr::filter(died <= 1)

# in the last step we calculate the age at the baseline
robusto_index_cohort <- robusto_index_cohort %>%
  dplyr::left_join(., basic %>% dplyr::select(ergoid, birthd, sex)) %>%
  dplyr::mutate(age = round((birthd %--% fu_start) / years(1), 1))

save(robusto_index_cohort,
     file = here::here("datasets", "robusto_data.Rdata"))




