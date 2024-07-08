# von korf 1992 index cleaning and data preparation

library(tidyverse)
library(haven)
setwd(here::here())


# mapping onto ATC code data ----------------------------------------------

vk <- haven::read_sav(here::here("data", "vonkorff1992", "VonKorff.sav")) |>
  rowwise() |>

  # heart disesase
  # "hd_Anticoagulants" = "b01a",
  # "hd_Hemostatics" = "b02",
  # "hd_ACE_plain" = "c09a",
  # "hd_ACE_comb" = "c09b",
  # "hd_Cardiac_ag" = "c01",
  # "hd_Diuretic_l" = "c03c"
  dplyr::mutate(hd_antico_hemo = sum(b01a, b02),
                hd_carda_ACE = sum(c09a, c09b, c01),
                hd_diu = c03c) |>

  # dichotomize
  dplyr::mutate(hd_antico_hemo = if_else(hd_antico_hemo > 0, 1, 0),
                hd_carda_ACE = if_else(hd_carda_ACE > 0, 1, 0),
                hd_diu = if_else(hd_diu > 0, 1, 0)
                ) |>

  # respiratory illness
  # c01ca	Respiratory illness	Epinephrine
  # r03da	Respiratory illness	Xanthine products
  # r03a	Respiratory illness	Beta-adrenergic, misc. (inhalants)
  # r03c	Respiratory illness	Beta-adrenergic, misc. (for systemic use)
  # r05cb, r03ba, r03bb, r01, r02, r06, r07 Resp. products incl bronchodilators
  # excl cromolyn.
  dplyr::mutate(ri_betaad = sum(r03a, r03c),
                ri_epin = c01ca,
                ri_xan = r03da,
                ri_res = sum(r05cb, r03ba, r03bb, r01, r02, r06, r07)) |>

  # dichotomize
  dplyr::mutate(ri_betaad  = if_else(ri_betaad > 0, 1, 0),
                ri_epin    = if_else(ri_epin   > 0, 1, 0),
                ri_xan     = if_else(ri_xan    > 0, 1, 0)
  ) |>

  # Asthma + rheumatism
  # h02ab	Asthma, rheumatism	Glucocorticoids
  dplyr::mutate(as_glucoco = h02ab) |>

  # dichotomize
  dplyr::mutate(as_glucoco  = if_else(as_glucoco > 0, 1, 0)) |>

  # RA
  # m01cb	Rheumatoid arthritis	Gold salts
  dplyr::mutate(ra_gos = m01cb) |>

  # dichotomize
  dplyr::mutate(ra_gos  = if_else(ra_gos > 0, 1, 0)) |>

  # cancer
  # l01	Cancer	Antineoplastics
  dplyr::mutate(ca_aneop = l01) |>

  # dichotomize
  dplyr::mutate(ca_aneop  = if_else(ca_aneop > 0, 1, 0)) |>

  # parkinson
  # n04ba	Parkinson’s	Levodopa
  dplyr::mutate(pa_levo = n04ba) |>

  # dichotomize
  dplyr::mutate(pa_levo  = if_else(pa_levo > 0, 1, 0)) |>

  # hypertension
  # c02	Hypertension	Antihypertensives (except ACE inhibitors)
  # c08	Hypertension	Calcium channel blockers
  # c07	Hypertension	Beta blockers
  # c03a	Hypertension	Diuretics
  # c03b	Hypertension	Diuretics
  # c03d	Hypertension	Diuretics
  # c03e	Hypertension	Diuretics
  # c03x	Hypertension	Diuretics
  # c10	Hypertension	Beta blockers
  dplyr::mutate(ht_anht = sum(c02, c08),
                ht_bb_diu = sum(c07, c10, c03a, c03b, c03d, c03e, c03x)) |>

  # dichotomize
  dplyr::mutate(ht_anht  = if_else(ht_anht > 0, 1, 0),
                ht_bb_diu = if_else(ht_bb_diu > 0, 1, 0)
  ) |>

  # diabetes
  # a10a	Diabetes	Insulin
  # a10b	Diabetes	Oral hypoglecemics
  dplyr::mutate(dia_ins_hgly = sum(a10a, a10b)) |>

  # dichotomize
  dplyr::mutate(dia_ins_hgly  = if_else(dia_ins_hgly > 0, 1, 0)) |>

  # epilepsy
  # n03	Epilepsy	Anticonvulsants
  dplyr::mutate(ep_anco = n03) |>

  # dichotomize
  dplyr::mutate(ep_anco  = if_else(ep_anco > 0, 1, 0)) |>

  # asthma + rhinitis
  # r03bc01	Asthma, rhinitis	Cromolyn
  dplyr::mutate(asr_cro = r03bc01) |>

  # dichotomize
  dplyr::mutate(asr_cro  = if_else(asr_cro > 0, 1, 0)) |>

  # acne
  # d10ad01	Acne	Antiacne tretinoin
  # d10af	Acne	Topical macrolides
  dplyr::mutate(acn_tre = d10ad01,
                acn_top = d10af) |>

  # dichotomize
  dplyr::mutate(acn_tre  = if_else(acn_tre > 1, 1, 0),
                acn_top  = if_else(acn_top > 1, 1, 0)) |>

  # ulcers
  # a02ba01	Ulcers	Cimetidine
  dplyr::mutate(ulc_cim = a02ba01) |>

  # dichotomize
  dplyr::mutate(ulc_cim  = if_else(ulc_cim > 0, 1, 0)) |>

  # glaucoma
  # s01e	Glaucoma	Ophthalmic miotics
  dplyr::mutate(gla_mio = s01e) |>

  # dichotomize
  dplyr::mutate(gla_mio  = if_else(gla_mio > 0, 1, 0)) |>

  # grout
  # m04aa	Gout, hyperuricemia	Uric acid agents
  # m04ab	Gout, hyperuricemia	Uric acid agents
  dplyr::mutate(go_ura = sum(m04aa, m04ab)) |>

  # dichotomize
  dplyr::mutate(go_ura  = if_else(go_ura > 0, 1, 0)) |>

  # migraines
  # n02ca	Migraines	Ergot derivatives
  dplyr::mutate(mi_erg = n02ca) |>

  # dichotomize
  dplyr::mutate(mi_erg  = if_else(mi_erg > 0, 1, 0)) |>

  # tuberculosis
  # j04	Tuberculosis	Antitubercular agents
  dplyr::mutate(tu_antu = j04) |>

  # dichotomize
  dplyr::mutate(tu_antu  = if_else(ra_gos > 0, 1, 0)) |>

  # select only new variables
  dplyr::select(ergoid, start, startmin1y, contains("_"))


# --------------- get the scoring per disease
vk <- vk |>
  rowwise() |>
  dplyr::mutate(hd_score = sum(across(contains("hd_"))),
                hd_score = case_when(
                      hd_score == 1 ~ 3,
                      hd_score == 2 ~ 4,
                      hd_score == 3 ~ 5,
    .default = 0
  )) |>

  dplyr::mutate(ri_score = sum(across(contains("ri_"))),
                ri_score = case_when(
                      ri_score == 1 ~ 2,
                      ri_score > 1 ~ 3,
                      .default = 0
  )) |>

  dplyr::mutate(asrh_score = case_when(
   as_glucoco == 1 ~ 3,
    .default = 0
  )) |>

  dplyr::mutate(ra_score = case_when(
    ra_gos == 1 ~ 3,
    .default = 0
  )) |>

  dplyr::mutate(ca_score = case_when(
    ca_aneop == 1 ~ 3,
    .default = 0
  )) |>

  dplyr::mutate(pa_score = case_when(
    pa_levo == 1 ~ 3,
    .default = 0
  )) |>

  dplyr::mutate(ht_score = case_when(
    ht_anht == 1 ~ 2,
    (ht_anht == 0 & ht_bb_diu == 1) ~ 1,
    .default = 0
  )) |>

  dplyr::mutate(dia_score = case_when(
    dia_ins_hgly == 1 ~ 2,
    .default = 0
  )) |>

  dplyr::mutate(ep_score = case_when(
    ep_anco == 1 ~ 2,
    .default = 0
  )) |>

  dplyr::mutate(asr_score = case_when(
    asr_cro == 1 ~ 2,
    .default = 0
  )) |>

  dplyr::mutate(acn_score = case_when(
    acn_tre == 1 ~ 1,
    acn_top == 1 ~ 1,
    .default = 0
  )) |>

  dplyr::mutate(ulc_score = case_when(
    ulc_cim == 1 ~ 1,
    .default = 0
  )) |>

  dplyr::mutate(gla_score = case_when(
    gla_mio == 1 ~ 1,
    .default = 0
  )) |>

  dplyr::mutate(go_score = case_when(
    go_ura == 1 ~ 1,
    .default = 0
  )) |>

  dplyr::mutate(mi_score = case_when(
    mi_erg == 1 ~ 1,
    .default = 0
  )) |>

  dplyr::mutate(tu_score = case_when(
    tu_antu == 1 ~ 1,
    .default = 0
  ))


vk_index <- vk |>
  dplyr::select(ergoid, start, startmin1y, contains("_score")) |>
  dplyr::mutate(ergoid = factor(ergoid))



# calculate the index value ----------------------------------------------

vk_index <- vk_index |>
  rowwise() |>
  dplyr::mutate(index_value = sum(across(contains("_score"))))

vk_index <- vk_index |>
  dplyr::select(ergoid, start, startmin1y, index_value)


# follow up time is 1year,
fu_time <- 1

vk_index <- vk_index %>%
  dplyr::mutate(fu_start = lubridate::ymd(start),
                fu_end = fu_start + lubridate::years(fu_time)) %>%
  dplyr::select(ergoid, fu_start, fu_end, index_value) %>%
  dplyr::mutate(ergoid = factor(ergoid))

# add mortality data
mortality_p <- here::here("data", "mortality", "fp_VitalStatus_2022-42.sav")

mortality <- haven::read_sav(mortality_p) %>%
  select(ergoid, fp_vitalstatus, fp_censordate, fp_mortdat) %>%
  rename(died = fp_vitalstatus) %>%
  dplyr::mutate(ergoid = factor(ergoid))

# we also get the event indicator with respect to the follow-up, using the
# assess_vital_status function form the get_cohort script
source(here::here("R", "99_get_cohorts.R"))

vonkorff_index <- vk_index %>%
  left_join(., mortality, by = "ergoid") %>%

  # we only select those with complete follow up
  assess_vital_status() %>%
  dplyr::filter(died <= 1) %>%
  mutate(index_censord = if_else(died == 1, fp_mortdat, fu_end))

# in the last step we calculate the age at the baseline
basic_p <- here::here("data", "basic", "RoterdamStudy_Basics2014.sav")
basic <- haven::read_sav(basic_p)

basic <- basic %>%
  dplyr::rename(birthd = date_of_birth) %>%
  dplyr::mutate(ergoid = factor(ergoid))



vonkorff_index <- vonkorff_index %>%
  dplyr::left_join(., basic %>% dplyr::select(ergoid, birthd, sex)) %>%
  dplyr::mutate(age = round((birthd %--% fu_start) / years(1), 1))

vonkorff_index <- vonkorff_index %>%
  dplyr::select(ergoid, fu_start, fu_end, index_value, died, index_censord, sex, age)


save(vonkorff_index, file = here::here("datasets", "vonkorff1992_data.RData"))





# # follow up time is 2 years -----------------------------------------------
#
# # follow up time is 1year,
# fu_time <- 2
#
# vk_index <- vk_index %>%
#   dplyr::mutate(fu_start = lubridate::ymd(startdat),
#                 fu_end = fu_start + lubridate::years(fu_time)) %>%
#   dplyr::select(ergoid, fu_start, fu_end, index_value) %>%
#   dplyr::mutate(ergoid = factor(ergoid))
#
# vonkorff_index <- vk_index %>%
#   left_join(., mortality, by = "ergoid") %>%
#
#   # we only select those with complete follow up
#   assess_vital_status() %>%
#   dplyr::filter(died <= 1) %>%
#   mutate(index_censord = if_else(died == 1, fp_mortdat, fu_end))
#
#
# vonkorff_index <- vonkorff_index %>%
#   dplyr::left_join(., basic %>% dplyr::select(ergoid, birthd, sex)) %>%
#   dplyr::mutate(age = round((birthd %--% fu_start) / years(1), 1))
#
# vonkorff_index <- vonkorff_index %>%
#   dplyr::select(ergoid, fu_start, fu_end, index_value, died, index_censord, sex, age)
#
#
# vonkorff_count <- get_count_cohort(index_cohort_data = vonkorff_index)
#
# vonkorff_index <- vonkorff_index[vonkorff_index$ergoid %in% vonkorff_count$ergoid, ]
#
# save(vonkorff_index, file = here::here("RESULTS_FINAL", "final_data", "vonkorff1992_data_v02.RData"))
#
