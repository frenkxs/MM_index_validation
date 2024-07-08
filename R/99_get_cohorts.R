# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT 2: Cohort builder ----------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# .This script prepares individual disease data for all indices.

# For all questions, contact Premysl Velek at p.velek@erasmusmc.nl

# LIBRARIES ------------------------------------------------------------

library(tidyverse)
library(pROC)
library(predtools)
library(cowplot)
library(colorspace)
library(ggsurvfit)
library(boot)
library(rms)
library(grid)
library(gridExtra)
load(here::here("data", "shift_1", "shift_data_2024-04-22.Rdata"))

# COMPILE COHORT ------------------------------------------------------------

#' first we need to compile the cohort data from the individual disesases. This
#' is done by the function below. It takes vector of all the individual diseases
#' and other life style covariates (when relevant) and produces a raw data for
#' all diseases listed
compile_cohort <- function(diseases){
  # load ergo basic
  res <- read_rds(here::here("datasets", "ergo_basic.RData"))

  # read in all the data specified
  for(i in diseases) {
    res <- left_join(res, read_rds(here::here("datasets", paste0(i, ".RData"))) ,
                     by = "ergoid")
  }
  res
}


# COUNT COHORT ------------------------------------------------------------


# function to calculate the data for the count model.
# The input here is the cleaned data for each individual index
# with ergoids, vital status, age and follow-up start of all the ergo participants
# included
#
# The output is cleaned data with ergoids, vital status and the number of
# diseases each participant had at the start of the follow-up
# (defined by the index data follow-up start). Participants with missing data are
# removed, hence the size of the count model cohort is likely to be smaller than the
# index cohort (as more diseases are involved)

get_count_cohort <- function(index_cohort_data, data = shift_data){
  # get the ergo id, age, fu start and outcome from the index data
  res <- index_cohort_data %>%
    select(ergoid, age, fu_start, died, index_censord) %>%
    left_join(., data, by = "ergoid") %>%
    mutate(startd_asthma = startd_lung,
           startd_COPD = startd_lung) %>%

  #' For each of the 10 diseases we re-calculate the prevalence/
  #' incidence with respect to the baseline (fu_start) of the
  #' index cohort

 # 1. Heart failure
    define_incidence("hf") %>%

 # 2. Coronary heart disease
    define_incidence("chd") %>%

 #' 3. Cancer, all except non-melanoma skin cancer
    define_incidence("can1") %>%

 # 4. Diabetes
    define_incidence("dia") %>%

 # 5. Depression
    define_incidence("dep1") %>%

 # 6. COPD
    define_incidence("COPD") %>%

 # 7. Asthma
    define_incidence("asthma") %>%

 # 8. Parkinsonism
    define_incidence("park") %>%

 # 9. Dementia
    define_incidence("dem") %>%

 # 10. stroke
    define_incidence("stroke") %>%

  dplyr::select(hf, chd, can1, dia, dep1, COPD, asthma, park,
                stroke, dem, age, sex, ergoid, fu_start, died,
                index_censord) %>%
  drop_na() %>%

  # finaly calculate the disease count
  rowwise() %>%
  mutate(count = sum(c_across(hf:dem))) %>%

  dplyr::select(ergoid, sex, age, fu_start, count, died, index_censord)

  message(paste("The size of the updated cohort:", nrow(res)))

  res
}


# INDEX COHORT ------------------------------------------------------------

# function to match the age and sex of ERGO participants with that of the original index cohort
# It takes the cleaned index data as input, together with the mean age, sex ratio we aim for and the
# follow up time
#

#' ARGUMENTS:
#' index_data: data containing all the diseases in a given index. It also has to have
#' the start and end of follow up for each individual disease and the prevalence/incidence
#' indicators. It also has to have the information about the individual ERGO cohorts,
#' e.g. RS-I, RS-II, ...
#'
#' mean_age_index: the mean age of the index cohort, i.e. the mean age to which we
#' need to get as close as possible to warrant fair comparison
#'
#' sex_ratio_index: whether men only, women only or both men and women should be
#' included in the cohort, based on the original cohort of each index. Only three
#' values are possible: women_only will results in a cohort with only women, men_only
#' will result in a cohort with only men, both - the default - will not change the
#' sex ratio
#'
#' diseases: character vector with the diseases included in the index you want to
#' validate. They have to be in the same abbreviated forms as in the list below:
#'
#' dia: diabetes
#' stroke: stroke
#' can1: cancer, all types
#' dem: dementia
#' hf: heart failure
#' chd: coronary heart disease
#' park: parkinsonism
#' dep1: depression
#' COPD: chronic obstructive pulmonary disease
#' asthma: asthma
#'
#' Abbreviations for diseases created specifically for a particular index should
#' match those in the 'index_data' data frame.
#'
#' fu_time_index: the follow-up time of the original paper

get_index_cohort <- function(index_data, mean_age_index = NULL,
                             sex_ratio_index = c("both", "women_only", "men_only"),
                             longitudinal_diseases,
                             cross_sectional_diseases = NULL,
                             fu_time_index){

  sex_ratio_index <- match.arg(sex_ratio_index)

  all_diseases <- c(longitudinal_diseases, cross_sectional_diseases)

  # add cohort info to the index data
  index_data <- index_data |>
    dplyr::select(contains(all_diseases), sex, birthd, ergoid,
                  rs_cohort, fp_censordate, died) |>
    # remove participants with missing values and calculate age at fu_start
    drop_na()

  # calculate the earliest possible baseline date for all participants based on the longitudinal data

  longitudinal_diseases_start <- paste0("startd_", longitudinal_diseases)
  cross_sectional_diseases_start <- paste0("startd_", cross_sectional_diseases)


  index_data <- index_data |>
    rowwise() |>
    mutate(fu_start_longitudinal_diseases = max(c_across(any_of(longitudinal_diseases_start))),

           # this will get overwritten if there are any cross-sectional diseases
           fu_start = fu_start_longitudinal_diseases) |>
    ungroup()

  # this deals with the fact that cross sectional diseases don't have any follow-up
  # hence they have to be baseline measures
  if(!is.null(cross_sectional_diseases)){
    index_data <- index_data |>
      rowwise() |>
      mutate(cross_sectional_diseases_min = min(c_across(any_of(cross_sectional_diseases_start))),
             cross_sectional_diseases_max = max(c_across(any_of(cross_sectional_diseases_start))),
             cross_sectional_diseases_range =
               (cross_sectional_diseases_min %--% cross_sectional_diseases_max) / years(1),

             # the fu_start date for the cross sectional diseases will be the mean of the start dates
             fu_start_cross_sectional_diseases =
               mean.Date(c_across(any_of(cross_sectional_diseases_start)))) |>

      # check gap between cross section and longitudinal diseases
      mutate(overlap =
               (fu_start_cross_sectional_diseases %--% fu_start_longitudinal_diseases) / months(1)) |>

      # remove those with a gap of more than 4 months
      dplyr::filter(overlap < 4)


    # get the 75% quantile of the range. This will be reported back!
    quantile_75 <- quantile(index_data$cross_sectional_diseases_range, probs = 0.75)

    # Now fu_start for long diseases must be earlier than fu_start for cross sectional diseases.
    # Otherwise we cannot proceed.
    overlap <- index_data$overlap

    # percentage of participants with a gap greater than 4 months between the start of
    # cross-sectional diseases and longitudinal diseases
    overlap_perc <- sum(overlap > 4) / length(overlap)

    # now we can set up the overall follow-up start which will be the date of ascertainment of the cross
    # sectional diseases
    index_data$fu_start <- index_data$fu_start_cross_sectional_diseases
  }

  # now we can calculate age at baseline
  index_data$age <- (index_data$birthd %--% index_data$fu_start) / years(1)


  # by how much to move the baseline date to get to the desired mean age
  if(is.numeric(mean_age_index) & is.null(cross_sectional_diseases)){
    # mean age by ERGO cohorts
    mean_age_ergo <- tapply(index_data$age, index_data$rs_cohort, mean)

    age_diff <- round(mean_age_index - mean_age_ergo)

    #' if the difference in negative for some of the cohort - ie. if the mean age of the index is lower
    #' than the mean age of the ERGO cohorts at baseline - we set it to zero as we cannot set the
    #' follow up start earlier than the earliest possible date
    age_diff[age_diff < 0] <- 0

    # we move the follow up start differently, depending on the cohort. The amount of years to be moved
    # is equal to the difference between the mean age of the index cohort and the mean age of the
    # ERGO cohort
    index_data <- index_data |>
      mutate(fu_start = case_when(
        rs_cohort == "RS-I" ~ fu_start %m+% lubridate::years(age_diff[1]),
        rs_cohort == "RS-II" ~ fu_start %m+% lubridate::years(age_diff[2]),
        rs_cohort == "RS-III" ~ fu_start %m+% lubridate::years(age_diff[3]),
      ))
  }

  # add the follow up end based on the follow up time which in turn is based on
  # the original follow up time of each index (the argument fu_time)
  index_data <- index_data |>
    mutate(fu_end = fu_start + lubridate::years(fu_time_index))

  # assess vital status at follow up end
  index_data <- index_data |>
    assess_vital_status() |>
    dplyr::filter(died <= 1)

  # define censor data within the index cohort and follow up time: 1. those who
  # died within the fu time have censordate equal to their date of death 2.
  # those who were alive at the end of fu time their censor date equal to the
  # end of follow up
  index_data <- index_data |>
    dplyr::mutate(index_censord = case_when(
      died == 1 ~ fp_censordate,
      died == 0 ~ fu_end,
      .default = NA
    ))

  # now, we need to recalculate incidence/prevalence of the disease with
  # respect to the new follow-up start
  res <- as.data.frame(sapply(longitudinal_diseases, define_incidence, dat = index_data,
                              return_vector = TRUE))

  res$ergoid <- arrange(index_data, ergoid)$ergoid

  res <- right_join(index_data |> dplyr::select(ergoid, sex, birthd, fu_start,
                                                fu_end, fp_censordate, died,
                                                index_censord,
                                                any_of(cross_sectional_diseases)),
                    res, by = "ergoid") |>

    # recalculate age
    mutate(age = (birthd %--% fu_start) / years(1)) |>
    drop_na()

  # select only women or men as and if relevant
  if(sex_ratio_index == "both") {
    # if both men and women are included calculate the proportion of men
    t <- table(res$sex)
    perc_males <- round(t[1] / sum(t), 2)
  } else if(sex_ratio_index == "women_only") {
    res <- res[res$sex == "female", ]
    perc_males <- 0
  } else if(sex_ratio_index == "men_only") {
    res <- res[res$sex == "male", ]
    perc_males <- 1
  }

  new_age <- round(mean(res$age), 2)

  message(paste("The size of the updated cohort:", nrow(res)))
  message(paste("The mean age of the updated cohort:", new_age))
  message(paste("The proportion of men in the updated cohort:", perc_males))

  if(!is.null(cross_sectional_diseases)){
    message(paste0(overlap_perc, "% of participants had follow up start for longitudinal diseases more than 4 months later than the date of ascertainment of the cross-sectional diseases. They were removed from the cohort."))
    message(paste("The 75% percentile of the range of dates of ascertainment of the cross sectional diseases is:", round(quantile_75, 2), "months"))
  }

  res
}

# Analysis ------------------------------------------------------------------------------------

#' This is a function that calculates the index value from the presence/absence
#of individual ' diseases. It takes as input the final index cohort data, and
#then vector of diseases included in ' the index and vector of corresponding
#weights for each disease
#
#' The output is a data frame with the index value, age at baseline, sex and
#outcome (dead/alive) at ' the end of follow up
get_index_values <- function(index_cohort_data, diseases, weights){

  # matrix with rows = n participants and columns = n diseases. The values will be the same for all participants namely the weight assigned to each individual disease by the index
  matrix_value <- matrix(data = rep(weights, each = nrow(index_cohort_data)),
                         nrow = nrow(index_cohort_data))

  # boolean matrix with diseases for individual patients
  matrix_diseases <- index_cohort_data %>%
    select(all_of(diseases)) %>% as.matrix

  # now we have matrix with weights for each disease and each participant
  as_tibble(matrix_value * matrix_diseases) %>%

    # which we can sum up to get to the index value
    rowwise() %>%
    mutate(index_value = sum(across(everything()))) %>%
    ungroup() %>%
    select(index_value) %>%
    add_column(ergoid = index_cohort_data$ergoid,
               sex = index_cohort_data$sex,
               age = index_cohort_data$age,
               died = index_cohort_data$died,
               fu_start = index_cohort_data$fu_start,
               index_censord = index_cohort_data$index_censord) %>%
    select(ergoid, sex,  age, fu_start, index_value, died, index_censord)

}


# This is a function that runs logistic regression on the index data (index cohort and count cohort).\
validate_index_v02 <- function(dat, index_name,
                               include_age = TRUE,
                               include_sex = TRUE) {
  if(!("age" %in% names(dat)))
    stop("Age not found in the dataset. Add age at baseline of your sample population.")

  # check if both men and women are included
  sex <- ifelse(length(unique(dat$sex)) == 2, " + sex", "")

  # define formula for the base model (with or without sex)
  frml_base <-  as.formula(paste0("died ~ age", sex))

  # fit base model with only sex and age
  fit_base <- rms::lrm(frml_base, x = TRUE, y = TRUE, data = dat)

  # check if age should be included in the full model
  age <- ifelse(include_age, " + age", "")

  # check if sex should be included in the full model
  sex <- ifelse(include_sex, " + sex", "")

  # define formula for the full model (with or without sex or age)
  frml_full <-  as.formula(paste0("died ~ index_value", age, sex))

  # fit full model
  fit_full <- rms::lrm(frml_full, x = TRUE, y = TRUE, data = dat)

  # define formula for the count model (with or without sex)
  frml_count <-  as.formula(paste0("died ~ count + age", sex))

  # fit count model
  fit_count <- rms::lrm(frml_count, x = TRUE, y = TRUE, data = dat)

  # validate all three models
  v_base <- rms::validate(fit_base, B = 1500)
  v_full <- rms::validate(fit_full, B = 1500)
  v_count <- rms::validate(fit_count, B = 1500)

  # Dxy
  D_base <-  v_base["Dxy", "index.corrected"]
  D_full <-  v_full["Dxy", "index.corrected"]
  D_count <- v_count["Dxy", "index.corrected"]

  # Nagelkerke R^2
  R_base <-  v_base["R2", "index.corrected"]
  R_full <-  v_full["R2", "index.corrected"]
  R_count <- v_count["R2", "index.corrected"]

  # C
  C_base <-  (D_base + 1) / 2
  C_full <-  (D_full + 1) / 2
  C_count <- (D_count + 1) / 2

  # Brier
  B_base <-  v_base["B", "index.corrected"]
  B_full <-  v_full["B", "index.corrected"]
  B_count <- v_count["B", "index.corrected"]

  # scaled Brier score - first with respect to the B max model
  p_mean <- mean(dat$died)
  b_max <- p_mean * (1 - p_mean)

  sB_max_base <- 1 - (B_base / b_max)
  sB_max_full <- 1 - (B_full / b_max)
  sB_max_count <- 1 - (B_count / b_max)

  # now with respect to the base model
  sB_base_full <- 1 - (B_full / B_base)
  sB_base_count <- 1 - (B_count / B_base)

  # and finally with respect to the count model
  sB_count_index <- 1 - (B_full / B_count)

  # discrimination slope
  dat$pr_full <- plogis(fit_full$linear.predictors)
  dat$pr_count <- plogis(fit_count$linear.predictors)
  dat$pr_base <- plogis(fit_base$linear.predictors)

  mean_pr_full0 <- mean(dat$pr_full[dat$died == 0])
  mean_pr_full1 <- mean(dat$pr_full[dat$died == 1])

  mean_pr_count0 <- mean(dat$pr_count[dat$died == 0])
  mean_pr_count1 <- mean(dat$pr_count[dat$died == 1])

  mean_pr_base0 <- mean(dat$pr_base[dat$died == 0])
  mean_pr_base1 <- mean(dat$pr_base[dat$died == 1])

  bp <- list(
    geom_boxplot(),
    stat_summary(fun = mean, geom = "point",
                 size = 3, shape = 15),
    scale_y_continuous(name = NULL, limits = c(0, 1)),
    scale_x_discrete(labels = c("Alive", "Dead"), name = NULL),
    theme_minimal(base_size = 16)
  )

  bp_full <- dat |>
    ggplot(aes(x = factor(died), y = pr_full)) +
    bp +
    ggtitle(paste0(index_name, ": index model"),
            subtitle = paste0("Discrimination slope: ",
                              round(mean_pr_full1 - mean_pr_full0, 3)))

  bp_count <- dat |>
    ggplot(aes(x = factor(died), y = pr_count)) +
    bp +
    ggtitle(paste0(index_name, ": count model"),
            subtitle = paste0("Discrimination slope: ",
                              round(mean_pr_count1 - mean_pr_count0, 3)))

  bp_base <- dat |>
    ggplot(aes(x = factor(died), y = pr_count)) +
    bp +
    ggtitle(paste0(index_name, ": base model"),
            subtitle = paste0("Discrimination slope: ",
                              round(mean_pr_base1 - mean_pr_base0, 3)))


  d_slope <- cowplot::plot_grid(bp_full, bp_count, bp_base, ncol = 3, align = "h")

  y.grob <- grid::textGrob("Predicted probabilities", rot = 90)

  pdf(paste0("RESULTS_FINAL/calibration/d_slope_", index_name, ".pdf"), width = 12, height = 7)
  gridExtra::grid.arrange(gridExtra::arrangeGrob(d_slope, left = y.grob))
  dev.off()



  # bootstrap

  stats_ci <- function(data, index, .b_max = b_max){

    base <- rms::lrm(frml_base, data = dat[index, ])
    full <- rms::lrm(frml_full, data = dat[index, ])
    count <- rms::lrm(frml_count, data = dat[index, ])

    # Train -------------------
    # Dxy
    D_train_base <- base[["stats"]]["Dxy"]
    D_train_full <- full[["stats"]]["Dxy"]
    D_train_count <- count[["stats"]]["Dxy"]

    # Nagelkerke R^2
    R_train_base <- base[["stats"]]["R2"]
    R_train_full <- full[["stats"]]["R2"]
    R_train_count <- count[["stats"]]["R2"]

    # C
    C_train_base <- base[["stats"]]["C"]
    C_train_full <- full[["stats"]]["C"]
    C_train_count <- count[["stats"]]["C"]

    # Brier
    B_train_base <- base[["stats"]]["Brier"]
    B_train_full <- full[["stats"]]["Brier"]
    B_train_count <- count[["stats"]]["Brier"]

    # scaled Brier max model
    sB_max_train_base <-  1 - (B_train_base  / .b_max)
    sB_max_train_full <-  1 - (B_train_full  / .b_max)
    sB_max_train_count <- 1 - (B_train_count / .b_max)

    sB_base_train_full <-  1 - (B_train_full  / B_train_base)
    sB_base_train_count <- 1 - (B_train_count / B_train_base)

    sB_count_train_full <- 1 - (B_train_full / B_train_count)

    # Test -----------------------------------------------
    test_base <-  update(base, data = dat)
    test_full <-  update(full, data = dat)
    test_count <- update(count, data = dat)

    # Dxy
    D_test_base <-  test_base[["stats"]]["Dxy"]
    D_test_full <-  test_full[["stats"]]["Dxy"]
    D_test_count <- test_count[["stats"]]["Dxy"]

    # Nagelkerke R^2
    R_test_base <-  test_base[["stats"]]["R2"]
    R_test_full <-  test_full[["stats"]]["R2"]
    R_test_count <- test_count[["stats"]]["R2"]

    # C
    C_test_base <-  test_base[["stats"]]["C"]
    C_test_full <-  test_full[["stats"]]["C"]
    C_test_count <- test_count[["stats"]]["C"]

    # Brier
    B_test_base <-  test_base[["stats"]]["Brier"]
    B_test_full <-  test_full[["stats"]]["Brier"]
    B_test_count <- test_count[["stats"]]["Brier"]

    # scaled Brier max model
    sB_max_test_base <- 1 - (B_test_base / .b_max)
    sB_max_test_full <- 1 - (B_test_full / .b_max)
    sB_max_test_count <- 1 - (B_test_count / .b_max)

    # # scaled Brier base model
    sB_base_test_full <- 1 - (B_test_full / B_test_base)
    sB_base_test_count <- 1 - (B_test_count / B_test_base)

    # scaled Brier count model
    sB_count_test_full <- 1 - (B_test_full / B_test_count)


    # Optimism
    D_ba <- D_train_base - D_test_base
    D_fu <- D_train_full - D_test_full
    D_co <- D_train_count - D_test_count

    R_ba <- R_train_base -  R_test_base
    R_fu <- R_train_full -  R_test_full
    R_co <- R_train_count - R_test_count

    C_ba <- C_train_base -  C_test_base
    C_fu <- C_train_full -  C_test_full
    C_co <- C_train_count - C_test_count

    B_ba <- B_train_base -  B_test_base
    B_fu <- B_train_full -  B_test_full
    B_co <- B_train_count - B_test_count

    sB_max_ba <- sB_max_train_base -  sB_max_test_base
    sB_max_fu <- sB_max_train_full -  sB_max_test_full
    sB_max_co <- sB_max_train_count - sB_max_test_count

    sB_base_fu <- sB_base_train_full - sB_base_test_full
    sB_base_co <- sB_base_train_count - sB_base_test_count

    sB_co_fu <- sB_count_train_full - sB_count_test_full

    c(
      # apparent metrics
      D_train_base, D_train_full, D_train_count,
      R_train_base, R_train_full, R_train_count,
      C_train_base, C_train_full, C_train_count,
      B_train_base, B_train_full, B_train_count,
      sB_max_train_base,  sB_max_train_full,  sB_max_train_count,
      sB_base_train_full, sB_base_train_count,
      sB_count_train_full,

      # optimism
      D_ba, D_fu, D_co,
      R_ba, R_fu, R_co,
      C_ba, C_fu, C_co,
      B_ba, B_fu, B_co,

      sB_max_ba, sB_max_fu, sB_max_co,
      sB_base_fu, sB_base_co, sB_co_fu)
  }

  D <- boot::boot(data = dat, statistic = stats_ci, R = 1500)

  metrics = c("D_base", "D_index", "D_count",
              "R_base", "R_index", "R_count",
              "C_base", "C_index", "C_count",
              "B_base", "B_index", "B_count",
              "sB_max_base", "sB_max_index", "sB_max_count",
              "sB_base_index", "sB_base_count",
              "sB_count_index")

  # CI for apparent metrics
  perform <- as.data.frame(t(apply(D$t[, 1:18], 2,
                                   quantile,
                                   probs = c(0.025, 0.975),
                                   na.rm = TRUE))) |>
    # add_row(`2.5%` = rep(NA, 5), `97.5%` = rep(NA, 5)) |>
    add_column(metric = metrics, .before = "2.5%")



  optimism <- apply(D$t[, 19:36], 2, mean)

  perform$`2.5%` <- perform$`2.5%` - optimism
  perform$`97.5%` <- perform$`97.5%` - optimism
  perform$optimism <- optimism

  perform$pe <- c(D_base, D_full, D_count,
                  R_base, R_full, R_count,
                  C_base, C_full, C_count,
                  B_base, B_full, B_count,
                  sB_max_base, sB_max_full, sB_max_count,
                  sB_base_full, sB_base_count,
                  sB_count_index)

  perform[, 2:5] <- round(perform[, 2:5], 4)

  # perform <- data.frame(metric = metrics,
  #                       value = paste0(perform$pe, " (", perform$`2.5%`, "-",
  #                                      perform$`97.5%`, ")")
  # )

  perform <- data.frame(metric = metrics,
                        value = perform$pe,
                        ci_lb = perform$`2.5%`,
                        ci_ub = perform$`97.5%`)


  # # convert to wide format
  # perform <- perform |>
  #   pivot_wider(names_from = metric, values_from = value) |>
  #   add_column(index = index_name, .before = "D_base")



  # get calibration plots
  cal_base <- rms::calibrate(fit_base, B = 1500)

  cal_full <- rms::calibrate(fit_full, B = 1500)

  cal_count <- rms::calibrate(fit_count, B = 1500)

  pdf(file = paste0("RESULTS_FINAL/calibration/cal_", index_name, ".pdf"),
      width = 7, height = 9)
  par(mfrow = c(3, 1))

  plot(cal_base, main = paste("Base model:", index_name),
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
  plot(cal_full, main = paste("Index model:", index_name),
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
  plot(cal_count, main = paste("Count model:", index_name),
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
  dev.off()

  observed <- cal_full[, 3]
  predicted <- cal_full[, 1]
  cal_data_index <- data.frame(Observed = observed,
                               Predicted = predicted,
                               model = "Index")

  observed <- cal_count[, 3]
  predicted <- cal_count[, 1]
  cal_data_count <- data.frame(Observed = observed,
                          Predicted = predicted,
                          model = "Count")

  observed <- cal_base[, 3]
  predicted <- cal_base[, 1]
  cal_data_base <- data.frame(Observed = observed,
                               Predicted = predicted,
                               model = "Base")

  cal_data <- bind_rows(cal_data_count, cal_data_index,
                        cal_data_base)

  cal_plot <- ggplot(cal_data, aes(x = Predicted,
                              y = Observed,
                              group = model,
                              colour = model)) +
    geom_line(linewidth = 1.3) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_continuous(limits = c(0, NA)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    scale_color_manual(name = NULL, values = c("grey", "skyblue", "orange")) +
    theme_minimal(base_size = 13)




  # continuous net reclassification improvement (based on Pencina and Steyrberg 10.1002/sim.4085)
  nri_base_index <- Hmisc::improveProb(x1 = dat$pr_base, x2 = dat$pr_full, y = dat$died)
  nri_base_count <- Hmisc::improveProb(x1 = dat$pr_base, x2 = dat$pr_count, y = dat$died)
  nri_count_index <- Hmisc::improveProb(x1 = dat$pr_count, x2 = dat$pr_full, y = dat$died)


  # Kaplan Maier curves --

  # divide index score into quantiles
  dat$value_cat <- cut(dat$index_value,
                       breaks = c(-Inf, unique(quantile(dat$index_value,
                                                        na.rm = TRUE))))

  dat$count_cat <- cut(dat$count,
                       breaks = c(-Inf, unique(quantile(dat$count,
                                                        na.rm = TRUE))))


  # KM plot formatting
  formats <- list(
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent,
      expand = c(0.01, 0)
    ),
    colorspace::scale_colour_discrete_sequential(name = "Index values",
                                                 palette = "Viridis"),
    labs(
      x = "Follow-up time (years)",
      y = "Survival probability"
    ),
    theme_minimal()
  )

  # divide the data into four age groups
  # divide the data into max four index value groups
  dat <- dat |>
    dplyr::mutate(time = (fu_start %--% index_censord) / years(1),
                  age_cat = cut(age, breaks = quantile(dat$age,
                                                       na.rm = TRUE),
                                labels = c("1st_quantile",
                                           "2nd_quantile",
                                           "3rd_quantile",
                                           "4th_quantile")),
                  value_cat = cut(index_value,
                                  breaks = c(-Inf, unique(quantile(dat$index_value,
                                                                   na.rm = TRUE)))),

                  count_cat <- cut(count,
                                   breaks = c(-Inf, unique(quantile(dat$count,
                                                                    na.rm = TRUE))))
    )

  require(ggsurvfit)
  km_plot1_full <- ggsurvfit::survfit2(ggsurvfit::Surv(time, died) ~ value_cat,
                                       data = dat[dat$age_cat == "1st_quantile", ]) %>%
    ggsurvfit::ggsurvfit(linewidth = 0.8) +
    # add_risktable() +
    formats +
    ggtitle("Age - 1st quantile")

  km_plot1_count <- survfit2(Surv(time, died) ~ count_cat,
                             data = dat[dat$age_cat == "1st_quantile", ]) %>%
    ggsurvfit(linewidth = 0.8) +
    # add_risktable() +
    formats +
    ggtitle("Age - 1st quantile")

  km_plot2_full <- survfit2(Surv(time, died) ~ value_cat,
                            data = dat[dat$age_cat == "2nd_quantile", ]) %>%
    ggsurvfit(linewidth = 0.8) +
    # add_risktable() +
    formats +
    ggtitle("Age - 2nd quantile")

  km_plot2_count <- survfit2(Surv(time, died) ~ count_cat,
                             data = dat[dat$age_cat == "2nd_quantile", ]) %>%
    ggsurvfit(linewidth = 0.8) +
    # add_risktable() +
    formats +
    ggtitle("Age - 2nd quantile")

  km_plot3_full <- survfit2(Surv(time, died) ~ value_cat,
                            data = dat[dat$age_cat == "3rd_quantile", ]) %>%
    ggsurvfit(linewidth = 0.8) +
    # add_risktable() +
    formats +
    ggtitle("Age - 3rd quantile")

  km_plot3_count <- survfit2(Surv(time, died) ~ count_cat,
                             data = dat[dat$age_cat == "3rd_quantile", ]) %>%
    ggsurvfit(linewidth = 0.8) +
    # add_risktable() +
    formats +
    ggtitle("Age - 3rd quantile")

  km_plot4_full <- survfit2(Surv(time, died) ~ value_cat,
                            data = dat[dat$age_cat == "4th_quantile", ]) %>%
    ggsurvfit(linewidth = 0.8) +
    # add_risktable() +
    formats +
    ggtitle("Age - 4th quantile")

  km_plot4_count <- survfit2(Surv(time, died) ~ count_cat,
                             data = dat[dat$age_cat == "4th_quantile", ]) %>%
    ggsurvfit(linewidth = 0.8) +
    # add_risktable() +
    formats +
    ggtitle("Age - 4th quantile")

  count_one <-    ggsurvfit_build(km_plot1_count)
  count_two <-    ggsurvfit_build(km_plot2_count)
  count_three <-  ggsurvfit_build(km_plot3_count)
  count_fourth <- ggsurvfit_build(km_plot4_count)

  full_one <-    ggsurvfit_build(km_plot1_full)
  full_two <-    ggsurvfit_build(km_plot2_full)
  full_three <-  ggsurvfit_build(km_plot3_full)
  full_fourth <- ggsurvfit_build(km_plot4_full)

  km_plots_full <- cowplot::plot_grid(full_one, full_two, full_three, full_fourth)
  km_plots_count <- cowplot::plot_grid(count_one, count_two, count_three, count_fourth)

  # now add the title
  title_full <- cowplot::ggdraw() +
    cowplot::draw_label(
      paste0("Survival probability by index value (", index_name, ")"),
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )

  title_count <- cowplot::ggdraw() +
    cowplot::draw_label(
      paste0("Survival probability by disease counts"),
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )

  plot_res_full <- cowplot::plot_grid(
    title_full,  km_plots_full,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

  plot_res_count <- cowplot::plot_grid(
    title_full,  km_plots_count,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

  n_deaths_full <- dat |>
    group_by(index_value) |>
    summarise(deaths = sum(died),
              number_pat = n(),
              perc_pat = round(number_pat / nrow(dat), 2),
              perc_death = round(deaths / number_pat, 2)
    )

  n_deaths_count <- dat |>
    group_by(count) |>
    summarise(deaths = sum(died),
              number_pat = n(),
              perc_pat = round(number_pat / nrow(dat), 2),
              perc_death = round(deaths / number_pat, 2)
    )

  # now get the same but sum the values for groups with less than 10 participants
  i <- n_deaths_full$index_value[which.max(n_deaths_full$number_pat < 10)]

  n_deaths_full_02 <- n_deaths_full |>
    dplyr::mutate(value = cut(n_deaths_full$index_value, breaks = c(-Inf, 0:(i - 3), Inf))) |>
    group_by(value) |>
    summarise(deaths = sum(deaths),
              patients = sum(number_pat),
              risk_death = deaths / patients)

  n_deaths_full_02$value <- 0:(length(levels(droplevels(n_deaths_full_02$value))) - 1)
  n_deaths_full_02$model <- "index"



  i <- n_deaths_count$count[which.max(n_deaths_count$number_pat <= 10)]

  n_deaths_count_02 <- n_deaths_count |>
    dplyr::mutate(value = cut(n_deaths_count$count, breaks = c(-Inf, 0:(i - 3), Inf))) |>
    group_by(value) |>
    summarise(deaths = sum(deaths),
              patients = sum(number_pat),
              risk_death = deaths / patients)

  n_deaths_count_02$value <- 0:(length(levels(droplevels(n_deaths_count_02$value))) - 1)
  n_deaths_count_02$model <- "count"

  n_deaths <- bind_rows(n_deaths_count_02, n_deaths_full_02)

  death_v_value <- ggplot(n_deaths, aes(x = value, y = risk_death, colour = model, group = model)) +
    geom_point(size = 4) +
    geom_line(linewidth = 2) +
    ggtitle("Multimorbidity value versus risk of mortality") +
    theme_minimal(base_size = 16) +
    scale_y_continuous(name = "Empirical mortality risk") +
    scale_color_manual(values = c("skyblue", "orange"), name = "Model")

  plot_res_count
  plot_res_full

  # basic data about the sample
  basic <- data.frame(age_mean = round(mean(dat$age), 2),
                age_sd = round(sd(dat$age), 2),
                size = nrow(dat),
                n_events = sum(dat$died),
                risk_mort = round(sum(dat$died) / nrow(dat), 2),
                women = sum(dat$sex == "female"),
                women_perc = sum(dat$sex == "female") / nrow(dat)
  )


  res <- list(basic = t(basic),
              model_performance = perform,
              discrim_slope = d_slope,
              nri = list(base_index = nri_base_index,
                          base_count = nri_base_count,
                          count_index = nri_count_index),
              KM_plots_full = plot_res_full,
              KM_plots_count = plot_res_count,
              deaths_tables = list(index = list(n_deaths_full, n_deaths_full_02),
                                   count = list(n_deaths_count, n_deaths_count_02)),
              death_v_value_plot = death_v_value,
              fits = list(fit_base, fit_full, fit_count),
              calibration_plot = cal_plot
  )

  return(res)

}


# HELPERS -----------------------------------------------------------------


# helper function to re-define incidence and prevalence of an individual disease
# with respect to the new follow-up start (based on index data frame)
#
# the return_vector argument specifies whether the output should be a vector or whether the
# column should be updated dynamically in an existing data frame.
define_incidence <- function(dat, disease, return_vector = FALSE){

  startd <- paste0("startd_", disease)
  endd <- paste0("endd_", disease)
  prev <- paste0("prev_", disease)
  inc <- paste0("inc_", disease)

  # we need to make sure the ergoids are always in the same order, hence
  # arranging by ergoid
  dat <- arrange(dat, ergoid)

  res <- mutate(dat,
           !!disease := case_when(

            # catch all missing values
            is.na(!!sym(startd)) ~ NA,
            is.na(!!sym(endd)) ~ NA,

            (is.na(!!sym(prev)) | is.na(!!sym(inc))) ~ NA,

            # if disease fu start is later than the new fu start.
            # This should not happen or at least not very often
            (!!sym(startd) > fu_start) ~ NA,

            # now we only have participants with complete data and follow-up start
            # later or equal to the disease follow up start

            # if incident
            (!!sym(inc) == "yes" & !!sym(endd) <= fu_start) ~ 1,
            (!!sym(inc) == "yes" & !!sym(endd) > fu_start) ~ 0,

            # if disease free
            (!!sym(inc) == "no" & !!sym(endd) >= fu_start) ~ 0,
            (!!sym(inc) == "no" & !!sym(endd) < fu_start) ~ NA,

            # if prevalent
            (!!sym(inc) == "prev") ~ 1,

           # any other case not covered above
           .default = NA
           )
         )

  if (return_vector) res <- res %>% pull(!!disease)
  res
}

#' helper function to re-define mortality with respect to the new follow-up end
#' (based on index data frame)
assess_vital_status <- function(dat){
  dplyr::mutate(dat,
         died = case_when(

            # if alive
            (died == 0 & fp_censordate >= fu_end) ~ 0,

            # incomplete follow up - censored before the end of follow up
            (died == 0 & fp_censordate < fu_end) ~ 5,

            # if dead
            (died == 1 & fp_censordate > fu_end) ~ 0,
            (died == 1 & fp_censordate %within% (fu_start %--% fu_end)) ~ 1,

            # died before the follow up start
            (died == 1 & fp_censordate < fu_start) ~ 6,

            .default = 7
                )
  )

}

