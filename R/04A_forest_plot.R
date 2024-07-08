# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT 2: Plotting c-statistic ----------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# .This script plots C-statistic for each individual index with a comparison with
# the corresponding count model

# For all questions, contact Premysl Velek at p.velek@erasmusmc.nl



# ------------------------ Preliminaries ---------------------------------------

# libraries needed in this script
# packages_needed <- c("foreign", "tidyverse", "lubridate", "here")
#
# # install missing libraries
# install.packages(setdiff(packages_needed, rownames(installed.packages())))
# rm(packages_needed)

# load libraries
require(tidyverse)
require(lubridate)
require(gtsummary)
require(survival)
library(gtsummary)
library(ggsurvfit)
library(colorspace)
library(patchwork)

# start here
setwd(here::here())


# Load data ---------------------------------------------------------------

indices <- c("desai2002", "fan2002", "lee2006", "quan2011",
             "robusto2016", "tooth2008", "vonkorff1992")

dat <- new.env()
load_c <- function(index){
  load(here::here("RESULTS_FINAL", "validation", paste0(index, ".RData")), envir = dat)
}

lapply(indices, load_c)

save(dat, file = here::here("RESULTS_FINAL", "all_validation_data.RData"))
load(here::here("RESULTS_FINAL", "all_validation_data.RData"))

# calculate scaled Brier score

index <- value <- ci_ub <- ci_lb <- rep(NA, 7)

B_max_base <- data.frame(index,  value, ci_ub, ci_lb, model = "base")
B_max_index <- data.frame(index, value, ci_ub, ci_lb, model = "index")
B_max_count <- data.frame(index, value, ci_ub, ci_lb, model = "count")

indices <- c("desai", "fan", "lee", "quan",
             "robusto", "tooth", "vonkorff")

for (i in 1:length(indices)){
  res <- paste0('results_', indices[i])
  p <- dat[[res]][["basic"]][4, 1] / dat[[res]][["basic"]][3, 1]
  B_max <- p * (1 - p)

  sB_base <- 1 - (dat[[res]][["model_performance"]][10, 2:4] / B_max)
  sB_index <- 1 - (dat[[res]][["model_performance"]][11, 2:4] / B_max)
  sB_count <- 1 - (dat[[res]][["model_performance"]][12, 2:4] / B_max)

  B_max_base[i, 1:4] <- c(indices[i], sB_base)
  B_max_index[i, 1:4] <- c(indices[i], sB_index)
  B_max_count[i, 1:4] <- c(indices[i], sB_count)

}

B_max <- bind_rows(B_max_base, B_max_index, B_max_count)

ggplot(B_max |> filter(model != "base"), aes(y = model, colour = model, group = model)) +
  geom_point(aes(x = value), size = 2) +
  geom_linerange(aes(xmin = ci_lb, xmax = ci_ub), linewidth = 2) +
  facet_grid(index ~ .) +
  scale_colour_discrete(guide = "none") +
  scale_x_continuous(breaks = seq(-0.5, 0.5, length.out = 11),
                     minor_breaks = seq(-0.45, 0.45, length.out = 10)) +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
  theme_minimal(base_size = 16)

plot_statistics <- function(statistic = c("D", "R", "C", "B", "sB_max", "sB_base"),
                            data = dat, title){

  # get the number of indices
  n_ind <- length(dat)

  stat_base <- c(rep(NA, n_ind))
  stat_index <- c(rep(NA, n_ind))
  stat_count <- c(rep(NA, n_ind))

  stat_base_ci_lb <- c(rep(NA, n_ind))
  stat_index_ci_lb <- c(rep(NA, n_ind))
  stat_count_ci_lb <- c(rep(NA, n_ind))

  stat_base_ci_ub <- c(rep(NA, n_ind))
  stat_index_ci_ub <- c(rep(NA, n_ind))
  stat_count_ci_ub <- c(rep(NA, n_ind))

  # get a starting index for a given statistic (based on the column order in data)
  index <- case_when(
    statistic == "D" ~ 1,
    statistic == "R" ~ 4,
    statistic == "C" ~ 7,
    statistic == "B" ~ 10,
    statistic == "sB_max" ~ 13,
    statistic == "sB_base" ~ 16
  )



  # extract statistics
  for (i in 1:n_ind){
    stat_index[i] <- data[[ls(data)[[i]]]][[2]][[index + 1, 2]]
    stat_count[i] <- data[[ls(data)[[i]]]][[2]][[index + 2, 2]]

    stat_index_ci_lb[i] <- data[[ls(data)[[i]]]][[2]][[index + 1, 3]]
    stat_index_ci_ub[i] <- data[[ls(data)[[i]]]][[2]][[index + 1, 4]]

    stat_count_ci_lb[i] <- data[[ls(data)[[i]]]][[2]][[index + 2, 3]]
    stat_count_ci_ub[i] <- data[[ls(data)[[i]]]][[2]][[index + 2, 4]]

    if (statistic != "sB_base" ) {
      stat_base[i] <- data[[ls(data)[[i]]]][[2]][[index, 2]]
      stat_base_ci_lb[i] <-  data[[ls(data)[[i]]]][[2]][[index, 3]]
      stat_base_ci_ub[i] <-  data[[ls(data)[[i]]]][[2]][[index, 4]]
    }
  }

  mod <- c(rep("Base", n_ind), rep("Index", n_ind), rep("Count", n_ind))


  stat <- data.frame(indices = rep(ls(data), 3),
                       stat = c(stat_base, stat_index, stat_count),
                       ci_lb =  c(stat_base_ci_lb, stat_index_ci_lb, stat_count_ci_lb),
                       ci_ub =  c(stat_base_ci_ub, stat_index_ci_ub, stat_count_ci_ub),
                       model = mod) |>
    dplyr::mutate(indices = stringr::str_to_title(substring(indices, first = 9)))

  upper_limit <- ifelse(statistic == "B" | statistic == "sB_max" | statistic == "sB_base", 0.15, 1)
  lower_limit <- ifelse(statistic == "sB_base", -0.15, 0)

  stat |>
    ggplot(aes(y = model, colour = model, group = model)) +
    geom_point(aes(x = stat), size = 5) +
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub), linewidth = 2) +
    facet_grid(indices ~ .) +
    scale_x_continuous(name = NULL, limits = c(lower_limit, upper_limit), breaks = seq(0, 1, by = 0.1)) +
    scale_color_manual(values = c("grey", "skyblue", "orange"), guide = "none") +
    ggtitle(paste0("Comparison of model performance: ", title)) +
    theme_minimal(base_size = 16) +
    theme(strip.background = element_rect(fill = "lightgrey", color = "lightgrey"))
}


plot_index <- function(data = dat, .title = "", index) {
  ind <- paste0("results_", index)

  df <- data[[ind]][["model_performance"]] |>
    dplyr::filter(metric %in% c("R_base", "R_index", "R_count",
                                "C_base", "C_index", "C_count",
                                "B_base", "B_index", "B_count")) |>
    dplyr::mutate(model = str_sub(metric, start = 3))

  geomrange <- list(geom_pointrange(aes(xmin = ci_lb, xmax = ci_ub), linewidth = 2, size = 1),
                    theme_minimal(base_size = 12),
                    scale_color_manual(name = NULL, values = c("grey", "skyblue", "orange")),
                    theme(legend.position = "none"),
                    scale_y_discrete(name = ""),
                    theme(plot.title = element_text(size = 12)))

  c_stat <- ggplot2::ggplot(df |> dplyr::filter(metric %in% c("C_base", "C_index", "C_count")),
                            aes(x = value, y = model, colour = model)) + geomrange +
    scale_x_continuous(limits = c(0.5, 1), name = "") +
    ggtitle("C-statistic")


  R_nag <- ggplot2::ggplot(df |> dplyr::filter(metric %in% c("R_base", "R_index", "R_count")),
                            aes(x = value, y = model, colour = model)) + geomrange +
    scale_x_continuous(limits = c(0, 0.5), name = "") +
    ggtitle(expression(paste("Nagelgerke's R"^2)))

  B_scor <- ggplot2::ggplot(df |> dplyr::filter(metric %in% c("B_base", "B_index", "B_count")),
                            aes(x = value, y = model, colour = model)) + geomrange +
    scale_x_continuous(limits = c(0, 0.1), name = "") +
    ggtitle("Brier score") + theme(legend.position = "bottom")

  cal <- dat[[ind]][["calibration_plot"]] +
    geom_line(linewidth = 2) +
    ggtitle("Calibration plot") + theme(legend.position = "none", plot.title = element_text(size = 12)) +
    scale_y_continuous(name = "Predicted risk") +
    scale_x_continuous(name = "Observed risk")

  val_mort <- dat[[ind]][["death_v_value_plot"]] +
    ggtitle("Multimorbidity score vs. mortality risk") + theme(legend.position = "none") +
    theme_minimal(base_size = 12) + theme(legend.position = "none", plot.title = element_text(size = 11)) +
    scale_x_continuous(name = "Multimorbidity score")


  pl <- ((c_stat / R_nag / B_scor) | cal) + val_mort + plot_layout(widths = c(1, 1, 1))
  pl + plot_annotation(title = ".title",
                       theme = theme(plot.title = element_text(size = 18)))
}



desai <- plot_index(.title = "Desai, 2002", index = "desai")
quan <- plot_index(.title = "Quan, 2011", index = "quan")


plot_statistics(statistic = "D", title = "Sommers' D")
plot_statistics(statistic = "C", title = "C-statistic")
plot_statistics(statistic = "R", title = "Nagelkerke's R")
plot_statistics(statistic = "B", title = "Brier score")
plot_statistics(statistic = "sB_max", title = "Scaled Brier score")
plot_statistics(statistic = "sB_base", title = "Scaled Brier score wrt base model")

dat$results_desai$basic
dat$results_fan$basic
dat$results_lee$basic
dat$results_quan$basic
dat$results_robusto$basic
dat$results_tooth$basic
dat$results_vonkorff$basic

dat$results_desai$deaths_table_full
dat$results_desai$deaths_table_count
dat$results_lee$model_performance

dat$results_desai$death_v_value_plot
dat$results_fan$death_v_value_plot
dat$results_lee$death_v_value_plot
dat$results_quan$death_v_value_plot
dat$results_robusto$death_v_value_plot
dat$results_tooth$death_v_value_plot
dat$results_vonkorff$death_v_value_plot


dat$results_desai$nri$count_index
dat$results_fan$nri$count_index
dat$results_lee$nri$count_index
dat$results_quan$nri$count_index
dat$results_robusto$nri$count_index
dat$results_tooth$nri$count_index
dat$results_vonkorff$nri$count_index
