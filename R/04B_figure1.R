# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT 2: Figure 1 ----------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# .This script creates Figure 1 of the manuscript with all performance metrics
# combined for each individual index

# For all questions, contact Premysl Velek or Marije Splinter:
# p.velek@erasmusmc.nl
# m.splinter@erasmusmc.nl


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
library(ggplot2)


# start here
setwd(here::here())


# Load data ---------------------------------------------------------------

load(here::here("RESULTS_FINAL", "all_validation_data.RData"))

# Create plot for each index separately -----------------------------------------


# this is a function that compiles all the individual plots relevant to one index
# and combine them into one object

# dat argument is the data with all performance stats loaded above

# index is character string with name of the indices, it can only take those
# values specified in the brackets. If not specified it takes the first value, ie. "desai"

#If you want to run separately, run this first.
index="desai"
.title="title"
data=dat

# .title is the title of the resulting plot, typically "Desai, 2002" and similar
plot_index <- function(data = dat, index = c("desai", "fan", "lee", "quan",
                                            "robusto", "tooth", "vonkorff"),
                       .title)
  {
  index <- match.arg(index)
  ind <- paste0("results_", index)

  # create data frame specific for each index
  df <- data[[ind]][["model_performance"]] %>%
      dplyr::filter(metric %in% c("R_base", "R_index", "R_count",
                                  "C_base", "C_index", "C_count",
                                  "B_base", "B_index", "B_count")) %>%
      dplyr::mutate(model = str_sub(metric, start = 3))

  # formating common for all plots
  geomrange_frmt <- list(geom_pointrange(aes(xmin = ci_lb, xmax = ci_ub), linewidth = 2, size = 1),
                      theme_minimal(base_size = 12),
                      scale_color_manual(name = NULL,
                                         values = c("grey", "skyblue", "orange")),
                      scale_y_discrete(name = "")
                    )


  all_frmt <- list(theme(legend.position = "none",
                           plot.title = element_text(size =  rel(0.9)),
                           axis.text = element_text(size = rel(0.5)),
                           axis.title = element_text(size = rel(0.7)))
                     )

  # C-statistic plot
  c_stat <- ggplot2::ggplot(df %>% dplyr::filter(metric %in% c("C_base", "C_index", "C_count")),
                              aes(x = value, y = model, colour = model)) +
    geomrange_frmt + all_frmt +
    scale_x_continuous(limits = c(0.5, 1), name = "") +
      ggtitle("C-statistic")

  # Nagelkerke plot
  R_nag <- ggplot2::ggplot(df %>% dplyr::filter(metric %in% c("R_base", "R_index", "R_count")),
                             aes(x = value, y = model, colour = model)) +
    geomrange_frmt + all_frmt +
    scale_x_continuous(limits = c(0, 0.5), name = "") +
      ggtitle(expression(paste("Nagelgerke's R"^2)))

  # Brier score plot
  B_scor <- ggplot2::ggplot(df %>% dplyr::filter(metric %in% c("B_base", "B_index", "B_count")),
                              aes(x = value, y = model, colour = model)) +
    geomrange_frmt + all_frmt +
    scale_x_continuous(limits = c(0, 0.1), name = "") +
      ggtitle("Brier score")

  # calibration plot
  cal <- data[[ind]][["calibration_plot"]] +
      geom_line(linewidth = 2) +
      ggtitle("Calibration plot") +   all_frmt +
      scale_y_continuous(name = "Predicted risk") +
      scale_x_continuous(name = "Observed risk")

  # MM score vs mortality risk
  val_mort <- data[[ind]][["death_v_value_plot"]] +
      ggtitle("Multimorbidity score vs. mortality risk") + theme(legend.position = "none") +
      theme_minimal(base_size = 12) + all_frmt +
      scale_x_continuous(name = "Multimorbidity score")

# combine all the abvove plots into one graphic
  pl <- ((c_stat / R_nag / B_scor) | cal) + val_mort + plot_layout(widths = c(1, 1, 1))

  # add title
  pl & plot_annotation(title = .title) &
    theme(plot.title = element_text(hjust = 0))

}


# create individual plots
desai <- plot_index(index = "desai", .title = "Desai, 2002")
tooth <- plot_index(index = "tooth", .title = "Tooth, 2008")
fan <- plot_index(index = "fan", .title = "Fan, 2002")
quan <- plot_index(index = "quan", .title = "Quan, 2011")
robusto <- plot_index(index = "robusto", .title = "Robusto, 2016")
vonkorff <- plot_index(index = "vonkorff", .title = "Von Korff, 1992")
lee <- plot_index(index = "lee", .title = "Lee, 2006")

# once you have all plots ready, stack them on top of each other
wrap_elements(desai) /
  wrap_elements(fan) /
  wrap_elements(lee) /
  wrap_elements(quan) /
  wrap_elements(robusto) /
  wrap_elements(tooth) /
  wrap_elements(vonkorff)

