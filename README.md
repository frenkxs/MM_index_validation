
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MM_index_validation

These are the scripts for the article by Velek, Splinter et all,
*Counting co-occurring diseases to predict mortality is as accurate as
multimorbidity indices: results from an external validation study in the
general population*

NB: The scripts rely on data that are not available in this repository.
We are unable to place data in a public repository due to legal and
ethical restraints. Sharing of individual participant data was not
included in the informed consent of the study, and there is potential
risk of revealing participants’ identities as it is not possible to
completely anonymise the data.

All questions regarding the scripts should be directed to Premysl Velek,
<p.velek@erasmusmc.nl>

Below is explanation of how the script was built and the workflow used.
For details on specific analytic choices, refer to the respective
functions in the `99_get_cohorts.R` script. Note that data cleaning is
not included in the workflow as it builds on [our previous
work](https://doi.org/10.1186/s12916-022-02487-x). Details can be found
in `01_clean_index.R`, and in the specific scripts to clean data
according to Robusto, 2016 and von Korff, 1992 (to be found in their
respective folders).

## Introduction

This is the basic workflow for building specific cohort (subset of the
Rotterdam Study participants) with the following characteristics defined
by the user:

-   Follow-up for specific diseases (from among those available to us)
-   Specific follow-up time
-   Specific mean age at baseline
-   Specific sex ratio (men only, women only or both)

The workflow has several functions (detailed below), the output are two
data frames with data for the same cohort (ie. same participants, same
follow-up time, the same baseline date and the same outcome
(mortality)), but with different independent variables:

-   *index cohort* contains indicator variable for presence of set of
    user defined diseases at baseline
-   *count cohort* contains the number of co-occurring diseases at
    baseline from among 10 non-communicable diseases with high burden in
    older populations (cancer, dementia, diabetes, coronary heart
    disease, heart failure, COPD, asthma, parkinsonism, stroke,
    depression)

The goal is to compare the performance of two approaches to defining
multimorbidity (with respect to mortality): one is simple count of
co-occurring diseases, the other is weighted approach in which different
diseases have different weights. The weights are derived for different
diseases coming from different multimorbidity indices.

## Data preparation

As the workflow builds specific cohort, it has to be repeated for each
individual index you want to test.

The workflow relies on a couple of libraries and a couple of helper
functions. They all have to be loaded for it to work. All code in the
document assumes you’re working in an RStudio project.

``` r
library(tidyverse)
library(lubridate)

source(here::here("R", "get_cohorts.R"))
```

### Compile index

You first compile the index from follow-up data on specific diseases,
defined by the user. The code below compiles the index by Tooth, 2008
which uses six diseases with the following weights:

-   heart disease = 1
-   stroke = 2
-   low iron = 1
-   lung disease = 2
-   diabetes = 1
-   cancer = 3
-   alzheimer disease = 4

``` r
d <- c("hd", "stroke", "anemia", "lung", "dia", "can", "dem")

w <- c(1, 2, 1, 2, 1, 3, 4)

tooth <- compile_cohort(diseases = d)
```

### Index cohort

The function `get_index_cohort` takes as an input the index data,
selects ERGO participants who have a complete follow up for particular
diseases used in the given index and selects the baseline date (the
follow up start) so that the mean age of the cohort at baseline matches
with the mean age of the cohort used to develop that particular index.
It can also select men or women only if that’s the case of the original
index.

The arguments of the function are:

-   index_data: data containing all the diseases in a given index. The
    following data have to be included:

    -   Start and end of follow up for each individual disease and the
        prevalence/incidence indicators. The names of the variables have
        to be in the standard SHIFT form, e.g. ‘startd_chd’, ‘endd_chd’,
        ‘inc_chd’, ‘prev_chd’. (Diseases that don’t require any
        additional change to match the disease definition in the index
        cohort can be taken directly form the SHIFT data.) This
        structure have to be included even for additional variables for
        which we don’t have longitudinal data (e.g. ADL). In this case,
        all cases will be prevalent and no case will be incident. The
        follow up start would then be the date of the interview, test,
        screening, etc. (In this particular case the end of follow up is
        not relevant and can stay empty)
    -   Information about the individual ERGO cohorts (RS-I, RS-II,
        RS-II).  
    -   Date of birth (birthd), censor date (fp_censordate) and vital
        status for each participants (died), coded with 1 for death and
        0 for living.
    -   Overall follow-up start (the latest follow-up start among the
        individual diseases, fu_start) and follow_up end (the earliest
        follow up end from among the individual diseases, fu_end)

-   longitudinal diseases: character vector with the diseases included
    in the index you want to validate. They have to be in the standard
    SHIFT form, as indicated in the list below. Importantly, as the name
    suggests, the diseases have to have continuous follow up.

    -   dia: diabetes
    -   stroke: stroke
    -   can1: cancer, all types
    -   dem: dementia
    -   hf: heart failure
    -   chd: coronary heart disease
    -   park: parkinsonism
    -   dep1: depression
    -   COPD: chronic obstructive pulmonary disease
    -   asthma: asthma

-   cross_sectional_diseases: diseases, risk factors and lifestyle data
    measured only at one (or several) point in time.

-   mean_age_index: the mean age of the index cohort, i.e. the mean age
    to which we need to get as close as possible to warrant fair
    comparison. Only integer values are allowed. **IMPORTANT NOTE: if
    your data does not allow you to move the follow up start (typically
    because you only have cross-sectional data for some diseases),
    ignore this argument. The function will not move the follow-up
    start.**

-   sex_ratio_index: whether men only, women only or both men and women
    should be included in the cohort, based on the original cohort of
    each index. Only three values are allowed: women_only will results
    in a cohort with only women, men_only will result in a cohort with
    only men, both - the default - will not change the sex ratio

-   fu_time_index: the follow-up time of the original paper

``` r
tooth_index <- get_index_cohort(index_data = tooth,
                                mean_age_index = NULL,
                                sex_ratio_index = "women_only",
                                longitudinal_diseases = c("hd", "stroke", "lung",
                                                          "dia", "can", "dem"),
                                cross_sectional_diseases = c("anemia"),
                                fu_time_index = 6)

save(tooth_index_values, tooth_count, tooth_index,
     file = here::here("RESULTS_FINAL", "final_data", "tooth2008_data.RData"))
```

### Count cohort

Once you created the index cohort, you will then use it to create the
count cohort. It will have the same follow up start the same follow up
time and the same participants. But in this case, we will use all ten
diseases included in the SHIFT data and count how many diseases each
participant had at the start of the follow up. The function
`get_count_cohort` creates this count cohort, based on the index cohort
(the output of the `get_index_cohort` function).

The arguments of the function are:

-   index_data: the index cohort dataset, output of the
    `get_index_cohort` function
-   data: the original SHIFT data. By default, it is assumed to be the
    shift_data but it has be loaded into R beforehand

``` r
tooth_count <- get_count_cohort(index_cohort_data = tooth_index)
```

Note that the sample size will likely be smaller than that for the index
cohort as there is more variables in the count cohort and hence greater
chance of any of the variables being missing. That’s why we need to do
one last step.

### Index cohort update

As the last step, we need to remove those patients who have complete
follow up in the index cohort but who miss some variables (diseases) in
the count cohort:

``` r
tooth_index <- tooth_index[tooth_index$ergoid %in% tooth_count$ergoid, ]
```

### Index values

The last step is to calculate the index value based on the assigned
weights:

``` r
tooth_index_values <- get_index_values(index_cohort_data = tooth_index,
                                      diseases = d,
                                      weights = w)
```

Now, we have two cohorts *tooth_index* and *tooth_count* that have the
same participants, the same follow up start, the same follow-up time and
the same outcome. We save them.

``` r
save(tooth_index_values, tooth_count, tooth_index,
     file = here::here("RESULTS_FINAL", "final_data", "tooth2008_data.RData"))
```

We can now move on to compare them. Hurrah :D

The workflow up until this point is executed in the
*02_compile_index.R*.

## Data analysis

For data analysis, we use the `validate_index_v02` function. It has four
arguments:

-   dat: data containing both the index values and disease counts for a
    particular cohort
-   index_name: name of the index to be used on plots and other output
-   include_age: shall we include age as a separate independent
    variable? (Some indices incorporate age directly into their index
    value)
-   include_sex” shall we include sex as a separate independent
    variable? (Some indices are sex specific)

This function fits a logistic regression model on the data and produce a
series of metrics and outcomes:

-   Basic information about the cohort (mean age, size, number of
    events, sex ratio)
-   Performance of the count, index and base model in terms of
    Nagelkerke’s R^2, C-statistics and Brier score
-   Discrimination slope and discrimination box plots
-   Net reclassification improvement
-   Kaplan-Meier plots by age quartiles for index and count models
-   Fits of the three models, as returned by the `rms::lrm` function
-   calibration plots

``` r
load(here::here("RESULTS_FINAL", "final_data", "desai2002_data.RData"))

df <- left_join(desai_count,
                desai_index_values |> dplyr::select(ergoid, index_value),
                by = "ergoid")


results_desai <- validate_index_v02(dat = df, index_name = "Desai 2002",
                                    include_age = TRUE)

save(results_desai,
     file = here::here("RESULTS_FINAL", "validation", "desai2002.RData"))
```

All data analysis is executed in the *03_validate_index.R* script.
