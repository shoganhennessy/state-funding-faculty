#!/usr/bin/R
## Senan Hogan-Hennessy, 24 April 2024
## IV for Prof's salaries, using IPEDS data.
print(Sys.time())
library(jtools) # Plotting estimates.
library(tidyverse) # Functions for data manipulation and visualization
library(lfe) # Functions for fast linear models with IV + FEs
library(plm) # Functions for panel data
library(car) # Function for F stat regarding IV models
# My custom flavour of Stargazer TeX tables:
# devtools::install_github("shoganhennessy/stargazer")
library(stargazer) # TeX tables
library(xtable)
set.seed(47)
# This file follows an adjusted Deming Walters (2017, p.10) approach to
# appropriations shock instrument.

# Define number of digits in tables and graphs
digits.no <- 3

# Size for figures
fig.width <- 10
fig.height <- fig.width * 0.9

# Define default colours.
colour.list <- c(
    "#D62728", # Red  -> First-stage.
    "#1f77b4", # Blue -> Lecturers
    "#c9721c", # Orange -> Asst Professors
    "#237e23", # Green -> Full Professors
    "#cc2c9f") # Strong pink -> All faculty.

# Define a function to standardise the dplyr lag function
# --- negative numbers mean a lag of t time periods, positive a lead of t.
lag_lead <- function(column, t) {
    if (t < 0) {
        lag_column <- dplyr::lag(column, -t)
    } else if (t >= 0) {
        lag_column <- dplyr::lead(column, t)
    }
    return(lag_column)
}


# Load data sources ------------------------------------------------------------

# Load IPEDS data, for only the public unis
ipeds_public.data <- read_csv("../../data/urban-ipeds/urban-clean-publicunis.csv")

# Load IPEDS data, for all the unis
ipeds_all.data <- read_csv("../../data/urban-ipeds/urban-clean-allunis.csv")


################################################################################
## Robustness check: effect on faculty with extra controls for selectivity.

# Select IPEDS variables, and define the panel at the university level
reg.data <- ipeds_public.data %>%
    # Create independent columns and rows.
    select(
        # Identifiers and controls
        unitid,
        inst_name,
        year,
        state,
        # Appropriations to the uni
        totalrevenues_real,
        stateappropriations_real,
        tuitionrev_real,
        # Uni spending.
        nonauxspending_real,
        # Uni enrollment
        enrollment_reported,
        enrollment_fte,
        acceptance_rate,
        completion_rate_150pct,
        # Shift-sahre shocks Per student
        appropriationshock_perEnroll_real,
        allstate_stateappropriations_real,
        staterevenues_baseshare,
        stateEnroll_count,
        # Count of full-time professors (tenured or not)
        lecturer_prof_count,
        assistant_prof_count,
        full_prof_count,
        all_prof_count,
        # Barron's ranking 2009
        barrons_rank_2009)

# Restrict data to unis + years with measured state appropriations & shocks
reg.data <- reg.data %>%
    filter(1990 <= year, #year < 2019,
        !is.na(enrollment_fte), enrollment_fte > 0,
        !is.na(all_prof_count),
        stateappropriations_real > 0,
        appropriationshock_perEnroll_real > 0,
        !is.na(appropriationshock_perEnroll_real)) %>%
    # Restrict to uni-years with all professor counts.
    filter(!is.na(lecturer_prof_count), lecturer_prof_count > 0) %>%
    filter(!is.na(assistant_prof_count), assistant_prof_count > 0) %>%
    filter(!is.na(full_prof_count), full_prof_count > 0)

## Get admission rate in the mid-2000s
reg.data <- reg.data %>%
    filter(year %in% c(2004:2006)) %>%
    group_by(unitid) %>%
    summarise(
        acceptance_rate_2000s = mean(acceptance_rate, na.rm = TRUE),
        completion_rate_150pct_2000s =
            mean(completion_rate_150pct, na.rm = TRUE)) %>%
    ungroup() %>%
    right_join(reg.data, by = "unitid")


## First-stage with Further Controls
# Raw version
firststage_raw.reg <- reg.data %>%
    mutate(
        stateappropriations_real = stateappropriations_real,
        appropriationshock_perEnroll_real =
            -appropriationshock_perEnroll_real,
        staterevenues_baseshare = staterevenues_baseshare * 100,
        acceptance_rate_2000s = acceptance_rate_2000s * 100,
        completion_rate_150pct_2000s = completion_rate_150pct_2000s * 100,
        tuitionrev_real = I(tuitionrev_real / 10^6),
        enrollment_fte = enrollment_fte,
        enrollment_fte_thousands = I(enrollment_fte / 10^3),
        stateEnroll_count = I(stateEnroll_count / 10^3),
        enrollment_state_percent = I(enrollment_fte / stateEnroll_count)) %>%
    felm(I(stateappropriations_real / enrollment_fte) ~ 1 +
        staterevenues_baseshare +
        acceptance_rate_2000s +
        completion_rate_150pct_2000s +
        tuitionrev_real +
        enrollment_fte_thousands +
        stateEnroll_count +
        enrollment_state_percent +
        appropriationshock_perEnroll_real |
        state + year |
        0 |
        state + year,
        data = .)
summary(firststage_raw.reg)
# Get the F.Stat
firststage_raw.fstat <-
    linearHypothesis(firststage_raw.reg, test = "F",
        c("appropriationshock_perEnroll_real"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Log Version
firststage_approp.reg <- reg.data %>%
    mutate(
        acceptance_rate_2000s = log(acceptance_rate_2000s),
        completion_rate_150pct_2000s = log(completion_rate_150pct_2000s),
        tuitionrev_real = log(tuitionrev_real),
        enrollment_fte = enrollment_fte,
        enrollment_fte_thousands = log(enrollment_fte),
        staterevenues_baseshare = log(staterevenues_baseshare),
        stateEnroll_count = log(stateEnroll_count),
        enrollment_state_percent = I(enrollment_fte / stateEnroll_count),
        appropriationshock_perEnroll_real =
            -log(appropriationshock_perEnroll_real)) %>%
    felm(log(stateappropriations_real / enrollment_fte) ~ 1 +
        staterevenues_baseshare +
        acceptance_rate_2000s +
        completion_rate_150pct_2000s +
        tuitionrev_real +
        enrollment_fte_thousands +
        stateEnroll_count +
        enrollment_state_percent +
        appropriationshock_perEnroll_real |
        state + year |
        0 |
        state + year,
        data = .)
summary(firststage_approp.reg)
# Get the F.Stat
firststage_approp.fstat <-
    linearHypothesis(firststage_approp.reg, test = "F",
        c("appropriationshock_perEnroll_real"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Collate the results to a LaTeX table
X.labels <- c(
        "State Funding, Base Share \\%",
        "Acceptance Rate, \\%",
        "6--Year Completion Rate, \\%",
        "Tuition Revenue, \\$ millions",
        "Enrolment, FTE thousands",
        "State Enrolment, thousands",
        "Percent of State Enrolment",
        "State Funding")
stargazer(
    firststage_raw.reg, firststage_approp.reg,
    dep.var.caption = "Dependent Variable: State Funding",
    column.labels = c("Raw Count Units", "Log Units"),
    dep.var.labels.include = FALSE,
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    order = c(8, 1:7),
    covariate.labels = X.labels[c(8, 1:7)],
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq", "f"),
    add.lines = list(
        c("Units ", "Raw counts", "Log, \\% terms"),
        c("F stat.", firststage_raw.fstat, firststage_approp.fstat),
        c("State + Year fixed effects?", "Yes", "Yes")),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/firststage-robustness-checks.tex")


## Log Faculty Count Regressions with Further Controls

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_fte)(fit)` =
        log(stateappropriations_real / enrollment_fte)) %>%
    felm(log(lecturer_prof_count / enrollment_fte) ~ 1 +
        log(staterevenues_baseshare) +
        log(acceptance_rate_2000s) +
        log(completion_rate_150pct_2000s) +
        log(tuitionrev_real) +
        log(enrollment_fte) +
        log(stateEnroll_count) +
        I(enrollment_fte / stateEnroll_count) +
        `log(stateappropriations_real/enrollment_fte)(fit)` |
        state + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- reg.data %>%
    felm(log(lecturer_prof_count / enrollment_fte) ~ 1 +
        log(staterevenues_baseshare) +
        log(acceptance_rate_2000s) +
        log(completion_rate_150pct_2000s) +
        log(staterevenues_baseshare) +
        log(tuitionrev_real) +
        log(enrollment_fte) +
        log(stateEnroll_count) +
        I(enrollment_fte / stateEnroll_count) |
        state + year |
        (log(stateappropriations_real / enrollment_fte) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Asst faculty Count
# Naive OLS Regression
naive_assistant_count.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_fte)(fit)` =
        log(stateappropriations_real / enrollment_fte)) %>%
    felm(log(assistant_prof_count / enrollment_fte) ~ 1 +
        log(staterevenues_baseshare) +
        log(acceptance_rate_2000s) +
        log(completion_rate_150pct_2000s) +
        log(tuitionrev_real) +
        log(enrollment_fte) +
        log(stateEnroll_count) +
        I(enrollment_fte / stateEnroll_count) +
        `log(stateappropriations_real/enrollment_fte)(fit)` |
        state + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- reg.data %>%
    felm(log(assistant_prof_count / enrollment_fte) ~ 1 +
        log(staterevenues_baseshare) +
        log(acceptance_rate_2000s) +
        log(completion_rate_150pct_2000s) +
        log(staterevenues_baseshare) +
        log(tuitionrev_real) +
        log(enrollment_fte) +
        log(stateEnroll_count) +
        I(enrollment_fte / stateEnroll_count) |
        state + year |
        (log(stateappropriations_real / enrollment_fte) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Full faculty Count
# Naive OLS Regression
naive_full_count.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_fte)(fit)` =
        log(stateappropriations_real / enrollment_fte)) %>%
    felm(log(full_prof_count / enrollment_fte) ~ 1 +
        log(staterevenues_baseshare) +
        log(acceptance_rate_2000s) +
        log(completion_rate_150pct_2000s) +
        log(tuitionrev_real) +
        log(enrollment_fte) +
        log(stateEnroll_count) +
        I(enrollment_fte / stateEnroll_count) +
        `log(stateappropriations_real/enrollment_fte)(fit)` |
        state + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- reg.data %>%
    felm(log(full_prof_count / enrollment_fte) ~ 1 +
        log(staterevenues_baseshare) +
        log(acceptance_rate_2000s) +
        log(completion_rate_150pct_2000s) +
        log(staterevenues_baseshare) +
        log(tuitionrev_real) +
        log(enrollment_fte) +
        log(stateEnroll_count) +
        I(enrollment_fte / stateEnroll_count) |
        state + year |
        (log(stateappropriations_real / enrollment_fte) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_fte)(fit)` =
        log(stateappropriations_real / enrollment_fte)) %>%
    felm(log(all_prof_count / enrollment_fte) ~ 1 +
        log(staterevenues_baseshare) +
        log(acceptance_rate_2000s) +
        log(completion_rate_150pct_2000s) +
        log(tuitionrev_real) +
        log(enrollment_fte) +
        log(stateEnroll_count) +
        I(enrollment_fte / stateEnroll_count) +
        `log(stateappropriations_real/enrollment_fte)(fit)` |
        state + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- reg.data %>%
    felm(log(all_prof_count / enrollment_fte) ~ 1 +
        log(staterevenues_baseshare) +
        log(acceptance_rate_2000s) +
        log(completion_rate_150pct_2000s) +
        log(staterevenues_baseshare) +
        log(tuitionrev_real) +
        log(enrollment_fte) +
        log(stateEnroll_count) +
        I(enrollment_fte / stateEnroll_count) |
        state + year |
        (log(stateappropriations_real / enrollment_fte) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    reg.data %>% mutate(outcome = lecturer_prof_count / (enrollment_fte / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% mutate(outcome = lecturer_prof_count / (enrollment_fte / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_count.reg
    reg.data %>% mutate(outcome = assistant_prof_count / (enrollment_fte / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% mutate(outcome = assistant_prof_count / (enrollment_fte / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_count.reg
    reg.data %>% mutate(outcome = full_prof_count / (enrollment_fte / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% mutate(outcome = full_prof_count / (enrollment_fte / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_count.reg
    reg.data %>% mutate(outcome = all_prof_count / (enrollment_fte / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% mutate(outcome = all_prof_count / (enrollment_fte / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no)
))
        
# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_count.reg, shiftshare_lecturer_count.reg,
    naive_assistant_count.reg, shiftshare_assistant_count.reg,
    naive_full_count.reg, shiftshare_full_count.reg,
    naive_all_count.reg, shiftshare_all_count.reg,
    add.lines = outcome.means,
    dep.var.caption =
        "Dependent Variable: Log Faculty Count per Students, by Position",
    dep.var.labels = c(
        "Lecturers", "Asst. Professors", "Full Professors", "All Faculty"),
    column.labels = rep(c("OLS", "2SLS"), 4),
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    intercept.bottom = TRUE,
    order = c(8, 1:7),
    covariate.labels = X.labels[c(8, 1:7)],
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq"),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/facultycount-robustness-checks.tex")


## Raw Faculty Count Regressions with Further Controls

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- reg.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_fte))(fit)` =
        I(stateappropriations_real/(1000 * enrollment_fte))) %>%
    felm(lecturer_prof_count ~ 1 +
        staterevenues_baseshare +
        acceptance_rate_2000s +
        completion_rate_150pct_2000s +
        I(tuitionrev_real / 10^6) +
        I(enrollment_fte / 10^3) +
        I(stateEnroll_count / 10^3) +
        I(enrollment_fte / stateEnroll_count) +
        `I(stateappropriations_real/(1000 * enrollment_fte))(fit)` |
        state + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- reg.data %>%
    felm(lecturer_prof_count ~ 1 +
        staterevenues_baseshare +
        acceptance_rate_2000s +
        completion_rate_150pct_2000s +
        I(tuitionrev_real / 10^6) +
        I(enrollment_fte / 10^3) +
        I(stateEnroll_count / 10^3) +
        I(enrollment_fte / stateEnroll_count) |
        state + year |
        (I(stateappropriations_real/(1000 * enrollment_fte)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

## Asst faculty Count
# Naive OLS Regression
naive_assistant_count.reg <- reg.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_fte))(fit)` =
        I(stateappropriations_real/(1000 * enrollment_fte))) %>%
    felm(assistant_prof_count ~ 1 +
        staterevenues_baseshare +
        acceptance_rate_2000s +
        completion_rate_150pct_2000s +
        I(tuitionrev_real / 10^6) +
        I(enrollment_fte / 10^3) +
        I(stateEnroll_count / 10^3) +
        I(enrollment_fte / stateEnroll_count) +
        `I(stateappropriations_real/(1000 * enrollment_fte))(fit)` |
        state + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- reg.data %>%
    felm(assistant_prof_count ~ 1 +
        staterevenues_baseshare +
        acceptance_rate_2000s +
        completion_rate_150pct_2000s +
        I(tuitionrev_real / 10^6) +
        I(enrollment_fte / 10^3) +
        I(stateEnroll_count / 10^3) +
        I(enrollment_fte / stateEnroll_count) |
        state + year |
        (I(stateappropriations_real/(1000 * enrollment_fte)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

## Full faculty Count
# Naive OLS Regression
naive_full_count.reg <- reg.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_fte))(fit)` =
        I(stateappropriations_real/(1000 * enrollment_fte))) %>%
    felm(full_prof_count ~ 1 +
        staterevenues_baseshare +
        acceptance_rate_2000s +
        completion_rate_150pct_2000s +
        I(tuitionrev_real / 10^6) +
        I(enrollment_fte / 10^3) +
        I(stateEnroll_count / 10^3) +
        I(enrollment_fte / stateEnroll_count) +
        `I(stateappropriations_real/(1000 * enrollment_fte))(fit)` |
        state + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- reg.data %>%
    felm(full_prof_count ~ 1 +
        staterevenues_baseshare +
        acceptance_rate_2000s +
        completion_rate_150pct_2000s +
        I(tuitionrev_real / 10^6) +
        I(enrollment_fte / 10^3) +
        I(stateEnroll_count / 10^3) +
        I(enrollment_fte / stateEnroll_count) |
        state + year |
        (I(stateappropriations_real/(1000 * enrollment_fte)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- reg.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_fte))(fit)` =
        I(stateappropriations_real/(1000 * enrollment_fte))) %>%
    felm(all_prof_count ~ 1 +
        staterevenues_baseshare +
        acceptance_rate_2000s +
        completion_rate_150pct_2000s +
        I(tuitionrev_real / 10^6) +
        I(enrollment_fte / 10^3) +
        I(stateEnroll_count / 10^3) +
        I(enrollment_fte / stateEnroll_count) +
        `I(stateappropriations_real/(1000 * enrollment_fte))(fit)` |
        state + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- reg.data %>%
    felm(all_prof_count ~ 1 +
        staterevenues_baseshare +
        acceptance_rate_2000s +
        completion_rate_150pct_2000s +
        I(tuitionrev_real / 10^6) +
        I(enrollment_fte / 10^3) +
        I(stateEnroll_count / 10^3) +
        I(enrollment_fte / stateEnroll_count) |
        state + year |
        (I(stateappropriations_real/(1000 * enrollment_fte)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    reg.data %>% pull(lecturer_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% pull(lecturer_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_count.reg
    reg.data %>% pull(assistant_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% pull(assistant_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_count.reg
    reg.data %>% pull(full_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% pull(full_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_count.reg
    reg.data %>% pull(all_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% pull(all_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no)
))

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_count.reg, shiftshare_lecturer_count.reg,
    naive_assistant_count.reg, shiftshare_assistant_count.reg,
    naive_full_count.reg, shiftshare_full_count.reg,
    naive_all_count.reg, shiftshare_all_count.reg,
    add.lines = outcome.means,
    dep.var.caption =
        "Dependent Variable: Faculty Count per 1,000 Students, by Position",
    dep.var.labels = c(
        "Lecturers", "Asst. Professors", "Full Professors", "All Faculty"),
    column.labels = rep(c("OLS", "2SLS"), 4),
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    intercept.bottom = TRUE,
    order = c(8, 1:7),
    covariate.labels = X.labels[c(8, 1:7)],
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq"),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/facultycount-rawcount-robustness-checks.tex")


################################################################################
## Falsification test: effect among the private unis.

# Take the private universities, and assign them a base share of state funding to the average of public unis of their same Barron's selectivity ranking, and then use this as an instrument for state funding (or total funding) in the IV analysis.

# Get the data of all private unis in IPEDS
ipeds_private.data <- ipeds_all.data %>%
    filter(fouryear == 1, public == 0, forprofit == 0)  %>%
    # Create independent columns and rows.
    select(
        # Identifiers and controls
        unitid,
        inst_name,
        year,
        state,
        # Appropriations to the uni
        totalrevenues_real,
        stateappropriations_real,
        tuitionrev_real,
        # Uni spending.
        nonauxspending_real,
        # Uni enrollment
        enrollment_reported,
        enrollment_fte,
        acceptance_rate,
        completion_rate_150pct,
        # Shift-sahre shocks Per student
        appropriationshock_perEnroll_real,
        allstate_stateappropriations_real,
        staterevenues_baseshare,
        stateEnroll_count,
        # Count of full-time professors (tenured or not)
        lecturer_prof_count,
        assistant_prof_count,
        full_prof_count,
        all_prof_count,
        # Barron's ranking 2009
        barrons_rank_2009)

# Get the average abse share, by state-barrons-year, from public unis
base.data <- ipeds_public.data %>%
    filter(staterevenues_baseshare > 0) %>%
    group_by(state, year, barrons_rank_2009) %>%
    summarise(staterevenues_baseshare =
        mean(staterevenues_baseshare, na.rm = TRUE)) %>%
    ungroup()

# SHow how many univertities are in each ranking
ipeds_public.data %>%
    filter(staterevenues_baseshare > 0) %>%
    group_by(unitid, barrons_rank_2009) %>%
    summarise(stateappropriations_real =
        mean(stateappropriations_real, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(barrons_rank_2009) %>%
    summarise(count = n()) %>%
    print()

# Calculate the instrument for the private unis based on these base shares.
ipeds_private.data <- ipeds_private.data %>%
    select(-staterevenues_baseshare) %>%
    left_join(base.data, by = c("state", "year", "barrons_rank_2009")) %>%
    mutate(appropriationshock_perEnroll_real = staterevenues_baseshare * (
            allstate_stateappropriations_real / stateEnroll_count)) %>%
    filter(enrollment_reported > 0,
        totalrevenues_real > 0, appropriationshock_perEnroll_real > 0,
        lecturer_prof_count > 0,
        assistant_prof_count > 0,
        full_prof_count > 0,
        all_prof_count > 0)

# First stage test, which is referenced in the text (don't bother TeX table)
ipeds_private.data %>%
    felm(log(totalrevenues_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .) %>%
    summary()

### Faculty Count Regression
## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- ipeds_private.data %>%
    mutate(`log(totalrevenues_real/enrollment_reported)(fit)` =
        log(totalrevenues_real / enrollment_reported)) %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 +
        `log(totalrevenues_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- ipeds_private.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(totalrevenues_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_count.reg <- ipeds_private.data %>%
    mutate(`log(totalrevenues_real/enrollment_reported)(fit)` =
        log(totalrevenues_real / enrollment_reported)) %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 +
        `log(totalrevenues_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- ipeds_private.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(totalrevenues_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_count.reg <- ipeds_private.data %>%
    mutate(`log(totalrevenues_real/enrollment_reported)(fit)` =
        log(totalrevenues_real / enrollment_reported)) %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 +
        `log(totalrevenues_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- ipeds_private.data %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(totalrevenues_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- ipeds_private.data %>%
    mutate(`log(totalrevenues_real/enrollment_reported)(fit)` =
        log(totalrevenues_real / enrollment_reported)) %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 +
        `log(totalrevenues_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- ipeds_private.data %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(totalrevenues_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    ipeds_private.data %>% mutate(outcome = lecturer_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    ipeds_private.data %>% mutate(outcome = lecturer_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_count.reg
    ipeds_private.data %>% mutate(outcome = assistant_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    ipeds_private.data %>% mutate(outcome = assistant_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_count.reg
    ipeds_private.data %>% mutate(outcome = full_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    ipeds_private.data %>% mutate(outcome = full_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_count.reg
    ipeds_private.data %>% mutate(outcome = all_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    ipeds_private.data %>% mutate(outcome = all_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no)
))

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_count.reg, shiftshare_lecturer_count.reg,
    naive_assistant_count.reg, shiftshare_assistant_count.reg,
    naive_full_count.reg, shiftshare_full_count.reg,
    naive_all_count.reg, shiftshare_all_count.reg,
    add.lines = outcome.means,
    dep.var.caption =
        "Dependent Variable: Log Faculty Count per Students, by Position",
    dep.var.labels = c(
        "Lecturers", "Asst. Professors", "Full Professors", "All Faculty"),
    column.labels = rep(c("OLS", "2SLS"), 4),
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    omit = "factor|count|year",
    intercept.bottom = TRUE,
    order = c(2, 1, 3),
    covariate.labels = c("State Funding", "Tuition Revenue", "Constant"),
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq"),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/facultycount-shock-private-robustness.tex")


# Faculty Count Regressions, in raw counts -------------------------------------

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- ipeds_private.data %>%
    mutate(`I(totalrevenues_real/(1000 * enrollment_reported))(fit)` =
        I(totalrevenues_real/(1000 * enrollment_reported))) %>%
    felm(lecturer_prof_count ~ 1 +
        `I(totalrevenues_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- ipeds_private.data %>%
    felm(lecturer_prof_count ~ 1 |
        unitid + year |
        (I(totalrevenues_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_count.reg <- ipeds_private.data %>%
    mutate(`I(totalrevenues_real/(1000 * enrollment_reported))(fit)` =
        I(totalrevenues_real/(1000 * enrollment_reported))) %>%
    felm(assistant_prof_count ~ 1 +
        `I(totalrevenues_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- ipeds_private.data %>%
    felm(assistant_prof_count ~ 1 |
        unitid + year |
        (I(totalrevenues_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_count.reg <- ipeds_private.data %>%
    mutate(`I(totalrevenues_real/(1000 * enrollment_reported))(fit)` =
        I(totalrevenues_real/(1000 * enrollment_reported))) %>%
    felm(full_prof_count ~ 1 +
        `I(totalrevenues_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- ipeds_private.data %>%
    felm(full_prof_count ~ 1 |
        unitid + year |
        (I(totalrevenues_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- ipeds_private.data %>%
    mutate(`I(totalrevenues_real/(1000 * enrollment_reported))(fit)` =
        I(totalrevenues_real/(1000 * enrollment_reported))) %>%
    felm(all_prof_count ~ 1 +
        `I(totalrevenues_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- ipeds_private.data %>%
    felm(all_prof_count ~ 1 |
        unitid + year |
        (I(totalrevenues_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    ipeds_private.data %>% pull(lecturer_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    ipeds_private.data %>% pull(lecturer_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_count.reg
    ipeds_private.data %>% pull(assistant_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    ipeds_private.data %>% pull(assistant_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_count.reg
    ipeds_private.data %>% pull(full_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    ipeds_private.data %>% pull(full_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_count.reg
    ipeds_private.data %>% pull(all_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    ipeds_private.data %>% pull(all_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no)
))

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_count.reg, shiftshare_lecturer_count.reg,
    naive_assistant_count.reg, shiftshare_assistant_count.reg,
    naive_full_count.reg, shiftshare_full_count.reg,
    naive_all_count.reg, shiftshare_all_count.reg,
    add.lines = outcome.means,
    dep.var.caption =
        "Dependent Variable: Faculty Count per 1,000 Students, by Position",
    dep.var.labels = c(
        "Lecturers", "Asst. Professors", "Full Professors", "All Faculty"),
    column.labels = rep(c("OLS", "2SLS"), 4),
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    omit = "factor|count|year",
    intercept.bottom = TRUE,
    order = c(2, 1, 3),
    covariate.labels = c("State Funding", "Tuition Revenue", "Constant"),
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq"),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/facultycount-shock-private-robustness-rawcount.tex")


################################################################################
## Heterogeneity analysis, in log terms.

# Define an empty table to add to.
heterogeneity.table <- matrix("", nrow = 10, ncol = 7)

## Most selective institutions.
selective.data <- reg.data %>%
    filter(barrons_rank_2009 < 3)
# First-Stage
firststage_approp.reg <- selective.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Lecturer
shiftshare_lecturer_count.reg <- selective.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Asst. Prof Count
shiftshare_assistant_count.reg <- selective.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Full professor Count
shiftshare_full_count.reg <- selective.data %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# All faculty Count
shiftshare_all_count.reg <- selective.data %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Put results into the table.
heterogeneity.table[2, 1] <- "Most Selective:"
# First-stage
heterogeneity.table[2, 2] <- firststage_approp.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[3, 2] <- firststage_approp.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[4, 2] <- selective.data %>% mutate(outcome = stateappropriations_real / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Lecturers
heterogeneity.table[2, 3] <- shiftshare_lecturer_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[3, 3] <- shiftshare_lecturer_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[4, 3] <- selective.data %>% mutate(outcome = lecturer_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Asst. Professors
heterogeneity.table[2, 4] <- shiftshare_assistant_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[3, 4] <- shiftshare_assistant_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[4, 4] <- selective.data %>% mutate(outcome = assistant_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Full Professors
heterogeneity.table[2, 5] <- shiftshare_full_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[3, 5] <- shiftshare_full_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[4, 5] <- selective.data %>% mutate(outcome = full_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# All Faculty.
heterogeneity.table[2, 6] <- shiftshare_all_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[3, 6] <- shiftshare_all_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[4, 6] <- selective.data %>% mutate(outcome = all_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Observation count
heterogeneity.table[2, 7] <- selective.data %>% nrow() %>% as.character()

## Selective institutions.
selective.data <- reg.data %>%
    filter(barrons_rank_2009 %in% 3:4)
# First-Stage
firststage_approp.reg <- selective.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Lecturer
shiftshare_lecturer_count.reg <- selective.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Asst. Prof Count
shiftshare_assistant_count.reg <- selective.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Full professor Count
shiftshare_full_count.reg <- selective.data %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# All faculty Count
shiftshare_all_count.reg <- selective.data %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Put results into the table.
heterogeneity.table[5, 1] <- "Selective:"
# First-stage
heterogeneity.table[5, 2] <- firststage_approp.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[6, 2] <- firststage_approp.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[7, 2] <- selective.data %>% mutate(outcome = stateappropriations_real / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Lecturers
heterogeneity.table[5, 3] <- shiftshare_lecturer_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[6, 3] <- shiftshare_lecturer_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[7, 3] <- selective.data %>% mutate(outcome = lecturer_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Asst. Professors
heterogeneity.table[5, 4] <- shiftshare_assistant_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[6, 4] <- shiftshare_assistant_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[7, 4] <- selective.data %>% mutate(outcome = assistant_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Full Professors
heterogeneity.table[5, 5] <- shiftshare_full_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[6, 5] <- shiftshare_full_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[7, 5] <- selective.data %>% mutate(outcome = full_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# All Faculty.
heterogeneity.table[5, 6] <- shiftshare_all_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[6, 6] <- shiftshare_all_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[7, 6] <- selective.data %>% mutate(outcome = all_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Observation count
heterogeneity.table[5, 7] <- selective.data %>% nrow() %>% as.character()

## Unranked institutions.
selective.data <- reg.data %>%
    filter(barrons_rank_2009 > 4)
# First-Stage
firststage_approp.reg <- selective.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Lecturer
shiftshare_lecturer_count.reg <- selective.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Asst. Prof Count
shiftshare_assistant_count.reg <- selective.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Full professor Count
shiftshare_full_count.reg <- selective.data %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# All faculty Count
shiftshare_all_count.reg <- selective.data %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Put results into the table.
heterogeneity.table[8, 1] <- "Unranked:"
# First-stage
heterogeneity.table[8, 2] <- firststage_approp.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[9, 2] <- firststage_approp.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[10, 2] <- selective.data %>% mutate(outcome = stateappropriations_real / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Lecturers
heterogeneity.table[8, 3] <- shiftshare_lecturer_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[9, 3] <- shiftshare_lecturer_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[10, 3] <- selective.data %>% mutate(outcome = lecturer_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Asst. Professors
heterogeneity.table[8, 4] <- shiftshare_assistant_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[9, 4] <- shiftshare_assistant_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[10, 4] <- selective.data %>% mutate(outcome = assistant_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Full Professors
heterogeneity.table[8, 5] <- shiftshare_full_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[9, 5] <- shiftshare_full_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[10, 5] <- selective.data %>% mutate(outcome = full_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# All Faculty.
heterogeneity.table[8, 6] <- shiftshare_all_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[9, 6] <- shiftshare_all_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[10, 6] <- selective.data %>% mutate(outcome = all_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Observation count
heterogeneity.table[8, 7] <- selective.data %>% nrow() %>% as.character()

# Put brackets around the SE rows.
heterogeneity.table[3, 2:6] <- paste0("(", heterogeneity.table[3, 2:6], ")")
heterogeneity.table[6, 2:6] <- paste0("(", heterogeneity.table[6, 2:6], ")")
heterogeneity.table[9, 2:6] <- paste0("(", heterogeneity.table[9, 2:6], ")")
# Put square brackets around the outcome mean rows.
heterogeneity.table[4, 2:6] <- paste0("[", heterogeneity.table[4, 2:6], "]")
heterogeneity.table[7, 2:6] <- paste0("[", heterogeneity.table[7, 2:6], "]")
heterogeneity.table[10, 2:6] <- paste0("[", heterogeneity.table[10, 2:6], "]")


# Write the outcome to TeX.
library(xtable)
print(xtable(heterogeneity.table),
    include.colnames = FALSE,
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = NULL,
    file = "../../text/tables/facultycount-heterogeneity.tex")


################################################################################
## Heterogeneity analysis, in raw counts.

# Define an empty table to add to.
heterogeneity.table <- matrix("", nrow = 10, ncol = 7)

## Most selective institutions.
selective.data <- reg.data %>%
    filter(barrons_rank_2009 < 3)
# First-Stage
firststage_approp.reg <- selective.data %>%
    felm(I(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Lecturer
shiftshare_lecturer_count.reg <- selective.data %>%
    felm(lecturer_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)
# Asst. Prof Count
shiftshare_assistant_count.reg <- selective.data %>%
    felm(assistant_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)
# Full professor Count
shiftshare_full_count.reg <- selective.data %>%
    felm(full_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)
# All faculty Count
shiftshare_all_count.reg <- selective.data %>%
    felm(all_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)
# Put results into the table.
heterogeneity.table[2, 1] <- "Most Selective:"
# First-stage
heterogeneity.table[2, 2] <- firststage_approp.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[3, 2] <- firststage_approp.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[4, 2] <- selective.data %>% mutate(outcome = stateappropriations_real / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Lecturers
heterogeneity.table[2, 3] <- shiftshare_lecturer_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[3, 3] <- shiftshare_lecturer_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[4, 3] <- selective.data %>% mutate(outcome = lecturer_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Asst. Professors
heterogeneity.table[2, 4] <- shiftshare_assistant_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[3, 4] <- shiftshare_assistant_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[4, 4] <- selective.data %>% mutate(outcome = assistant_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Full Professors
heterogeneity.table[2, 5] <- shiftshare_full_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[3, 5] <- shiftshare_full_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[4, 5] <- selective.data %>% mutate(outcome = full_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# All Faculty.
heterogeneity.table[2, 6] <- shiftshare_all_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[3, 6] <- shiftshare_all_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[4, 6] <- selective.data %>% mutate(outcome = all_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Observation count
heterogeneity.table[2, 7] <- selective.data %>% nrow() %>% as.character()

## Selective institutions.
selective.data <- reg.data %>%
    filter(barrons_rank_2009 %in% 3:4)
# First-Stage
firststage_approp.reg <- selective.data %>%
    felm(I(stateappropriations_real / (1000 * enrollment_reported)) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Lecturer
shiftshare_lecturer_count.reg <- selective.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Asst. Prof Count
shiftshare_assistant_count.reg <- selective.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Full professor Count
shiftshare_full_count.reg <- selective.data %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# All faculty Count
shiftshare_all_count.reg <- selective.data %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Put results into the table.
heterogeneity.table[5, 1] <- "Selective:"
# First-stage
heterogeneity.table[5, 2] <- firststage_approp.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[6, 2] <- firststage_approp.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[7, 2] <- selective.data %>% mutate(outcome = stateappropriations_real / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Lecturers
heterogeneity.table[5, 3] <- shiftshare_lecturer_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[6, 3] <- shiftshare_lecturer_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[7, 3] <- selective.data %>% mutate(outcome = lecturer_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Asst. Professors
heterogeneity.table[5, 4] <- shiftshare_assistant_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[6, 4] <- shiftshare_assistant_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[7, 4] <- selective.data %>% mutate(outcome = assistant_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Full Professors
heterogeneity.table[5, 5] <- shiftshare_full_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[6, 5] <- shiftshare_full_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[7, 5] <- selective.data %>% mutate(outcome = full_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# All Faculty.
heterogeneity.table[5, 6] <- shiftshare_all_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[6, 6] <- shiftshare_all_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[7, 6] <- selective.data %>% mutate(outcome = all_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Observation count
heterogeneity.table[5, 7] <- selective.data %>% nrow() %>% as.character()

## Unranked institutions.
selective.data <- reg.data %>%
    filter(barrons_rank_2009 > 4)
# First-Stage
firststage_approp.reg <- selective.data %>%
    felm(I(stateappropriations_real / (1000 * enrollment_reported)) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Lecturer
shiftshare_lecturer_count.reg <- selective.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Asst. Prof Count
shiftshare_assistant_count.reg <- selective.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Full professor Count
shiftshare_full_count.reg <- selective.data %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# All faculty Count
shiftshare_all_count.reg <- selective.data %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
# Put results into the table.
heterogeneity.table[8, 1] <- "Unranked:"
# First-stage
heterogeneity.table[8, 2] <- firststage_approp.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[9, 2] <- firststage_approp.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[10, 2] <- selective.data %>% mutate(outcome = stateappropriations_real / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Lecturers
heterogeneity.table[8, 3] <- shiftshare_lecturer_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[9, 3] <- shiftshare_lecturer_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[10, 3] <- selective.data %>% mutate(outcome = lecturer_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Asst. Professors
heterogeneity.table[8, 4] <- shiftshare_assistant_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[9, 4] <- shiftshare_assistant_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[10, 4] <- selective.data %>% mutate(outcome = assistant_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Full Professors
heterogeneity.table[8, 5] <- shiftshare_full_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[9, 5] <- shiftshare_full_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[10, 5] <- selective.data %>% mutate(outcome = full_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# All Faculty.
heterogeneity.table[8, 6] <- shiftshare_all_count.reg$coefficients[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[9, 6] <- shiftshare_all_count.reg$se[1] %>% round(digits.no) %>% as.character()
heterogeneity.table[10, 6] <- selective.data %>% mutate(outcome = all_prof_count / enrollment_reported) %>% pull(outcome) %>% mean() %>% round(digits.no) %>% as.character()
# Observation count
heterogeneity.table[8, 7] <- selective.data %>% nrow() %>% as.character()

# Put brackets around the SE rows.
heterogeneity.table[3, 2:6] <- paste0("(", heterogeneity.table[3, 2:6], ")")
heterogeneity.table[6, 2:6] <- paste0("(", heterogeneity.table[6, 2:6], ")")
heterogeneity.table[9, 2:6] <- paste0("(", heterogeneity.table[9, 2:6], ")")
# Put square brackets around the outcome mean rows.
heterogeneity.table[4, 2:6] <- paste0("[", heterogeneity.table[4, 2:6], "]")
heterogeneity.table[7, 2:6] <- paste0("[", heterogeneity.table[7, 2:6], "]")
heterogeneity.table[10, 2:6] <- paste0("[", heterogeneity.table[10, 2:6], "]")


# Write the outcome to TeX.
library(xtable)
print(xtable(heterogeneity.table),
    include.colnames = FALSE,
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = NULL,
    file = "../../text/tables/facultycount-rawcounts-heterogeneity.tex")


################################################################################
## Scatter plots of the IV analysis.


# First-stage scatter.
firststage.plot <- reg.data %>%
    ggplot(aes(x = - appropriationshock_perEnroll_real / 1000,
        y = stateappropriations_real / (1000 * enrollment_reported))) +
    geom_jitter(alpha = 0.075) +
    geom_smooth(method = "lm",
        colour = colour.list[1], fill = colour.list[1]) +
    #annotate("text", colour = colour.list[1],
    #    x = 0, y = 17.72,
    #    label = expression("Slope" %~~% " 0.67 Edyears"),
    #    size = 4.25, fontface = "bold") +
    theme_bw() +
    scale_x_continuous(expand = c(0, 0),
        name = "Funding Shift-share") +
        #breaks = seq(-5, 5, by = 1)
    scale_y_continuous(expand = c(0, 0.1),
        name = "",
        limits = c(0, 100)) +
        #breaks = seq(0, 20, by = 1)
    ggtitle("State Funding") +
    theme(plot.title = element_text(size = rel(1), hjust = 0),
        plot.title.position = "plot",
        plot.margin = unit(c(0.5, 3, 0, 0), "mm"))
firststage.plot
