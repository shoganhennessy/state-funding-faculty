#!/usr/bin/R
## Senan Hogan-Hennessy, 3 August 2023
## IV for faculty counts, using IPEDS data.
# Replicate a separate specification, and show 2011 base-share si correlated with
# follow-on state funding cuts (i.e., share + shift correlated)
# Whereas the 1990--1993 base-share are not.
print(Sys.time())
library(tidyverse) # Functions for data manipulation and visualization
library(lfe) # Functions for fast linear models with IV + FEs
library(car) # Function for F stat regarding IV models
# My custom flavour of Stargazer TeX tables, to avoid a bug:
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


################################################################################
## Load and standardised the IPEDS data.

# Load IPEDS data
ipeds.data <- read_csv("../../data/urban-ipeds/urban-clean-publicunis.csv")

# Select IPEDS variables, and define the panel at the university level
reg.data <- ipeds.data %>%
    # Create independent columns and rows.
    select(
        # Identifiers and controls
        unitid,
        year,
        state,
        # Appropriations to the uni
        totalrevenues_real,
        nonauxrevenues_real,
        fedappropriations_real,
        stateappropriations_real,
        localappropriations_real,
        statelocal_funding_real,
        tuitionrev_real,
        # Uni spending.
        nonauxspending_real,
        # Uni enrollment
        enrollment_reported,
        enrollment_fte,
        acceptance_rate,
        completion_rate_150pct,
        # Shift-share shocks Per student
        appropriationshock_perEnroll_real,
        allstate_stateappropriations_real,
        staterevenues_baseshare,
        stateEnroll_count,
        # Count of full-time professors (tenured or not)
        lecturer_prof_count,
        assistant_prof_count,
        full_prof_count,
        all_prof_count,
        # Research and instruction and other expenditures.
        instructionspending_total_real,
        instructionspending_salaries_real,
        researchspending_total_real,
        researchspending_salaries_real,
        exp_pub_serv_real,
        exp_student_serv_real,
        exp_acad_supp_real,
        exp_inst_supp_real,
        exp_opm_areal,
        exp_net_grant_aid_real,
        # Incomplete counts of full+ part time professors.
        lecturer_parttime_count,
        assistant_parttime_count,
        full_parttime_count,
        lecturer_fulltime_count,
        assistant_fulltime_count,
        full_fulltime_count,
        # Exploratory, areas of spending.
        exp_instruc_real,
        exp_research_real,
        exp_pub_serv_real,
        exp_student_serv_real,
        exp_acad_supp_real,
        exp_inst_supp_real,
        exp_opm_areal,
        exp_net_grant_aid_real)

# Restrict data to unis + years with measured state appropriations & shocks
reg.data <- reg.data %>%
    filter(1990 <= year, #year != 1998,
        !is.na(enrollment_reported), enrollment_reported > 0,
        !is.na(all_prof_count),
        stateappropriations_real > 0,
        !is.na(appropriationshock_perEnroll_real)) %>%
    # Restrict to uni-years with all professor counts.
    filter(!is.na(lecturer_prof_count), lecturer_prof_count > 0) %>%
    filter(!is.na(assistant_prof_count), assistant_prof_count > 0) %>%
    filter(!is.na(full_prof_count), full_prof_count > 0)


# Free up RAM by removing initially loaded files, and manually release it by gc.
rm(ipeds.data)
gc()


################################################################################
## Calculate 2011 based shift share.

# Calculate the shift-share with a base in 2011.
base2011.data <- reg.data %>%
    filter(year == 2011) %>%
    group_by(unitid) %>%
    summarise(baseshare2011 =
        mean(stateappropriations_real / nonauxrevenues_real, na.rm = TRUE)) %>%
    ungroup()

# Add the 2011 base shift share to the 2012-2019 IPEDS data.
replicate.data <- reg.data %>%
    filter(2012 <= year, year <= 2019) %>%
    left_join(base2011.data, by = "unitid")

# Calculate the shift-share as base \times shift
replicate.data <- replicate.data %>%
    mutate(
        shiftshare2011 = (
            allstate_stateappropriations_real * baseshare2011))

# Get a summary table, to compare data sources.
replicate.data %>%
    transmute(
        enrollment_reported = enrollment_reported,
        stateappropriations_real = stateappropriations_real / (10^6),
        totalrevenues_real = totalrevenues_real / (10^6),
        nonauxrevenues_real = nonauxrevenues_real / (10^6),
        lecturer_prof_count = lecturer_prof_count,
        assistant_prof_count = assistant_prof_count,
        full_prof_count = full_prof_count,
        all_prof_count = all_prof_count) %>%
    as.data.frame() %>%
    stargazer(summary = TRUE,
        summary.stat = c("mean", "sd", "n"),
        digits = 1,
        digits.extra = 0,
        covariate.labels = c(
            "Enrolment",
            "State Funding (millions 2021 USD)",
            "Total revenues (millions 2021 USD)",
            "Non-institutional revenues (millions 2021 USD)",
            "Lecturers count",
            "Assistant professors count",
            "Full professors count",
            "All faculty count"),
        omit.table.layout = "n",
        header = FALSE, float = FALSE, no.space = TRUE,
        type = "text",
        out = "../../text/tables/ipeds-summary-replicate.tex")


################################################################################
## Replicate the other specification, using 2011--2019 data.

## First-stage results
# Explain Revenues with a shock to (only) state appropriations.
firststage_approp2011.reg <- replicate.data %>%
    felm(stateappropriations_real ~ 1 + shiftshare2011 |
        unitid + year |
        0 |
        state + year,
        data = .)
# Get the F.Stat
firststage_approp2011.fstat <-
    linearHypothesis(firststage_approp2011.reg, test = "F",
        "shiftshare2011=0")["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Without the FEs
# Explain State appropriations with a shock to (only) state appropriations.
firststage_approp2011_noFE.reg <- replicate.data %>%
    felm(stateappropriations_real ~ 1 + shiftshare2011 |
        0 |
        0 |
        state + year,
        data = .)
# Get the F.Stat
firststage_approp2011_noFE.fstat <-
    linearHypothesis(firststage_approp2011_noFE.reg, test = "F",
        "shiftshare2011=0")["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Collate the results to a LaTeX table
stargazer(
    firststage_approp2011_noFE.reg, firststage_approp2011.reg,
    dep.var.caption = "Dependent Variable: State Funding",
    dep.var.labels.include = FALSE,
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    omit = "factor|count|year",
    intercept.bottom = TRUE,
    covariate.labels = c("Shift-Share, base 2011", "Constant"),
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq", "f"),
    add.lines = list(
        c("Uni. + Year fixed effects?", "No", "Yes"),
        c("F stat.",
            firststage_approp2011_noFE.fstat, firststage_approp2011.fstat)),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/firststage-replicate.tex")


################################################################################
##  Faculty Count Regressions, with 2011 base + no adjustment for student count.

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- replicate.data %>%
    mutate(`stateappropriations_real(fit)` = stateappropriations_real / 10^6) %>%
    felm(I(lecturer_prof_count) ~ 1 + `stateappropriations_real(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- replicate.data %>%
    mutate(stateappropriations_real = stateappropriations_real / 10^6) %>%
    felm(I(lecturer_prof_count) ~ 1 |
        unitid + year |
        (stateappropriations_real ~ shiftshare2011) |
        state + year,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_count.reg <- replicate.data %>%
    mutate(`stateappropriations_real(fit)` = stateappropriations_real / 10^6) %>%
    felm(I(assistant_prof_count) ~ 1 + `stateappropriations_real(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- replicate.data %>%
    mutate(stateappropriations_real = stateappropriations_real / 10^6) %>%
    felm(I(assistant_prof_count) ~ 1 |
        unitid + year |
        (stateappropriations_real ~ shiftshare2011) |
        state + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_count.reg <- replicate.data %>%
    mutate(`stateappropriations_real(fit)` = stateappropriations_real / 10^6) %>%
    felm(I(full_prof_count) ~ 1 + `stateappropriations_real(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- replicate.data %>%
    mutate(stateappropriations_real = stateappropriations_real / 10^6) %>%
    felm(I(full_prof_count) ~ 1 |
        unitid + year |
        (stateappropriations_real ~ shiftshare2011) |
        state + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- replicate.data %>%
    mutate(`stateappropriations_real(fit)` = stateappropriations_real / 10^6) %>%
    felm(I(all_prof_count) ~ 1 + `stateappropriations_real(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- replicate.data %>%
    mutate(stateappropriations_real = stateappropriations_real / 10^6) %>%
    felm(I(all_prof_count) ~ 1 |
        unitid + year |
        (stateappropriations_real ~ shiftshare2011) |
        state + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    replicate.data %>% mutate(outcome = lecturer_prof_count) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate.data %>% mutate(outcome = lecturer_prof_count) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_count.reg
    replicate.data %>% mutate(outcome = assistant_prof_count) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate.data %>% mutate(outcome = assistant_prof_count) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_count.reg
    replicate.data %>% mutate(outcome = full_prof_count) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate.data %>% mutate(outcome = full_prof_count) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_count.reg
    replicate.data %>% mutate(outcome = all_prof_count) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate.data %>% mutate(outcome = all_prof_count) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no)
))

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_count.reg, shiftshare_lecturer_count.reg,
    naive_assistant_count.reg, shiftshare_assistant_count.reg,
    naive_full_count.reg, shiftshare_full_count.reg,
    naive_all_count.reg, shiftshare_all_count.reg,
    add.lines = outcome.means,
    dep.var.caption =
        "Dependent Variable: Faculty Count, by Position",
    dep.var.labels = c(
        "Lecturers", "Asst. Professors", "Professors", "All Faculty"),
    column.labels = rep(c("OLS", "2SLS"), 4),
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    omit = "factor|count|year",
    intercept.bottom = TRUE,
    order = c(2, 1, 3),
    covariate.labels = c("State Funding", "Constant"),
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq"),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/facultycount-replicate.tex")


################################################################################
##  Faculty Count Regressions, with my specification in 1990s.

# Get a copy of the data, restricting to 1990s + early 2000s
replicate1990s.data <- reg.data %>%
    filter(1990 <= year & year < 2010)

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- replicate1990s.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- replicate1990s.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_count.reg <- replicate1990s.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- replicate1990s.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_count.reg <- replicate1990s.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- replicate1990s.data %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- replicate1990s.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- replicate1990s.data %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    replicate1990s.data %>% mutate(outcome = lecturer_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate1990s.data %>% mutate(outcome = lecturer_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_count.reg
    replicate1990s.data %>% mutate(outcome = assistant_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate1990s.data %>% mutate(outcome = assistant_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_count.reg
    replicate1990s.data %>% mutate(outcome = full_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate1990s.data %>% mutate(outcome = full_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_count.reg
    replicate1990s.data %>% mutate(outcome = all_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate1990s.data %>% mutate(outcome = all_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no)
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
        "Lecturers", "Asst. Professors", "Professors", "All Faculty"),
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
    out = "../../text/tables/facultycount-replicated-corrected-1990s.tex")


################################################################################
##  Faculty Count Regressions, with my specification in 2010s.

# Get a copy of the data, restricting to 2010s
replicate2010s.data <- reg.data %>%
    filter(2009 < year)

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- replicate2010s.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- replicate2010s.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_count.reg <- replicate2010s.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- replicate2010s.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_count.reg <- replicate2010s.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- replicate2010s.data %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- replicate2010s.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- replicate2010s.data %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    replicate2010s.data %>% mutate(outcome = lecturer_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate2010s.data %>% mutate(outcome = lecturer_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_count.reg
    replicate2010s.data %>% mutate(outcome = assistant_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate2010s.data %>% mutate(outcome = assistant_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_count.reg
    replicate2010s.data %>% mutate(outcome = full_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate2010s.data %>% mutate(outcome = full_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_count.reg
    replicate2010s.data %>% mutate(outcome = all_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    replicate2010s.data %>% mutate(outcome = all_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no)
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
        "Lecturers", "Asst. Professors", "Professors", "All Faculty"),
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
    out = "../../text/tables/facultycount-replicated-corrected-2010s.tex")
