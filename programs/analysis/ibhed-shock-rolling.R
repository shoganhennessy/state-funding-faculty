#!/usr/bin/R
## Senan Hogan-Hennessy, 3 August 2022
## IV for Prof's outcomes, using Illinois data.
## Using a rolling-share variant of the instrument.
print(c(Sys.time(), Sys.Date()))
library(tidyverse) # Functions for data manipulation and visualization
library(lfe) # Functions for fast linear models with IV + FEs
library(plm) # Functions for panel data
library(car) # Function for F stat regarding IV models
# My custom flavour of Stargazer TeX tables:
# devtools::install_github("shoganhennessy/stargazer")
library(stargazer) # TeX tables
set.seed(47)
# This file follows an adjusted Deming Walters (2017, p.10) approach to
# appropriations shock instrument.

# Define number of digits in tables and graphs
digits.no <- 3

# Size for figures
fig.width <- 10
fig.height <- fig.width * 0.85


# Load data --------------------------------------------------------------------

# Data on Illinois Professors
illinois.data <- read_csv("../../data/states/illinois-professors-anonymised.csv")

# Data from IPEDS, with relevant variables
ipeds.data <- read_csv("../../data/urban-ipeds/urban-clean-publicunis.csv") %>%
    select(
        # Identifiers and controls
        unitid,
        year,
        state,
        # Appropriations to the uni
        totalrevenues_real,
        nonauxrevenues_real,
        stateappropriations_real,
        tuitionrev_real,
        # Uni spending.
        nonauxspending_real,
        # Uni enrollment
        enrollment_reported,
        enrollment_fte,
        # Appropriation shocks Per student
        appropriationshock_perFTE_real,
        appropriationshock_peruni_real,
        # State-level values
        allstate_stateappropriations_real,
        stateEnroll_count)


# Clean data -------------------------------------------------------------------

# Join IPEDS data to the individual level
reg.data <- illinois.data %>%
    # Join the IPEDS finance data.
    left_join(ipeds.data, by = c("unitid", "year"))

# Generate a Professor's first year
reg.data <- reg.data %>%
    arrange(name, year, unitid) %>%
    group_by(name, unitid) %>%
    mutate(firstyear = min(year, na.rm = TRUE)) %>%
    ungroup()

# Calculate the rolling shock to state appropriations (Lovenheim 2021 p. 13)
# and link it to the prof's first year

# Generate a Professor's first year
reg.data <- reg.data %>%
    arrange(name, year, unitid) %>%
    group_by(name, unitid) %>%
    mutate(firstyear = min(year, na.rm = TRUE)) %>%
    ungroup()
# Generate the rolling share, for a Professor's first year of employment.
reg.data <- ipeds.data %>%
    transmute(unitid = unitid,
        firstyear = year,
        staterevenues_rollshare =
            stateappropriations_real / totalrevenues_real) %>%
    right_join(reg.data, by = c("unitid", "firstyear")) %>%
    # Remove the rolling shock if the panel does not include first year for prof
    mutate(staterevenues_rollshare =
        ifelse(2010 < firstyear & firstyear < 2021,
            staterevenues_rollshare, NA)) %>%
    # Get the shock as a product of rolling share and yearly state approp
    mutate(appropriationshock_perEnroll_rolling =
        staterevenues_rollshare * (
            allstate_stateappropriations_real / stateEnroll_count))

# Remove IPEDS + Illinois data to save memory
rm(ipeds.data)
rm(illinois.data)
gc()

# How many individual professors in 2010 and 2021?
reg.data %>% filter(year == 2010) %>% count(unitid, name) %>% nrow() %>% print()
reg.data %>% filter(year == 2021) %>% count(unitid, name) %>% nrow() %>% print()

# Binary for professor position
reg.data <- reg.data %>%
    mutate(
        lecturer = as.integer(position %in% c("Instructor", "Lecturer")),
        assistant = as.integer(position == "Assistant Professor"),
        full = as.integer(position %in% c("Associate Professor", "Professor")),
        administrator = as.integer(position %in% c("Chancellor",
            "Other Administrator", "President", "Senior Officer",
            "Unit Director", "Vice Chancellor", "Vice President")),
        other = as.integer(position == "No Rank or Other"),
        ranked_position = ifelse(lecturer == 1, 1,
            ifelse(assistant == 1, 2,
                ifelse(position == "Associate Professor", 3,
                    ifelse(position == "Professor", 4,
                        ifelse(administrator == 1, 5, NA))))))

# Restrict to only instructional staff (i.e. professors) with rank
reg.data <- reg.data %>%
    filter(!is.na(enrollment_reported), enrollment_reported > 0,
        (lecturer + assistant + full + administrator) > 0)

# For Profs who are at multi-unis/jobs, take their main job (most salary)
reg.data <- reg.data %>%
    group_by(name, year) %>%
    mutate(main_job =
        as.integer(salary_real == max(salary_real, na.rm = TRUE))) %>%
    filter(main_job == 1) %>%
    ungroup()

# Remove duplicate observations
reg.data <- reg.data %>%
    distinct(name, unitid, year, .keep_all = TRUE)

# Get indicator for whether a professor next year.
year.max <- reg.data %>% pull(year) %>% max(na.rm = TRUE)
year.min <- reg.data %>% pull(year) %>% min(na.rm = TRUE)
reg.data <- reg.data %>%
    arrange(name, year, unitid) %>%
    group_by(name) %>%
    mutate(
        notemployed_nextyear = ifelse(year == year.max, NA,
            as.integer(is.na(dplyr::lead(salary_real, 1)))),
        hired_lastyear = ifelse(year == year.min, NA,
            as.integer(is.na(dplyr::lag(salary_real, 1)))),
        promoted = ifelse(year %in% c(year.min, year.max), NA,
            as.integer(dplyr::lag(ranked_position, 1) < ranked_position)),
        demoted = ifelse(year %in% c(year.min, year.max), NA,
            as.integer(dplyr::lag(ranked_position, 1) > ranked_position))) %>%
    ungroup()

# How many individual professors in 2010 and 2021?
reg.data %>% filter(year == 2010) %>% count(unitid, name) %>% nrow() %>% print()
reg.data %>% filter(year == 2021) %>% count(unitid, name) %>% nrow() %>% print()

# Select non-missing values
reg.data <- reg.data %>%
    mutate(extra_salary_real = ifelse(is.na(extra_salary_real), 0,
        extra_salary_real)) %>%
    # Only unis + years with measured state appropriations & shocks & Prof count
    filter(!is.na(salary_real), salary_real > 0,
        !is.na(enrollment_reported), enrollment_reported > 0,
        !is.na(stateappropriations_real),
        !is.na(appropriationshock_perEnroll_rolling))

# How many individual professors in 2010 and 2021,
# who were hired in 2011 and later.
reg.data %>% filter(year == 2011) %>% count(unitid, name) %>% nrow() %>% print()
reg.data %>% filter(year == 2021) %>% count(unitid, name) %>% nrow() %>% print()

# Factor out the unitid, name designations for FELM functionality
reg.data <- reg.data %>%
    mutate(unitid = factor(unitid),
        name = factor(name))


# Summary Table ----------------------------------------------------------------

# Generate the TeX summary table.
reg.data %>%
    transmute(
        lecturer = 100 * lecturer,
        assistant = 100 * assistant,
        full = 100 * full,
        administrator = 100 * administrator,
        lecturer_salary_real = ifelse(lecturer == 100, 1, NA) * salary_real,
        assistant_salary_real = ifelse(assistant == 100, 1, NA) * salary_real,
        full_salary_real = ifelse(full == 100, 1, NA) * salary_real,
        admin_salary_real = ifelse(administrator == 100, 1, NA) * salary_real,
        salary_real = salary_real,
        lecturer_extra_salary_real = ifelse(lecturer == 100, 1, NA) * extra_salary_real,
        assistant_extra_salary_real = ifelse(assistant == 100, 1, NA) * extra_salary_real,
        full_extra_salary_real = ifelse(full == 100, 1, NA) * extra_salary_real,
        admin_extra_salary_real = ifelse(administrator == 100, 1, NA) * extra_salary_real,
        extra_salary_real = extra_salary_real) %>%
    as.data.frame() %>%
    stargazer(summary = TRUE,
        summary.stat = c("mean", "sd", "n"),
        digits = 0,
        digits.extra = 0,
        covariate.labels = c(
            "Lecturer, percent",
            "Assistant professor, percent",
            "Full professor, percent",
            "Administrator professor, percent",
            "Lecturer salary (2021 USD)",
            "Assistant salary (2021 USD)",
            "Full salary (2021 USD)",
            "Administrator salary (2021 USD)",
            "All salary (2021 USD)",
            "Lecturer benefits (2021 USD)",
            "Assistant benefits (2021 USD)",
            "Full benefits (2021 USD)",
            "Administrator benefits (2021 USD)",
            "All benefits (2021 USD)"),
        omit.table.layout = "n",
        header = FALSE, float = FALSE, no.space = TRUE,
        type = "text",
        out = "../../text/tables/illinois-summary-rolling.tex")


# First stage Regressions -----------------------------------------------------

# Explain Revenues with a shock to (only) state appropriations.
firststage_approp.reg <- reg.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_rolling)) |
        unitid + firstyear |
        0 |
        unitid + firstyear,
        data = .)
# Get the F.Stat
firststage_approp.fstat <-
    linearHypothesis(firststage_approp.reg, test = "F",
        c("I(-log(appropriationshock_perEnroll_rolling))=0"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Without the FEs
# Explain State appropriations with a shock to (only) state appropriations.
firststage_approp_noFE.reg <- reg.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_rolling)) |
        0 |
        0 |
        unitid + firstyear,
        data = .)
# Get the F.Stat
firststage_approp_noFE.fstat <-
    linearHypothesis(firststage_approp_noFE.reg, test = "F",
        c("I(-log(appropriationshock_perEnroll_rolling))=0"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Explain Revenues with a shock to (only) state appropriations.
firststage_approp_tuit.reg <- reg.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_rolling)) +
        log(tuitionrev_real / enrollment_reported) |
        unitid + firstyear |
        0 |
        unitid + firstyear,
        data = .)
# Get the F.Stat
firststage_approp_tuit.fstat <-
    linearHypothesis(firststage_approp_tuit.reg, test = "F",
        c("I(-log(appropriationshock_perEnroll_rolling))=0"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Without the FEs
# Explain State appropriations with a shock to (only) state appropriations.
firststage_approp_tuit_noFE.reg <- reg.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_rolling)) +
        log(tuitionrev_real / enrollment_reported) |
        0 |
        0 |
        unitid + firstyear,
        data = .)
# Get the F.Stat
firststage_approp_tuit_noFE.fstat <-
    linearHypothesis(firststage_approp_tuit_noFE.reg, test = "F",
        c("I(-log(appropriationshock_perEnroll_rolling))=0"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Collate the results to a LaTeX table
stargazer(
    firststage_approp_tuit.reg, firststage_approp_tuit_noFE.reg,
    firststage_approp.reg, firststage_approp_noFE.reg,
    dep.var.caption = "Dependent Variable: State Funding",
    dep.var.labels.include = FALSE,
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    omit = "factor|count|year",
    intercept.bottom = TRUE,
    covariate.labels = c("State Funding", "Tuition Revenue", "Constant"),
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq", "f"),
    add.lines = list(
        c("Fixed effects?", "Yes", "No", "Yes", "No"),
        c("F stat.",
            firststage_approp_tuit.fstat, firststage_approp_tuit_noFE.fstat,
            firststage_approp.fstat, firststage_approp_noFE.fstat)),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/firststage-illinois-rolling.tex")


# Faculty Salary Regressions ---------------------------------------------------

## Non-full faculty salaries
lecturer.data <- reg.data %>%
    filter(lecturer == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_salaries.reg <- lecturer.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## Tenure-track faculty salaries
assistant.data <- reg.data %>%
    filter(assistant == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_salaries.reg <- assistant.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## full faculty salaries
full.data <- reg.data %>%
    filter(full == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_salaries.reg <- full.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## Administrator faculty salaries
administrator.data <- reg.data %>%
    filter(administrator == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_administrator_salaries.reg <- administrator.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## All faculty Salaries
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_salaries.reg <- reg.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## Collate the results to a LaTeX table
stargazer(
    shiftshare_lecturer_salaries.reg,
    shiftshare_assistant_salaries.reg,
    shiftshare_full_salaries.reg,
    shiftshare_administrator_salaries.reg,
    shiftshare_all_salaries.reg,
    dep.var.caption  = "Dependent Variable: Salaries by Professor Group",
    dep.var.labels.include = FALSE,
    column.labels = c("Lecturer", "Assistant", "Full", "Admin", "All"),
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
    out = "../../text/tables/facultysalaries-shock-illinois-rolling.tex")


# Faculty Salary Regressions among first-years ---------------------------------

# Repeat analysis among the sample of new-hires, in their first year
newhire.data <- reg.data %>%
    filter(firstyear == year, year > 2010)

## Non-full faculty salaries
lecturer.data <- newhire.data %>%
    filter(lecturer == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_salaries.reg <- lecturer.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## Tenure-track faculty salaries
assistant.data <- newhire.data %>%
    filter(assistant == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_salaries.reg <- assistant.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## full faculty salaries
full.data <- newhire.data %>%
    filter(full == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_salaries.reg <- full.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## Administrator faculty salaries
administrator.data <- newhire.data %>%
    filter(administrator == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_administrator_salaries.reg <- administrator.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## All faculty Salaries
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_salaries.reg <- newhire.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## Collate the results to a LaTeX table
stargazer(
    shiftshare_lecturer_salaries.reg,
    shiftshare_assistant_salaries.reg,
    shiftshare_full_salaries.reg,
    shiftshare_administrator_salaries.reg,
    shiftshare_all_salaries.reg,
    dep.var.caption  = "Dependent Variable: Salaries by Professor Group",
    dep.var.labels.include = FALSE,
    column.labels = c("Lecturer", "Assistant", "Full", "Admin", "All"),
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
    out = "../../text/tables/newhiresalaries-shock-illinois-rolling.tex")


# Faculty Promotion rates among new Faculty ------------------------------------

# Generate indicator for whether the individual is ever observed as the position
reg.data <- reg.data %>%
    mutate(associate = as.integer(position %in% c("Associate Professor"))) %>%
    group_by(name, unitid) %>%
    mutate(
        ever_lecturer  = max(lecturer,  na.rm = TRUE),
        ever_assistant = max(assistant, na.rm = TRUE),
        ever_associate = max(associate, na.rm = TRUE)) %>%
    ungroup()

## Non-full faculty promotion
lecturer.data <- reg.data %>%
    filter(ever_lecturer == 1, lecturer + assistant > 0)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_promotion.reg <- lecturer.data %>%
    felm(promoted ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

print("How many lecturers ever go from lecturer to AP?")
lecturer.data %>%
    filter(ever_assistant == 1) %>%
    distinct(name, .keep_all = TRUE) %>%
    nrow() %>%
    print()
print("How many lecturers are there in total?")
lecturer.data %>% distinct(name, .keep_all = TRUE) %>% nrow() %>% print()

## Tenure-track faculty promotion
assistant.data <- reg.data %>%
    filter(ever_assistant == 1, assistant + associate > 0)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_promotion.reg <- assistant.data %>%
    felm(promoted ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## full faculty promotion
associate.data <- reg.data %>%
    filter(ever_associate == 1, full > 0)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_promotion.reg <- associate.data %>%
    felm(promoted ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## All faculty promotion
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_promotion.reg <- reg.data %>%
    felm(promoted ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## Collate the results to a LaTeX table
stargazer(
    shiftshare_lecturer_promotion.reg,
    shiftshare_assistant_promotion.reg,
    shiftshare_full_promotion.reg,
    shiftshare_all_promotion.reg,
    dep.var.caption  = "Dependent Variable: Promotion Rate by Professor Group",
    dep.var.labels.include = FALSE,
    column.labels = c("Lecturer", "Assistant", "Associate", "All"),
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
    out = "../../text/tables/promotion-shock-illinois-rolling.tex")


# Faculty exit-rate Regressions ------------------------------------------------

## Non-full faculty salaries
lecturer.data <- reg.data %>%
    filter(lecturer == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_exit.reg <- lecturer.data %>%
    felm(notemployed_nextyear ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## Tenure-track faculty salaries
assistant.data <- reg.data %>%
    filter(assistant == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_exit.reg <- assistant.data %>%
    felm(notemployed_nextyear ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## full faculty salaries
full.data <- reg.data %>%
    filter(full == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_exit.reg <- full.data %>%
    felm(notemployed_nextyear ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## Administrator faculty salaries
administrator.data <- reg.data %>%
    filter(administrator == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_administrator_exit.reg <- administrator.data %>%
    felm(notemployed_nextyear ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## All faculty Salaries
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_exit.reg <- reg.data %>%
    felm(notemployed_nextyear ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_rolling))) |
        unitid + firstyear,
        data = .)

## Collate the results to a LaTeX table
stargazer(
    shiftshare_lecturer_exit.reg,
    shiftshare_assistant_exit.reg,
    shiftshare_full_exit.reg,
    shiftshare_administrator_exit.reg,
    shiftshare_all_exit.reg,
    dep.var.caption  = "Dependent Variable: Exit rate by Professor Group",
    dep.var.labels.include = FALSE,
    column.labels = c("Lecturer", "Assistant", "Full", "Admin", "All"),
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
    out = "../../text/tables/facultyleaving-shock-illinois-rolling.tex")


# Local Projections for staying-power of effects -------------------------------
# install.packages("lpirfs")
# https://cran.r-project.org/web/packages/lpirfs/lpirfs.pdf
library(plm)
library(lpirfs)
time.horizon <- 5

# Define data sample for the LP estimation
lp.data <- reg.data %>%
    transmute(
        name_unitid = paste(name, unitid),
        year = year,
        unitid = unitid,
        firstyear = factor(firstyear),
        instid = factor(unitid),
        academicyear = factor(year),
        salary_real = log(salary_real + extra_salary_real),
        promoted = promoted,
        stateappropriations_real =
            log(stateappropriations_real / enrollment_reported),
        appropriationshock_perEnroll_rolling =
            - log(appropriationshock_perEnroll_rolling),
        tuitionrev_real = log(tuitionrev_real / enrollment_reported)) %>%
    pdata.frame(index = c("name_unitid", "year"), drop.index = FALSE)

# Run the LP method for the first-stage regression.
firststage.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "stateappropriations_real",
        # Predictor variable
        shock = "appropriationshock_perEnroll_rolling",
        # Contemporaneous control, plus FE for unitid * firstyear
        c_exog_data = c("instid", "firstyear"),
        # Option to use IV for predictor endogeneity (not used here)
        iv_reg = FALSE,
        # Add clustered SEs in the panel
        panel_model = "pooling",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/firststage-illinois-lp-rolling.png",
    plot = plot(firststage.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# Run the LP estimation for salaries among all profs.
all_salaries.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "salary_real",
        # Predictor variable
        shock = "stateappropriations_real",
        # Contemporaneous control, plus FE for unitid + firstyear
        c_exog_data = c("instid", "firstyear"),
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_rolling",
        # Add clustered SEs in the panel
        panel_model = "pooling",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/salaries-illinois-lp-rolling.png",
    plot = plot(all_salaries.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# Run the LP estimation for promotion rate among all profs.
all_promoted.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "promoted",
        # Predictor variable
        shock = "stateappropriations_real",
        # Contemporaneous control, plus FE for unitid + firstyear
        c_exog_data = c("instid", "firstyear"),
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_rolling",
        # Add clustered SEs in the panel
        panel_model = "pooling",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/promoted-illinois-lp-rolling.png",
    plot = plot(all_promoted.lpreg),
    units = "cm", width = fig.width, height = fig.height)
