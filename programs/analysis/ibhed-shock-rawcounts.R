#!/usr/bin/R
## Senan Hogan-Hennessy, 3 August 2022
## IV for Prof's outcomes, using Illinois data.
## Using a base-share version of the instrument.
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

# Define default colours.
colour.list <- c(
    "#D62728", # Red  -> First-stage.
    "#1f77b4", # Blue -> Lecturers
    "#c9721c", # Orange -> Asst Professors
    "#237e23", # Green -> Full Professors
    "#cc2c9f") # Strong pink -> All faculty.


# Load data --------------------------------------------------------------------

# Data on Illinois Professors
illinois.data <-
    read_csv("../../data/states/illinois-professors-anonymised.csv")

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
        appropriationshock_perEnroll_real,
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

# Remove IPEDS + Illinois data to save memory
rm(ipeds.data)
rm(illinois.data)
gc()

# Calculate the first and final years in the panel of professors
year.min <- reg.data %>% pull(year) %>% min(na.rm = TRUE)
year.max <- reg.data %>% pull(year) %>% max(na.rm = TRUE)

# Generate a Professor's first + last years of employment
reg.data <- reg.data %>%
    arrange(name, year, unitid) %>%
    group_by(name, unitid) %>%
    mutate(
        firstyear = min(year, na.rm = TRUE),
        lastyear = max(year, na.rm = TRUE)) %>%
    ungroup() %>%
    # If the first year is the start of the panel, replace with missing
    mutate(firstyear = ifelse(firstyear == year.min, NA, firstyear)) %>%
    # If the last year is the end of the panel, replace with missing
    mutate(lastyear = ifelse(lastyear == year.max, NA, lastyear))

# How many individual professors in 2010 and 2021?
reg.data %>% filter(year == year.min) %>% count(unitid, name) %>% nrow() %>% print()
reg.data %>% filter(year == year.max) %>% count(unitid, name) %>% nrow() %>% print()

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

# Generate an indicator for whether still a professor next year, excluding
# end points of the panel data series.
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

# Select non-missing values
reg.data <- reg.data %>%
    mutate(extra_salary_real = ifelse(is.na(extra_salary_real), 0,
        extra_salary_real)) %>%
    # Only unis + years with measured state appropriations & shocks & Prof count
    filter(!is.na(salary_real), salary_real > 0,
        !is.na(enrollment_reported), enrollment_reported > 0,
        !is.na(stateappropriations_real), stateappropriations_real > 0,
        !is.na(appropriationshock_perEnroll_real),
            appropriationshock_perEnroll_real > 0)

# Factor out the unitid, name designations for FELM functionality
reg.data <- reg.data %>%
    mutate(
        unitid = factor(unitid),
        name = factor(name))

# Calculate the institutional level info on faculty composition.
institutional.data <- reg.data %>%
    group_by(inst_name, unitid, year) %>%
    summarise(
        # The uni's state funding that year
        stateappropriations_real = mean(stateappropriations_real, na.rm = TRUE),
        # The uni's state funding shock that year
        appropriationshock_perEnroll_real =
            mean(appropriationshock_perEnroll_real, na.rm = TRUE),
        # The uni's enrolment that year
        enrollment_reported = mean(enrollment_reported, na.rm = TRUE),
        # Professor count, by position
        lecturer_prof_count = sum(lecturer, na.rm = TRUE),
        assistant_prof_count = sum(assistant, na.rm = TRUE),
        full_prof_count = sum(full, na.rm = TRUE),
        admin_prof_count = sum(administrator, na.rm = TRUE),
        all_prof_count = n(),
        # Professor hiring, by position
        lecturer_prof_hires =
            sum(lecturer * as.integer(firstyear == year), na.rm = TRUE),
        assistant_prof_hires =
            sum(assistant * as.integer(firstyear == year), na.rm = TRUE),
        full_prof_hires =
            sum(full * as.integer(firstyear == year), na.rm = TRUE),
        admin_prof_hires =
            sum(administrator * as.integer(firstyear == year), na.rm = TRUE),
        all_prof_hires = sum(as.integer(firstyear == year), na.rm = TRUE),
        # Professor leaves, by position
        lecturer_prof_leaves =
            sum(lecturer * as.integer(lastyear == year), na.rm = TRUE),
        assistant_prof_leaves =
            sum(assistant * as.integer(lastyear == year), na.rm = TRUE),
        full_prof_leaves =
            sum(full * as.integer(lastyear == year), na.rm = TRUE),
        admin_prof_leaves =
            sum(administrator * as.integer(lastyear == year), na.rm = TRUE),
        all_prof_leaves = sum(as.integer(lastyear == year), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
        # Replace the hires values in the first year as missing.
        lecturer_prof_hires = ifelse(year == year.min, NA, lecturer_prof_hires),
        assistant_prof_hires = ifelse(year == year.min, NA, assistant_prof_hires),
        full_prof_hires = ifelse(year == year.min, NA, full_prof_hires),
        admin_prof_hires = ifelse(year == year.min, NA, admin_prof_hires),
        all_prof_hires = ifelse(year == year.min, NA, all_prof_hires),
        # Replace the leaves values in the last year as missing.
        lecturer_prof_leaves = ifelse(year == year.max, NA, lecturer_prof_hires),
        assistant_prof_leaves = ifelse(year == year.max, NA, assistant_prof_hires),
        full_prof_leaves = ifelse(year == year.max, NA, full_prof_hires),
        admin_prof_leaves = ifelse(year == year.max, NA, admin_prof_hires),
        all_prof_leaves = ifelse(year == year.max, NA, all_prof_hires))



# Illinois Faculty Composition Regressions -------------------------------------

# RAw first stage
firststage_raw.reg <- institutional.data %>%
    felm(I(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Get the F.Stat
firststage_raw.fstat <-
    linearHypothesis(firststage_raw.reg, test = "F",
        c("I(-appropriationshock_perEnroll_real)"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)
# Show that the first stage, and its strength
summary(firststage_raw.reg)

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- institutional.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        stateappropriations_real / (1000 * enrollment_reported)) %>%
    felm(lecturer_prof_count ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- institutional.data %>%
    felm(lecturer_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_count.reg <- institutional.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        stateappropriations_real / (1000 * enrollment_reported)) %>%
    felm(assistant_prof_count ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- institutional.data %>%
    felm(assistant_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_count.reg <- institutional.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        stateappropriations_real / (1000 * enrollment_reported)) %>%
    felm(full_prof_count ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- institutional.data %>%
    felm(full_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- institutional.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        stateappropriations_real / (1000 * enrollment_reported)) %>%
    felm(all_prof_count ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- institutional.data %>%
    felm(all_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    institutional.data %>% pull(lecturer_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% pull(lecturer_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_count.reg
    institutional.data %>% pull(assistant_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% pull(assistant_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_count.reg
    institutional.data %>% pull(full_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% pull(full_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_count.reg
    institutional.data %>% pull(all_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% pull(all_prof_count) %>% mean(na.rm = TRUE) %>% round(digits.no)
))

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_count.reg, shiftshare_lecturer_count.reg,
    naive_assistant_count.reg, shiftshare_assistant_count.reg,
    naive_full_count.reg, shiftshare_full_count.reg,
    naive_all_count.reg, shiftshare_all_count.reg,
    add.lines = outcome.means,
    dep.var.caption = "Dependent Variable: Employment Count by Professor Group",
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
    out = "../../text/tables/facultycount-illinois-reg-rawcount.tex")


# Faculty hiring Regressions ---------------------------------------------------

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_hires.reg <- institutional.data %>%
    filter(lecturer_prof_hires > 0) %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        stateappropriations_real / (1000 * enrollment_reported)) %>%
    felm(lecturer_prof_hires ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_hires.reg <- institutional.data %>%
    filter(lecturer_prof_hires > 0) %>%
    felm(lecturer_prof_hires ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_hires.reg <- institutional.data %>%
    filter(assistant_prof_hires > 0) %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        stateappropriations_real / (1000 * enrollment_reported)) %>%
    felm(assistant_prof_hires ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_hires.reg <- institutional.data %>%
    filter(assistant_prof_hires > 0) %>%
    felm(assistant_prof_hires ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_hires.reg <- institutional.data %>%
    filter(full_prof_hires > 0) %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        stateappropriations_real / (1000 * enrollment_reported)) %>%
    felm(full_prof_hires ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_hires.reg <- institutional.data %>%
    filter(full_prof_hires > 0) %>%
    felm(full_prof_hires ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_hires.reg <- institutional.data %>%
    filter(all_prof_hires > 0) %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        stateappropriations_real / (1000 * enrollment_reported)) %>%
    felm(all_prof_hires ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_hires.reg <- institutional.data %>%
    filter(all_prof_hires > 0) %>%
    felm(all_prof_hires ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    institutional.data %>% filter(lecturer_prof_hires > 0) %>% pull(lecturer_prof_hires) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% filter(lecturer_prof_hires > 0) %>% pull(lecturer_prof_hires) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_hires.reg
    institutional.data %>% filter(assistant_prof_hires > 0) %>% pull(assistant_prof_hires) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% filter(assistant_prof_hires > 0) %>% pull(assistant_prof_hires) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_hires.reg
    institutional.data %>% filter(full_prof_hires > 0) %>% pull(full_prof_hires) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% filter(full_prof_hires > 0) %>% pull(full_prof_hires) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_hires.reg
    institutional.data %>% filter(all_prof_hires > 0) %>% pull(all_prof_hires) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% filter(all_prof_hires > 0) %>% pull(all_prof_hires) %>% mean(na.rm = TRUE) %>% round(digits.no)
))

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_hires.reg, shiftshare_lecturer_hires.reg,
    naive_assistant_hires.reg, shiftshare_assistant_hires.reg,
    naive_full_hires.reg, shiftshare_full_hires.reg,
    naive_all_hires.reg, shiftshare_all_hires.reg,
    add.lines = outcome.means,
    dep.var.caption = "Dependent Variable: Yearly New Hires by Professor Group",
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
    out = "../../text/tables/facultyhires-illinois-reg-rawcount.tex")
