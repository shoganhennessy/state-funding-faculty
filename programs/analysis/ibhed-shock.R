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

# Define default colours.
colour.list <- c(
    "#D62728", # Red  -> First-stage.
    "#1f77b4", # Blue -> Lecturers
    "#c9721c", # Orange -> Asst Professors
    "#237e23", # Green -> Full Professors
    "#cc2c9f") # Strong pink -> All faculty.

# Define number of digits in tables and graphs
digits.no <- 3

# Size for figures
fig.width <- 10
fig.height <- fig.width * 0.85


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

# How many individual professors in 2010 and 2021?
reg.data %>% filter(year == year.min) %>% count(unitid, name) %>% nrow() %>% print()
reg.data %>% filter(year == year.max) %>% count(unitid, name) %>% nrow() %>% print()

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


# Summary Table ----------------------------------------------------------------

# Generate the TeX summary table, of individual professors
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
        out = "../../text/tables/illinois-summary.tex")


# Density plot of wages --------------------------------------------------------

# Draw a plot of the salary distributions.
salary_density.plot <- reg.data %>%
    filter(3000 < salary_real + extra_salary_real,
        salary_real + extra_salary_real < 251000,
        year %in% c(2010, 2021)) %>%
    mutate(faculty =
        ifelse(position %in% c("Instructor", "Lecturer"), "Lecturer",
        ifelse(position == "Assistant Professor", "Asst. Professor",
        ifelse(position %in% c("Associate Professor", "Professor"), "Full Professor",
        ifelse(position %in% c("Chancellor",
            "Other Administrator", "President", "Senior Officer",
            "Unit Director", "Vice Chancellor", "Vice President"), "Administrator",
            ""))))) %>%
    ggplot(aes(x = (salary_real + extra_salary_real) / 1000,
        fill = faculty, colour = faculty)) +
    geom_density(alpha = 1 / 3) +
    theme_bw() +
    scale_x_continuous(expand = c(0, 0),
        name = "Annual Salary + Benefits, $ thousands",
        breaks = seq(0, 250, by = 50),
        limits = c(0, 265)) +
    scale_y_continuous(expand = c(0, 0.0001),
        name = "") +
    ggtitle("Density") +
    facet_grid(cols = vars(year)) +
    theme(
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -5)) +
    scale_fill_manual(values = colour.list) +
    scale_colour_manual(values = colour.list)

# Save this plot
presentation.width <- 17.5
presentation.height <- presentation.width / 2
ggsave("../../text/figures/salary-density.png",
    plot = salary_density.plot,
    units = "cm", dpi = 300,
    width = presentation.width, height = presentation.height)

# Show how many faculty in eah year, 2010 + 2021
reg.data %>%
    filter(3000 < salary_real + extra_salary_real,
        salary_real + extra_salary_real < 251000,
        year %in% c(2010, 2021)) %>%
    group_by(year) %>%
    count() %>%
    print()


# First stage Regressions ------------------------------------------------------

# Explain Revenues with a shock to (only) state appropriations.
firststage_approp.reg <- reg.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + firstyear |
        0 |
        unitid + year,
        data = .)
# Get the F.Stat
firststage_approp.fstat <-
    linearHypothesis(firststage_approp.reg, test = "F",
        c("I(-log(appropriationshock_perEnroll_real))=0"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Without the FEs
# Explain State appropriations with a shock to (only) state appropriations.
firststage_approp_noFE.reg <- reg.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        0 |
        0 |
        unitid + year,
        data = .)
# Get the F.Stat
firststage_approp_noFE.fstat <-
    linearHypothesis(firststage_approp_noFE.reg, test = "F",
        c("I(-log(appropriationshock_perEnroll_real))=0"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Explain Revenues with a shock to (only) state appropriations.
firststage_approp_tuit.reg <- reg.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) +
        log(tuitionrev_real / enrollment_reported) |
        unitid + firstyear |
        0 |
        unitid + year,
        data = .)
# Get the F.Stat
firststage_approp_tuit.fstat <-
    linearHypothesis(firststage_approp_tuit.reg, test = "F",
        c("I(-log(appropriationshock_perEnroll_real))=0"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Without the FEs
# Explain State appropriations with a shock to (only) state appropriations.
firststage_approp_tuit_noFE.reg <- reg.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) +
        log(tuitionrev_real / enrollment_reported) |
        0 |
        0 |
        unitid + year,
        data = .)
# Get the F.Stat
firststage_approp_tuit_noFE.fstat <-
    linearHypothesis(firststage_approp_tuit_noFE.reg, test = "F",
        c("I(-log(appropriationshock_perEnroll_real))=0"))["F"] %>%
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
    out = "../../text/tables/firststage-illinois.tex")


# Illinois Faculty Composition Regressions -------------------------------------

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- institutional.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- institutional.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_count.reg <- institutional.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- institutional.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_count.reg <- institutional.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- institutional.data %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- institutional.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- institutional.data %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    institutional.data %>% mutate(outcome = lecturer_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% mutate(outcome = lecturer_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_count.reg
    institutional.data %>% mutate(outcome = assistant_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% mutate(outcome = assistant_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_count.reg
    institutional.data %>% mutate(outcome = full_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% mutate(outcome = full_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_count.reg
    institutional.data %>% mutate(outcome = all_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% mutate(outcome = all_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no)
))

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_count.reg, shiftshare_lecturer_count.reg,
    naive_assistant_count.reg, shiftshare_assistant_count.reg,
    naive_full_count.reg, shiftshare_full_count.reg,
    naive_all_count.reg, shiftshare_all_count.reg,
    add.lines = outcome.means,
    dep.var.caption = "Dependent Variable: Employment Count",
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
    out = "../../text/tables/facultycount-illinois-reg-fte.tex")


# Faculty hiring Regressions ---------------------------------------------------

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_hires.reg <- institutional.data %>%
    filter(lecturer_prof_hires > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(lecturer_prof_hires / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_hires.reg <- institutional.data %>%
    filter(lecturer_prof_hires > 0) %>%
    felm(log(lecturer_prof_hires / enrollment_reported) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_hires.reg <- institutional.data %>%
    filter(assistant_prof_hires > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(assistant_prof_hires / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_hires.reg <- institutional.data %>%
    filter(assistant_prof_hires > 0) %>%
    felm(log(assistant_prof_hires / enrollment_reported) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_hires.reg <- institutional.data %>%
    filter(full_prof_hires > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(full_prof_hires / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_hires.reg <- institutional.data %>%
    filter(full_prof_hires > 0) %>%
    felm(log(full_prof_hires / enrollment_reported) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_hires.reg <- institutional.data %>%
    filter(all_prof_hires > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(all_prof_hires / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_hires.reg <- institutional.data %>%
    filter(all_prof_hires > 0) %>%
    felm(log(all_prof_hires / enrollment_reported) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    institutional.data %>% filter(lecturer_prof_hires > 0) %>% mutate(outcome = lecturer_prof_hires / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% filter(lecturer_prof_hires > 0) %>% mutate(outcome = lecturer_prof_hires / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_hires.reg
    institutional.data %>% filter(assistant_prof_hires > 0) %>% mutate(outcome = assistant_prof_hires / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% filter(assistant_prof_hires > 0) %>% mutate(outcome = assistant_prof_hires / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_hires.reg
    institutional.data %>% filter(full_prof_hires > 0) %>% mutate(outcome = full_prof_hires / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% filter(full_prof_hires > 0) %>% mutate(outcome = full_prof_hires / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_hires.reg
    institutional.data %>% filter(all_prof_hires > 0) %>% mutate(outcome = all_prof_hires / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    institutional.data %>% filter(all_prof_hires > 0) %>% mutate(outcome = all_prof_hires / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no)
))

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_hires.reg, shiftshare_lecturer_hires.reg,
    naive_assistant_hires.reg, shiftshare_assistant_hires.reg,
    naive_full_hires.reg, shiftshare_full_hires.reg,
    naive_all_hires.reg, shiftshare_all_hires.reg,
    add.lines = outcome.means,
    dep.var.caption = "Dependent Variable: Employment Count",
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
    out = "../../text/tables/facultyhires-illinois-reg-fte.tex")


# Faculty Salary Regressions ---------------------------------------------------

## Non-full faculty salaries
lecturer.data <- reg.data %>%
    filter(lecturer == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_salaries.reg <- lecturer.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Tenure-track faculty salaries
assistant.data <- reg.data %>%
    filter(assistant == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_salaries.reg <- assistant.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## full faculty salaries
full.data <- reg.data %>%
    filter(full == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_salaries.reg <- full.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Administrator faculty salaries
administrator.data <- reg.data %>%
    filter(administrator == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_administrator_salaries.reg <- administrator.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## All faculty Salaries
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_salaries.reg <- reg.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Collate the results to a LaTeX table
stargazer(
    shiftshare_lecturer_salaries.reg,
    shiftshare_assistant_salaries.reg,
    shiftshare_full_salaries.reg,
    shiftshare_administrator_salaries.reg,
    shiftshare_all_salaries.reg,
    dep.var.caption  = "Dependent Variable: Annual Salaries",
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
    out = "../../text/tables/facultysalaries-shock-illinois.tex")

# Investigate count + pay for new professors, by position, in each year Illinois
faculty_yearly.data <- reg.data %>%
    mutate(
        position = ifelse(lecturer == 1, "lecturer",
            ifelse(assistant == 1, "assistant",
                ifelse(full == 1, "full",
                    ifelse(administrator == 1, "administrator", NA))))) %>%
    # Get the uni-yearly count (enrollment is at the uni leevel, so is averaged)
    group_by(year, position, unitid) %>%
    summarise(prof_count = n(),
        enrollment_reported = mean(enrollment_reported, na.rm = TRUE),
        totalsalary_real =
            mean(salary_real + extra_salary_real, na.rm = TRUE)) %>%
    ungroup() %>%
    # Collapse to the year-position level
    group_by(year, position) %>%
    summarise(totalsalary_real = weighted.mean(totalsalary_real,
            weights = prof_count, na.rm = TRUE),
        prof_count = sum(prof_count, na.rm = TRUE),
        enrollment_reported = sum(enrollment_reported, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(prof_perstudent = prof_count / enrollment_reported)

# Plot the number of new professors, by position, in each year Illinois
faculty_count.plot <- faculty_yearly.data %>%
    ggplot(aes(x = year, y = prof_perstudent, colour = position)) +
    geom_point() +
    geom_line() +
    # Adjust the names and axis
    scale_x_continuous(name = "Year",
        breaks = seq(2010, 2021, by = 2)) +
    scale_y_continuous(name = "",
        limits = c(0, 0.035),
        breaks = seq(0, 0.035, by = 0.005),
        labels = scales::comma) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
    scale_colour_discrete(name = "",
        breaks = c("lecturer", "assistant", "full", "administrator"),
        labels = c("Lecturer", "Assistant", "Full", "Administrator"))
# Save this plot
ggsave("../../text/figures/faculty-count-illinois.png",
    plot = faculty_count.plot,
    units = "cm", width = fig.width, height = fig.height)

# Plot the salaries for new professors, by position, in each year Illinois
faculty_salary.plot <- faculty_yearly.data %>%
    ggplot(aes(x = year, y = totalsalary_real, colour = position)) +
    geom_point() +
    geom_line() +
    # Adjust the names and axis
    scale_x_continuous(name = "Year",
        breaks = seq(2010, 2021, by = 2)) +
    scale_y_continuous(name = "",
        limits = c(0, 130000),
        breaks = seq(0, 130000, by = 20000),
        labels = scales::comma) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
    scale_colour_discrete(name = "",
        breaks = c("lecturer", "assistant", "full", "administrator"),
        labels = c("Lecturer", "Assistant", "Full", "Administrator"))
# Save this plot
ggsave("../../text/figures/faculty-salary-illinois.png",
    plot = faculty_salary.plot,
    units = "cm", width = fig.width, height = fig.height)


# Faculty Salary Regressions among first-years ---------------------------------

# Repeat analysis among the sample of new-hires, in their first year
newhire.data <- reg.data %>%
    filter(firstyear == year, year > 2010)

## First year Lecturer salaries
lecturer.data <- newhire.data %>%
    filter(lecturer == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_salaries.reg <- lecturer.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## First year Tenure-track faculty salaries
assistant.data <- newhire.data %>%
    filter(assistant == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_salaries.reg <- assistant.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## First year full faculty salaries
full.data <- newhire.data %>%
    filter(full == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_salaries.reg <- full.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## First year Administrator faculty salaries
administrator.data <- newhire.data %>%
    filter(administrator == 1)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_administrator_salaries.reg <- administrator.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## First year All faculty Salaries
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_salaries.reg <- newhire.data %>%
    felm(log(salary_real + extra_salary_real) ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Collate the results to a LaTeX table
stargazer(
    shiftshare_lecturer_salaries.reg,
    shiftshare_assistant_salaries.reg,
    shiftshare_full_salaries.reg,
    shiftshare_administrator_salaries.reg,
    shiftshare_all_salaries.reg,
    dep.var.caption  = "Dependent Variable: Annual Salaries",
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
    out = "../../text/tables/newhiresalaries-shock-illinois.tex")

# Investigate count + pay for new professors, by position, in each year Illinois
newhire_yearly.data <- newhire.data %>%
    mutate(position = ifelse(lecturer == 1, "lecturer",
        ifelse(assistant == 1, "assistant",
            ifelse(full == 1, "full",
                ifelse(administrator == 1, "administrator", NA))))) %>%
    group_by(year, position) %>%
    summarise(prof_count = n(),
        totalsalary_real =
            mean(salary_real + extra_salary_real, na.rm = TRUE))

# Plot the number for new professors, by position, in each year Illinois
newhire_count.plot <- newhire_yearly.data %>%
    ggplot(aes(x = year, y = prof_count, colour = position)) +
    geom_point() +
    geom_line() +
    # Adjust the names and axis
    scale_x_continuous(name = "Year",
        breaks = seq(2010, 2021, by = 2)) +
    scale_y_continuous(name = "",
        limits = c(0, 1000),
        breaks = seq(0, 1000, by = 200),
        labels = scales::comma) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
    scale_colour_discrete(name = "",
        breaks = c("lecturer", "assistant", "full", "administrator"),
        labels = c("Lecturer", "Assistant", "Full", "Administrator"))
# Save this plot
ggsave("../../text/figures/newhire-count-illinois.png",
    plot = newhire_count.plot,
    units = "cm", width = fig.width, height = fig.height)

# Plot the salaries for new professors, by position, in each year Illinois
newhire_salary.plot <- newhire_yearly.data %>%
    ggplot(aes(x = year, y = totalsalary_real, colour = position)) +
    geom_point() +
    geom_line() +
    # Adjust the names and axis
    scale_x_continuous(name = "Year",
        breaks = seq(2010, 2021, by = 2)) +
    scale_y_continuous(name = "",
        limits = c(0, 125000),
        breaks = seq(0, 125000, by = 20000),
        labels = scales::comma) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
    scale_colour_discrete(name = "",
        breaks = c("lecturer", "assistant", "full", "administrator"),
        labels = c("Lecturer", "Assistant", "Full", "Administrator"))
# Save this plot
ggsave("../../text/figures/newhire-salary-illinois.png",
    plot = newhire_salary.plot,
    units = "cm", width = fig.width, height = fig.height)


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
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Tenure-track faculty promotion
assistant.data <- reg.data %>%
    filter(ever_assistant == 1, assistant + associate > 0)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_promotion.reg <- assistant.data %>%
    felm(promoted ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## full faculty promotion
associate.data <- reg.data %>%
    filter(ever_associate == 1, full > 0)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_promotion.reg <- associate.data %>%
    felm(promoted ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## All faculty promotion
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_promotion.reg <- reg.data %>%
    felm(promoted ~ 1 |
        unitid + firstyear |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Collate the results to a LaTeX table
stargazer(
    shiftshare_lecturer_promotion.reg,
    shiftshare_assistant_promotion.reg,
    shiftshare_full_promotion.reg,
    shiftshare_all_promotion.reg,
    dep.var.caption  = "Dependent Variable: Promotion Rate",
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
    out = "../../text/tables/promotion-shock-illinois.tex")


# Faculty exit-rate Regressions ------------------------------------------------

## Lecturer faculty salaries
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_exit.reg <- reg.data %>%
    filter(lecturer == 1) %>%
    felm(notemployed_nextyear ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Tenure-track faculty salaries
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_exit.reg <- reg.data %>%
    filter(assistant == 1) %>%
    felm(notemployed_nextyear ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## full faculty salaries
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_exit.reg <- reg.data %>%
    filter(full == 1) %>%
    felm(notemployed_nextyear ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Administrator faculty salaries
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_administrator_exit.reg <- reg.data %>%
    filter(administrator == 1) %>%
    felm(notemployed_nextyear ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## All faculty exit rate
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_exit.reg <- reg.data %>%
    felm(notemployed_nextyear ~ 1 |
        unitid |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Collate the results to a LaTeX table
stargazer(
    shiftshare_lecturer_exit.reg,
    shiftshare_assistant_exit.reg,
    shiftshare_full_exit.reg,
    shiftshare_administrator_exit.reg,
    shiftshare_all_exit.reg,
    dep.var.caption  = "Dependent Variable: Exit rate",
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
    out = "../../text/tables/facultyleaving-shock-illinois.tex")


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
        firstyear = firstyear,
        instid = factor(unitid),
        academicyear = factor(year),
        salary_real = log(salary_real + extra_salary_real),
        promoted = promoted,
        stateappropriations_real =
            log(stateappropriations_real / enrollment_reported),
        appropriationshock_perEnroll_real =
            - log(appropriationshock_perEnroll_real),
        tuitionrev_real = log(tuitionrev_real / enrollment_reported)) %>%
    pdata.frame(index = c("unitid", "year"), drop.index = FALSE)

# Run the LP method for the first-stage regression.
firststage.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "stateappropriations_real",
        # Predictor variable
        shock = "appropriationshock_perEnroll_real",
        # Contemporaneous control, FE for unitid * year
        c_exog_data = c("instid", "academicyear"),
        # Option to use IV for predictor endogeneity (not used here)
        iv_reg = FALSE,
        # Add clustered SEs in the panel
        panel_model = "pooling",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/firststage-illinois-lp.png",
    plot = plot(firststage.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# Run the LP estimation for salaries among all profs.
all_salaries.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "salary_real",
        # Predictor variable
        shock = "stateappropriations_real",
        # Contemporaneous control, plus FE for unitid + year
        c_exog_data = c("instid", "academicyear"),
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # Add clustered SEs in the panel
        panel_model = "pooling",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/salaries-illinois-lp.png",
    plot = plot(all_salaries.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# Run the LP estimation for promotion rate among all profs.
all_promoted.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "promoted",
        # Predictor variable
        shock = "stateappropriations_real",
        # Contemporaneous control, FE for unitid + year
        c_exog_data = c("instid", "academicyear"),
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # Add clustered SEs in the panel
        panel_model = "pooling",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/promoted-illinois-lp.png",
    plot = plot(all_promoted.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# Run the LP estimation for leaving rate among all profs.
all_exit.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "notemployed_nextyear",
        # Predictor variable
        shock = "stateappropriations_real",
        # Contemporaneous control, FE for unitid + year
        c_exog_data = c("instid", "academicyear"),
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # Add clustered SEs in the panel
        panel_model = "pooling",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/exit-illinois-lp.png",
    plot = plot(all_exit.lpreg),
    units = "cm", width = fig.width, height = fig.height)
