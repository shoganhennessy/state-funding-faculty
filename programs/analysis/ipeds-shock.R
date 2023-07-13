#!/usr/bin/R
## Senan Hogan-Hennessy, 3 August 2022
## IV for Prof's salaries, using IPEDS data.
print(Sys.time())
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
fig.height <- fig.width * 0.85

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

# Load IPEDS data
ipeds.data <- read_csv("../../data/urban-ipeds/urban-clean-publicunis.csv")


# Clean data -------------------------------------------------------------------

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
        # Count of full-time professors (tenured or not)
        lecturer_prof_count,
        assistant_prof_count,
        full_prof_count,
        all_prof_count,
        # Count of professors by tenure-rate (only 1990-)
        nontenured_tenure_count,
        tenuretrack_tenure_count,
        tenured_tenure_count,
        all_tenure_count,
        # Average faculty salary
        full_profmeansalary_real,
        assistant_profmeansalary_real,
        lecturer_profmeansalary_real,
        all_profmeansalary_real,
        # Total paid on professor salaries
        all_profoutlays_real,
        # Research and instruction expenditures.
        instructionspending_total_real,
        instructionspending_salaries_real,
        researchspending_total_real,
        researchspending_salaries_real,
        nonauxspending_real)


# Restrict data to unis + years with measured state appropriations & shocks
reg.data <- reg.data %>%
    filter(1990 <= year,
        !is.na(enrollment_reported), enrollment_reported > 0,
        !is.na(all_prof_count),
        !is.na(stateappropriations_real),
        !is.na(appropriationshock_perEnroll_real)) %>%
    # Restrict to uni-years with all professor counts.
    filter(!is.na(lecturer_prof_count), lecturer_prof_count > 0) %>%
    filter(!is.na(assistant_prof_count), assistant_prof_count > 0) %>%
    filter(!is.na(full_prof_count), full_prof_count > 0)


# Save memory by removing initally loaded files
rm(ipeds.data)
gc()


# Summary Table ----------------------------------------------------------------
reg.data %>%
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
        digits = 0,
        digits.extra = 0,
        covariate.labels = c(
            "Enrolment",
            "State Funding (millions 2021 USD)",
            "Total revenues (millions 2021 USD)",
            "Non-institutional revenues (millions 2021 USD)",
            "Lecturers count",
            "Assistant professors count",
            "Full professors count",
            "All professors count"),
        omit.table.layout = "n",
        header = FALSE, float = FALSE, no.space = TRUE,
        type = "text",
        out = "../../text/tables/ipeds-summary-fte.tex")


# Plot funding sources over time -----------------------------------------------

# Plot total gov funding by state + year.
mean_funding_fte.data <- reg.data %>%
    group_by(state, year) %>%
    summarise(
        totalrevenues_real =
            mean(totalrevenues_real / enrollment_reported, na.rm = TRUE),
        nonauxrevenues_real =
            mean(nonauxrevenues_real / enrollment_reported, na.rm = TRUE),
        stateappropriations_real =
            mean(stateappropriations_real / enrollment_reported, na.rm = TRUE),
        tuitionrev_real =
            mean(tuitionrev_real / enrollment_reported, na.rm = TRUE))

# SHow how much finding MA gets in 1990 and 2021
mean_funding_fte.data %>%
    filter(state %in% c("CA", "IL")) %>%
    filter(year %in% c(1990, 2021)) %>%
    print()

# Plot total gov funding by year.
mean_funding_fte.data <- reg.data %>%
    group_by(year) %>%
    summarise(
        totalrevenues_real =
            mean(totalrevenues_real / enrollment_reported, na.rm = TRUE),
        nonauxrevenues_real =
            mean(nonauxrevenues_real / enrollment_reported, na.rm = TRUE),
        stateappropriations_real =
            mean(stateappropriations_real / enrollment_reported, na.rm = TRUE),
        tuitionrev_real =
            mean(tuitionrev_real / enrollment_reported, na.rm = TRUE))

# Define the plot
mean_funding_fte.graph <- mean_funding_fte.data %>%
    select(-totalrevenues_real) %>%
    pivot_longer(!year, names_to = "variable", values_to = "millions") %>%
    ggplot(aes(x = year, y = millions / 10^3, colour = variable)) +
    geom_point() +
    geom_line() +
    # Adjust the names and axis
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(0, 21),
        breaks = seq(0, 50, by = 2.5),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Funding, $ thousands") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "",
        breaks = c("totalrevenues_real", "nonauxrevenues_real",
        "stateappropriations_real", "tuitionrev_real"),
        labels = c("Total", "Non-inst.", "State", "Tuition"))
# Save this plot
ggsave("../../text/figures/mean-funding-fte.png",
    plot = mean_funding_fte.graph,
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Plot total gov funding by year.
mean_funding.data <- reg.data %>%
    group_by(year) %>%
    summarise(
        totalrevenues_real =
            mean(totalrevenues_real, na.rm = TRUE) / (10^6),
        nonauxrevenues_real =
            mean(nonauxrevenues_real, na.rm = TRUE) / (10^6),
        stateappropriations_real =
            mean(stateappropriations_real, na.rm = TRUE) / (10^6),
        tuitionrev_real =
            mean(tuitionrev_real, na.rm = TRUE) / (10^6))
# Define the plot
mean_funding.graph <- mean_funding.data %>%
    select(-totalrevenues_real) %>%
    pivot_longer(!year, names_to = "variable", values_to = "millions") %>%
    ggplot(aes(x = year, y = millions, colour = variable)) +
    geom_point() +
    geom_line() +
    # Adjust the names and axis
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(0, 300),
        breaks = seq(0, 500, by = 50),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Funding, $ millions") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "",
        breaks = c("totalrevenues_real", "nonauxrevenues_real",
        "stateappropriations_real", "tuitionrev_real"),
        labels = c("Total", "Non-inst.", "State", "Tuition"))
# Save this plot
ggsave("../../text/figures/mean-funding-total.png",
    plot = mean_funding.graph,
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# SHow figures fo 1990 and 2020, mean and total
print("total rev:")
print(c("Mean total rev per uni, 1990",
    mean_funding.data %>% filter(year == 1990) %>% pull(totalrevenues_real)))
print(c("Mean total rev per uni, 2020",
    mean_funding.data %>% filter(year == 2020) %>% pull(totalrevenues_real)))
print(c("Mean total rev per student, 1990",
    mean_funding_fte.data %>% filter(year == 1990) %>% pull(totalrevenues_real)))
print(c("Mean total rev per student, 2020",
    mean_funding_fte.data %>% filter(year == 2020) %>% pull(totalrevenues_real)))
print("state approp:")
print(c("Mean state rev per uni, 1990",
    mean_funding.data %>% filter(year == 1990) %>% pull(stateappropriations_real)))
print(c("Mean state rev per uni, 2020",
    mean_funding.data %>% filter(year == 2020) %>% pull(stateappropriations_real)))
print(c("Mean state rev per student, 1990",
    mean_funding_fte.data %>% filter(year == 1990) %>% pull(stateappropriations_real)))
print(c("Mean state rev per student, 2020",
    mean_funding_fte.data %>% filter(year == 2020) %>% pull(stateappropriations_real)))
print("enrollment in 1990 as a percent of that in 2020 among PUs")
enrollment_yearly.data <- reg.data %>%
    group_by(year) %>%
    summarise(enrollment_reported = sum(enrollment_reported, na.rm = TRUE))
print((
    (enrollment_yearly.data %>% filter(year == 2020) %>% pull(enrollment_reported)) -
    (enrollment_yearly.data %>% filter(year == 1991) %>% pull(enrollment_reported))
    ) / (enrollment_yearly.data %>% filter(year == 1991) %>% pull(enrollment_reported)
))

# Repeat the analysis for Illinois
# Plot total gov funding by year.
illinois_funding_fte.data <- reg.data %>%
    filter(state == "IL") %>%
    group_by(year) %>%
    summarise(
        totalrevenues_real =
            mean(totalrevenues_real / enrollment_reported, na.rm = TRUE),
        nonauxrevenues_real =
            mean(nonauxrevenues_real / enrollment_reported, na.rm = TRUE),
        stateappropriations_real =
            mean(stateappropriations_real / enrollment_reported, na.rm = TRUE),
        tuitionrev_real =
            mean(tuitionrev_real / enrollment_reported, na.rm = TRUE))
# Define the plot
illinois_funding_fte.graph <- illinois_funding_fte.data %>%
    select(-totalrevenues_real) %>%
    pivot_longer(!year, names_to = "variable", values_to = "millions") %>%
    ggplot(aes(x = year, y = millions / 10^3, colour = variable)) +
    geom_point() +
    geom_line() +
    # Adjust the names and axis
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(0, 25),
        breaks = seq(0, 50, by = 5),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Funding, $ thousands per student") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "",
        breaks = c("totalrevenues_real", "nonauxrevenues_real",
        "stateappropriations_real", "tuitionrev_real"),
        labels = c("Total", "Total Non-inst.", "State", "Tuition"))
# Save this plot
ggsave("../../text/figures/illinois-funding-fte.png",
    plot = illinois_funding_fte.graph,
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Plot total gov funding by year.
illinois_funding.data <- reg.data %>%
    filter(state == "IL") %>%
    group_by(year) %>%
    summarise(
        totalrevenues_real =
            mean(totalrevenues_real, na.rm = TRUE) / (10^6),
        nonauxrevenues_real =
            mean(nonauxrevenues_real, na.rm = TRUE) / (10^6),
        stateappropriations_real =
            mean(stateappropriations_real, na.rm = TRUE) / (10^6),
        tuitionrev_real =
            mean(tuitionrev_real, na.rm = TRUE) / (10^6))
# Define the plot
illinois_funding.graph <- illinois_funding.data %>%
    select(-totalrevenues_real) %>%
    pivot_longer(!year, names_to = "variable", values_to = "millions") %>%
    ggplot(aes(x = year, y = millions, colour = variable)) +
    geom_point() +
    geom_line() +
    # Adjust the names and axis
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(0, 400),
        breaks = seq(0, 1000, by = 50),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Funding, $ millions") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "",
        breaks = c("totalrevenues_real", "nonauxrevenues_real",
        "stateappropriations_real", "tuitionrev_real"),
        labels = c("Total", "Total Non-inst.", "State", "Tuition"))
# Save this plot
ggsave("../../text/figures/illinois-funding-total.png",
    plot = illinois_funding.graph,
    units = "cm", dpi = 300, width = fig.width, height = fig.height)


# Tables for Instrument Exogeneity ---------------------------------------------

# Regressions for Instrument balance test, in log terms
balance_log_enrollment_reported.reg <- reg.data %>%
    felm(log(enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_log_stateappropriations_real.reg <- reg.data %>%
    felm(log(stateappropriations_real) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_log_totalrevenues_real.reg <- reg.data %>%
    felm(log(totalrevenues_real) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_log_nonauxrevenues_real.reg <- reg.data %>%
    felm(log(nonauxrevenues_real) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_log_lecturer_prof_count.reg <- reg.data %>%
    felm(log(lecturer_prof_count) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_log_assistant_prof_count.reg <- reg.data %>%
    felm(log(assistant_prof_count) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_log_full_prof_count.reg <- reg.data %>%
    felm(log(full_prof_count) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_log_all_prof_count.reg <- reg.data %>%
    felm(log(all_prof_count) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Collate the results to a LaTeX table
stargazer(
    balance_log_enrollment_reported.reg,
    balance_log_stateappropriations_real.reg,
    balance_log_totalrevenues_real.reg,
    balance_log_nonauxrevenues_real.reg,
    balance_log_lecturer_prof_count.reg,
    balance_log_assistant_prof_count.reg,
    balance_log_full_prof_count.reg,
    balance_log_all_prof_count.reg,
    dep.var.caption = "Dependent Variables: University Characteristics",
    dep.var.labels = c(
        "Enrolment",
        "State Funding",
        "Total revenues",
        "Non-inst. revenues",
        "Lecturers",
        "Assistant professors",
        "Full professors",
        "All professors"),
    covariate.labels = c("State Funding"),
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    omit = "factor|count|year",
    intercept.bottom = TRUE,
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq", "f"),
    add.lines = list(
        c("Uni. + Year fixed effects?", rep("Yes", 8))),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/firststage-balance-log.tex")

# Regressions for Instrument balance test, in raw terms
balance_raw_enrollment_reported.reg <- reg.data %>%
    felm(enrollment_reported ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_raw_stateappropriations_real.reg <- reg.data %>%
    felm(stateappropriations_real ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_raw_totalrevenues_real.reg <- reg.data %>%
    felm(totalrevenues_real ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_raw_nonauxrevenues_real.reg <- reg.data %>%
    felm(nonauxrevenues_real ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_raw_lecturer_prof_count.reg <- reg.data %>%
    felm(lecturer_prof_count ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_raw_assistant_prof_count.reg <- reg.data %>%
    felm(assistant_prof_count ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_raw_full_prof_count.reg <- reg.data %>%
    felm(full_prof_count ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        state + year,
        data = .)
balance_raw_all_prof_count.reg <- reg.data %>%
    felm(all_prof_count ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Collate the results to a LaTeX table
stargazer(
    balance_raw_enrollment_reported.reg,
    balance_raw_stateappropriations_real.reg,
    balance_raw_totalrevenues_real.reg,
    balance_raw_nonauxrevenues_real.reg,
    balance_raw_lecturer_prof_count.reg,
    balance_raw_assistant_prof_count.reg,
    balance_raw_full_prof_count.reg,
    balance_raw_all_prof_count.reg,
    dep.var.caption = "Dependent Variables: University Characteristics",
    dep.var.labels = c(
        "Enrolment",
        "State Funding",
        "Total revenues",
        "Non-inst. revenues",
        "Lecturers",
        "Assistant professors",
        "Full professors",
        "All professors"),
    covariate.labels = c("State Funding"),
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    omit = "factor|count|year",
    intercept.bottom = TRUE,
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq", "f"),
    add.lines = list(
        c("Uni. + Year fixed effects?", rep("Yes", 8))),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/firststage-balance-count.tex")

# Summary Statistics for the Quantiles of the shift-share instrument.
quantile.count <- 5
reg.data %>%
    mutate(instrument_quantile = round((quantile.count - 1) *
        ecdf(appropriationshock_perEnroll_real)(
            appropriationshock_perEnroll_real))) %>%
    group_by(instrument_quantile) %>%
    summarise(
        `State Funding Shock, \\$ per student`   = round(mean(appropriationshock_perEnroll_real, an.rm = TRUE)),
        `State Funding, per student`             = round(mean(stateappropriations_real / enrollment_reported, na.rm = TRUE)),
        `Total Full-time Enrolment`              = round(mean(enrollment_reported, na.rme = TRUE)),
        `State Funding, \\$ millions`            = round(mean(stateappropriations_real / 10^6, na.rme = TRUE)),
        `Total Revenues, \\$ millions`           = round(mean(totalrevenues_real / 10^6, na.rme = TRUE)),
        `Total Revenues, \\$ per student`        = round(mean(totalrevenues_real / enrollment_reported, na.rm = TRUE)),
        `Lecturer Count`                         = round(mean(lecturer_prof_count, na.rm = TRUE)),
        `Assistant Professor Count`              = round(mean(assistant_prof_count, na.rm = TRUE)),
        `Full Professor Count`                   = round(mean(full_prof_count, na.rm = TRUE)),
        `Total Professor Count`                  = round(mean(all_prof_count, na.rm = TRUE)),
        `Lecturers, per 100 students`            = mean(100 * lecturer_prof_count / enrollment_reported, na.rm = TRUE),
        `Assistant Professors, per 100 students` = mean(100 * assistant_prof_count / enrollment_reported, na.rm = TRUE),
        `Full Professors, per 100 students`      = mean(100 * full_prof_count / enrollment_reported, na.rm = TRUE),
        `Total Professors, per 100 students`     = mean(100 * all_prof_count / enrollment_reported, na.rm = TRUE)
            ) %>%
    ungroup() %>%
    pivot_longer(!instrument_quantile,
        names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = "instrument_quantile",
        values_from = "value") %>%
    transmute(`Instrument Quantile:` = variable,
        `1st` = `0`,
        `2nd` = `1`,
        `3rd` = `2`,
        `4th` = `3`,
        `5th` = `4`) %>%
    xtable(type = "latex",
        digits = 2,
        align = "llccccc") %>%
    print(
        sanitize.colnames.function = identity,
        sanitize.text.function = identity,
        format.args = list(big.mark = ",", decimal.mark = "."),
        floating = FALSE,
        floating.environment = "tabular",
        include.rownames = FALSE,
        file = "../../text/tables/summary-quantiles.tex")


# First stage Regressions ------------------------------------------------------

# Explain Revenues with a shock to (only) state appropriations.
firststage_approp.reg <- reg.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-log(appropriationshock_perEnroll_real)) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Get the F.Stat
firststage_approp.fstat <-
    linearHypothesis(firststage_approp.reg, test = "F",
        c("I(-log(appropriationshock_perEnroll_real))"))["F"] %>%
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
        state + year,
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
        unitid + year |
        0 |
        state + year,
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
        state + year,
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
    firststage_approp.reg, firststage_approp_noFE.reg,
    firststage_approp_tuit.reg, firststage_approp_tuit_noFE.reg,
    dep.var.caption = "Dependent Variable: State Funding",
    dep.var.labels.include = FALSE,
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    omit = "factor|count|year",
    intercept.bottom = TRUE,
    covariate.labels = c("Funding Shock", "Tuition Revenue", "Constant"),
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq", "f"),
    add.lines = list(
        c("Uni. + Year fixed effects?", "Yes", "No", "Yes", "No"),
        c("F stat.",
            firststage_approp_tuit.fstat, firststage_approp_tuit_noFE.fstat,
            firststage_approp.fstat, firststage_approp_noFE.fstat)),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/firststage-reg-fte.tex")

# Plot the IV event-study equivalent.
coefficient.list <- c()
upper_ci.list <- c()
lower_ci.list <- c()
# Loop across time periods, to estimate the correlation between years.
lag.levels <- -7:5
for (t in lag.levels){
    print(t)
    lag.reg <- reg.data %>%
        mutate(lag_log_outcome_fte = log(lag_lead(
            stateappropriations_real / enrollment_reported, t))) %>%
        felm(lag_log_outcome_fte ~ 1 +
            I(-log(appropriationshock_perEnroll_real)) |
            unitid + year |
            0 |
            state + year,
            data = .) %>%
        summary()
    ci.halflength <- 1.96 * lag.reg$coefficients[2]
    coefficient.list <- c(coefficient.list, lag.reg$coefficients[1])
    upper_ci.list <- c(upper_ci.list, lag.reg$coefficients[1] + ci.halflength)
    lower_ci.list <- c(lower_ci.list, lag.reg$coefficients[1] - ci.halflength)
}
lag.data <- data.frame(
    t = lag.levels,
    estimate = coefficient.list,
    conf.low = upper_ci.list,
    conf.high = lower_ci.list)
# Plot the lagged results.
lag.plot <- lag.data %>%
    ggplot(aes(x = t)) +
    geom_vline(xintercept = 0, alpha = 0.5) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
        alpha = 0.4, fill = "grey70") +
    geom_point(aes(y = estimate), colour = "blue") +
    geom_line(aes(y = estimate), colour = "blue") +
    geom_line(aes(y = conf.low), linetype = "dashed") +
    geom_line(aes(y = conf.high), linetype = "dashed") +
    scale_x_continuous(name = "Years, Relative to Initital Shock",
        breaks = lag.data$t, expand = c(0.01, 0.01)) +
    scale_y_continuous(name = "",
        #limits = c(-0.2, 0.75),
        breaks = seq(-5, 5, by = 0.1)) +
    theme_bw() +
    ggtitle("Estimate") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 1, 0, 0), "mm"))
# Save this plot.
ggsave("../../text/figures/lag-firststage.png",
    plot = lag.plot,
    units = "cm", dpi = 300, width = 1.5 * fig.width, height = fig.height)


# Faculty Count Regressions ----------------------------------------------------

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- reg.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_count.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- reg.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_count.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- reg.data %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- reg.data %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    # Lecturers
    reg.data %>% mutate(outcome = lecturer_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% mutate(outcome = lecturer_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_assistant_count.reg
    reg.data %>% mutate(outcome = assistant_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% mutate(outcome = assistant_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_full_count.reg
    reg.data %>% mutate(outcome = full_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% mutate(outcome = full_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    # naive_all_count.reg
    reg.data %>% mutate(outcome = all_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no),
    reg.data %>% mutate(outcome = all_prof_count / (enrollment_reported / 100)) %>% pull(outcome) %>% mean(na.rm = TRUE) %>% round(digits.no)
))

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_count.reg, shiftshare_lecturer_count.reg,
    naive_assistant_count.reg, shiftshare_assistant_count.reg,
    naive_full_count.reg, shiftshare_full_count.reg,
    naive_all_count.reg, shiftshare_all_count.reg,
    add.lines = outcome.means,
    dep.var.caption = "Dependent Variable: Employment Count by Professor Group",
    dep.var.labels = c("Lecturer", "Assistant", "Full", "All"),
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
    out = "../../text/tables/facultycount-shock-reg-fte.tex")


# Get the rates of susbtituion from the elasticity estimates -------------------

# Plot the elasticities for profesorrs w.r.t state funding
fixest::coefplot(list(
    shiftshare_lecturer_count.reg, shiftshare_all_count.reg,
    shiftshare_assistant_count.reg, shiftshare_full_count.reg))
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html#plot_summs()_and_plot_coefs()

# Calculate the elasticities of susbtituion, with SEs bootstrapped.
boot.count <- 10000

## write a function to calculate the lecturer <-> assistant prof elasticity
lecturer_assistant.fun <- function(data, inds){
    substitution.reg <- fixest::feols(c(
            log(lecturer_prof_count / enrollment_reported),
            log(assistant_prof_count / enrollment_reported)
        ) ~ 1 |
        unitid + year |
        log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real)),
        vcov = ~ state^year,
        data = data[inds, ])
    # Get the lecturer elasticity
    lecturer.elast <- coef(substitution.reg)[1, 3]
    # Get the assistant professor elasticity.
    prof.elast <- coef(substitution.reg)[2, 3]
    # Return the division of the coefficients.
    substitution.est <- lecturer.elast / prof.elast
    return(substitution.est)
}
# bootstrap the function
boot.calc <- boot::boot(reg.data,
    statistic = lecturer_assistant.fun, R = boot.count, parallel = "multicore")
# calculate the standard error
summary(boot.calc)
boot.mean <- mean(boot.calc$t)
boot.se <- sd(boot.calc$t)
boot.ci <- quantile(boot.calc$t, probs = c(0.025, 0.975))
# Check for normality -> result roughly normal
boot.calc$t %>%
    as.data.frame() %>%
    ggplot(aes(V1)) +
    geom_histogram(aes(y = after_stat(density)),
        fill = "lightgray", col = "black") +
    stat_function(fun = dnorm, args = list(mean = boot.mean, sd = boot.se))
print(c("Calculated point est for substitution between lecturers + ast profs",
    boot.mean, "with SEs", boot.se,
    "and 95 % CI", boot.ci))

## write a function to calculate the lecturer <-> full prof elasticity
lecturer_full.fun <- function(data, inds){
    substitution.reg <- fixest::feols(c(
            log(lecturer_prof_count / enrollment_reported),
            log(full_prof_count / enrollment_reported)
        ) ~ 1 |
        unitid + year |
        log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real)),
        vcov = ~ state^year,
        data = data[inds, ])
    # Get the lecturer elasticity
    lecturer.elast <- coef(substitution.reg)[1, 3]
    # Get the assistant professor elasticity.
    prof.elast <- coef(substitution.reg)[2, 3]
    # Return the division of the coefficients.
    substitution.est <- lecturer.elast / prof.elast
    return(substitution.est)
}
# bootstrap the function
boot.calc <- boot::boot(reg.data,
    statistic = lecturer_full.fun, R = boot.count, parallel = "multicore")
# calculate the standard error
summary(boot.calc)
boot.mean <- mean(boot.calc$t)
boot.se <- sd(boot.calc$t)
boot.ci <- quantile(boot.calc$t, probs = c(0.025, 0.975))
# Check for normality -> result roughly normal
boot.calc$t %>%
    as.data.frame() %>%
    ggplot(aes(V1)) +
    geom_histogram(aes(y = after_stat(density)),
        fill = "lightgray", col = "black") +
    stat_function(fun = dnorm, args = list(mean = boot.mean, sd = boot.se))
print(c("Calculated point est for substitution between lecturers + full profs",
    boot.mean, "with SEs", boot.se,
    "and 95 % CI", boot.ci))

## write a function to calculate the assistant <-> full prof elasticity
assistant_full.fun <- function(data, inds){
    substitution.reg <- fixest::feols(c(
            log(assistant_prof_count / enrollment_reported),
            log(full_prof_count / enrollment_reported)
        ) ~ 1 |
        unitid + year |
        log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real)),
        vcov = ~ state^year,
        data = data[inds, ])
    # Get the lecturer elasticity
    assistant.elast <- coef(substitution.reg)[1, 3]
    # Get the assistant professor elasticity.
    prof.elast <- coef(substitution.reg)[2, 3]
    # Return the division of the coefficients.
    substitution.est <- assistant.elast / prof.elast
    return(substitution.est)
}
# bootstrap the function
boot.calc <- boot::boot(reg.data,
    statistic = assistant_full.fun, R = boot.count, parallel = "multicore")
# calculate the standard error
summary(boot.calc)
boot.mean <- mean(boot.calc$t)
boot.se <- sd(boot.calc$t)
boot.ci <- quantile(boot.calc$t, probs = c(0.025, 0.975))
# Check for normality -> result roughly normal
boot.calc$t %>%
    as.data.frame() %>%
    ggplot(aes(V1)) +
    geom_histogram(aes(y = after_stat(density)),
        fill = "lightgray", col = "black") +
    stat_function(fun = dnorm, args = list(mean = boot.mean, sd = boot.se))
print(c("Calculated point est for substitution between ast + full profs",
    boot.mean, "with SEs", boot.se,
    "and 95 % CI", boot.ci))


# Faculty Count (per student) Regressions, by tenured vs non-tenured -----------

## Non-tenured faculty Count
# Naive OLS Regression
naive_nontenured_count.reg <- reg.data %>%
    filter(!is.na(nontenured_tenure_count), nontenured_tenure_count > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(nontenured_tenure_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_nontenured_count.reg <- reg.data %>%
    filter(!is.na(nontenured_tenure_count), nontenured_tenure_count > 0) %>%
    felm(log(nontenured_tenure_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Tenure-track faculty Count
# Naive OLS Regression
naive_tenuretrack_count.reg <- reg.data %>%
    filter(!is.na(tenuretrack_tenure_count), tenuretrack_tenure_count > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(tenuretrack_tenure_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_tenuretrack_count.reg <- reg.data %>%
    filter(!is.na(tenuretrack_tenure_count), tenuretrack_tenure_count > 0) %>%
    felm(log(tenuretrack_tenure_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Tenured faculty Count
# Naive OLS Regression
naive_tenured_count.reg <- reg.data %>%
    filter(!is.na(tenured_tenure_count), tenured_tenure_count > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(tenured_tenure_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_tenured_count.reg <- reg.data %>%
    filter(!is.na(tenured_tenure_count), tenured_tenure_count > 0) %>%
    felm(log(tenured_tenure_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## All faculty Count

# Naive OLS Regression
naive_alltenure_count.reg <- reg.data %>%
    filter(!is.na(all_tenure_count), all_tenure_count > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(all_tenure_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)

# Shift-share IV Regression, explained by state appropriation shock
shiftshare_alltenure_count.reg <- reg.data %>%
    filter(!is.na(all_tenure_count), all_tenure_count > 0) %>%
    felm(log(all_tenure_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

# Collate the results to a LaTeX table
stargazer(
    naive_nontenured_count.reg, shiftshare_nontenured_count.reg,
    naive_tenuretrack_count.reg, shiftshare_tenuretrack_count.reg,
    naive_tenured_count.reg, shiftshare_tenured_count.reg,
    naive_alltenure_count.reg, shiftshare_alltenure_count.reg,
    dep.var.caption = "Dependent Variable: Employment Count by Tenure Group",
    dep.var.labels = c("Non-tenure", "Tenure-Track", "Tenured", "All"),
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
    out = "../../text/tables/tenurecount-shock-reg-fte.tex")


# Research + Instruction Spending Regressions ----------------------------------

# restrict to observations with info on research and instruction spending.
research_instruct.data <- reg.data %>%
    filter(
        !is.na(instructionspending_total_real),
        instructionspending_total_real > 0,
        !is.na(instructionspending_salaries_real),
        instructionspending_salaries_real > 0,
        !is.na(researchspending_total_real),
        researchspending_total_real > 0,
        !is.na(researchspending_salaries_real),
        researchspending_salaries_real > 0)

## Total instruction spending.
# Naive OLS Regression
naive_instruction_spending.reg <- research_instruct.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(instructionspending_total_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_instruction_spending.reg <- research_instruct.data %>%
    felm(log(instructionspending_total_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Total research spending.
# Naive OLS Regression
naive_research_spending.reg <- research_instruct.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(researchspending_total_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_research_spending.reg <- research_instruct.data %>%
    felm(log(researchspending_total_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Total non-operating spending.
# Naive OLS Regression
naive_nonaux_spending.reg <- research_instruct.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(nonauxspending_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_nonaux_spending.reg <- research_instruct.data %>%
    felm(log(nonauxspending_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

# Collate the results to a LaTeX table
stargazer(
    naive_instruction_spending.reg, shiftshare_instruction_spending.reg,
    naive_research_spending.reg, shiftshare_research_spending.reg,
    naive_nonaux_spending.reg, shiftshare_nonaux_spending.reg,
    dep.var.caption = "Dependent Variable: University Expenditures",
    dep.var.labels = c("Instruction", "Research", "All"),
    column.labels = rep(c("OLS", "2SLS"), 3),
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    omit = "factor|count|year",
    intercept.bottom = TRUE,
    order = c(2, 1, 3),
    covariate.labels = c("State Funding", "Tuition Revenue", "Constant"),
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq"),
    #star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/expenditures-shock-reg-fte.tex")


## Salaries instruction spending.
# Naive OLS Regression
naive_instruction_salaries.reg <- research_instruct.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(instructionspending_salaries_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_instruction_salaries.reg <- research_instruct.data %>%
    felm(log(instructionspending_salaries_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Salaries research spending.
# Naive OLS Regression
naive_research_salaries.reg <- research_instruct.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(researchspending_salaries_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_research_salaries.reg <- research_instruct.data %>%
    felm(log(researchspending_salaries_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Total salaries spending.
# Naive OLS Regression
naive_nonaux_salaries.reg <- research_instruct.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(all_profoutlays_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_nonaux_salaries.reg <- research_instruct.data %>%
    felm(log(all_profoutlays_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

# Collate the results to a LaTeX table
stargazer(
    naive_instruction_salaries.reg, shiftshare_instruction_salaries.reg,
    naive_research_salaries.reg, shiftshare_research_salaries.reg,
    naive_nonaux_salaries.reg, shiftshare_nonaux_salaries.reg,
    dep.var.caption = "Dependent Variable: Salary Expenditures",
    dep.var.labels = c("Instruction", "Research", "All"),
    column.labels = rep(c("OLS", "2SLS"), 3),
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
    out = "../../text/tables/expenditures-shock-reg-fte.tex")
