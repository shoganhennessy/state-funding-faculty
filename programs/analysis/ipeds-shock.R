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
fig.width <- 9
fig.height <- fig.width * 0.85


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
        !is.na(appropriationshock_perEnroll_real))


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
            "State appropriations (millions 2021 USD)",
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

# Summary Statistics for the Quantiles of the shift-share instrument.
quantile.count <- 5
reg.data %>%
    mutate(instrument_quantile = round((quantile.count - 1) *
        ecdf(appropriationshock_perEnroll_real)(
            appropriationshock_perEnroll_real))) %>%
    group_by(instrument_quantile) %>%
    summarise(
        `State Funding Shock, \\$ per student` = round(mean(appropriationshock_perEnroll_real, an.rm = TRUE)),
        `State Funding, per student`           = round(mean(stateappropriations_real / enrollment_reported, na.rm = TRUE)),
        `Total Full-time Enrolment`            = round(mean(enrollment_reported, na.rme = TRUE)),
        `State Funding, \\$ millions`          = round(mean(stateappropriations_real / 10^6, na.rme = TRUE)),
        `Total Revenues, \\$ millions`         = round(mean(totalrevenues_real / 10^6, na.rme = TRUE)),
        `Total Revenues, \\$ per student`      = round(mean(totalrevenues_real / enrollment_reported, na.rm = TRUE)),
        `Lecturer Count`                       = round(mean(lecturer_prof_count, na.rm = TRUE)),
        `Assistant Professor Count`            = round(mean(assistant_prof_count, na.rm = TRUE)),
        `Full Professor Count`                 = round(mean(full_prof_count, na.rm = TRUE)),
        `Total Professor Count`                = round(mean(all_prof_count, na.rm = TRUE)),
        `Lecturers, per student`               = mean(lecturer_prof_count / enrollment_reported, na.rm = TRUE),
        `Assistant Professors, per student`    = mean(assistant_prof_count / enrollment_reported, na.rm = TRUE),
        `Full Professors, per student`         = mean(full_prof_count / enrollment_reported, na.rm = TRUE),
        `Total Professors, per student`        = mean(all_prof_count / enrollment_reported, na.rm = TRUE)
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
        digits = digits.no,
        align = "llccccc") %>%
    print(
        sanitize.colnames.function = identity,
        sanitize.text.function = identity,
        format.args = list(big.mark = ",", decimal.mark = "."),
        floating = FALSE,
        floating.environment = "tabular",
        include.rownames = FALSE,
        file = "../../text/tables/summary-quantiles.tex")


# Plot funding sources over time -----------------------------------------------

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
        limits = c(0, 255),
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
    ggtitle("Funding, $ thousands") +
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
    covariate.labels = c("Appropriations Shock", "Tuition Revenue", "Constant"),
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


# Faculty Count Regressions ----------------------------------------------------

## Non-tenured faculty Count
lecturer.data <- reg.data %>%
    filter(!is.na(lecturer_prof_count), lecturer_prof_count > 0)
# Naive OLS Regression
naive_lecturer_count.reg <- lecturer.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- lecturer.data %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Tenure-track faculty Count
assistant.data <- reg.data %>%
    filter(!is.na(assistant_prof_count), assistant_prof_count > 0)
# Naive OLS Regression
naive_assistant_count.reg <- assistant.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- assistant.data %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)


## Tenured faculty Count
full.data <- reg.data %>%
    filter(!is.na(full_prof_count), full_prof_count > 0)
# Naive OLS Regression
naive_full_count.reg <- full.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- full.data %>%
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

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_count.reg, shiftshare_lecturer_count.reg,
    naive_assistant_count.reg, shiftshare_assistant_count.reg,
    naive_full_count.reg, shiftshare_full_count.reg,
    naive_all_count.reg, shiftshare_all_count.reg,
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


# Faculty Count (per student) Regressions --------------------------------------

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


# Faculty Salary Regressions ---------------------------------------------------

## Non-tenured faculty salaries
lecturer.data <- reg.data %>%
    filter(!is.na(lecturer_profmeansalary_real),
        lecturer_profmeansalary_real > 0)
# Naive OLS Regression
naive_lecturer_salaries.reg <- lecturer.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(lecturer_profmeansalary_real) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_salaries.reg <- lecturer.data %>%
    felm(log(lecturer_profmeansalary_real) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Tenure-track faculty salaries
assistant.data <- reg.data %>%
    filter(!is.na(assistant_profmeansalary_real),
        assistant_profmeansalary_real > 0)
# Naive OLS Regression
naive_assistant_salaries.reg <- assistant.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(assistant_profmeansalary_real) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_salaries.reg <- assistant.data %>%
    felm(log(assistant_profmeansalary_real) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## Tenured faculty salaries
full.data <- reg.data %>%
    filter(!is.na(full_profmeansalary_real),
        full_profmeansalary_real > 0)
# Naive OLS Regression
naive_full_salaries.reg <- full.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(full_profmeansalary_real) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_salaries.reg <- full.data %>%
    felm(log(full_profmeansalary_real) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

## All faculty salaries
all.data <- reg.data %>%
    filter(!is.na(all_profmeansalary_real), all_profmeansalary_real > 0)
# Naive OLS Regression
naive_all_salaries.reg <- all.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(all_profmeansalary_real) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_salaries.reg <- all.data %>%
    felm(log(all_profmeansalary_real) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)

# Collate the results to a LaTeX table
stargazer(
    naive_lecturer_salaries.reg, shiftshare_lecturer_salaries.reg,
    naive_assistant_salaries.reg, shiftshare_assistant_salaries.reg,
    naive_full_salaries.reg, shiftshare_full_salaries.reg,
    naive_all_salaries.reg, shiftshare_all_salaries.reg,
    dep.var.caption = "Dependent Variable: Mean Salary by Professor Group",
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
    out = "../../text/tables/facultysalaries-shock-reg-fte.tex")


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



# Local Projections for staying-power of effects -------------------------------

# https://cran.r-project.org/web/packages/lpirfs/lpirfs.pdf
library(plm)
library(lpirfs)
time.horizon <- 10

# Define data sample for the LP estimation
lp.data <- reg.data %>%
    transmute(unitid = unitid,
        year = year,
        instid = factor(unitid),
        academicyear = factor(year),
        all_prof_count = log(all_prof_count / enrollment_reported),
        lecturer_prof_count = log(lecturer_prof_count / enrollment_reported),
        assistant_prof_count = log(assistant_prof_count / enrollment_reported),
        full_prof_count = log(full_prof_count / enrollment_reported),
        all_profmeansalary_real = log(all_profmeansalary_real),
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
        # Contemporaneous control, FE for unitid + year
        c_exog_data = c("instid", "academicyear"),
        # Option to use IV for predictor endogeneity (not used here)
        iv_reg = FALSE,
        # Add fixed effects + clsutered SEs in the panel
        panel_model = "within",
        panel_effect = "twoways",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
plot(firststage.lpreg)
# Save this plot
ggsave("../../text/figures/firststage-lp.png",
    plot = plot(firststage.lpreg),
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Run the LP method for the second-stage, outcome count lecturers per student
lecturer_count.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "lecturer_prof_count",
        # Predictor variable
        shock = "stateappropriations_real",
        # Contemporaneous control, FE for unitid + year
        c_exog_data = c("instid", "academicyear"),
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # Add fixed effects + clsutered SEs in the panel
        panel_model = "within",
        panel_effect = "twoways",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/lecturer-count-lp.png",
    plot = plot(lecturer_count.lpreg),
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Run the LP method for the second-stage, outcome count APs per student
assistant_count.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "assistant_prof_count",
        # Predictor variable
        shock = "stateappropriations_real",
        # Contemporaneous control, FE for unitid + year
        c_exog_data = c("instid", "academicyear"),
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # Add fixed effects + clsutered SEs in the panel
        panel_model = "within",
        panel_effect = "twoways",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/assistant-count-lp.png",
    plot = plot(assistant_count.lpreg),
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Run the LP method for the second-stage, outcome count APs per student
full_count.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "full_prof_count",
        # Predictor variable
        shock = "stateappropriations_real",
        # Contemporaneous control, FE for unitid + year
        c_exog_data = c("instid", "academicyear"),
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # Add fixed effects + clsutered SEs in the panel
        panel_model = "within",
        panel_effect = "twoways",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/full-count-lp.png",
    plot = plot(full_count.lpreg),
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Run the LP method for the second-stage, outcome count profs per student
all_count.lpreg <-
    lp_lin_panel(data_set = lp.data,
        # Outcome variable
        endog_data = "all_prof_count",
        # Predictor variable
        shock = "stateappropriations_real",
        # Contemporaneous control, FE for unitid + year
        c_exog_data = c("instid", "academicyear"),
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # Add fixed effects + clsutered SEs in the panel
        panel_model = "within",
        panel_effect = "twoways",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
# Save this plot
ggsave("../../text/figures/all-count-lp.png",
    plot = plot(all_count.lpreg),
    units = "cm", dpi = 300, width = fig.width, height = fig.height)
