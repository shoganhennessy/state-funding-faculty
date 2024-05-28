#!/usr/bin/R
## Senan Hogan-Hennessy, 3 August 2022
## IV for faculty counts, using IPEDS data.
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

# Define default colours.
colour.list <- c(
    "#D62728", # Red  -> First-stage.
    "#1f77b4", # Blue -> Lecturers
    "#c9721c", # Orange -> Asst Professors
    "#237e23", # Green -> Full Professors
    "#cc2c9f") # Strong pink -> All faculty.


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
    filter(1990 <= year, #year < 2019,
        !is.na(enrollment_reported), enrollment_reported > 0,
        !is.na(all_prof_count),
        stateappropriations_real > 0,
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
            "All faculty count"),
        omit.table.layout = "n",
        header = FALSE, float = FALSE, no.space = TRUE,
        type = "text",
        out = "../../text/tables/ipeds-summary-fte.tex")


# Plot funding sources over time -----------------------------------------------

# SHow how much finding CA gets in 1990 and end
reg.data %>%
    filter(
        nonauxrevenues_real > 0,
        stateappropriations_real > 0,
        tuitionrev_real > 0) %>%
    group_by(state, year) %>%
    summarise(
        totalrevenues_real =
            mean(totalrevenues_real / enrollment_reported, na.rm = TRUE),
        nonauxrevenues_real =
            mean(nonauxrevenues_real / enrollment_reported, na.rm = TRUE),
        stateappropriations_real =
            mean(stateappropriations_real / enrollment_reported, na.rm = TRUE),
        tuitionrev_real =
            mean(tuitionrev_real / enrollment_reported, na.rm = TRUE),
        statelocal_funding_real =
            mean(statelocal_funding_real / enrollment_reported, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(state %in% c("CA", "IL")) %>%
    filter(year %in% c(1990, 2018:2021)) %>%
    print()

# Plot total gov funding by year.
mean_funding_fte.data <- reg.data %>%
    filter(
        nonauxrevenues_real > 0,
        stateappropriations_real > 0,
        tuitionrev_real > 0) %>%
    group_by(year) %>%
    summarise(
        totalrevenues_real =
            mean(totalrevenues_real / enrollment_reported, na.rm = TRUE),
        nonauxrevenues_real =
            mean(nonauxrevenues_real / enrollment_reported, na.rm = TRUE),
        stateappropriations_real =
            mean(stateappropriations_real / enrollment_reported, na.rm = TRUE),
        tuitionrev_real =
            mean(tuitionrev_real / enrollment_reported, na.rm = TRUE)) %>%
    ungroup()

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
        limits = c(0, 25),
        breaks = seq(0, 50, by = 5),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Mean Funding per Student, $ thousands") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "",
        breaks = c("totalrevenues_real", "nonauxrevenues_real",
            "stateappropriations_real", "tuitionrev_real"),
        labels = c("Total", "Total Non-inst.", "State", "Tuition"))
# Save this plot
ggsave("../../text/figures/mean-funding-fte.png",
    plot = mean_funding_fte.graph,
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Plot total gov funding by year.
mean_funding.data <- reg.data %>%
    filter(
        totalrevenues_real > 0,
        nonauxrevenues_real > 0,
        stateappropriations_real > 0,
        tuitionrev_real > 0) %>%
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
        limits = c(0, 302),
        breaks = seq(0, 500, by = 50),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Mean Total Funding, $ millions") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "",
        breaks = c("totalrevenues_real", "nonauxrevenues_real",
            "stateappropriations_real", "tuitionrev_real"),
        labels = c("Total", "Total Non-inst.", "State", "Tuition"))
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
    filter(state == "IL",
        nonauxrevenues_real > 0,
        stateappropriations_real > 0,
        tuitionrev_real > 0) %>%
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
    ggtitle("Mean Funding per Student, $ thousands") +
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
    filter(state == "IL",
        nonauxrevenues_real > 0,
        stateappropriations_real > 0,
        tuitionrev_real > 0) %>%
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
    ggtitle("Total Funding, $ millions") +
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

# Show the percent of spending going to instruction spending.
percent_instruction.plot <- reg.data %>%
    filter(instructionspending_salaries_real > 0,
        nonauxspending_real > 0) %>%
    group_by(year) %>%
    summarise(
        instructionspending_salaries_real =
            sum(instructionspending_salaries_real, na.rm = TRUE),
        nonauxspending_real =
            sum(nonauxspending_real, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(instructionsalaries_percent =
        instructionspending_salaries_real / nonauxspending_real) %>%
    ggplot(aes(x = year, y = instructionsalaries_percent)) +
    geom_bar(stat = "identity", fill = "blue") +
    theme_bw() +
    scale_x_continuous(expand = c(0.005, 0.005),
        name = "Year",
        breaks = seq(1990, 2022, by = 5)) +
    scale_y_continuous(expand = c(0.01, 0),
        name = "",
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1)) +
    ggtitle("Percent of Total Spending going to Instruction Salaries") +
    theme(plot.title = element_text(size = rel(1), hjust = 0),
        plot.title.position = "plot",
        plot.margin = unit(c(0.5, 3, 0, 0), "mm"))
# Save this plot
ggsave("../../text/figures/percent-instruction.png",
    plot = percent_instruction.plot,
    units = "cm", dpi = 300, width = 1.25 * fig.width, height = fig.height)

# Show the percent of spending going to instruction spending + everything else.
percent_spending.plot <- reg.data %>%
    filter(year != 2000) %>%
    group_by(year) %>%
    summarise(
        # Instruction spending.
        instructionspending_salaries = mean(instructionspending_salaries_real, na.rm = TRUE),
        instructionspending_nonsalaries =
            mean((instructionspending_total_real - instructionspending_salaries_real
                ), na.rm = TRUE),
        # Research spending.
        researchspending_salaries = mean(researchspending_salaries_real, na.rm = TRUE),
        researchspending_nonsalaries =
            mean((researchspending_total_real - researchspending_salaries_real
                ), na.rm = TRUE),
        # Other areas.
        exp_pub_serv_real = mean(exp_pub_serv_real, na.rm = TRUE),
        exp_student_serv_real = mean(exp_student_serv_real, na.rm = TRUE),
        exp_acad_supp_real = mean(exp_acad_supp_real, na.rm = TRUE),
        exp_inst_supp_real = mean(exp_inst_supp_real, na.rm = TRUE),
        exp_opm_areal = mean(exp_opm_areal, na.rm = TRUE),
        exp_net_grant_aid_real = mean(exp_net_grant_aid_real, na.rm = TRUE),
        nonauxspending_real = mean(nonauxspending_real, na.rm = TRUE)) %>%
    ungroup() %>%
    transmute(year = year,
        instructionspending_salaries = instructionspending_salaries / nonauxspending_real,
        instructionspending_nonsalaries = instructionspending_nonsalaries / nonauxspending_real,
        researchspending_salaries = researchspending_salaries / nonauxspending_real,
        researchspending_nonsalaries = researchspending_nonsalaries / nonauxspending_real,
        exp_pub_serv_real = exp_pub_serv_real / nonauxspending_real,
        exp_student_serv_real = exp_student_serv_real / nonauxspending_real,
        exp_acad_supp_real = exp_acad_supp_real / nonauxspending_real,
        exp_inst_supp_real = exp_inst_supp_real / nonauxspending_real,
        exp_opm_areal = exp_opm_areal / nonauxspending_real,
        exp_net_grant_aid_real = exp_net_grant_aid_real / nonauxspending_real,
        #other = (1 - instructionspending_salaries +
        #    instructionspending_nonsalaries +
        #    researchspending_salaries +
        #    researchspending_nonsalaries +
        #    exp_pub_serv_real +
        #    exp_student_serv_real +
        #    exp_acad_supp_real +
        #    exp_inst_supp_real +
        #    exp_opm_areal +
        #    exp_net_grant_aid_real)
        ) %>%
    pivot_longer(!year, names_to = "category", values_to = "percent") %>%
    ggplot(aes(x = year, y = percent, fill = category)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    scale_x_continuous(expand = c(0.005, 0.005),
        name = "Year",
        breaks = seq(1990, 2022, by = 5)) +
    scale_y_continuous(expand = c(0.01, 0),
        name = "",
        limits = c(0, 1.01),
        breaks = seq(0, 1, by = 0.1)) +
    scale_fill_manual(
        values = viridis::plasma(10),
        limits = c(
            "instructionspending_salaries",
            "instructionspending_nonsalaries",
            "researchspending_salaries",
            "researchspending_nonsalaries",
            "exp_pub_serv_real",
            "exp_student_serv_real",
            "exp_acad_supp_real",
            "exp_inst_supp_real",
            "exp_opm_areal",
            "exp_net_grant_aid_real"), #"other")
        labels = c("Instruction, salaries",
            "Instruction, general",
            "Research, salaries",
            "Research, general",
            "Public service",
            "Student services",
            "Acad. support",
            "Inst. support",
            "Operations",
            "Grant Aid")) + #"Other")
    ggtitle("Percent of Total Spending going to Different Areas") +
    theme(plot.title = element_text(size = rel(1), hjust = 0),
        plot.title.position = "plot",
        plot.margin = unit(c(0.5, 3, 0, 0), "mm"),
        legend.position = "top",
        legend.title = element_blank()) +
    guides(fill = guide_legend(ncol = 4))
#viridis::viridis(10)
#viridis::magma(n)
#viridis::inferno(n)
#viridis::plasma(n)
# Save this plot
ggsave("../../text/figures/percent-spending.png",
    plot = percent_spending.plot,
    units = "cm", dpi = 300, width = 1.5 * fig.width, height = 1.25 * fig.height)


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
        "All faculty"),
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
        `IV Components, \\$ per student:` = NA,
        `Funding shift--share`        = round(mean(- appropriationshock_perEnroll_real, na.rm = TRUE)),
        `Shift in state--wide funding`   = round(mean(- allstate_stateappropriations_real / stateEnroll_count, na.rm = TRUE)),
        `Share reliance on state funding, \\% in 1990--1993`    = round(mean(staterevenues_baseshare, na.rm = TRUE), 2),
        `\\hline University Funding and Spending, \\$ millions:` = NA,
        `State funding`               = round(mean(stateappropriations_real / 10^6, na.rm = TRUE)),
        `Tuition revenue`             = round(mean(tuitionrev_real / 10^6, na.rm = TRUE)),
        `Total non-inst. revenues`    = round(mean(nonauxrevenues_real / 10^6, na.rm = TRUE)),
        `Instruction spending`        = round(mean(instructionspending_total_real / 10^6, na.rm = TRUE)),
        `Research Spending`           = round(mean(researchspending_total_real / 10^6, na.rm = TRUE)),
        `\\hline University Funding and Spending, \\$ per student` = NA,
        `fte State funding`              = round(mean(stateappropriations_real / enrollment_reported, na.rm = TRUE)),
        `fte Tuition revenue`            = round(mean(tuitionrev_real / enrollment_reported, na.rm = TRUE)),
        `fte Total non-inst. revenues`   = round(mean(nonauxrevenues_real / enrollment_reported, na.rm = TRUE)),
        `fte Instruction spending`       = round(mean(instructionspending_total_real / enrollment_reported, na.rm = TRUE)),
        `fte Research spending`          = round(mean(researchspending_total_real / enrollment_reported, na.rm = TRUE)),
        `\\hline Selectivity:` = NA,
        `Reported enrolment`            = round(mean(enrollment_reported, na.rm = TRUE)),
        `Full-time equivalent enrolment` = round(mean(enrollment_fte, na.rm = TRUE)),
        `Acceptance rate, \\%`          = round(mean(acceptance_rate, na.rm = TRUE), 2),
        `6 Year graduation rate, \\%`   = round(mean(completion_rate_150pct, na.rm = TRUE), 2)) %>%
    ungroup() %>%
    pivot_longer(!instrument_quantile,
        names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = "instrument_quantile",
        values_from = "value") %>%
    mutate(variable = str_replace(variable, "fte ", "")) %>%
    transmute(`Instrument Quantile:` = variable,
        `1st` = `0`,
        `2nd` = `1`,
        `3rd` = `2`,
        `4th` = `3`,
        `5th` = `4`) %>%
    xtable(type = "latex",
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
    covariate.labels = c("Funding Shift-Share", "Tuition Revenue", "Constant"),
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
    geom_point(aes(y = estimate), colour = colour.list[1]) +
    geom_line(aes(y = estimate), colour = colour.list[1]) +
    geom_line(aes(y = conf.low), linetype = "dashed") +
    geom_line(aes(y = conf.high), linetype = "dashed") +
    scale_x_continuous(name = "Years, Relative to Funding Cut",
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
    out = "../../text/tables/facultycount-shock-reg-fte.tex")

# Plot the results.
# Estimate with separate names
plot_lecturer_count.reg <- reg.data %>%
    mutate(lect_funding = - log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(lecturer_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (lect_funding ~ I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
plot_assistant_count.reg <- reg.data %>%
    mutate(asst_funding = - log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(assistant_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (asst_funding ~ I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
plot_full_count.reg <- reg.data %>%
    mutate(full_funding = - log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(full_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (full_funding ~ I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
plot_all_count.reg <- reg.data %>%
    mutate(all_funding = - log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(all_prof_count / enrollment_reported) ~ 1 |
        unitid + year |
        (all_funding ~ I(-log(appropriationshock_perEnroll_real))) |
        state + year,
        data = .)
elasticity.plot <- plot_summs(
    plot_lecturer_count.reg,
    plot_assistant_count.reg,
    plot_full_count.reg,
    plot_all_count.reg,
    coefs = c(
        "Lecturers" = "`lect_funding(fit)`",
        "Asst. Professors" = "`asst_funding(fit)`",
        "Full Professors" = "`full_funding(fit)`",
        "All Faculty" = "`all_funding(fit)`"),
    legend.title = "",
    plot.distributions = TRUE,
    inner_ci_level = 0.95) +
    theme_bw() +
    theme(
        #plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "none",
        legend.margin = margin(t = -10)) +
    scale_x_continuous(expand = c(0, 0),
        name = "IV Estimate",
        breaks = seq(-1, 1, by = 0.1))
# Save this plot
ggsave("../../text/figures/elasticity-plot.png",
    plot = elasticity.plot,
    units = "cm", dpi = 300, width = 1.25 * fig.width, height = 0.8 * fig.height)


################################################################################
## exogenoeity of instrument relative to outcomes:
# See if preceding faculty outcomes are related to funding shifts.
all_coefficient.list <- c()
all_upper_ci.list <- c()
all_lower_ci.list <- c()

# Loop across time periods, to estimate the correlation between years.
lag.levels <- -7:5
for (t in lag.levels){
    print(t)
    # All Faculty
    lag.reg <- reg.data %>%
        mutate(lag_log_outcome_fte = log(lag_lead(
            all_prof_count / enrollment_fte, t))) %>%
        felm(lag_log_outcome_fte ~ 1 +
            I(-log(appropriationshock_perEnroll_real)) |
            unitid + year |
            0 |
            state + year,
            data = .) %>%
        summary()
    all_ci.halflength <- 1.96 * lag.reg$coefficients[2]
    all_coefficient.list <- c(all_coefficient.list, lag.reg$coefficients[1])
    all_upper_ci.list <- c(all_upper_ci.list, lag.reg$coefficients[1] + ci.halflength)
    all_lower_ci.list <- c(all_lower_ci.list, lag.reg$coefficients[1] - ci.halflength)
}
# Collect together
faculty_lag.data <- data.frame(
    lag.levels,
    all_coefficient.list,
    all_upper_ci.list,
    all_lower_ci.list)
# Plot the lagged results.
faculty_lag.plot <- faculty_lag.data %>%
    ggplot(aes(x = lag.levels)) +
    geom_vline(xintercept = 0, alpha = 0.5) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    #  All prof
    geom_ribbon(aes(ymin = all_lower_ci.list, ymax = all_upper_ci.list),
        alpha = 0.4, fill = "grey70") +
    geom_line(aes(y = all_lower_ci.list), linetype = "dashed") +
    geom_line(aes(y = all_upper_ci.list), linetype = "dashed") +
    geom_point(aes(y = all_coefficient.list), colour = "orange") +
    geom_line(aes(y = all_coefficient.list), colour = "orange") +
    scale_x_continuous(name = "Years, Relative to Funding Cut",
        breaks = lag.data$t, expand = c(0.01, 0.01)) +
    scale_y_continuous(name = "",
        #limits = c(-0.2, 0.75),
        breaks = seq(-5, 5, by = 0.1)) +
    theme_bw() +
    ggtitle("Estimate") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 1, 0, 0), "mm"))
# Save this plot.
ggsave("../../text/figures/faculty-dynamic-balance.png",
    plot = faculty_lag.plot,
    units = "cm", dpi = 300, width = 1.5 * fig.width, height = fig.height)



# Get the rates of substituion from the elasticity estimates -------------------

# Plot the elasticities for profesorrs w.r.t state funding
fixest::coefplot(list(
    shiftshare_lecturer_count.reg, shiftshare_all_count.reg,
    shiftshare_assistant_count.reg, shiftshare_full_count.reg))
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html#plot_summs()_and_plot_coefs()

# Calculate the elasticities of susbtituion, with SEs bootstrapped.
boot.count <- 10

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


# Research + Instruction Spending Regressions ----------------------------------

# Restrict to spending data with no missings
spending.data <- reg.data %>%
    filter(nonauxspending_real > 0)

## Total instruction spending.
# Naive OLS Regression
naive_instruction_spending.reg <- spending.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(instructionspending_total_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_instruction_spending.reg <- spending.data %>%
    felm(log(instructionspending_total_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Total research spending.
# Naive OLS Regression
naive_research_spending.reg <- spending.data %>%
    filter(researchspending_total_real > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(researchspending_total_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_research_spending.reg <- spending.data %>%
    filter(researchspending_total_real > 0) %>%
    felm(log(researchspending_total_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Public service spending.
# Naive OLS Regression
naive_pub_serv_spending.reg <- spending.data %>%
    filter(exp_pub_serv_real > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(exp_pub_serv_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_pub_serv_spending.reg <- spending.data %>%
    filter(exp_pub_serv_real > 0) %>%
    felm(log(exp_pub_serv_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Student services spending.
# Naive OLS Regression
naive_student_serv_spending.reg <- spending.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(exp_student_serv_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_student_serv_spending.reg <- spending.data %>%
    felm(log(exp_student_serv_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Academic support spending.
# Naive OLS Regression
naive_acad_supp_spending.reg <- spending.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(exp_acad_supp_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_acad_supp_spending.reg <- spending.data %>%
    felm(log(exp_acad_supp_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Institutional support spending.
# Naive OLS Regression
naive_inst_supp_spending.reg <- spending.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(exp_inst_supp_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_inst_supp_spending.reg <- spending.data %>%
    felm(log(exp_inst_supp_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Operations spending.
# Naive OLS Regression
naive_opm_spending.reg <- spending.data %>%
    filter(exp_opm_areal > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(exp_opm_areal / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_opm_spending.reg <- spending.data %>%
    filter(exp_opm_areal > 0) %>%
    felm(log(exp_opm_areal / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Grant + aid spending
# Naive OLS Regression
naive_grant_aid_spending.reg <- spending.data %>%
    filter(exp_net_grant_aid_real > 0) %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(exp_net_grant_aid_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_grant_aid_spending.reg <- spending.data %>%
    filter(exp_net_grant_aid_real > 0) %>%
    felm(log(exp_net_grant_aid_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Total non-operating spending.
# Naive OLS Regression
naive_nonaux_spending.reg <- spending.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(nonauxspending_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_nonaux_spending.reg <- spending.data %>%
    felm(log(nonauxspending_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

## Tuition revenue.
# Naive OLS Regression
naive_tuition_rev.reg <- spending.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(tuitionrev_real / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_tuition_rev.reg <- spending.data %>%
    felm(log(tuitionrev_real / enrollment_reported) ~ 1 |
        unitid + year |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        unitid + year,
        data = .)

# Collate the outcome means
outcome.means <- list(c("Outcome Mean",
    spending.data %>% mutate(instructionspending_total_real = (instructionspending_total_real / 10^3) /  enrollment_reported) %>% pull(instructionspending_total_real) %>% mean(na.rm = TRUE) %>% round(digits.no),
    spending.data %>% mutate(researchspending_total_real = (researchspending_total_real / 10^3) /  enrollment_reported) %>% pull(researchspending_total_real)  %>% mean(na.rm = TRUE) %>% round(digits.no),
    spending.data %>% mutate(exp_pub_serv_real = (exp_pub_serv_real / 10^3) /  enrollment_reported) %>% pull(exp_pub_serv_real) %>% mean(na.rm = TRUE) %>% round(digits.no),
    spending.data %>% mutate(exp_student_serv_real = (exp_student_serv_real / 10^3) /  enrollment_reported) %>% pull(exp_student_serv_real) %>% mean(na.rm = TRUE) %>% round(digits.no),
    spending.data %>% mutate(exp_acad_supp_real = (exp_acad_supp_real / 10^3) /  enrollment_reported) %>% pull(exp_acad_supp_real) %>% mean(na.rm = TRUE) %>% round(digits.no),
    spending.data %>% mutate(exp_inst_supp_real = (exp_inst_supp_real / 10^3) /  enrollment_reported) %>% pull(exp_inst_supp_real) %>% mean(na.rm = TRUE) %>% round(digits.no),
    spending.data %>% mutate(exp_opm_areal = (exp_opm_areal / 10^3) /  enrollment_reported) %>% pull(exp_opm_areal) %>% mean(na.rm = TRUE) %>% round(digits.no),
    spending.data %>% mutate(exp_net_grant_aid_real = (exp_net_grant_aid_real / 10^3) /  enrollment_reported) %>% pull(exp_net_grant_aid_real) %>% mean(na.rm = TRUE) %>% round(digits.no),
    spending.data %>% mutate(nonauxspending_real = (nonauxspending_real / 10^3) /  enrollment_reported) %>% pull(nonauxspending_real) %>% mean(na.rm = TRUE) %>% round(digits.no),
    spending.data %>% mutate(tuitionrev_real = (tuitionrev_real / 10^3) /  enrollment_reported) %>% pull(tuitionrev_real) %>% mean(na.rm = TRUE) %>% round(digits.no))
)

# Collate the results to a LaTeX table
stargazer(
    # naive_instruction_spending.reg,
    shiftshare_instruction_spending.reg,
    # naive_research_spending.reg,
    shiftshare_research_spending.reg,
    # naive_pub_serv_spending.reg,
    shiftshare_pub_serv_spending.reg,
    # naive_student_serv_spending.reg,
    shiftshare_student_serv_spending.reg,
    # naive_acad_supp_spending.reg,
    shiftshare_acad_supp_spending.reg,
    # naive_inst_supp_spending.reg,
    shiftshare_inst_supp_spending.reg,
    # naive_opm_spending.reg,
    shiftshare_opm_spending.reg,
    # naive_grant_aid_spending.reg,
    shiftshare_grant_aid_spending.reg,
    # naive_nonaux_spending.reg,
    shiftshare_nonaux_spending.reg,
    # naive_tuition_rev.reg,
    shiftshare_tuition_rev.reg,
    add.lines = outcome.means,
    dep.var.caption = "Dependent Variable: University Spending",
    dep.var.labels = c(
        #"Instruction",
        #"Research",
        #"Public Service",
        #"Student Services",
        #"Academic Support",
        #"Inst. Support",
        #"Operations",
        #"Grant Aid",
        #"Tuition Revenue",
        #"All Spending",
        #"Tuition Revenue"),
        "\\multirow{2}{1.1cm}{Instruction}",
        "\\multirow{2}{1cm}{Research}",
        "\\multirow{2}{0.8cm}{Public \\\\ Service}",
        "\\multirow{2}{1cm}{Student \\\\ Services}",
        "\\multirow{2}{1cm}{Academic \\\\ Support}",
        "\\multirow{2}{1cm}{Inst. \\\\ Support}",
        "\\multirow{2}{1cm}{Operations}",
        "\\multirow{2}{1cm}{Grant \\\\ Aid}",
        "\\multirow{2}{1cm}{All \\\\ Spending}",
        "\\multirow{2}{1cm}{Tuition \\\\ Revenue} \\\\"),
    digits = digits.no,
    digits.extra = digits.no,
    model.names = FALSE,
    model.numbers = FALSE,
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


################################################################################
## Show rate of part-time lecturers in the incomplete recent data.

# Show which years these data are available for.
reg.data %>%
    filter(!is.na(lecturer_parttime_count),
        !is.na(lecturer_fulltime_count)) %>%
    group_by(year) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    print()

# Look at the number of part time lecturers at UICHICAGO
reg.data %>%
    filter(unitid == 145600, year > 2004) %>%
    select(unitid, year, state,
        lecturer_parttime_count,
        assistant_parttime_count,
        full_parttime_count,
        lecturer_fulltime_count,
        assistant_fulltime_count,
        full_fulltime_count) %>%
    View()

# Get the rate of part time lecturers in each year.
count_lecturers.data <- reg.data %>%
    filter(!is.na(lecturer_parttime_count), lecturer_parttime_count > 0,
        !is.na(lecturer_fulltime_count), lecturer_fulltime_count > 0) %>%
    group_by(year) %>%
    summarise(
        lecturer_parttime_count = sum(lecturer_parttime_count, na.rm = TRUE),
        lecturer_fulltime_count = sum(lecturer_fulltime_count, na.rm = TRUE)) %>%
    ungroup()

# Show the percent who are part-time lecturers.
count_lecturers.plot <- count_lecturers.data %>%
    filter(year > 2004) %>%
    mutate(lecturer_parttime_rate = lecturer_parttime_count / (
            lecturer_parttime_count + lecturer_fulltime_count)) %>%
    ggplot(aes(x = year, y = lecturer_parttime_rate)) +
    geom_bar(stat = "identity", fill = "red") +
    #annotate("text", colour = "red",
    #    x = 1, y = 80,
    #    label = expression("Mean" %~~% " +45%"),
    #    size = 4.25, fontface = "bold") +
    theme_bw() +
    scale_x_continuous(expand = c(0.005, 0.005),
        name = "Year",
        breaks = seq(2000, 2022, by = 2)) +
    scale_y_continuous(expand = c(0.01, 0),
        name = "",
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1)) +
    ggtitle("Percent of Lecturers Working Part-time") +
    theme(plot.title = element_text(size = rel(1), hjust = 0),
        plot.title.position = "plot",
        plot.margin = unit(c(0.5, 3, 0, 0), "mm"))
# Save this plot
ggsave("../../text/figures/partime-lecturers.png",
    plot = count_lecturers.plot,
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# SHow the amount of spending that goes to instructional salaries (total).
instruct_spending.plot <- reg.data %>%
    filter(instructionspending_salaries_real > 0, nonauxspending_real > 0) %>%
    group_by(year) %>%
    summarise(instructionspending_salaries_fraction =
        mean(instructionspending_salaries_real / nonauxspending_real)) %>%
    ggplot(aes(x = year, y = instructionspending_salaries_fraction)) +
    #geom_point(colour = "blue") +
    geom_bar(stat = "identity", fill = "blue") +
    theme_bw() +
    scale_x_continuous(expand = c(0.005, 0.005),
        name = "Year",
        breaks = seq(1990, 2022, by = 5)) +
    scale_y_continuous(expand = c(0.01, 0),
        name = "",
        limits = c(0, 0.35),
        breaks = seq(0, 1, by = 0.05)) +
    ggtitle("Spending on Faculty Salaries as a Percent of Total") +
    theme(plot.title = element_text(size = rel(1), hjust = 0),
        plot.title.position = "plot")
# Save this plot
ggsave("../../text/figures/instruct-spending.png",
    plot = instruct_spending.plot,
    units = "cm", dpi = 300, width = 1.25 * fig.width, height = fig.height)
