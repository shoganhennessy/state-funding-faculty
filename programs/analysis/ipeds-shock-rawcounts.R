#!/usr/bin/R
## Senan Hogan-Hennessy, 5 July 2023
## IV for Prof's salaries, using IPEDS data.
print(Sys.time())
library(tidyverse) # Functions for data manipulation and visualization
library(lfe) # Functions for fast linear models with IV + FEs
library(plm) # Functions for panel data
library(car) # Function for F stat regarding IV models
# My custom flavour of Stargazer TeX tables:
# devtools::install_github("shoganhennessy/stargazer")
# TeX tables:
library(stargazer)
library(xtable)
set.seed(47)
# This file follows an adjusted Deming Walters (2017, p.10) approach to
# Funding Shock instrument.

# Define number of digits in tables and graphs
digits.no <- 3

# Size for figures
fig.width <- 10
fig.height <- fig.width * 0.85

# Define a function to standardise the dplyr lag function
# --- negative numbers mean a lag of t time periods, positive a lead of t.
lag_lead <- function(column, t){
    if (t < 0){
        lag_column <- dplyr::lag(column, -t)
    }
    else if (t >= 0){
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
        nonauxspending_real)


# Restrict data to unis + years with measured state appropriations & shocks
reg.data <- reg.data %>%
    filter(1990 <= year,
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


# First stage Regressions ------------------------------------------------------

# Explain Revenues with a shock to (only) state appropriations.
firststage_raw.reg <- reg.data %>%
    felm(I(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Get the F.Stat
firststage_raw.fstat <-
    linearHypothesis(firststage_raw.reg, test = "F",
        c("I(-appropriationshock_perEnroll_real)"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Without the FEs
# Explain State appropriations with a shock to (only) state appropriations.
firststage_raw_noFE.reg <- reg.data %>%
    felm(I(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-appropriationshock_perEnroll_real) |
        0 |
        0 |
        state + year,
        data = .)
# Get the F.Stat
firststage_raw_noFE.fstat <-
    linearHypothesis(firststage_raw_noFE.reg, test = "F",
        c("I(-appropriationshock_perEnroll_real)=0"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Explain Revenues with a shock to (only) state appropriations.
firststage_raw_tuit.reg <- reg.data %>%
    felm(I(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-appropriationshock_perEnroll_real) +
        I(tuitionrev_real / enrollment_reported) |
        unitid + year |
        0 |
        state + year,
        data = .)
# Get the F.Stat
firststage_raw_tuit.fstat <-
    linearHypothesis(firststage_raw_tuit.reg, test = "F",
        c("I(-appropriationshock_perEnroll_real)=0"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Without the FEs
# Explain State appropriations with a shock to (only) state appropriations.
firststage_raw_tuit_noFE.reg <- reg.data %>%
    felm(I(stateappropriations_real / enrollment_reported) ~ 1 +
        I(-appropriationshock_perEnroll_real) +
        I(tuitionrev_real / enrollment_reported) |
        0 |
        0 |
        state + year,
        data = .)
# Get the F.Stat
firststage_raw_tuit_noFE.fstat <-
    linearHypothesis(firststage_raw_tuit_noFE.reg, test = "F",
        c("I(-appropriationshock_perEnroll_real)=0"))["F"] %>%
    unlist() %>%
    nth(2) %>%
    as.numeric() %>%
    round(digits.no)

# Collate the results to a LaTeX table
stargazer(
    firststage_raw.reg, firststage_raw_noFE.reg,
    firststage_raw_tuit.reg, firststage_raw_tuit_noFE.reg,
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
            firststage_raw_tuit.fstat, firststage_raw_tuit_noFE.fstat,
            firststage_raw.fstat, firststage_raw_noFE.fstat)),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/firststage-reg-rawcount.tex")


# Faculty Count Regressions, in raw counts -------------------------------------

## Lecturer faculty Count
# Naive OLS Regression
naive_lecturer_count.reg <- reg.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        I(stateappropriations_real/(1000 * enrollment_reported))) %>%
    felm(lecturer_prof_count ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_lecturer_count.reg <- reg.data %>%
    felm(lecturer_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

## assistant professor Count
# Naive OLS Regression
naive_assistant_count.reg <- reg.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        I(stateappropriations_real/(1000 * enrollment_reported))) %>%
    felm(assistant_prof_count ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_assistant_count.reg <- reg.data %>%
    felm(assistant_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

## Full professor faculty Count
# Naive OLS Regression
naive_full_count.reg <- reg.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        I(stateappropriations_real/(1000 * enrollment_reported))) %>%
    felm(full_prof_count ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_full_count.reg <- reg.data %>%
    felm(full_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)

## All faculty Count
# Naive OLS Regression
naive_all_count.reg <- reg.data %>%
    mutate(`I(stateappropriations_real/(1000 * enrollment_reported))(fit)` =
        I(stateappropriations_real/(1000 * enrollment_reported))) %>%
    felm(all_prof_count ~ 1 +
        `I(stateappropriations_real/(1000 * enrollment_reported))(fit)` |
        unitid + year |
        0 |
        state + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_all_count.reg <- reg.data %>%
    felm(all_prof_count ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (1000 * enrollment_reported)) ~
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
    omit = "factor|count|year",
    intercept.bottom = TRUE,
    order = c(2, 1, 3),
    covariate.labels = c("State Funding", "Tuition Revenue", "Constant"),
    omit.stat = c("LL", "ser", "aic", "wald", "adj.rsq"),
    star.cutoffs = NA,
    header = FALSE, float = FALSE, no.space = TRUE,
    omit.table.layout = "n", notes.append = FALSE,
    type = "text",
    out = "../../text/tables/facultycount-shock-reg-rawcount.tex")

# Plot the results.
# Estimate with separate names
plot_lecturer_count.reg <- reg.data %>%
    mutate(lect_funding = - (stateappropriations_real / (1000 * enrollment_reported))) %>%
    felm(lecturer_prof_count ~ 1 |
        unitid + year |
        (lect_funding ~ I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)
plot_assistant_count.reg <- reg.data %>%
    mutate(asst_funding = - (stateappropriations_real / (1000 * enrollment_reported))) %>%
    felm(assistant_prof_count ~ 1 |
        unitid + year |
        (asst_funding ~ I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)
plot_full_count.reg <- reg.data %>%
    mutate(full_funding = - (stateappropriations_real / (1000 * enrollment_reported))) %>%
    felm(full_prof_count ~ 1 |
        unitid + year |
        (full_funding ~ I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)
plot_all_count.reg <- reg.data %>%
    mutate(all_funding = - (stateappropriations_real / (1000 * enrollment_reported))) %>%
    felm(all_prof_count ~ 1 |
        unitid + year |
        (all_funding ~ I(-appropriationshock_perEnroll_real)) |
        state + year,
        data = .)
substitution_rawcount.plot <-
    jtools::plot_summs(
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
        breaks = seq(-14, 14, by = 2))
# Save this plot
ggsave("../../text/figures/substitution-rawcount-plot.png",
    plot = substitution_rawcount.plot,
    units = "cm", dpi = 300, width = 1.25 * fig.width, height = 0.8 * fig.height)


# Research + Instruction Spending Regressions ----------------------------------

# Restrict to spending data with no missings
spending.data <- reg.data %>%
    filter(nonauxspending_real > 0)

## Total instruction spending.
# Naive OLS Regression
naive_instruction_spending.reg <- spending.data %>%
    mutate(`I(stateappropriations_real/(enrollment_reported))(fit)` =
        I(stateappropriations_real/(enrollment_reported))) %>%
    felm(I(instructionspending_total_real / enrollment_reported) ~ 1 +
        `I(stateappropriations_real/(enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_instruction_spending.reg <- spending.data %>%
    felm(I(instructionspending_total_real / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Total research spending.
# Naive OLS Regression
naive_research_spending.reg <- spending.data %>%
    filter(researchspending_total_real > 0) %>%
    mutate(`I(stateappropriations_real/(enrollment_reported))(fit)` =
        I(stateappropriations_real/(enrollment_reported))) %>%
    felm(I(researchspending_total_real / enrollment_reported) ~ 1 +
        `I(stateappropriations_real/(enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_research_spending.reg <- spending.data %>%
    filter(researchspending_total_real > 0) %>%
    felm(I(researchspending_total_real / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Public service spending.
# Naive OLS Regression
naive_pub_serv_spending.reg <- spending.data %>%
    filter(exp_pub_serv_real > 0) %>%
    mutate(`I(stateappropriations_real/(enrollment_reported))(fit)` =
        I(stateappropriations_real/(enrollment_reported))) %>%
    felm(I(exp_pub_serv_real / enrollment_reported) ~ 1 +
        `I(stateappropriations_real/(enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_pub_serv_spending.reg <- spending.data %>%
    filter(exp_pub_serv_real > 0) %>%
    felm(I(exp_pub_serv_real / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Student services spending.
# Naive OLS Regression
naive_student_serv_spending.reg <- spending.data %>%
    mutate(`I(stateappropriations_real/(enrollment_reported))(fit)` =
        I(stateappropriations_real/(enrollment_reported))) %>%
    felm(I(exp_student_serv_real / enrollment_reported) ~ 1 +
        `I(stateappropriations_real/(enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_student_serv_spending.reg <- spending.data %>%
    felm(I(exp_student_serv_real / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Academic support spending.
# Naive OLS Regression
naive_acad_supp_spending.reg <- spending.data %>%
    mutate(`I(stateappropriations_real/(enrollment_reported))(fit)` =
        I(stateappropriations_real/(enrollment_reported))) %>%
    felm(I(exp_acad_supp_real / enrollment_reported) ~ 1 +
        `I(stateappropriations_real/(enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_acad_supp_spending.reg <- spending.data %>%
    felm(I(exp_acad_supp_real / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Institutional support spending.
# Naive OLS Regression
naive_inst_supp_spending.reg <- spending.data %>%
    mutate(`I(stateappropriations_real/(enrollment_reported))(fit)` =
        I(stateappropriations_real/(enrollment_reported))) %>%
    felm(I(exp_inst_supp_real / enrollment_reported) ~ 1 +
        `I(stateappropriations_real/(enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_inst_supp_spending.reg <- spending.data %>%
    felm(I(exp_inst_supp_real / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Operations spending.
# Naive OLS Regression
naive_opm_spending.reg <- spending.data %>%
    filter(exp_opm_areal > 0) %>%
    mutate(`I(stateappropriations_real/(enrollment_reported))(fit)` =
        I(stateappropriations_real/(enrollment_reported))) %>%
    felm(I(exp_opm_areal / enrollment_reported) ~ 1 +
        `I(stateappropriations_real/(enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_opm_spending.reg <- spending.data %>%
    filter(exp_opm_areal > 0) %>%
    felm(I(exp_opm_areal / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Grant + aid spending
# Naive OLS Regression
naive_grant_aid_spending.reg <- spending.data %>%
    filter(exp_net_grant_aid_real > 0) %>%
    mutate(`I(stateappropriations_real/(enrollment_reported))(fit)` =
        I(stateappropriations_real/(enrollment_reported))) %>%
    felm(I(exp_net_grant_aid_real / enrollment_reported) ~ 1 +
        `I(stateappropriations_real/(enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_grant_aid_spending.reg <- spending.data %>%
    filter(exp_net_grant_aid_real > 0) %>%
    felm(I(exp_net_grant_aid_real / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Total non-operating spending.
# Naive OLS Regression
naive_nonaux_spending.reg <- spending.data %>%
    mutate(`I(stateappropriations_real/(enrollment_reported))(fit)` =
        I(stateappropriations_real/(enrollment_reported))) %>%
    felm(I(nonauxspending_real / enrollment_reported) ~ 1 +
        `I(stateappropriations_real/(enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_nonaux_spending.reg <- spending.data %>%
    felm(I(nonauxspending_real / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
        unitid + year,
        data = .)

## Tuition revenue.
# Naive OLS Regression
naive_tuition_rev.reg <- spending.data %>%
    mutate(`I(stateappropriations_real/(enrollment_reported))(fit)` =
        I(stateappropriations_real/(enrollment_reported))) %>%
    felm(I(tuitionrev_real / enrollment_reported) ~ 1 +
        `I(stateappropriations_real/(enrollment_reported))(fit)` |
        unitid + year |
        0 |
        unitid + year,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock
shiftshare_tuition_rev.reg <- spending.data %>%
    felm(I(tuitionrev_real / enrollment_reported) ~ 1 |
        unitid + year |
        (I(stateappropriations_real / (enrollment_reported)) ~
            I(-appropriationshock_perEnroll_real)) |
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
    out = "../../text/tables/expenditures-shock-rawcount.tex")

