#!/usr/bin/R
## Senan Hogan-Hennessy, 3 August 2022
## IV for Prof's salaries, using IPEDS data.
print(Sys.time())
library(tidyverse) # Functions for data manipulation and visualization
library(lfe) # Functions for fast linear models with IV + FEs
library(plm) # Functions for panel data
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
        !is.na(stateappropriations_real), stateappropriations_real > 0,
        !is.na(appropriationshock_perEnroll_real),
        appropriationshock_perEnroll_real > 0)

# Define data sample, with releavnt transformations, for the LP estimation
reg.data <- reg.data %>%
    transmute(unitid = unitid,
        year = year,
        state = state,
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
            - log(appropriationshock_perEnroll_real)) %>%
    pdata.frame(index = c("unitid", "year"), drop.index = FALSE)

# Save memory by removing initally loaded files
rm(ipeds.data)
gc()


# Long-run effects, where outcome is Y_{i, t + k} for k = 0, ..., 10 -----------

# Run the simple linear regression in a loop, and plot the results
# Make a note of the possibly entangled X, Y channel effects, thus need for LP.


# Local Projections for staying-power of effects -------------------------------

# https://cran.r-project.org/web/packages/lpirfs/lpirfs.pdf
library(plm)
library(lpirfs)
time.horizon <- 11
limits.vector <- c(-0.1, 0.6)

## Define a function to nicely plot the LP results.
lpreg.plot <- function(model.lpreg, limits.vector, given.colour) {
    ## Input a LP-reg (estimates an elesticity \in [-1,1])
    ## And the time.hirozon for how far to plot LP model
    # Get the estimates
    model.lpdata <- data.frame(
        t = c(1:length(model.lpreg$irf_panel_mean)) - 1,
        estimate = as.vector(model.lpreg$irf_panel_mean),
        conf.low = as.vector(model.lpreg$irf_panel_low),
        conf.high = as.vector(model.lpreg$irf_panel_up))
    # Plot the LP etsimation, with my own custom plot.
    model.plot <- model.lpdata %>%
        ggplot(aes(x = t)) +
        geom_hline(yintercept = 0, alpha = 0.25) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
            alpha = 0.25, fill = given.colour) + #"grey70") +
        geom_point(aes(y = estimate), colour = given.colour) +
        geom_line(aes(y = estimate), colour = given.colour) +
        geom_line(aes(y = conf.low), linetype = "dashed", colour = given.colour) +
        geom_line(aes(y = conf.high), linetype = "dashed", colour = given.colour) +
        scale_x_continuous(name = "Years, Relative to Funding Cut",
            breaks = model.lpdata$t, expand = c(0.01, 0.01)) +
        scale_y_continuous(name = "",
            limits = limits.vector,
            breaks = seq(-5, 5, by = 0.1)) +
        theme_bw() +
        ggtitle("Estimate") +
        theme(plot.title = element_text(size = rel(1)),
            plot.margin = unit(c(0.5, 1, 0, 0), "mm"))
    # Return the plot.
    return(model.plot)
}

# Run the LP method for the first-stage regression -> staying power of rev shock
firststage.lpreg <-
    lp_lin_panel(
        data_set = reg.data,
        # Outcome variable
        endog_data = "stateappropriations_real",
        # Predictor variable
        shock = "appropriationshock_perEnroll_real",
        # Option to use IV for predictor endogeneity (not used here)
        iv_reg = FALSE,
        # LP options
        diff_shock = FALSE,
        cumul_mult = FALSE,
        # Add fixed effects + clustered SEs in the panel
        panel_model = "within",
        panel_effect = "twoways",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
lpreg.plot(firststage.lpreg, c(-1.15, 0.2), colour.list[1])
# Save this plot
ggsave("../../text/figures/firststage-lp.png",
    plot = lpreg.plot(firststage.lpreg, c(-1.15, 0.2), colour.list[1]),
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Run the LP method for the second-stage, outcome count lecturers per student
lecturer_count.lpreg <-
    lp_lin_panel(data_set = reg.data,
        # Outcome variable
        endog_data = "lecturer_prof_count",
        # Predictor variable
        shock = "stateappropriations_real",
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # LP options
        cumul_mult = FALSE,
        diff_shock = FALSE,
        # Add fixed effects + clsutered SEs in the panel
        panel_model = "within",
        panel_effect = "twoways",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
plot(lecturer_count.lpreg)
# Save this plot
ggsave("../../text/figures/lecturer-count-lp.png",
    plot = lpreg.plot(lecturer_count.lpreg, - limits.vector[c(2, 1)],
        colour.list[2]),
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Run the LP method for the second-stage, outcome count APs per student
assistant_count.lpreg <-
    lp_lin_panel(data_set = reg.data,
        # Outcome variable
        endog_data = "assistant_prof_count",
        # Predictor variable
        shock = "stateappropriations_real",
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # LP options
        diff_shock = FALSE,
        cumul_mult = FALSE,
        # Add fixed effects + clsutered SEs in the panel
        panel_model = "within",
        panel_effect = "individual",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
plot(assistant_count.lpreg)
# Save this plot
ggsave("../../text/figures/assistant-count-lp.png",
    plot = lpreg.plot(assistant_count.lpreg, limits.vector, colour.list[3]),
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Run the LP method for the second-stage, outcome count APs per student
full_count.lpreg <-
    lp_lin_panel(data_set = reg.data,
        # Outcome variable
        endog_data = "full_prof_count",
        # Predictor variable
        shock = "stateappropriations_real",
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # LP options
        diff_shock = FALSE,
        cumul_mult = FALSE,
        # Add fixed effects + clsutered SEs in the panel
        panel_model = "within",
        panel_effect = "twoways",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
plot(full_count.lpreg)
# Save this plot
ggsave("../../text/figures/full-count-lp.png",
    plot = lpreg.plot(full_count.lpreg, limits.vector, colour.list[4]),
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Run the LP method for the second-stage, outcome count profs per student
all_count.lpreg <-
    lp_lin_panel(data_set = reg.data,
        # Outcome variable
        endog_data = "all_prof_count",
        # Predictor variable
        shock = "stateappropriations_real",
        # Option to use IV for predictor endogeneity
        iv_reg = TRUE,
        instrum = "appropriationshock_perEnroll_real",
        # LP options
        diff_shock = FALSE,
        cumul_mult = FALSE,
        # Add fixed effects + clsutered SEs in the panel
        panel_model = "within",
        panel_effect = "twoways",
        robust_cov = "vcovHC",
        robust_cluster = c("group", "time"),
        confint = 1.96,
        hor = time.horizon)
plot(all_count.lpreg)
# Save this plot
ggsave("../../text/figures/all-count-lp.png",
    plot = lpreg.plot(all_count.lpreg, limits.vector, colour.list[5]),
    units = "cm", dpi = 300, width = fig.width, height = fig.height)
