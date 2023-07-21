#!/usr/bin/R
## Senan Hogan-Hennessy, 21 July 2023
## IV for Prof's outcomes, using Illinois data.
## Long term outcomes by local projections method.
## Using a rolling-share variant of the instrument.
print(Sys.time())
library(tidyverse) # Functions for data manipulation and visualisation
library(lfe) # Functions for fast linear models with IV + FEs
library(plm) # Functions for panel data
library(car) # Function for F stat regarding IV models
# https://cran.r-project.org/web/packages/lpirfs/lpirfs.pdf
library(lpirfs) # Function for LP regression.
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

# Select non-missing values
reg.data <- reg.data %>%
    mutate(extra_salary_real = ifelse(is.na(extra_salary_real), 0,
        extra_salary_real)) %>%
    # Only unis + years with measured state appropriations & shocks & Prof count
    filter(!is.na(salary_real), salary_real > 0,
        !is.na(enrollment_reported), enrollment_reported > 0,
        !is.na(stateappropriations_real),
        !is.na(appropriationshock_perEnroll_rolling))

# Factor out the unitid, name designations for FELM functionality
reg.data <- reg.data %>%
    mutate(unitid = factor(unitid),
        name = factor(name))

# Generate indicator for whether the individual is ever observed as the position
reg.data <- reg.data %>%
    mutate(associate = as.integer(position %in% c("Associate Professor"))) %>%
    group_by(name, unitid) %>%
    mutate(
        ever_lecturer  = max(lecturer,  na.rm = TRUE),
        ever_assistant = max(assistant, na.rm = TRUE),
        ever_associate = max(associate, na.rm = TRUE)) %>%
    ungroup()


# Set- up the Local Projections envrironment -----------------------------------

# Define length of time horizon to plot over
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
        lecturer = lecturer,
        assistant = assistant,
        full = full,
        administrator = administrator,
        salary_real = log(salary_real + extra_salary_real),
        promoted = promoted,
        ever_lecturer  = ever_lecturer,
        ever_assistant = ever_assistant,
        ever_associate = ever_associate,
        notemployed_nextyear = notemployed_nextyear,
        stateappropriations_real =
            log(stateappropriations_real / enrollment_reported),
        appropriationshock_perEnroll_rolling =
            - log(appropriationshock_perEnroll_rolling),
        tuitionrev_real = log(tuitionrev_real / enrollment_reported)) %>%
    pdata.frame(index = c("name_unitid", "year"), drop.index = FALSE)

## Define a function to nicely plot the LP results.
lpreg.plot <- function(model.lpreg) {
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
        geom_hline(yintercept = 0, alpha = 0.5) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
            alpha = 0.4, fill = "grey70") +
        geom_point(aes(y = estimate), colour = "blue") +
        geom_line(aes(y = estimate), colour = "blue") +
        geom_line(aes(y = conf.low), linetype = "dashed") +
        geom_line(aes(y = conf.high), linetype = "dashed") +
        scale_x_continuous(name = "Years, Relative to Initital Shock",
            breaks = model.lpdata$t, expand = c(0.01, 0.01)) +
        scale_y_continuous(name = "",
            limits = c(-0.05, 0.1),
            breaks = seq(-2, 1, by = 0.025)) +
        theme_bw() +
        ggtitle("Estimate") +
        theme(plot.title = element_text(size = rel(1)),
            plot.margin = unit(c(0.5, 1, 0, 0), "mm"))
    # Return the plot.
    return(model.plot)
}

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
# Save this plot of th first stage.
ggsave("../../text/figures/firststage-illinois-lp-rolling.png",
    plot = lpreg.plot(firststage.lpreg),
    units = "cm", width = fig.width, height = fig.height)


# Local Projection Estimates of Faculty Salaries -------------------------------

# LP estimation for salaries of lecturers.
lecturer_salaries.lpreg <-
    lp_lin_panel(data_set = filter(lp.data, lecturer == 1),
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
ggsave("../../text/figures/salaries-lecturer-illinois-lp-rolling.png",
    plot = lpreg.plot(lecturer_salaries.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# LP estimation for salaries of APs.
assistant_salaries.lpreg <-
    lp_lin_panel(data_set = filter(lp.data, assistant == 1),
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
ggsave("../../text/figures/salaries-assistant-illinois-lp-rolling.png",
    plot = lpreg.plot(assistant_salaries.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# LP estimation for salaries of full profs.
full_salaries.lpreg <-
    lp_lin_panel(data_set = filter(lp.data, full == 1),
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
ggsave("../../text/figures/salaries-full-illinois-lp-rolling.png",
    plot = lpreg.plot(full_salaries.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# LP estimation for salaries of administrator faculty.
administrator_salaries.lpreg <-
    lp_lin_panel(data_set = filter(lp.data, administrator == 1),
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
ggsave("../../text/figures/salaries-administrator-illinois-lp-rolling.png",
    plot = lpreg.plot(administrator_salaries.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# LP estimation for salaries of all faculty.
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
ggsave("../../text/figures/salaries-all-illinois-lp-rolling.png",
    plot = lpreg.plot(all_salaries.lpreg),
    units = "cm", width = fig.width, height = fig.height)


# Local Projection Estimates of Faculty Promotion Rate -------------------------

# LP estimation for salaries of lecturers.
lecturer_promoted.lpreg <-
    lp_lin_panel(
        data_set =
            filter(lp.data, ever_lecturer == 1, lecturer + assistant > 0),
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
ggsave("../../text/figures/promoted-lecturer-illinois-lp-rolling.png",
    plot = lpreg.plot(lecturer_promoted.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# LP estimation for promotion rate of APs.
assistant_promoted.lpreg <-
    lp_lin_panel(
        data_set =
            filter(lp.data, ever_assistant == 1, assistant + associate > 0),
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
ggsave("../../text/figures/promoted-assistant-illinois-lp-rolling.png",
    plot = lpreg.plot(assistant_promoted.lpreg),
    units = "cm", width = fig.width, height = fig.height)

# LP estimation for promotion rate of full profs.
full_promoted.lpreg <-
    lp_lin_panel(data_set = filter(lp.data, full == 1),
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
ggsave("../../text/figures/promoted-full-illinois-lp-rolling.png",
    plot = lpreg.plot(full_promoted.lpreg),
    units = "cm", width = fig.width, height = fig.height)








# Local Projection Estimates of Faculty Exit Rate -------------------------
#TODO: WRITE HERE FOR THE LP OF EXITS

        endog_data = "notemployed_nextyear"