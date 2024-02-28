#!/usr/bin/R
## Senan Hogan-Hennessy, 3 February 2023
## IV for hiring of faculty, using IPEDS + hiring networks data.
print(Sys.time())
set.seed(47)
# Functions for data manipulation and visualisation
library(tidyverse)
# Functions for fast linear models with IV + FEs
library(lfe)
# TeX tables
library(stargazer)
# Define number of digits in tables and graphs
digits.no <- 3
# Size for figures
fig.width <- 12
fig.height <- fig.width * 7 / 12


# Load data sources ------------------------------------------------------------

# Load IPEDS data on all universities
ipeds.data <- read_csv("../../data/urban-ipeds/urban-clean-allunis.csv")

# Load hiring data, which is a sum for years 2010--2021
hiring.data <- read_csv("../../data/states/hiring-count.csv")


# Clean data -------------------------------------------------------------------

# Select IPEDS variables, and define the panel at the university level
ipeds.data <- ipeds.data %>%
    # Non for-profit unis, 4-year
    filter(fouryear == 1, forprofit == 0) %>%
    # Remove years with incomplete data
    filter(1990 <= year, year != 2001, year <= 2021) %>%
    # Restrict to relevant columns and rows.
    select(
        # Identifiers and controls
        unitid,
        inst_name,
        year,
        state,
        public,
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
        # Appropriation + tuition shocks Per uni
        appropriationshock_perEnroll_real,
        appropriationshock_perFTE_real,
        appropriationshock_peruni_real)

# Sum data for 2011-2020, which the hiring data pertain to.
ipeds.data <- ipeds.data %>%
    filter(2011 <= year, year <= 2021) %>%
    # Overate the sum.
    group_by(unitid, state, public) %>%
    summarise(
        # Appropriations to the uni
        totalrevenues_real = mean(totalrevenues_real, na.rm = TRUE),
        nonauxrevenues_real = mean(nonauxrevenues_real, na.rm = TRUE),
        stateappropriations_real = mean(stateappropriations_real, na.rm = TRUE),
        tuitionrev_real = mean(tuitionrev_real, na.rm = TRUE),
        # Uni spending.
        nonauxspending_real = mean(nonauxspending_real, na.rm = TRUE),
        # Uni enrollment
        enrollment_reported = mean(enrollment_reported, na.rm = TRUE),
        enrollment_fte = mean(enrollment_fte, na.rm = TRUE),
        # Appropriation + tuition shocks per student + Per uni
        appropriationshock_perEnroll_real =
            mean(appropriationshock_perEnroll_real, na.rm = TRUE),
        appropriationshock_peruni_real =
            mean(appropriationshock_peruni_real, na.rm = TRUE)) %>%
    ungroup()

# restrct to relevant data in hiring data
hiring.data <- hiring.data %>%
    select(-inst_name)

# Join to the hiring data.
ipeds_hiring.data <- ipeds.data %>%
    left_join(hiring.data, by = "unitid")

# Restrict data to unis + years with measured state appropriations & shocks
reg.data <- ipeds_hiring.data %>%
    filter(
        public == 1,
        !is.na(total_hired),
        !is.na(enrollment_reported), enrollment_reported > 0,
        !is.na(stateappropriations_real), stateappropriations_real > 0,
        !is.na(appropriationshock_peruni_real),
            appropriationshock_peruni_real > 0,
        !is.na(appropriationshock_perEnroll_real),
            appropriationshock_perEnroll_real > 0) %>%
    # Create per student revenue variable.
    mutate(stateappropriations_perEnroll =
        stateappropriations_real / enrollment_reported) %>%
    mutate(stateappropriations_perEnroll = ifelse(
        stateappropriations_perEnroll < 10^6,
            stateappropriations_perEnroll, NA))

# Put togethe a visualisation of the rate of hiring between public + private
# for 2010--2021
quantile.count <- 20

unitid.data <- read_csv("../../data/urban-ipeds/urban-clean-allunis.csv") %>%
    filter(year == 2021) %>%
    select(unitid, inst_name)


# Viusalise how these two vary
hiring_correlation.plot <- reg.data %>%
    # Create the per-student values
    mutate(
        stateappropriations_perEnroll =
            (stateappropriations_real / enrollment_reported),
        total_hired_perEnroll =
            (total_hired / (enrollment_reported / 100))) %>%
    # Take out outliers
    filter(0.5 < total_hired_perEnroll, total_hired_perEnroll < 15,
        stateappropriations_perEnroll < 15000,
        stateappropriations_perEnroll > 2000) %>%
    # Declare a plot to show the correlation.
    ggplot(aes(
        x = stateappropriations_perEnroll / 10^3,
        y = total_hired_perEnroll)) +
    geom_jitter() +
    geom_smooth(method = "lm") +
    scale_x_continuous(
        name = "State Funding per Year, $ thousands per student",
        expand = c(0, 0),
        limits = c(2.5, 15),
        breaks = seq(0, 15, by = 1)) +
    scale_y_continuous(name = "",
        breaks = seq(0, 15, by = 2),
        limits = c(0, 12)) +
    theme_bw() +
    ggtitle("Professors Hired, count / 100 students") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 2, 0, 0), "mm"))
# Save this plot
ggsave("../../text/figures/hiring-correlation.png",
    plot = hiring_correlation.plot,
    units = "cm", dpi = 300, width = fig.width, height = fig.height)

# Give the value of the correlation:
reg.data %>%
    lm(I(total_hired / (enrollment_reported / 1000)) ~ 1 +
        I(stateappropriations_real / (1000 * enrollment_reported)),
        data = .) %>%
    summary() %>%
    print()


################################################################################
## OLS + IV between hiring & appropriations.

# Ensure the instrument is strong in these collapsed data.
firststage_approp.reg <- reg.data %>%
    felm(log(stateappropriations_real / enrollment_reported) ~ 0 +
        I(-log(appropriationshock_perEnroll_real)) |
        state |
        0 |
        state,
        data = .)
firststage_approp.reg %>% summary() %>% print()
# Get the F.Stat
firststage_approp.fstat <-
    car::linearHypothesis(firststage_approp.reg, test = "F",
        c("I(-log(appropriationshock_perEnroll_real))"))["F"]

# Naive OLS Regression, for men hiring
naive_menhiring.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(men_hired / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        state |
        0 |
        state,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock for men hiring
shiftshare_menhiring.reg <- reg.data %>%
    felm(log(men_hired / enrollment_reported) ~ 1 |
        state |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state,
        data = .)
# Naive OLS Regression, for women hiring
naive_womenhiring.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(women_hired / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        state |
        0 |
        state,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock for women hiring
shiftshare_womenhiring.reg <- reg.data %>%
    felm(log(women_hired / enrollment_reported) ~ 1 |
        state |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state,
        data = .)

# Naive OLS Regression, for total hiring
naive_hiring.reg <- reg.data %>%
    mutate(`log(stateappropriations_real/enrollment_reported)(fit)` =
        log(stateappropriations_real / enrollment_reported)) %>%
    felm(log(total_hired / enrollment_reported) ~ 1 +
        `log(stateappropriations_real/enrollment_reported)(fit)` |
        state |
        0 |
        state,
        data = .)
# Shift-share IV Regression, explained by state appropriation shock for total hiring
shiftshare_hiring.reg <- reg.data %>%
    felm(log(total_hired / enrollment_reported) ~ 1 |
        state |
        (log(stateappropriations_real / enrollment_reported) ~
            I(-log(appropriationshock_perEnroll_real))) |
        state,
        data = .)

# Collate the results to a LaTeX table
stargazer(
    naive_menhiring.reg, shiftshare_menhiring.reg,
    naive_womenhiring.reg, shiftshare_womenhiring.reg,
    naive_hiring.reg, shiftshare_hiring.reg,
    dep.var.caption = "Dependent Variable: Professor Hiring Count",
    dep.var.labels = c("Men", "Women", "Total"),
    column.labels = rep(c("OLS", "2SLS"), 3),
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
    out = "../../text/tables/hiring-shock-reg-fte.tex")
