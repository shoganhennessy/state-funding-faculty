#!/usr/bin/R
## Senan Hogan-Hennessy, 10 June 2022
## Investigate differences between public and private uni prof composition.
print(c(Sys.time(), Sys.Date()))
set.seed(47)
# Functions for data manipulation and visualisation
library(tidyverse)
# Functions for panel data
library(plm)

# Define number of digits in tables and graphs
digits.no <- 3

# Size for figures
fig.width <- 10
fig.height <- fig.width * 0.75


# Load data sources ------------------------------------------------------------

# Load IPEDS data on all universities
ipeds.data <- read_csv("../../data/urban-ipeds/urban-clean-allunis.csv")


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
        fouryear,
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
        appropriationshock_peruni_real,
        tuitionshock_peruni_real,
        # Count of full-time professors (tenured or not)
        lecturer_prof_count,
        assistant_prof_count,
        full_prof_count,
        all_prof_count,
        # Average faculty salary
        full_profmeansalary_real,
        assistant_profmeansalary_real,
        lecturer_profmeansalary_real,
        all_profmeansalary_real,
        # Total professor salaries paid
        all_profoutlays_real)
gc()

# Public vs private enrollment comparison --------------------------------------


# Show enrollment at public vs private over time: total + average
enrollment.data <- ipeds.data %>%
    group_by(public, year) %>%
    summarise(
        enrollment_total = sum(enrollment_reported, na.rm = TRUE),
        enrollment_mean = mean(enrollment_reported, na.rm = TRUE)) %>%
    ungroup()
# Draw the graph for enrollment total
enrollment_total.plot <- enrollment.data %>%
    ggplot(aes(x = year, y = enrollment_total, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(0, 10^7),
        labels = scales::comma) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/enrollment-total.png",
    plot = enrollment_total.plot,
    units = "cm", width = fig.width, height = fig.height)
# Draw the graph for enrollment mean
enrollment_mean.plot <- enrollment.data %>%
    ggplot(aes(x = year, y = enrollment_mean, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(0, 12500),
        labels = scales::comma) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/enrollment-mean.png",
    plot = enrollment_mean.plot,
    units = "cm", width = fig.width, height = fig.height)


# Public vs private salary comparison ------------------------------------------

# Compare to IPEDS provided table
# https://nces.ed.gov/ipeds/search/ViewTable?tableId=10102&returnUrl=%2Fipeds%2Fsearch
#ipeds.data %>%
#    group_by(year, public, fouryear) %>%
#    summarise(
#        all_profmeansalary_real = weighted.mean(all_profmeansalary_real,
#            weights = all_prof_count, na.rm = TRUE),
#        full_profmeansalary_real = weighted.mean(full_profmeansalary_real,
#            weights = lecturer_prof_count, na.rm = TRUE),
#        assistant_profmeansalary_real = weighted.mean(assistant_profmeansalary_real,
#            weights = assistant_prof_count, na.rm = TRUE),
#        lecturer_profmeansalary_real = weighted.mean(lecturer_profmeansalary_real,
#            weights = full_prof_count, na.rm = TRUE),
#        full_prof_count = sum(full_prof_count, na.rm = TRUE),
#        all_prof_count =  sum(all_prof_count, na.rm = TRUE)) %>%
#    filter(year %in% 2018:2019) %>%
#    View()
#! Massive under-count for private uni prof salaries, as compared to here
#! https://nces.ed.gov/ipeds/search/ViewTable?tableId=25036&returnUrl=%2Fipeds%2Fsearch

# Graph mean prof salary at public vs private uni.
ipeds.data %>%
    group_by(public, year) %>%
    summarise(
        full_profmeansalary_real = weighted.mean(full_profmeansalary_real,
            weights = lecturer_prof_count, na.rm = TRUE),
        assistant_profmeansalary_real = weighted.mean(assistant_profmeansalary_real,
            weights = assistant_prof_count, na.rm = TRUE),
        lecturer_profmeansalary_real = weighted.mean(lecturer_profmeansalary_real,
            weights = full_prof_count, na.rm = TRUE),
        all_profmeansalary_real = weighted.mean(all_profmeansalary_real,
            weights = all_prof_count, na.rm = TRUE)) %>%
    # Declare the plot.
    ggplot(aes(x = year, colour = factor(public))) +
    #geom_point(aes(y = all_profmeansalary_real, shape = "All Profs")) +
    #geom_line(aes(y = all_profmeansalary_real,  shape = "All Profs")) +
    geom_point(aes(y = assistant_profmeansalary_real, shape = "Assistant")) +
    geom_line(aes(y = assistant_profmeansalary_real,  shape = "Assistant")) +
    geom_point(aes(y = full_profmeansalary_real, shape = "Full")) +
    geom_line(aes(y = full_profmeansalary_real,  shape = "Full")) +
    geom_point(aes(y = lecturer_profmeansalary_real, shape = "Lecturer")) +
    geom_line(aes(y = lecturer_profmeansalary_real,  shape = "Lecturer")) +
    # Adjust the names and axis
    scale_x_continuous(name = "Year",
        breaks = seq(1990, 2020, by = 5)) +
    scale_y_continuous(name = "",
        #limits = c(0, 560),
        #breaks = seq(0, 600, by = 50),
        labels = scales::comma) +
    theme_bw()

#! The salaries data show that public profs are paid more then private, which is not the case elsewhere
#! There must be a problem with how these salary info are constructed specifically for private unis, so do not use these as an outcome in the analysis.


# Plot prof per student over time ----------------------------------------------

# Graph mean prof count per students at public vs private uni.
prof_perfte.data <- ipeds.data %>%
    group_by(public, year) %>%
    summarise(
        full_fte_perprof     = sum(full_prof_count, na.rm = TRUE) /
            sum(enrollment_reported, na.rm = TRUE),
        assistant_fte_perprof = sum(assistant_prof_count, na.rm = TRUE) /
            sum(enrollment_reported, na.rm = TRUE),
        lecturer_fte_perprof  = sum(lecturer_prof_count, na.rm = TRUE) /
            sum(enrollment_reported, na.rm = TRUE),
        all_fte_perprof         = sum(all_prof_count, na.rm = TRUE) /
            sum(enrollment_reported, na.rm = TRUE)) %>%
    ungroup()

# Draw the graph for lecturers Professors per student
lecturer_fte_perprof.plot <- prof_perfte.data %>%
    ggplot(aes(x = year, y = 1 / lecturer_fte_perprof, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        #limits = c(0, 350),
        breaks = seq(0, 350, by = 25),
        labels = scales::comma) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/lecturer-fte-perprof.png",
    plot = lecturer_fte_perprof.plot,
    units = "cm", width = fig.width, height = fig.height)

# Draw the graph for TT Professors per student
assistant_fte_perprof.plot <- prof_perfte.data %>%
    ggplot(aes(x = year, y = 1 / assistant_fte_perprof, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        #limits = c(0, 350),
        breaks = seq(0, 350, by = 5),
        labels = scales::comma) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/assistant-fte-perprof.png",
    plot = assistant_fte_perprof.plot,
    units = "cm", width = fig.width, height = fig.height)

# Draw the graph for tenured Professors per student
full_fte_perprof.plot <- prof_perfte.data %>%
    ggplot(aes(x = year, y = 1 / full_fte_perprof, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(30, 55),
        breaks = seq(0, 350, by = 5),
        labels = scales::comma) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/full-fte-perprof.png",
    plot = full_fte_perprof.plot,
    units = "cm", width = fig.width, height = fig.height)

# Draw the graph for all Professors per student
all_fte_perprof.plot <- prof_perfte.data %>%
    ggplot(aes(x = year, y = 1 / all_fte_perprof, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(20, 30),
        breaks = seq(0, 35, by = 2),
        labels = scales::comma) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/all-fte-perprof.png",
    plot = all_fte_perprof.plot,
    units = "cm", width = fig.width, height = fig.height)


# Get figures in revenues (total + per student) public vs private --------------
mean_funding.data <- ipeds.data %>%
    group_by(year, public) %>%
    summarise(
        totalrevenues_mill =
            mean(totalrevenues_real, na.rm = TRUE) / (10^6),
        nonauxrevenues_mill =
            mean(nonauxrevenues_real, na.rm = TRUE) / (10^6),
        stateappropriations_mill =
            mean(stateappropriations_real, na.rm = TRUE) / (10^6),
        tuitionrev_mill =
            mean(tuitionrev_real, na.rm = TRUE) / (10^6),
        totalrevenues_perEnroll =
            mean(totalrevenues_real / enrollment_reported, na.rm = TRUE),
        nonauxrevenues_perEnroll =
            mean(nonauxrevenues_real / enrollment_reported, na.rm = TRUE),
        stateappropriations_perEnroll =
            mean(stateappropriations_real / enrollment_reported, na.rm = TRUE),
        tuitionrev_perEnroll =
            mean(tuitionrev_real / enrollment_reported, na.rm = TRUE))
