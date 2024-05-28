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
fig.width <- 9
fig.height <- fig.width * 0.9


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

# How many public unis + students in 2020/2021?
ipeds.data %>%
    group_by(public, year) %>%
    summarise(
        total_count = n(),
        enrollment_reported = sum(enrollment_reported, na.rm = TRUE)) %>%
    ungroup() %>%
    print() #View()

# Show enrollment at public vs private over time: total + average
enrollment.data <- ipeds.data %>%
    group_by(public, year) %>%
    summarise(
        enrollment_total = sum(enrollment_reported, na.rm = TRUE),
        enrollment_mean = mean(enrollment_reported, na.rm = TRUE)) %>%
    ungroup()
# Draw the graph for enrollment total
enrollment_total.plot <- enrollment.data %>%
    ggplot(aes(x = year, y = enrollment_total / 10^6, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(0, 10),
        breaks = seq(0, 10, by = 2),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Student Enrolment, millions") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/enrollment-total.png",
    plot = enrollment_total.plot,
    units = "cm", width = fig.width, height = fig.height)
# Draw the graph for enrollment mean
enrollment_mean.plot <- enrollment.data %>%
    ggplot(aes(x = year, y = enrollment_mean / 10^3, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(0, 12.5),
        breaks = seq(0, 13, by = 2),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Student Enrolment, thousands") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
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
            sum(enrollment_reported / 100, na.rm = TRUE),
        assistant_fte_perprof = sum(assistant_prof_count, na.rm = TRUE) /
            sum(enrollment_reported / 100, na.rm = TRUE),
        lecturer_fte_perprof  = sum(lecturer_prof_count, na.rm = TRUE) /
            sum(enrollment_reported / 100, na.rm = TRUE),
        all_fte_perprof         = sum(all_prof_count, na.rm = TRUE) /
            sum(enrollment_reported / 100, na.rm = TRUE)) %>%
    ungroup()

# Draw the graph for lecturers Professors per student
lecturer_fte_perprof.plot <- prof_perfte.data %>%
    ggplot(aes(x = year, y = lecturer_fte_perprof, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.25),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Lecturer count / 100 students") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/lecturer-fte-perprof.png",
    plot = lecturer_fte_perprof.plot,
    units = "cm", width = fig.width, height = fig.height)

# Draw the graph for TT Professors per student
assistant_fte_perprof.plot <- prof_perfte.data %>%
    ggplot(aes(x = year, y = assistant_fte_perprof, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(0.5, 1.5),
        breaks = seq(0, 2, by = 0.25),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Asst. Professors / 100 students") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/assistant-fte-perprof.png",
    plot = assistant_fte_perprof.plot,
    units = "cm", width = fig.width, height = fig.height)

# Draw the graph for tenured Professors per student
full_fte_perprof.plot <- prof_perfte.data %>%
    ggplot(aes(x = year, y = full_fte_perprof, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(1.5, 3),
        breaks = seq(1.5, 3, by = 0.25),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("Tenured Professors / 100 students") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/full-fte-perprof.png",
    plot = full_fte_perprof.plot,
    units = "cm", width = fig.width, height = fig.height)

# Draw the graph for all Professors per student
all_fte_perprof.plot <- prof_perfte.data %>%
    ggplot(aes(x = year, y = all_fte_perprof, colour = factor(public))) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year",
        breaks = seq(1985, 2020, by = 5)) +
    scale_y_continuous(name = "",
        limits = c(3, 5),
        breaks = seq(0, 5, by = 0.25),
        labels = scales::comma) +
    theme_bw() +
    ggtitle("All Faculty count / 100 students") +
    theme(plot.title = element_text(size = rel(1)),
        plot.margin = unit(c(0.5, 0, 0, 0), "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = -10)) +
    scale_colour_discrete(name = "", labels = c("Private", "Public"))
ggsave("../../text/figures/all-fte-perprof.png",
    plot = all_fte_perprof.plot,
    units = "cm", width = fig.width, height = fig.height)
