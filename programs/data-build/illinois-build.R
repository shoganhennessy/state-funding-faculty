#!/usr/bin/R
## Senan Hogan-Hennessy, 12 April 2022
print(c(Sys.time(), Sys.Date()))
## Put raw Illinois data to readable format.
# functions for data manipulation and visualisation
library(tidyverse)
# Fuzzy matching
library(fuzzyjoin)
set.seed(47)


################################################################################
## Load raw data

# Load the first year
illinois.data <-
    readxl::read_xls("../../data/states/illinois/2010_Salaries.xls") %>%
    mutate(year = 2010)

# Add the next years
for (extrayear in c(2011:2021)) {
    print(extrayear)
    # Read next year of data
    extra.data <- paste0("../../data/states/illinois/", as.character(extrayear),
            "_Salaries.xls") %>%
        readxl::read_xls() %>%
        mutate(year = extrayear)
    # Append relevant data
    illinois.data <- illinois.data %>%
        bind_rows(extra.data)
    # Save memory (thanks to R having no auto GC)
    rm(extra.data)
    gc()
}

# CPI-U from FREDS (since Urban provided cpi has missing years)
# Yearly average, seasonally adjusted, base year 1982-1984=100
# https://fred.stlouisfed.org/series/CPIAUCSL
cpiu.data <- read_csv("../../data/urban-ipeds/raw-data/freds-cpiu.csv")
# Clean CPI data, and put in terms of base year 2021
base.year <- 2021
base.index <- cpiu.data %>%
    filter(base.year ==
        (DATE %>% substr(start = 0, stop = 4) %>% as.integer())) %>%
    pull(CPIAUCSL)
cpiu.data <- cpiu.data %>%
    transmute(
        year = DATE %>% substr(start = 0, stop = 4) %>% as.integer(),
        cpi2021 = CPIAUCSL / base.index)


################################################################################
## Clean data

# Get all relevant columns
clean_illinois.data <- illinois.data %>%
    transmute(
        name = Name,
        inst_name = `Institution/System Office`,
        year = year,
        position = Position,
        # Salary from strings. NAs produced by Excel negative values, which is ok
        salary_nominal = `Base Salary` %>%
            sub("\\$", "", .) %>% sub(",", "", .) %>% as.numeric(),
        extra_salary_nominal = `Additional Compensation` %>%
            sub("\\$", "", .) %>% sub(",", "", .) %>% as.numeric()) %>%
    # One observation has a negative salary, so remove it.
    filter(!is.na(salary_nominal))

# CLean the strings in names of Professors
clean_illinois.data <- clean_illinois.data %>%
    # Remove punctuation, replace with spaces
    mutate(name = str_replace_all(name, "[[:punct:]]", " ")) %>%
    # To lower case
    mutate(name = tolower(name)) %>%
    # Remove white-space at beginning + end.
    mutate(name = trimws(name)) %>%
    # Remove left-over white space
    mutate(name = str_squish(name)) %>%
    # Take only first and last name (to remove inconsistencies in middle names)
    mutate(name = paste(word(name, 1), " ", word(name, -1))) %>%
    # Remove left-over white space
    mutate(name = str_squish(name))

# Make salary figures real.
clean_illinois.data <- clean_illinois.data %>%
    left_join(cpiu.data, by = "year") %>%
    mutate(
        salary_real = salary_nominal / cpi2021,
        extra_salary_real = extra_salary_nominal / cpi2021) %>%
    # Remove the nominal values
    select(-c(cpi2021, salary_nominal, extra_salary_nominal))


################################################################################
## Connect to IPEDS finance data.

# Load constructed IPEDS data, for Illinois, 2010 onwards
ipeds.data <- read_csv("../../data/urban-ipeds/urban-clean-publicunis.csv") %>%
    filter(state == "IL", year >= 2010)
gc()

# Get the connecting variables for IPEDs: unitid, college name
ipeds.key <- ipeds.data %>%
    group_by(unitid, inst_name) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    select(unitid, inst_name)

# Get the connecting variable for Illinois: college name
illinois.key <- clean_illinois.data %>%
    group_by(inst_name) %>%
    summarise(prof_count = n()) %>%
    ungroup() %>%
    select(inst_name) %>%
    # Make a manual adjustment for "U of I" matching
    mutate(inst_name = str_replace(inst_name, "U of I",
        "University of Illinois"))

## Connect the IPEDS unitid back to Illinois data by fuzzy name match
# Note that Southern Illinois University School of Medicine has no IPEDS data
clean_illinois.data <- illinois.key %>%
    stringdist_join(ipeds.key,
        by = "inst_name",
        mode = "left",
        method = "jw",
        max_dist = 0.1,
        distance_col = "dist") %>%
    rename(inst_name = inst_name.x) %>%
    group_by(inst_name) %>%
    mutate(best_match = as.integer(dist == min(dist, na.rm = FALSE))) %>%
    ungroup() %>%
    filter(best_match == 1 | is.na(best_match)) %>%
    select(inst_name, unitid) %>%
    right_join(clean_illinois.data, by = "inst_name")


################################################################################
## Save the constructed data file of Illinois data + IPEDS key connecter.
clean_illinois.data %>%
    write_csv("../../data/states/illinois-professors.csv")
