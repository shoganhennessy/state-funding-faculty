#!/usr/bin/R
## Senan Hogan-Hennessy, 3 February 2023
print(Sys.time())
set.seed(47)
## Take faculty hiring network data from Scince (2022) article,
# As a measure for uni hiring for the time period 2010--2021
# functions for data manipulation and visualisation
library(tidyverse)
# Fuzzy matching
library(fuzzyjoin)


################################################################################
## Load raw data, open source from Scieence article
#! PUT CITATION HERE.

# Load the academic hiring network (edges) data.
hiring_edges.data <-
    read_csv("../../data/us-faculty-hiring-networks/data/edge_lists.csv")


################################################################################
## Clean data

hiring_edges.data %>% head(100)
hiring_edges.data %>% names()
# Count the sum of hires, by each uni 2011--2020, from the edge data
hiring.data <- hiring_edges.data %>%
    transmute(
        # This inst_id is not the same as the one for IPEDS
        inst_id = InstitutionId,
        inst_name = InstitutionName,
        men_hired = Men,
        women_hired = Women,
        total_hired = Total) %>%
    group_by(inst_id, inst_name) %>%
    summarise(
        men_hired = sum(men_hired, na.rm = TRUE),
        women_hired = sum(women_hired, na.rm = TRUE),
        total_hired = sum(total_hired, na.rm = TRUE)) %>%
    ungroup()


################################################################################
## Connect to IPEDS finance data.

# Load constructed IPEDS data, 2011 onwards
ipeds.data <- read_csv("../../data/urban-ipeds/urban-clean-allunis.csv") %>%
    filter(year >= 2011)
gc()

# Get the connecting variables for IPEDs: unitid, college name
ipeds.key <- ipeds.data %>%
    #mutate(inst_name = str_sub(inst_name, end = 30)) %>%
    group_by(unitid, inst_name) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    select(unitid, inst_name)

ipeds.key %>% arrange(unitid, inst_name)
hiring.data %>% arrange(inst_id, inst_name)


# Get the connecting variable for hiring data: college name
hiring.key <- hiring.data %>%
    #mutate(inst_name = str_sub(inst_name, end = 30)) %>%
    group_by(inst_name) %>%
    summarise(inst_count = n()) %>%
    ungroup() %>%
    select(inst_name)

## Connect the IPEDS unitid back to Hiring data by fuzzy name match
hiring.data <- hiring.data %>%
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
    filter(best_match == 1 | is.na(best_match))

# If there are multiple matches (i.e. multiple campuses have gone to the IPEDS
# central campus), then go to the first one.
hiring.data <- hiring.data %>%
    group_by(inst_name, unitid) %>%
    mutate(
        inst_count = n(),
        inst_no = 1:n()) %>%
    ungroup() %>%
    filter(inst_no == 1) %>%
    select(-inst_count, -inst_no)

# Restrict the hiring data to IPEDS relevant data.
hiring.data <- hiring.data %>%
    select(unitid, inst_name,
        men_hired, women_hired, total_hired)


################################################################################
## Save the constructed data file of hiring data + IPEDS key connecter.
hiring.data %>%
    write_csv("../../data/states/hiring-count.csv")
