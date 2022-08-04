#!/usr/bin/R
## Senan Hogan-Hennessy, 21 December 2021
## Build IPEDS data with the Urban Inst's package
## https://educationdata.urban.org/documentation/colleges.html
## https://urbaninstitute.github.io/education-data-package-r/
library(tidyverse) ## functions for data manipulation and visualization
set.seed(47)


# Load data --------------------------------------------------------------------

# Directory info for each uni
directory.data <- read_csv("../../data/urban-ipeds/raw-data/colleges_ipeds_directory.csv")

# University level finances
finance.data <- read_csv("../../data/urban-ipeds/raw-data/colleges_ipeds_finance.csv") %>%
    # Include Supplementary finances for 2018-2020
    bind_rows(read_csv(
        "../../data/urban-ipeds/raw-data/finance/ipeds-finance-20182020.csv"))

# University enrollment (hand made from raw IPEDS values)
enrollment.data <- read_csv("../../data/urban-ipeds/raw-data/enrollment/ipeds-enrollment.csv")

# Professors' salaries (hand made from raw IPEDS values)
salaries.data <- read_csv("../../data/urban-ipeds/raw-data/faculty/ipeds-salaries.csv")
# Professors' faculty rate (hand made from raw IPEDS values)
tenure.data <- read_csv("../../data/urban-ipeds/raw-data/faculty/ipeds-tenure.csv")

# CPI-U from FREDS (since Urban provided cpi has missing years)
# Yearly average, seasonally adjusted, base year 1982-1984=100
# https://fred.stlouisfed.org/series/CPIAUCSL
cpiu.data <- read_csv("../../data/urban-ipeds/raw-data/freds-cpiu.csv")


# Clean data -----------------------------------------------------------------

# Keep releveant directory info
directory.data <- directory.data %>%
    transmute(unitid = unitid,
        year = year,
        inst_name = inst_name,
        state = state_abbr,
        public = as.integer(inst_control == 1),
        forprofit = as.integer(inst_control == 3),
        fouryear = as.integer(institution_level == 4),
        associatescollege = as.integer(inst_category == 4 | inst_category == 6))

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

# Keep relevant finance data
finance.data <- finance.data %>%
    # Bring in years of the CPI-U, base year 2021
    left_join(cpiu.data, by = "year") %>%
    # Keep state finances, and make real
    transmute(
        unitid = unitid,
        year = year,
        cpi2021 = cpi2021,
        # Federal appropriations
        fedappropriations_real = replace(rev_appropriations_fed,
            rev_appropriations_fed <= 0, NA) / cpi2021,
        # State Gov. appropriations
        stateappropriations_real = replace(rev_appropriations_state,
            rev_appropriations_state <= 0, NA) / cpi2021,
        # Local appropriations
        localappropriations_real = replace(rev_appropriations_local,
            rev_appropriations_local <= 0, NA) / cpi2021,
        # Tuition (+ fees) revenue
        tuitionrev_real = replace(rev_tuition_fees_gross,
            rev_tuition_fees_gross <= 0, NA) / cpi2021,
        # Expenditures on instruction
        instructionspending_total = replace(exp_instruc_total,
            exp_instruc_total <= 0, NA) / cpi2021,
        # Salary Expenditures on instruction
        instructionspending_salaries = replace(exp_instruc_salaries,
            exp_instruc_salaries <= 0, NA) / cpi2021,
        # Expenditures on research
        researchspending_total = replace(exp_research_total,
            exp_research_total <= 0, NA) / cpi2021,
        # Salary Expenditures on research
        researchspending_salaries = replace(exp_research_salaries,
            exp_research_salaries <= 0, NA) / cpi2021,
        # All revenues (including tuition + appropriations)
        totalrevenues_real = replace(rev_total_current,
            rev_total_current <= 0, NA) / cpi2021,
        # Non-aux revenues (including tuition + appropriations)
        nonauxrevenues_real = (
            replace(rev_appropriations_fed, is.na(rev_appropriations_fed) |
                rev_appropriations_fed <= 0, 0) +
            replace(rev_appropriations_state, is.na(rev_appropriations_state) |
                rev_appropriations_state <= 0, 0) +
            replace(rev_appropriations_local, is.na(rev_appropriations_local) |
                rev_appropriations_local <= 0, 0) +
            replace(rev_tuition_fees_gross, is.na(rev_tuition_fees_gross) |
                rev_tuition_fees_gross <= 0, 0)
            ) / cpi2021,
        # Total non-aux spending, i.e. as DCost sum(instruction01, research01,
        # pubserv01, acadsupp01, studserv01, instsupp01, opermain01, grants01)
        # Possible there is double counting here, if spending used in analysis.
        nonauxspending_real = (
            replace(exp_instruc_total, is.na(exp_instruc_total) |
                exp_instruc_total <= 0, 0) +
            replace(exp_research_total, is.na(exp_research_total) |
                exp_research_total <= 0, 0) +
            replace(exp_pub_serv_total, is.na(exp_pub_serv_total) |
                exp_pub_serv_total <= 0, 0) +
            replace(exp_student_serv_total, is.na(exp_student_serv_total) |
                exp_student_serv_total <= 0, 0) +
            replace(exp_acad_supp_total, is.na(exp_acad_supp_total) |
                exp_acad_supp_total <= 0, 0) +
            replace(exp_inst_supp_total, is.na(exp_inst_supp_total) |
                exp_inst_supp_total <= 0, 0) +
            replace(exp_total_opm, is.na(exp_total_opm) |
                exp_total_opm <= 0, 0) +
            replace(exp_net_grant_aid_total, is.na(exp_net_grant_aid_total) |
                exp_net_grant_aid_total <= 0, 0)
                ) / cpi2021) %>%
    # Replace missings
    mutate(
        nonauxrevenues_real =
            replace(nonauxrevenues_real, nonauxrevenues_real <= 0, NA),
        nonauxspending_real =
            replace(nonauxspending_real, nonauxspending_real <= 0, NA)) %>%
    # Finance data only reliable after 1987
    filter(year >= 1987)

# Generate tenure data from hand-created file.
tenure.data <- tenure.data %>%
    # Generate professor count for each category of tenure.
    transmute(unitid = unitid,
        year = year,
        # Total profs employed
        nontenured_tenure_count =
            notenure9month_prof_count + notenure12month_prof_count,
        tenuretrack_tenure_count =
            tenuretrack9month_prof_count + tenuretrack12month_prof_count,
        tenured_tenure_count =
            tenured9month_prof_count + tenured12month_prof_count,
        all_tenure_count =
            total9month_prof_count + total12month_prof_count)

# Generate salaries data from hand-created file.
salaries.data <- salaries.data %>%
    # Generate professor count (regardless of contract length)
    transmute(unitid = unitid,
        year = year,
        # Total profs employed
        lecturer_prof_count =
            lect9month_prof_count + lect12month_prof_count +
                instruct9month_prof_count + instruct12month_prof_count,
        assistant_prof_count =
            assist9month_prof_count + assist12month_prof_count,
        full_prof_count =
            full9month_prof_count + assoc9month_prof_count +
                    full12month_prof_count + assoc12month_prof_count,
        all_prof_count =
            total9month_prof_count + total12month_prof_count,
        # Totals paid to profs
        lecturer_profoutlays_nominal =
            replace_na(lect_profoutlays_nominal, 0) +
                replace_na(lect9month_profoutlays_nominal, 0) +
                    replace_na(lect12month_profoutlays_nominal, 0) +
            replace_na(instruct_profoutlays_nominal, 0) +
                replace_na(instruct9month_profoutlays_nominal, 0) +
                    replace_na(instruct12month_profoutlays_nominal, 0),
        assistant_profoutlays_nominal =
            replace_na(assist_profoutlays_nominal, 0) +
                replace_na(assist9month_profoutlays_nominal + assist12month_profoutlays_nominal, 0),
        full_profoutlays_nominal =
            replace_na(full_profoutlays_nominal + assoc_profoutlays_nominal, 0) +
                replace_na(full9month_profoutlays_nominal + assoc9month_profoutlays_nominal +
                    full12month_profoutlays_nominal + assoc12month_profoutlays_nominal, 0),
        all_profoutlays_nominal =
            replace_na(total_profoutlays_nominal, 0) +
                replace_na(total9month_profoutlays_nominal, 0) +
                    replace_na(total12month_profoutlays_nominal, 0)) %>%
    # Average paid to profs
    mutate(lecturer_profsalaries_nominal =
            lecturer_profoutlays_nominal / lecturer_prof_count,
        assistant_profsalaries_nominal =
            assistant_profoutlays_nominal / assistant_prof_count,
        full_profsalaries_nominal =
            full_profoutlays_nominal / full_prof_count,
        all_profsalaries_nominal =
            all_profoutlays_nominal / all_prof_count) %>%
    # Replace zeros with NAs after summing NAs
    mutate(
        lecturer_prof_count = na_if(lecturer_prof_count, 0),
        assistant_prof_count = na_if(assistant_prof_count, 0),
        full_prof_count = na_if(full_prof_count, 0),
        all_prof_count = na_if(all_prof_count, 0),
        lecturer_profoutlays_nominal = na_if(lecturer_profoutlays_nominal, 0),
        assistant_profoutlays_nominal = na_if(assistant_profoutlays_nominal, 0),
        full_profoutlays_nominal = na_if(full_profoutlays_nominal, 0),
        all_profoutlays_nominal = na_if(all_profoutlays_nominal, 0),
        lecturer_profsalaries_nominal = na_if(lecturer_profsalaries_nominal, 0),
        assistant_profsalaries_nominal = na_if(assistant_profsalaries_nominal, 0),
        full_profsalaries_nominal = na_if(full_profsalaries_nominal, 0),
        all_profsalaries_nominal = na_if(all_profsalaries_nominal, 0)) %>%
    # Adjust salaries for inflation, and compute averages
    left_join(cpiu.data, by = "year") %>%
    mutate(
        # Total spent on prof salaries
        lecturer_profoutlays_real = lecturer_profoutlays_nominal / cpi2021,
        assistant_profoutlays_real = assistant_profoutlays_nominal / cpi2021,
        full_profoutlays_real = full_profoutlays_nominal / cpi2021,
        all_profoutlays_real = all_profoutlays_nominal / cpi2021,
        # Mean real salaries
        lecturer_profmeansalary_real = lecturer_profsalaries_nominal / cpi2021,
        assistant_profmeansalary_real = assistant_profsalaries_nominal / cpi2021,
        full_profmeansalary_real = full_profsalaries_nominal / cpi2021,
        all_profmeansalary_real = all_profsalaries_nominal / cpi2021) %>%
    # Remove the nominal values
    select(-c(cpi2021,
        lecturer_profoutlays_nominal, lecturer_profsalaries_nominal,
        assistant_profoutlays_nominal, assistant_profsalaries_nominal,
        full_profoutlays_nominal, full_profsalaries_nominal,
        all_profoutlays_nominal, all_profsalaries_nominal))

# Combine data
urban_ipeds.data <- directory.data %>%
    left_join(finance.data, by = c("unitid", "year")) %>%
    left_join(enrollment.data, by = c("unitid", "year")) %>%
    left_join(salaries.data, by = c("unitid", "year")) %>%
    left_join(tenure.data, by = c("unitid", "year"))


## Calculate the shift-share instruments (see Deming Walters 2017, p.10)

# state appropriations as share of all revenues per uni, in base year 1990-3
# + net tuition fees as a share of all revenues per uni, in base year 1990-3
urban_ipeds.data <- urban_ipeds.data %>%
    filter(year %in% c(1990:1993)) %>%
    group_by(unitid) %>%
    summarise(tuitionrev_baseshare =
            mean(tuitionrev_real / totalrevenues_real, na.rm = TRUE),
        staterevenues_baseshare =
            mean(stateappropriations_real / totalrevenues_real, na.rm = TRUE)) %>%
    ungroup() %>%
    right_join(urban_ipeds.data, by = "unitid")

# Total state uni appropriations + number of (public) unis in the state
urban_ipeds.data <- urban_ipeds.data %>%
    # Restrict to public uni's
    filter(public == 1, forprofit == 0, fouryear == 1) %>%
    # Restrict to unis with financial info
    filter(!is.na(stateappropriations_real), stateappropriations_real > 0) %>%
    # Count the entire state appropriations + tuition revenue by state
    group_by(state, year) %>%
    summarise(allstate_stateappropriations_real =
            sum(stateappropriations_real, na.rm = TRUE),
        allstate_tuitionrev_real =
            sum(tuitionrev_real, na.rm = TRUE),
        stateEnroll_count = sum(enrollment_reported, na.rm = TRUE),
        stateFTE_count = sum(enrollment_fte, na.rm = TRUE),
        stateprof_count = sum(all_prof_count, na.rm = TRUE),
        stateuni_count = n()) %>%
    ungroup() %>%
    # Put back to joined data.
    right_join(urban_ipeds.data, by = c("state", "year"))

# Combine for the base-year appropriations + tuition shift-share instruments
urban_ipeds.data <- urban_ipeds.data %>%
    mutate(
        appropriationshock_peruni_real = staterevenues_baseshare * (
            allstate_stateappropriations_real / stateuni_count),
        tuitionshock_peruni_real = tuitionrev_baseshare * (
            allstate_tuitionrev_real / stateuni_count),
        appropriationshock_perEnroll_real = staterevenues_baseshare * (
            allstate_stateappropriations_real / stateEnroll_count),
        tuitionshock_perEnroll_real = tuitionrev_baseshare * (
            allstate_tuitionrev_real / stateEnroll_count),
        appropriationshock_perFTE_real = staterevenues_baseshare * (
            allstate_stateappropriations_real / stateFTE_count),
        tuitionshock_perFTE_real = tuitionrev_baseshare * (
            allstate_tuitionrev_real / stateFTE_count))

# Reorder the variables
urban_ipeds.data <- urban_ipeds.data %>%
    select(
        unitid, year, inst_name, state,
        public, forprofit, fouryear, associatescollege, cpi2021,
        enrollment_reported, enrollment_fte,
        stateEnroll_count,
        stateFTE_count,
        stateuni_count,
        stateprof_count,
        allstate_stateappropriations_real,
        staterevenues_baseshare,
        allstate_tuitionrev_real,
        tuitionrev_baseshare,
        totalrevenues_real,
        nonauxrevenues_real,
        nonauxspending_real,
        fedappropriations_real,
        stateappropriations_real,
        localappropriations_real,
        tuitionrev_real,
        appropriationshock_peruni_real,
        tuitionshock_peruni_real,
        appropriationshock_perEnroll_real,
        tuitionshock_perEnroll_real,
        appropriationshock_perFTE_real,
        tuitionshock_perFTE_real,
        lecturer_prof_count,
        lecturer_profoutlays_real,
        lecturer_profmeansalary_real,
        assistant_prof_count,
        assistant_profoutlays_real,
        assistant_profmeansalary_real,
        full_prof_count,
        full_profoutlays_real,
        full_profmeansalary_real,
        all_prof_count,
        all_profoutlays_real,
        all_profmeansalary_real,
        nontenured_tenure_count,
        tenuretrack_tenure_count,
        tenured_tenure_count,
        all_tenure_count) %>%
    arrange(unitid, year)

# Remove double observations created by the multiple merges.
urban_ipeds.data <- urban_ipeds.data %>%
    distinct(unitid, year, .keep_all = TRUE)

# Make year refer to the year of spring term, and not autumn of calendar year
urban_ipeds.data <- urban_ipeds.data %>%
    mutate(year = year + 1)


################################################################################
## Save the constructed data file.

# all unis
urban_ipeds.data %>%
    write_csv("../../data/urban-ipeds/urban-clean-allunis.csv")

# Only 4-year, public, non-forprofit unis
urban_ipeds.data %>%
    filter(fouryear == 1, public == 1, forprofit == 0) %>%
    write_csv("../../data/urban-ipeds/urban-clean-publicunis.csv")

#! WHich years are covered?
print("all years")
urban_ipeds.data %>% pull(year) %>% table()
print("variables")
urban_ipeds.data %>% filter(!is.na(enrollment_fte)) %>% pull(year) %>% max(na.rm = TRUE)
urban_ipeds.data %>% filter(!is.na(totalrevenues_real)) %>% pull(year) %>% max(na.rm = TRUE)
urban_ipeds.data %>% filter(!is.na(nonauxspending_real)) %>% pull(year) %>% max(na.rm = TRUE)
urban_ipeds.data %>% filter(!is.na(appropriationshock_perEnroll_real)) %>% pull(year) %>% table()
