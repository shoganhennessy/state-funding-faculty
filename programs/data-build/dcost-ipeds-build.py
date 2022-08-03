#!~/anaconda3/bin/python3

## Senan Hogan-Hennessy, 17 August 2021
## Wrangle a dataframe of interest from delta cost project data

# Various packages for use.
import numpy as np
npSeed = np.random.default_rng(47)
import pandas as pd
import os
# data viewer for exploration
from gtabview import view
# Plotting in exploration
import matplotlib.pyplot as plt

# freq table
from collections import Counter


###############################################################################
## Import data
dataFolder = os.path.join("..", "..", "data", "delta-cost", "Delta_database_87_2015_STATA")
dataPath = os.path.join(dataFolder, "delta_public_release_87_15.dta")

# Load the master data file
dcostData = pd.read_stata(dataPath)

# Sort by Institution and year
dcostData.sort_values(by=["instname", "academicyear"], inplace=True)
dcostData.set_index(["unitid", "academicyear"], drop=False, inplace=True)


###############################################################################
## Take relevant variables

# First the identifying variables of the panel, name year IPEDid state
cleanData = dcostData[["instname", "unitid", "academicyear", "state"]].copy()

# Number of full-time enrollment
cleanData["fullenrollment_count"] = dcostData["fte_count"]

# CPI for 2015
cleanData["cpi2015"] = dcostData["cpi_scalar_2015"]

# Indicator for public uni
cleanData["public"] = (dcostData["control"] == 1).astype(int)

# Indicator for the uni being for-profit
cleanData["forprofit"] = (np.abs(dcostData["control"]) == 3).astype(int)

# Indicator for 4-year unis
cleanData["fouryear"] = (np.abs(dcostData["iclevel"]) == 1).astype(int)

# Grand total expenditures - current year total
cleanData["totalspending_nominal"] = dcostData["total01"]
#print(Counter(cleanData[pd.notna(cleanData["totalspending_nominal"])]["academicyear"]))
# Total01 starts in 1997, so use a sum of expenditures instead.
cleanData["totalspending_nominal"] = dcostData["eandg01_sum"]
#print(Counter(cleanData[pd.notna(cleanData["totalspending_nominal"])]["academicyear"]))

# Revenue from federal appropriations
cleanData["fedappropriations_nominal"] = dcostData["federal03"]

# Revenue from state appropriations
cleanData["stateappropriations_nominal"] = dcostData["state03"]

# Revenue from local appropriations
cleanData["localappropriations_nominal"] = dcostData["local03"]

# Total current funds revenues (IPEDS)
cleanData["totalrevenues_nominal"] = dcostData["total03_revenue"]
#print(Counter(cleanData[pd.notna(cleanData["totalrevenues_nominal"])]["academicyear"]))

# Number of tenure/tenure-track faculty
cleanData["proftenured_count"] = dcostData["ft_tenure_t1"]

# Number of non-tenure/tenure-track faculty
cleanData["profsnotenure_count"] = dcostData["ft_tenure_t2"] + dcostData[
    "ft_tenure_t3"] + dcostData["ft_tenure_t4"]

# Salaries paid to teaching faculty
cleanData["profsalaries_nominal"] = dcostData["salarytotal"]

# Salaries paid to non-teaching faculty
cleanData["profsnotenure_nominal"] = dcostData["salarytotal_fteap1b"]

# Salaries paid to management faculty
cleanData["profsnotenure_nominal"] = dcostData["salarytotal_fteap3"]

# Expenditures for instruction - current year total
cleanData["instructionspending_nominal"] = dcostData["instruction01"]

# Expenditures for instruction - salaries and wages
cleanData["instructionsalaries_nominal"] = dcostData["instruction02"]

# Expenditures for research - current year total
cleanData["researchspending_nominal"] = dcostData["research01"]

# Expenditures for research - salaries and wages
cleanData["researchsalaries_nominal"] = dcostData["research02"]

# Full-time faculty (instruction/research/public service) all
cleanData["faculty_fulltime"] = dcostData["ftall1"]

# Full-time executive/administrative and managerial all
cleanData["admin_fulltime"] = dcostData["ftall3"]

# Full-time other professionals all
cleanData["other_fulltime"] = dcostData["ftall4"]

# Full-time technical and paraprofessionals all
cleanData["technical_fulltime"] = dcostData["ftall5"]

# Full-time clerical and secretarial all
cleanData["clerical_fulltime"] = dcostData["ftall6"]

# Full-time skilled crafts all
cleanData["crafts_fulltime"] = dcostData["ftall7"]

# Full-time service/maintenance all
cleanData["service_fulltime"] = dcostData["ftall8"]

# Full-time All
cleanData["all_fulltime"] = cleanData["faculty_fulltime"].fillna(0
    ) + cleanData["admin_fulltime"].fillna(0
    ) + cleanData["faculty_fulltime"].fillna(0
    ) + cleanData["other_fulltime"].fillna(0
    ) + cleanData["technical_fulltime"].fillna(0
    ) + cleanData["clerical_fulltime"].fillna(0
    ) + cleanData["crafts_fulltime"].fillna(0
    ) + cleanData["service_fulltime"].fillna(0)

# Part-time faculty (instruction/research/public service) all
cleanData["faculty_parttime"] = dcostData["ptall1"]

# Part-time instruction/research assistants all
cleanData["instructionRA_parttime"] = dcostData["ptall2"]

# Part-time executive/administrative and managerial all
cleanData["admin_parttime"] = dcostData["ptall3"]

# Part-time other professionals all
cleanData["other_parttime"] = dcostData["ptall4"]

# Part-time technical and paraprofessionals all
cleanData["technical_parttime"] = dcostData["ptall5"]

# Part-time clerical and secretarial all
cleanData["clerical_parttime"] = dcostData["ptall6"]

# Part-time skilled crafts all
cleanData["crafts_parttime"] = dcostData["ptall7"]

# Part-time service/maintenance all
cleanData["service_parttime"] = dcostData["ptall8"]

# Part-time All
cleanData["all_parttime"] = cleanData["faculty_parttime"].fillna(0
    ) + cleanData["instructionRA_parttime"].fillna(0
    ) + cleanData["admin_parttime"].fillna(0
    ) + cleanData["faculty_parttime"].fillna(0
    ) + cleanData["other_parttime"].fillna(0
    ) + cleanData["technical_parttime"].fillna(0
    ) + cleanData["clerical_parttime"].fillna(0
    ) + cleanData["crafts_parttime"].fillna(0
    ) + cleanData["service_parttime"].fillna(0)

# Full-time instructional faculty, tenure or tenure-track
cleanData["tenured_faculty_count"] = dcostData["ft_tenure_t1"]

# Full-time instructional faculty, multi-year contract
cleanData["contract_faculty_count"] = dcostData["ft_tenure_t2"]

# Full-time instructional faculty, annual or less than one-year contract
cleanData["annual_faculty_count"] = dcostData["ft_tenure_t3"]

# Full-time instructional faculty, without faculty status (incl. no tenure system)
cleanData["notenure_faculty_count"] = dcostData["ft_tenure_t4"]

# All non-tenured Full-time instructional faculty
cleanData["nontenured_faculty_count"] = cleanData[
    "contract_faculty_count"].fillna(0
    ) + cleanData["annual_faculty_count"].fillna(0
    ) + cleanData["notenure_faculty_count"].fillna(0)

# Average salary for full-time faculty
cleanData["average_facultysalary_nominal"] = dcostData["ft_faculty_salary"]

# Total number of full-time instructional faculty
cleanData["total_faculty_count"] = dcostData["faculty_instr_headcount"]

# Total salary outlays of full-time instructional faculty
cleanData["total_facultysalary_nominal"] = dcostData["salarytotal"]


###############################################################################
## Manipulate these data.

cleanData["year"] = cleanData["academicyear"]

# Count of students in the entire state
cleanData["statestudent_count"] = cleanData.groupby(["state", "year"]
    )["fullenrollment_count"].transform("sum")

# Count of uni-staff in the entire state
cleanData["statefulltime_count"] = cleanData.groupby(["state", "year"]
    )["all_fulltime"].transform("sum")
cleanData["stateparttime_count"] = cleanData.groupby(["state", "year"]
    )["all_parttime"].transform("sum")
# Replace zero entires with missing
cleanData["statefulltime_count"].replace(0, np.NaN, inplace=True)
cleanData["statefulltime_count"].replace(np.inf, np.NaN, inplace=True)
cleanData["stateparttime_count"].replace(0, np.NaN, inplace=True)
cleanData["stateparttime_count"].replace(np.inf, np.NaN, inplace=True)

# Count of public unis in the entire state
cleanData["stateuni_count"] = ((cleanData["fouryear"] == 1) + (
    cleanData["public"] == 1) + (
        cleanData["forprofit"] == 0)).astype(int)
cleanData["stateuni_count"] = cleanData.groupby(["state", "year"]
    )["stateuni_count"].transform("sum")

## Create the "appropriation shock" Deming Walters (2017, p.11)
# Share of appropriations coming from the state
cleanData["stateappropriations_share"] = cleanData[
    "stateappropriations_nominal"] / cleanData["totalrevenues_nominal"]

# Share of appropriations coming from the state, in base year 1990
cleanData["stateappropriations1990_nominal"] = cleanData[
    "stateappropriations_nominal"] * (cleanData["year"] == 1990).astype(int)
cleanData["stateappropriations1990_nominal"] = cleanData.groupby(
    ["instname"])["stateappropriations1990_nominal"].transform("sum")
cleanData["totalrevenues1990_nominal"] = cleanData[
    "totalrevenues_nominal"] * (cleanData["year"] == 1990).astype(int)
cleanData["totalrevenues1990_nominal"] = cleanData.groupby(
    ["instname"])["totalrevenues1990_nominal"].transform("sum")
cleanData["stateappropriations1990_share"] = cleanData[
    "stateappropriations1990_nominal"] / cleanData["totalrevenues1990_nominal"]
cleanData["entire_stateappropriations_nominal"] = cleanData.groupby(
    ["state", "year"])["stateappropriations_nominal"].transform("sum")

## The instrument in terms of student populations 
# The entire state's appropriations, divided by student populations.
cleanData["stateappropriations_perstudent_nominal"] = cleanData[
    "entire_stateappropriations_nominal"] / cleanData["statestudent_count"]
# Create the instrument as product share * approp (per FTE)
cleanData["appropriationshock_perstudent_nominal"] = cleanData[
    "stateappropriations1990_share"] * cleanData[
        "stateappropriations_perstudent_nominal"]

## The instrument in terms of number of state uni staff
# The entire state's appropriations, divided by full-time) staff populations.
cleanData["stateappropriations_perfulltime_nominal"] = cleanData[
    "entire_stateappropriations_nominal"] / cleanData["statefulltime_count"]
# Create the instrument as product share * approp (per FTE)
cleanData["appropriationshock_perfulltime_nominal"] = cleanData[
    "stateappropriations1990_share"] * cleanData[
        "stateappropriations_perfulltime_nominal"]

## The instrument in terms of number of state unis 
# The entire state's appropriations, divided by full-time) staff populations.
cleanData["stateappropriations_peruni_nominal"] = cleanData[
    "entire_stateappropriations_nominal"] / cleanData["stateuni_count"]
# Create the instrument as product share * approp (per state uni)
cleanData["appropriationshock_peruni_nominal"] = cleanData[
    "stateappropriations1990_share"] * cleanData[
        "stateappropriations_peruni_nominal"]

# Drop redundant column
cleanData.drop(columns="academicyear", inplace=True)


###############################################################################
## Save the constructed data file.
cleanData.to_csv(os.path.join("..", "..", "data", "delta-cost",
    "delta-cost-clean-allunis.csv"), index=False)

# Save the subset of only public, 4-year, not for-profit unis
cleanData = cleanData[cleanData["fouryear"] == 1].copy()
cleanData = cleanData[cleanData["public"] == 1].copy()
cleanData = cleanData[cleanData["forprofit"] == 0].copy()
cleanData.drop(columns=["fouryear", "public", "forprofit"], inplace=True)
cleanData.to_csv(os.path.join("..", "..", "data", "delta-cost",
    "delta-cost-clean-publicunis.csv"), index=False)
