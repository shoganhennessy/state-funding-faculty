#!~/anaconda3/bin/python3
## Senan Hogan-Hennessy, 17 August 2021
## Clean raw NSF-HERD data files to one data matrix

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


################################################################################
## Define functions to consistently take columns from the yearly HERD files.

# Take a column from the raw data, with empty if some weeks don't have the value.
def emptyColumn(data, column):
    dataColumns = list(data.columns)
    if column in dataColumns:
        return data[column]
    else:
        return pd.Series([None] * data.shape[0])

# Functionalise creating a copy from the raw data.
def cleanHerdData(herdData, year):
    # Take a copy of useful variables
    rawData = herdData[["question", "row", "data"]].copy()
    # Take the HERDS identifier
    if year < 2010 : unitid = "fice"
    else : unitid = "inst_id"
    rawData["herds_id"] = herdData[unitid]
    # Take the IPEDS id (if available)
    rawData["ipeds_id"] = emptyColumn(herdData, "ipeds_unitid")
    # Restrict to useful rows of the (long format) survey
    rawData = rawData[rawData["question"] == "Source"]
    # Long to wide
    cleanData = rawData.pivot(index=["herds_id", "ipeds_id"],
        columns="row", values="data")
    cleanData.reset_index(level=["herds_id", "ipeds_id"], inplace=True)
    # Record which it is
    cleanData["year"] = year
    # Return the data
    return cleanData


###############################################################################
## Import data

herdPath = os.path.join("..", "data", "nsf-herd", "downloaded-csv")
# Create a base to append the years of the panel to
collectData = []

# Loop over years of the raw data.
for year in range(1972, 2019 + 1):
    print(year)
    # Import the raw pulse data
    herdFilePath = os.path.join(herdPath, "herd_" + str(year) + ".csv")
    herdData = pd.read_csv(herdFilePath, encoding="cp1252")
    ## Take relevant variables
    cleanData = cleanHerdData(herdData, year)
    # Add to the list
    collectData.append(cleanData)

# Put together the collected data
collectData = pd.concat(collectData, ignore_index=True)


###############################################################################
## Clean collected data.
collectData.sort_values(by=["herds_id", "year"], inplace=True)

collectData.columns

# Clean the total funded research
collectData["total_research_nominal"] = collectData["Total"]

# Clean the institutional funded research, from two interspersed columns
collectData["institional_research_nominal"] = collectData[
    "Institution funds, total"]
collectData.loc[pd.isnull(collectData["institional_research_nominal"]),
    "institional_research_nominal"] = collectData.loc[
        pd.isnull(collectData["institional_research_nominal"]),
            "Institution funds"]

# Clean the federally funded research, from two interspersed columns
collectData["federal_research_nominal"] = collectData["Federal"]
collectData.loc[pd.isnull(collectData["federal_research_nominal"]),
    "federal_research_nominal"] = collectData.loc[
        pd.isnull(collectData["federal_research_nominal"]),
            "Federal government"]

# Clean the industry funded research, from two interspersed columns
collectData["industry_research_nominal"] = collectData["Industry"]
collectData.loc[pd.isnull(collectData["industry_research_nominal"]),
    "industry_research_nominal"] = collectData.loc[
        pd.isnull(collectData["industry_research_nominal"]),
            "Business"]

# Clean the state funded research
collectData["state_research_nominal"] = collectData["State and local government"]

# Otherwise funded research
collectData["other_research_nominal"] = collectData[
    "total_research_nominal"] - collectData[
        "institional_research_nominal"].fillna(0) - collectData[
        "federal_research_nominal"    ].fillna(0) - collectData[
        "state_research_nominal"      ].fillna(0) - collectData[
        "industry_research_nominal"   ].fillna(0)

# Non-state funded research
collectData["nonstate_research_nominal"] = collectData[
    "total_research_nominal"] - collectData[
        "state_research_nominal"].fillna(0)

# Spread IPEDS ID across every year of data
collectData["ipeds_id"] = collectData.groupby(
    ["herds_id"])["ipeds_id"].transform("max")

# Restrict to relevant columns
relevantVar = ["total_research_nominal",
    "institional_research_nominal",
    "federal_research_nominal",
    "state_research_nominal", 
    "industry_research_nominal",
    "other_research_nominal",
    "nonstate_research_nominal"]
collectData = collectData[["ipeds_id", "year"] + relevantVar]

# Scale for units of HERD survey (every question is answered to nearest 1000)
for var in relevantVar:
    collectData[var] = 1000 * collectData[var]

# Save data file.
collectData.to_csv(os.path.join("..", "data", "nsf-herd", "nsf-herd-clean.csv"),
    index=False)
