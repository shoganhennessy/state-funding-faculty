#!~/anaconda3/bin/python3

## Senan Hogan-Hennessy, 27 September 2021
## Graph from cleaned data

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
# On Public unis
dataFolder = os.path.join("..", "..", "data", "delta-cost",
    "delta-cost-clean.csv")
publicUniData = pd.read_csv(os.path.join(dataFolder,
    "delta-cost-clean-publicunis.csv"))

# On all unis
allUniData = pd.read_csv(os.path.join(dataFolder,
    "delta-cost-clean-allunis.csv"))


###############################################################################
## Write figures.

# Replicate figure A-4 of Lovenheim appendix (2020)
sa_share_1987 = publicUniData[publicUniData["academicyear"] == 1987][
    ["instname", "stateappropriations_share"]]
sa_share_1987.rename(columns={"stateappropriations_share" :
    "stateappropriations_1987_share"}, inplace=True)
sa_share_2015 = publicUniData[publicUniData["academicyear"] == 2015][
    ["instname", "stateappropriations_share"]]
sa_share_2015.rename(columns={"stateappropriations_share" :
    "stateappropriations_2015_share"}, inplace=True)
saShare = pd.merge(sa_share_1987, sa_share_2015, how="left", on="instname")
import statsmodels.api as sm
X = np.array(saShare["stateappropriations_1987_share"]).reshape(-1,1)
Y = np.array(saShare["stateappropriations_2015_share"]).reshape(-1,1)
bestFit = sm.OLS(Y, X, missing="drop").fit()
# Start the plot
plt.figure(figsize=(7, 5))
plt.scatter(X, Y)
plt.plot(X, bestFit.params[0] * X, color="orange")
plt.xlabel("State Appropriations Share, 1987")
#plt.margins(x=0)
plt.ylabel("State Appropriations Share, 2015", rotation=0)
plt.gca().yaxis.set_label_coords(0.1, 1.025)
plt.tight_layout()
plt.savefig(os.path.join("..", "..", "text", "presentation",
    "figures", "A4-replicate"), bbox_inches="tight")
