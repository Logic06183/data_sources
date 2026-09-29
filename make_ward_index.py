"""Ward heat vulnerability index table for Johannesburg, from data.csv.

Reproduces the index in reproduce.R (first principal component of the 22
standardised indicators, rescaled 0 to 1) and writes it as a table that other
analyses can join on. See CORRECTIONS.md for why this file exists.

Output: outputs/hvi_ward_index.csv
  WardID, WardNo, hvi (0 = least, 1 = most vulnerable), hvi_rank, hvi_quintile,
  in_johannesburg, hvi_published_rows (index computed on all 135 rows, as in the paper)
"""
import numpy as np
import pandas as pd

VARS = ["Crowded dwellings", "No piped water", "Using public healthcare facilities", "Poor health status",
        "Failed to find healthcare when needed", "No medical insurance", "Household hunger risk",
        "Benefiting from school feeding scheme", "UTFVI", "LST", "NDVI", "NDBI__mean", "concern_he",
        "cancer_pro", "diabetes_p", "pneumonia_", "heart_dise", "hypertensi", "hiv_prop", "tb_prop",
        "covid_prop", "60_plus_pr"]


def pc1_index(df):
    z = df[VARS].astype(float)
    z = (z - z.mean()) / z.std(ddof=1)
    _, s, vt = np.linalg.svd(z.to_numpy(), full_matrices=False)
    pc = z.to_numpy() @ vt[0]
    # PCA sign is arbitrary: orient so that higher = more vulnerable (more crowding)
    if np.corrcoef(pc, df["Crowded dwellings"])[0, 1] < 0:
        pc = -pc
    return (pc - pc.min()) / (pc.max() - pc.min()), float(s[0] ** 2 / np.sum(s ** 2))


d = pd.read_csv("data.csv")
d["WardID"] = d["WardID_"].astype(str)
d["in_johannesburg"] = d["WardID"].str.startswith("798")
d["hvi_published_rows"], _ = pc1_index(d)
jhb = d[d["in_johannesburg"]].copy()
jhb["hvi"], var = pc1_index(jhb)
jhb["hvi_rank"] = jhb["hvi"].rank(ascending=False, method="min").astype(int)
jhb["hvi_quintile"] = pd.qcut(jhb["hvi"], 5, labels=[1, 2, 3, 4, 5]).astype(int)
out = d[["WardID", "WardNo_", "in_johannesburg", "hvi_published_rows"]].merge(
    jhb[["WardID", "hvi", "hvi_rank", "hvi_quintile"]], on="WardID", how="left").rename(columns={"WardNo_": "WardNo"})
out = out.sort_values(["in_johannesburg", "WardNo"], ascending=[False, True])
out.to_csv("outputs/hvi_ward_index.csv", index=False)
print(f"{int(jhb.shape[0])} Johannesburg wards; PC1 explains {var:.1%}; "
      f"r(corrected, published rows) = {np.corrcoef(jhb['hvi'], jhb['hvi_published_rows'])[0, 1]:.4f}")
