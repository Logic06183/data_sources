# Corrections (September 2026)

Two problems were found in this folder while reusing the ward index for later work. Neither changes the published maps or the list of most vulnerable wards, but both matter for anyone joining the data to other tables.

## 1. The HVI columns in data.csv run the wrong way

The precomputed columns `HVI_PC1`, `HVI_PC1_standardized`, `HVI_weighted` and `HVI_weighted_standardized` (and the `Ward_HVI_1_*` copies) are oriented so that a higher value means *less* vulnerable. They correlate at -1.00 with the index as computed in `reproduce.R`, and Sandton scores highest on them. The sign of a principal component is arbitrary and was not fixed when those columns were written.

The maps and the top-10 ward list come from `reproduce.R`, which recomputes the index, so they are correct. Only the stored columns are affected.

**Use `outputs/hvi_ward_index.csv`** (column `hvi`, 0 = least and 1 = most vulnerable), made by `make_ward_index.py`. The original columns are left in `data.csv` unchanged, as the record of what was used.

## 2. One row is not a City of Johannesburg ward

The row with `WardID_ = 74205010` (numbered ward 10) is a ward from the neighbouring municipality (Municipal Demarcation Board code 742), not Johannesburg ward 10 (`79800010`), which is also present. Johannesburg ward 78 (`79800078`) is missing from `data.csv` and `geometry.shp`. The analysis therefore covered 134 Johannesburg wards plus one outside the city.

Excluding the extra ward changes the index negligibly (r = 1.000 with the index on all 135 rows) and it sat mid-table (rank 67), so the published findings stand. `hvi_ward_index.csv` flags it (`in_johannesburg = False`) and computes `hvi` on the 134 Johannesburg wards only. Ward 78 has no value.

**Join on `WardID`, not ward number**, because ward number 10 appears twice.

## 3. reproduce.R working directory

`reproduce.R` set a fixed Windows working directory. It now runs from the folder it sits in.
