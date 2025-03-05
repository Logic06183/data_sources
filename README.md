# Heat Vulnerability Index (HVI) Johannesburg - Data Sources

This folder contains the primary data sources used in the Heat Vulnerability Index analysis for Johannesburg.

## Files

### 1. data.csv
Tabular data containing various indicators used for the Heat Vulnerability Index analysis, including:

#### Socioeconomic Variables:
- Crowded dwellings
- No piped water
- Using public healthcare facilities
- No medical insurance
- Household hunger risk
- Benefiting from school feeding scheme
- 60+ population proportion

#### Health Variables:
- Poor health status
- Failed to find healthcare when needed
- Health concern
- Disease prevalence indicators:
  - Cancer proportion
  - Diabetes proportion
  - Pneumonia proportion
  - Heart disease proportion
  - Hypertension proportion
  - HIV proportion
  - TB proportion
  - COVID proportion

#### Environmental Variables:
- UTFVI (Urban Thermal Field Variance Index)
- LST (Land Surface Temperature)
- NDVI (Normalized Difference Vegetation Index)
- NDBI (Normalized Difference Built-up Index)

### 2. geometry.shp (and related files)
Spatial data containing the ward boundaries for Johannesburg. The shapefile is in WGS 1984 UTM Zone 35S projection and includes:
- geometry.shp: The main shapefile with geometry
- geometry.shx: Shape index file
- geometry.dbf: Attribute data
- geometry.prj: Projection information
- geometry.cpg: Character encoding information

## Data Processing
These files are used in the Heat Vulnerability Index analysis to:
1. Perform Geographically Weighted Principal Component Analysis (GWPCA)
2. Calculate the Heat Vulnerability Index
3. Identify the most vulnerable wards in Johannesburg
4. Analyze spatial patterns of vulnerability

The analysis scripts can be found in the main project directory and the cache folder.
