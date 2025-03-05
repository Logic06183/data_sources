# Heat Vulnerability Analysis in Johannesburg
# Complete R Script for replicating results with improved visualization saving

# Set global options
options(warn = -1)  # Suppress warnings

# Load necessary libraries
suppressPackageStartupMessages({
  library(sf)            # For handling spatial data
  library(dplyr)         # Data manipulation
  library(ggplot2)       # Data visualization
  library(corrplot)      # Correlation plots
  library(psych)         # PCA and psychometric analysis
  library(GWmodel)       # Geographically Weighted Models
  library(spdep)         # Spatial dependence analysis
  library(factoextra)    # Visualization for PCA
  library(RColorBrewer)  # Color palettes
  library(viridis)       # Color palettes
  library(tidyr)         # Data reshaping
  library(scales)        # Scaling data
})

# Set working directory
# IMPORTANT: Change this to your own directory where the data is stored
setwd("C:/Users/CraigParker/OneDrive - Wits PHR/Desktop/HVI_Johannesburg")

# Create output directory if it doesn't exist
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Test basic plot saving to diagnose any device issues
cat("Testing basic plot saving...\n")
test_plot <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point() + 
  labs(title = "Test Plot")

# Save using ggsave (more reliable)
ggsave("outputs/test_plot.pdf", test_plot, width = 8, height = 6)
ggsave("outputs/test_plot.png", test_plot, width = 8, height = 6, dpi = 100)
cat("Test plots saved. If these open, ggsave is working correctly.\n\n")

# Read spatial data
cat("Reading spatial data...\n")
gwpca_sf <- st_read("geometry.shp", quiet = TRUE)

# If the data is in a different CRS, transform it to a common CRS
gwpca_sf <- st_transform(gwpca_sf, crs = 32735)  # WGS 84 / UTM zone 35S

# Check the actual column names in your data
cat("Available columns in the dataset:\n")
print(names(gwpca_sf))

# List of variables to be used in the analysis
vars <- c(
  "Crowded.dw", "No.piped.w", "Using.publ", 
  "Poor.healt", "Failed.to", "No.medical",
  "Household", "Benefiting", "UTFVI", "LST", "NDVI",
  "NDBI__mean", "concern_he", "cancer_pro", "diabetes_p", 
  "pneumonia_", "heart_dise", "hypertensi", "hiv_prop", 
  "tb_prop", "covid_prop", "X60_plus_pr"
)

# Verify variable names in the dataset
cat("Verifying variable names...\n")
missing_vars <- vars[!vars %in% names(gwpca_sf)]
if(length(missing_vars) > 0){
  cat("The following variables are missing from gwpca_sf:\n")
  print(missing_vars)
  cat("Please check the variable names in your dataset.\n")
  stop("Missing variables. Check column names in your data.")
} else {
  cat("All variables in 'vars' are present in gwpca_sf.\n")
}

# Subset the data to include only the variables in 'vars' and geometry
gwpca_data <- gwpca_sf %>%
  select(all_of(vars), geometry)

# Convert variables to numeric
for (var in vars) {
  gwpca_data[[var]] <- as.numeric(gwpca_data[[var]])
}

# Remove rows with missing values in the selected variables
gwpca_data <- gwpca_data %>%
  drop_na(all_of(vars))

cat("Data preparation complete. Number of observations:", nrow(gwpca_data), "\n")

# Compute summary statistics
cat("Computing summary statistics...\n")
summary_stats <- gwpca_data %>%
  st_set_geometry(NULL) %>%
  summarise(across(everything(), list(
    mean = mean, sd = sd, median = median, min = min, max = max
  )))

# Reshape data for better display
summary_stats_long <- summary_stats %>%
  pivot_longer(cols = everything(),
               names_to = c("Variable", "Statistic"),
               names_sep = "_",
               values_to = "Value")

print(summary_stats_long)

# Define variable groups
variable_groups <- c(
  "Crowded.dw" = "Socioeconomic",
  "No.piped.w" = "Socioeconomic",
  "Using.publ" = "Socioeconomic",
  "Poor.healt" = "Health",
  "Failed.to" = "Health",
  "No.medical" = "Socioeconomic",
  "Household" = "Socioeconomic",
  "Benefiting" = "Socioeconomic",
  "UTFVI" = "Environmental",
  "LST" = "Environmental",
  "NDVI" = "Environmental",
  "NDBI__mean" = "Environmental",
  "concern_he" = "Health",
  "cancer_pro" = "Health",
  "diabetes_p" = "Health",
  "pneumonia_" = "Health",
  "heart_dise" = "Health",
  "hypertensi" = "Health",
  "hiv_prop" = "Health",
  "tb_prop" = "Health",
  "covid_prop" = "Health",
  "X60_plus_pr" = "Socioeconomic"
)

# Update variable labels to be more descriptive
variable_labels <- c(
  "Crowded.dw" = "Crowded Dwellings",
  "No.piped.w" = "No Piped Water",
  "Using.publ" = "Public Healthcare Use",
  "Poor.healt" = "Poor Health Status",
  "Failed.to" = "Failed to Find Healthcare",
  "No.medical" = "No Medical Insurance",
  "Household" = "Household Hunger Risk",
  "Benefiting" = "School Feeding Scheme",
  "UTFVI" = "UTFVI",
  "LST" = "Land Surface Temp",
  "NDVI" = "NDVI",
  "NDBI__mean" = "NDBI Mean",
  "concern_he" = "Health Concern",
  "cancer_pro" = "Cancer Proportion",
  "diabetes_p" = "Diabetes Proportion",
  "pneumonia_" = "Pneumonia Proportion",
  "heart_dise" = "Heart Disease Proportion",
  "hypertensi" = "Hypertension Proportion",
  "hiv_prop" = "HIV Proportion",
  "tb_prop" = "TB Proportion",
  "covid_prop" = "COVID Proportion",
  "X60_plus_pr" = "60+ Population Proportion"
)

# Create a data frame with variable names, groups, and labels
variable_info <- data.frame(
  variable = names(variable_groups),
  group = unname(variable_groups),
  label = unname(variable_labels),
  stringsAsFactors = FALSE
)

# Compute correlation matrix
cat("Computing correlation matrix...\n")
corr_matrix <- gwpca_data %>%
  st_set_geometry(NULL) %>%
  cor(use = "complete.obs")

# Order variables by group
variable_info_ordered <- variable_info %>%
  arrange(factor(group, levels = c("Environmental", "Health", "Socioeconomic")), variable)

ordered_variables <- variable_info_ordered$variable
variable_labels_ordered <- variable_info_ordered$label

# Reorder the correlation matrix
corr_matrix_ordered <- corr_matrix[ordered_variables, ordered_variables]

# Set the row and column names to the descriptive labels
rownames(corr_matrix_ordered) <- variable_labels_ordered
colnames(corr_matrix_ordered) <- variable_labels_ordered

# Assign colors to groups
group_colors <- c("Environmental" = "forestgreen", "Health" = "darkorange", "Socioeconomic" = "purple")
label_colors_ordered <- group_colors[variable_info_ordered$group]

# Create a correlation plot
cat("Creating correlation plot...\n")

# Save as PDF using the direct approach with corrplot
pdf("outputs/correlation_matrix.pdf", width = 12, height = 10)
cat("PDF device opened...\n")
corrplot(corr_matrix_ordered,
         method = "color",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         type = "upper",
         order = "original",
         tl.cex = 0.6,
         tl.col = label_colors_ordered,
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.5,
         mar = c(0,0,2,0))
legend("topright", legend = names(group_colors), col = group_colors, pch = 15, cex = 0.8)
cat("Plot drawn to PDF...\n")
dev.off()
cat("PDF device closed...\n")

# Save as PNG
png("outputs/correlation_matrix.png", width = 1200, height = 1000, res = 100)
cat("PNG device opened...\n")
corrplot(corr_matrix_ordered,
         method = "color",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         type = "upper",
         order = "original",
         tl.cex = 0.6,
         tl.col = label_colors_ordered,
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.5,
         mar = c(0,0,2,0))
legend("topright", legend = names(group_colors), col = group_colors, pch = 15, cex = 0.8)
cat("Plot drawn to PNG...\n")
dev.off()
cat("PNG device closed...\n")

cat("Correlation plot saved to 'outputs/correlation_matrix.pdf' and 'outputs/correlation_matrix.png'\n")

# Perform PCA
cat("Performing PCA...\n")
pca_data <- gwpca_data %>%
  st_set_geometry(NULL) %>%
  drop_na()

pca_result <- prcomp(pca_data, scale. = TRUE)

# Print PCA summary
cat("PCA Summary:\n")
print(summary(pca_result))

# Create PCA variable loadings plot
cat("Creating PCA variable loadings plot...\n")
pca_loadings_plot <- fviz_pca_var(pca_result,
                                  geom = c("arrow", "text"),
                                  col.var = variable_info$group,
                                  palette = group_colors,
                                  legend.title = "Variable Groups",
                                  repel = TRUE) +
  labs(title = "PCA Variable Loadings Grouped by Category")

# Save PCA plot using ggsave
ggsave("outputs/pca_loadings.pdf", pca_loadings_plot, width = 10, height = 8)
ggsave("outputs/pca_loadings.png", pca_loadings_plot, width = 10, height = 8, dpi = 100)

cat("PCA loadings plot saved to 'outputs/pca_loadings.pdf' and 'outputs/pca_loadings.png'\n")

# Extract the first principal component for HVI
cat("Calculating Heat Vulnerability Index...\n")
gwpca_data$HVI <- pca_result$x[, 1]

# Normalize HVI to a 0-1 scale
gwpca_data$HVI <- rescale(gwpca_data$HVI, to = c(0, 1))

# Create HVI map
cat("Creating HVI map...\n")
hvi_map <- ggplot(gwpca_data) +
  geom_sf(aes(fill = HVI)) +
  scale_fill_viridis_c(option = "inferno") +
  theme_minimal() +
  labs(title = "Heat Vulnerability Index", fill = "HVI")

# Save HVI map using ggsave
ggsave("outputs/hvi_map.pdf", hvi_map, width = 10, height = 8)
ggsave("outputs/hvi_map.png", hvi_map, width = 10, height = 8, dpi = 100)

cat("HVI map saved to 'outputs/hvi_map.pdf' and 'outputs/hvi_map.png'\n")

# Select top 10 most vulnerable wards
top_10_vulnerable_wards <- gwpca_data %>%
  arrange(desc(HVI)) %>%
  slice(1:10)

cat("Top 10 most vulnerable wards:\n")
print(top_10_vulnerable_wards %>% 
        st_set_geometry(NULL) %>%
        select(HVI))

# Create top 10 vulnerable wards map
cat("Creating top 10 vulnerable wards map...\n")
top10_map <- ggplot() +
  geom_sf(data = gwpca_data, aes(fill = HVI), alpha = 0.6) +
  geom_sf(data = top_10_vulnerable_wards, fill = "black", color = "black", size = 0.7, alpha = 0.8) +
  scale_fill_viridis_c(option = "inferno") +
  theme_minimal() +
  labs(title = "Top 10 Most Vulnerable Wards in Johannesburg", fill = "HVI")

# Save top 10 map using ggsave
ggsave("outputs/top_10_vulnerable_wards.pdf", top10_map, width = 10, height = 8)
ggsave("outputs/top_10_vulnerable_wards.png", top10_map, width = 10, height = 8, dpi = 100)

cat("Top 10 vulnerable wards map saved to 'outputs/top_10_vulnerable_wards.pdf' and 'outputs/top_10_vulnerable_wards.png'\n")

# Spatial autocorrelation analysis
cat("Performing spatial autocorrelation analysis...\n")
gwpca_sp <- as_Spatial(gwpca_data)

# Create spatial weights matrix
neighbors <- poly2nb(gwpca_sp)
weights <- nb2listw(neighbors, style = "W")

# Calculate Global Moran's I
global_moran <- moran.test(gwpca_data$HVI, listw = weights)
cat("Global Moran's I for HVI:\n")
print(global_moran)

# Calculate Local Moran's I
local_moran <- localmoran(gwpca_data$HVI, listw = weights)

# Add Local Moran's I results to the sf dataframe
gwpca_data$LocalI <- local_moran[, 1]  # Using column index for Ii
# Fixed column reference for p-value
gwpca_data$LocalI_pvalue <- local_moran[, 5]  # Using column index for p-value

# Create Local Moran's I map
cat("Creating Local Moran's I map...\n")
lisa_map <- ggplot(gwpca_data) +
  geom_sf(aes(fill = LocalI)) +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  theme_minimal() +
  labs(title = "Local Moran's I for HVI", fill = "Local I")

# Save Local Moran's I map using ggsave
ggsave("outputs/local_morans_i.pdf", lisa_map, width = 10, height = 8)
ggsave("outputs/local_morans_i.png", lisa_map, width = 10, height = 8, dpi = 100)

cat("Local Moran's I map saved to 'outputs/local_morans_i.pdf' and 'outputs/local_morans_i.png'\n")

# Geographically Weighted Principal Component Analysis (GWPCA)
cat("Performing GWPCA...\n")

# Create a cache directory if it doesn't exist
if (!dir.exists("cache")) {
  dir.create("cache")
}

# Define cache file paths
bw_cache_file <- "cache/bw_gwpca.RData"
gwpca_cache_file <- "cache/gwpca_result.RData"

# Force recomputation regardless of cache
cat("Computing GWPCA (this may take a while)...\n")

# Convert sf to Spatial*DataFrame for GWmodel
gwpca_data_sp <- as(gwpca_data, "Spatial")

# Estimate bandwidth
cat("Estimating optimal bandwidth...\n")
bw <- bw.gwpca(
  data = gwpca_data_sp,
  vars = vars,
  k = 2,
  robust = TRUE,
  adaptive = TRUE
)

cat("Optimal adaptive bandwidth:", bw, "\n")

# Save bandwidth to cache
save(bw, file = bw_cache_file)

# Perform GWPCA
cat("Computing GWPCA with optimal bandwidth...\n")
gwpca_result <- gwpca(
  data = gwpca_data_sp,
  vars = vars,
  k = 2,
  robust = TRUE,
  bw = bw,
  adaptive = TRUE,
  scores = TRUE
)

# Save GWPCA results to cache
save(gwpca_result, file = gwpca_cache_file)

# Extract scores
cat("Extracting GWPCA scores...\n")
num_components <- 2
scores_matrix <- matrix(NA, nrow = length(gwpca_result$gwpca.scores), ncol = num_components)
colnames(scores_matrix) <- paste0("PC", 1:num_components)

for (i in 1:length(gwpca_result$gwpca.scores)) {
  local_scores <- gwpca_result$gwpca.scores[[i]]
  if (!is.null(local_scores)) {
    if (ncol(local_scores) >= num_components) {
      scores_matrix[i, ] <- local_scores[1, 1:num_components]
    } else {
      scores_matrix[i, 1:ncol(local_scores)] <- local_scores[1, ]
    }
  }
}

# Add scores to gwpca_sf
gwpca_sf <- st_as_sf(gwpca_result$SDF)
gwpca_sf <- cbind(gwpca_sf, as.data.frame(scores_matrix))

# Calculate HVI based on GWPCA
gwpca_sf$HVI <- scale(gwpca_sf$PC1)

# Create GWPCA HVI map
cat("Creating GWPCA HVI map...\n")
gwpca_hvi_map <- ggplot(gwpca_sf) +
  geom_sf(aes(fill = HVI)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Heat Vulnerability Index (GWPCA)", fill = "HVI")

# Save GWPCA HVI map using ggsave
ggsave("outputs/gwpca_hvi.pdf", gwpca_hvi_map, width = 10, height = 8)
ggsave("outputs/gwpca_hvi.png", gwpca_hvi_map, width = 10, height = 8, dpi = 100)

cat("GWPCA HVI map saved to 'outputs/gwpca_hvi.pdf' and 'outputs/gwpca_hvi.png'\n")

# Top 10 most vulnerable wards based on GWPCA
top_10_gwpca <- gwpca_sf %>%
  arrange(desc(HVI)) %>%
  slice(1:10)

cat("Top 10 most vulnerable wards based on GWPCA:\n")
print(top_10_gwpca %>% 
        st_set_geometry(NULL) %>%
        select(HVI))

# Create top 10 GWPCA wards map
cat("Creating top 10 GWPCA vulnerable wards map...\n")
top10_gwpca_map <- ggplot() +
  geom_sf(data = gwpca_sf, aes(fill = HVI), alpha = 0.6) +
  geom_sf(data = top_10_gwpca, fill = NA, color = "black", size = 0.7) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  labs(title = "Top 10 Most Vulnerable Wards (GWPCA)", fill = "HVI")

# Save top 10 GWPCA map using ggsave
ggsave("outputs/top_10_gwpca.pdf", top10_gwpca_map, width = 10, height = 8)
ggsave("outputs/top_10_gwpca.png", top10_gwpca_map, width = 10, height = 8, dpi = 100)

cat("Top 10 GWPCA vulnerable wards map saved to 'outputs/top_10_gwpca.pdf' and 'outputs/top_10_gwpca.png'\n")

# Extract loadings array
cat("Analyzing GWPCA loadings...\n")
loadings_array <- gwpca_result$loadings  # Dimensions: locations x variables x components

# Compute the mean loadings across all locations
mean_loadings <- apply(loadings_array, c(2,3), mean, na.rm = TRUE)  # Result is variables x components

# Ensure the length of vars matches the number of rows in mean_loadings
if (nrow(mean_loadings) == length(vars)) {
  rownames(mean_loadings) <- vars
} else {
  stop("Mismatch between the number of rows in mean_loadings and the length of vars.")
}

# Extract loadings for the first principal component
pc1_loadings <- mean_loadings[, 1]

# Create a data frame of variables and their loadings for PC1
loadings_df <- data.frame(Variable = rownames(mean_loadings), PC1_Loading = pc1_loadings)

# Sort the loadings by absolute value to find the most contributing variables
loadings_df <- loadings_df %>%
  mutate(Absolute_Loading = abs(PC1_Loading)) %>%
  arrange(desc(Absolute_Loading))

# Display the top contributing variables to PC1
top_contributing_variables <- loadings_df[1:10, ]

cat("Top contributing variables to PC1:\n")
print(top_contributing_variables, row.names = FALSE)

# Create PC1 loadings plot
cat("Creating PC1 loadings plot...\n")
loadings_plot <- ggplot(loadings_df, aes(x = reorder(Variable, PC1_Loading), y = PC1_Loading)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Loadings for PC1", x = "Variables", y = "PC1 Loading")

# Save PC1 loadings plot using ggsave
ggsave("outputs/pc1_loadings.pdf", loadings_plot, width = 8, height = 10)
ggsave("outputs/pc1_loadings.png", loadings_plot, width = 8, height = 10, dpi = 100)

cat("PC1 loadings plot saved to 'outputs/pc1_loadings.pdf' and 'outputs/pc1_loadings.png'\n")

# Save the complete workspace for future reference
cat("Saving workspace...\n")
save.image("HVI_Johannesburg_complete.RData")

cat("\nAnalysis complete. Results saved to 'outputs' directory.\n")