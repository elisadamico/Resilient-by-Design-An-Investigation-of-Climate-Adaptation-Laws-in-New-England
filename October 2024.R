
### IV CORR DV ###


# Select relevant variables
selected_vars <- pdata %>%
  dplyr::select(RPL_THEMES, EPL_POV150_log, EPL_UNEMP_log, M_HBURD_POP_log, M_MINRTY_POP_log,
                M_SNGPNT_POP_log, M_MOBILE_POP_log, M_LIMENG_POP_log, crsi_NaturalEnvironment_log)

# Compute the correlation matrix
correlation_matrix <- cor(selected_vars)

# Print the correlation matrix
print(correlation_matrix)

# Fit a linear model
model <- lm(RPL_THEMES ~ ., data = selected_vars)

# Summary of the model
summary(model)

# Load the GGally package
library(GGally)

# Create pairwise plots
ggpairs(selected_vars)



### PCA PART 

# Step 1: Select only the continuous variables (excluding RPL_THEMES)
selected_cvs <- selected_vars %>%
  dplyr::select(-RPL_THEMES)  # Exclude RPL_THEMES

# Step 2: Perform PCA
pca_cvs<- prcomp(selected_cvs, scale. = TRUE)

# Step 3: Calculate cumulative proportion of variance explained
cumulative_variance <- cumsum(pca_cvs$sdev^2) / sum(pca_cvs$sdev^2)

# Step 4: Create the plot
plot(cumulative_variance, 
     xlab = "Number of Principal Components",
     ylab = "Cumulative Proportion of Variance Explained", 
     type = "b", 
     main = "Cumulative Variance Explained by PCA",
     ylim = c(0, 1))  # Set y-limits from 0 to 1 for better visibility

# Step 5: Add a horizontal line at 0.9
abline(h = 0.9, col = "red", lty = 2)

# Optionally, add a vertical line to indicate the number of PCs needed to reach the threshold
cut_off <- which(cumulative_variance >= 0.9)[1]  # Find the first index where cumulative variance >= 0.9
abline(v = cut_off, col = "blue", lty = 2)  # Vertical line for the cut-off

pca_components <- pca_cvs$x[, 1:7]
colnames(pca_components) <- paste0("PC", 1:7, "_cv")
pdata_withCV_pca <- cbind(pdata, pca_components)

### RUN CORR PLOT WITH PCA VARS
# Step 2: Perform PCA

pca_result_impacts <- prcomp(selected_cvs, scale. = TRUE)

# Step 3: Extract eigenvalues
eigenvalues <- pca_result_impacts$sdev^2
names(eigenvalues) <- paste("PC", 1:length(eigenvalues), sep = "")

# Step 4: Create a data frame for loadings
loadings <- as.data.frame(pca_result_impacts$rotation)
loadings <- round(loadings, 3)  # Round for better readability

# Step 5: Summarize eigenvalues and determine significant components
eigenvalue_summary <- data.frame(
  PC = names(eigenvalues),
  Eigenvalue = eigenvalues
)

# Step 6: Filter PCs with eigenvalues greater than 1
significant_pcs <- eigenvalue_summary %>%
  filter(Eigenvalue > 1)

# Output the results
print("Significant Principal Components (Eigenvalues > 1):")
print(significant_pcs)

# Step 7: Check loadings for significant PCs
print("Loadings for Significant Principal Components:")
# Use the names directly instead of converting to numeric
print(loadings[, significant_pcs$PC])

print(colnames(loadings))  # Check column names in the loadings data frame



# Assuming pdata_withCV_pca is your DataFrame with PCs and RPL_THEMES
# Step 2: Calculate correlations
correlations <- sapply(pdata_withCV_pca[, paste0("PC", 1:7, "_cv")], function(pc) {
  cor(pc, pdata_withCV_pca$RPL_THEMES, use = "complete.obs")  # use = "complete.obs" to handle NA values
})

# Step 3: Create a summary data frame
correlation_summary <- data.frame(
  PC = paste0("PC", 1:7),
  Correlation = correlations
)

# Step 4: Print the correlation summary
print("Correlations between RPL_THEMES and Principal Components:")
print(correlation_summary)



# Step 5: Optional - Visualize correlations
ggplot(correlation_summary, aes(x = PC, y = Correlation)) +
  geom_bar(stat = "identity", fill = "#17a2b8", width = 0.5) +  # Adjusted width for thinner bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "#ffcc00") +
  labs(title = "Correlation of Dependent Variables with Principal Components",
       x = "Principal Components",
       y = "Correlation Coefficient") +
  theme_minimal(base_family = "Times New Roman") +  # Set font to Times New Roman
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(face = "bold", size = 14),  # Bold title and size
    axis.title.x = element_text(size = 12),  # X-axis title size
    axis.title.y = element_text(size = 12),  # Y-axis title size
    axis.text.x = element_text(size = 10),    # X-axis text size
    axis.text.y = element_text(size = 10),    # Y-axis text size
    panel.grid.major = element_line(color = "lightgray"),  # Customize grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )



## Main Models

pdata_withCV_pca <- pdata.frame(pdata_withCV_pca, index = c("stcty", "year"))


###################
#### ALL LAWS ####
###################

### FEATURE




# FEATURE Vars and Control Variables
FEATURE_vars <- c(
  "FEATURE_economicresilience",
  "FEATURE_ecosystemsnaturalresources",
  "FEATURE_governmentbylawsordinancescodes",
  "FEATURE_infrastructurebuilt",
  "FEATURE_socialenvironmentaljustice"
)

control_vars <- c(
  "PC2_cv","PC3_cv","PC4_cv","PC5_cv","PC6_cv","PC7_cv", "PC1_imp", "PC2_imp", "PC3_imp", "PC4_imp", "PC1_plan", 
  "PC2_plan", "PC3_plan", "PC4_plan", "PC5_plan", 
  "COMMUNITY_town", "COMMUNITY_organization", "COMMUNITY_tribe", "COMMUNITY_state" 
)

# Loop through FEATURE_vars and fit models for features of interest
models_list <- list()
clustered_se_list <- list()  # List to store clustered standard errors

for (feature in FEATURE_vars) {
  formula <- as.formula(paste("RPL_THEMES ~", paste(c(control_vars, feature), collapse = " + ")))
  model <- plm(formula, data = pdata_withCV_pca, model = "within", index = c("stcty", "year"))  # Specify index for panel data
  
  # Store the model
  models_list[[feature]] <- model
  
  # Calculate clustered standard errors using vcovHC
  clustered_se <- sqrt(diag(vcovHC(model, type = "HC1", cluster = "group")))  # Use "group" for clustering by stcty
  clustered_se_list[[feature]] <- clustered_se
}

# Print models with clustered standard errors
for (feature in FEATURE_vars) {
  cat("Model for feature:", feature, "\n")
  model_summary <- summary(models_list[[feature]])
  print(model_summary)  # Print model summary
  
  cat("\nClustered Standard Errors:\n")
  print(clustered_se_list[[feature]])
  cat("\n\n")
}

# Create stargazer table for the models
stargazer(
  models_list,
  title = "Panel Data Models with Fixed Effects and Clustered Standard Errors (PC)",
  header = FALSE
)

# Extract coefficients for each model
coefficients <- lapply(models_list, tidy)
selected_coefficients <- lapply(coefficients, function(model_coef) {
  model_coef %>% filter(term %in% FEATURE_vars)
})

# Combine results into a single data frame
selected_coefficients_df <- bind_rows(selected_coefficients, .id = "Model")

# Recode term names for better readability
selected_coefficients_df$term <- factor(
  selected_coefficients_df$term,
  levels = FEATURE_vars,
  labels = c(
    "Economic Resilience",
    "Ecosystem Natural Resources",
    "Government Bylaws & Codes",
    "Infrastructure Built",
    "Social Environmental Justice"
  )
)

# Custom color palette for the plot
custom_colors <- c("#17a2b8", "#76d7c4", "#20c997", "#ffdd44", "#ffcc00")

# Plot for FEATURE Vars
plot_features <- ggplot(selected_coefficients_df, aes(x = Model, y = estimate, color = term, shape = term)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
       x = NULL, 
       y = "Coefficient Estimate", 
       color = "Legend", 
       shape = "Legend") +
  scale_color_manual(values = custom_colors) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(face = "bold")
  )

print(plot_features)

# Load necessary libraries
library(plm)
library(lmtest)
library(sandwich)
library(broom)
library(dplyr)
library(ggplot2)
library(stargazer)

#### PLOT TYPE ####
PLAN_TYPE_vars <- c(
  "PLAN_TYPE_adaptationplan", 
  "PLAN_TYPE_casestudy",
  "PLAN_TYPE_climatemitigationdocument",
  "PLAN_TYPE_disasterrecoveryplan",
  "PLAN_TYPE_resilienceplan"
)

# Define control variables
control_vars_plan <- c(
  "PC2_cv","PC3_cv","PC4_cv","PC5_cv","PC6_cv","PC7_cv", "PC1_imp", "PC2_imp", "PC3_imp", "PC4_imp", 
  "COMMUNITY_town", "COMMUNITY_organization", "COMMUNITY_tribe", "COMMUNITY_state",
  "PC1_fea", "PC2_fea", "PC3_fea", "PC4_fea"
)

# Fit models for each PLAN_TYPE variable and calculate clustered standard errors
models_list_plan <- list()
clustered_se_list_plan <- list()  # List to store clustered standard errors

for (plan in PLAN_TYPE_vars) {
  formula <- as.formula(paste("RPL_THEMES ~", paste(c(control_vars_plan, plan), collapse = " + ")))
  model <- plm(formula, data = pdata_withCV_pca, model = "within", index = c("stcty", "year"))  # Specify index for panel data
  models_list_plan[[plan]] <- model
  
  # Calculate clustered standard errors
  clustered_se <- sqrt(diag(vcovHC(model, type = "HC1", cluster = "group")))  # Cluster by stcty
  clustered_se_list_plan[[plan]] <- clustered_se
}

# Create stargazer table
stargazer(
  models_list_plan,
  type = "text", 
  title = "Panel Data Models with Clustered Standard Errors for Plan Types (PC)",
  align = TRUE,
  omit.stat = c("F", "ser"),
  header = FALSE,
  dep.var.caption = "Dependent Variable: RPL_THEMES"
)

# Extract coefficients and filter for PLAN_TYPE variables
coefficients_plan <- lapply(models_list_plan, tidy)
selected_coefficients_plan <- lapply(coefficients_plan, function(model_coef) {
  model_coef %>% filter(term %in% PLAN_TYPE_vars)
})

# Combine results into a single data frame
selected_coefficients_df_plan <- bind_rows(selected_coefficients_plan, .id = "Model")

# Recode term names for better readability
selected_coefficients_df_plan$term <- factor(
  selected_coefficients_df_plan$term,
  levels = PLAN_TYPE_vars,
  labels = c(
    "Adaptation Plan",
    "Case Study",
    "Climate Mitigation Document",
    "Disaster Recovery Plan",
    "Resilience Plan"
  )
)

# Custom color palette for the plot
custom_colors_plan <- c("#17a2b8", "#76d7c4", "#20c997", "#ffdd44", "#ffcc00")

# Plot for PLAN_TYPE Vars
plot_plan_types <- ggplot(selected_coefficients_df_plan, aes(x = Model, y = estimate, color = term, shape = term)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
       x = NULL, 
       y = "Coefficient Estimate", 
       color = "Plan Type", 
       shape = "Plan Type") +
  scale_color_manual(values = custom_colors_plan) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(face = "bold")
  )

print(plot_plan_types)






# COMMUNITY Vars and Control Variables
model_com <- plm(RPL_THEMES ~  PC2_cv + PC3_cv + PC4_cv + PC5_cv + PC6_cv + PC7_cv + PC1_imp + PC2_imp + PC3_imp + PC4_imp + PC1_plan + 
                   PC2_plan + PC3_plan + PC4_plan + PC5_plan + 
                   COMMUNITY_town + COMMUNITY_organization + COMMUNITY_tribe + 
                   COMMUNITY_state + COMMUNITY_watershed +
                   PC1_fea + PC2_fea + PC3_fea + PC4_fea ,
                 data = pdata_withCV_pca, model = "within", random.method = "swar")

# Calculate clustered standard errors around 'stcty'
clustered_se <- vcovHC(model_com, type = "HC1", cluster = "group")

# Use coeftest to get the summary with clustered standard errors
model_com_summary <- coeftest(model_com, vcov = clustered_se)

# Print summary with clustered standard errors
print(model_com_summary)

# Create stargazer table with clustered standard errors
stargazer(
  model_com_summary,
  type = "text", 
  title = "Random Effects Panel Data Model for RPL_THEMES with Clustered Standard Errors (PC)",
  align = TRUE,
  omit.stat = c("F", "ser"),
  header = FALSE,
  dep.var.caption = "Dependent Variable: RPL_THEMES"
)

# Extract coefficients
coefficients_com <- tidy(model_com)

# Filter coefficients for specific variables
community_terms <- c("COMMUNITY_town", "COMMUNITY_organization", "COMMUNITY_tribe", "COMMUNITY_state", "COMMUNITY_watershed")

# Filter coefficients for community variables
filtered_community_coefficients <- coefficients_com %>%
  filter(term %in% community_terms)

# Custom color palette for the plot
custom_colors_com <- c("#17a2b8", "#76d7c4", "#20c997", "#ffdd44", "#f39c12")

# Create a named vector for the legend labels
legend_labels <- c("COMMUNITY_town" = "Town", 
                   "COMMUNITY_organization" = "Organization", 
                   "COMMUNITY_tribe" = "Tribe", 
                   "COMMUNITY_state" = "State", 
                   "COMMUNITY_watershed" = "Watershed")

# Plot for COMMUNITY Vars
plot_com <- ggplot(filtered_community_coefficients, aes(x = term, y = estimate, color = term, shape = term)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
       x = NULL, 
       y = "Coefficient Estimate", 
       color = "Legend", 
       shape = "Legend") +
  scale_color_manual(values = custom_colors_com, labels = legend_labels) +  # Update legend labels
  scale_shape_manual(values = c(16, 17, 18, 19, 15), labels = legend_labels) +  # Update shape labels
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(face = "bold")
  )

print(plot_com)





# IMPACTS Vars and Control Variables
IMPACTS_vars <- c(
  "IMPACTS_extremeheat",
  "IMPACTS_flooding",
  "IMPACTS_saltwaterintrusion",
  "IMPACTS_sealevelrise",
  "IMPACTS_stormsurge"
)

control_vars_impacts <- c(
  "PC2_cv","PC3_cv","PC4_cv","PC5_cv","PC6_cv","PC7_cv", "PC1_plan", "PC2_plan", "PC3_plan", "PC4_plan", "PC5_plan", 
  "COMMUNITY_town", "COMMUNITY_organization", "COMMUNITY_tribe",
  "PC1_fea", "PC2_fea", "PC3_fea", "PC4_fea"
)

# Loop through IMPACTS_vars and fit models for impacts of interest
models_list_impacts <- list()
for (impact in IMPACTS_vars) {
  formula <- as.formula(paste("RPL_THEMES ~", paste(c(control_vars_impacts, impact), collapse = " + ")))
  
  # Fit model and cluster standard errors around stcty
  model <- plm(formula, data = pdata_withCV_pca, model = "within", index = c("stcty"), effect = "individual")
  models_list_impacts[[impact]] <- model
}

# Print models list
print(models_list_impacts)

# Create stargazer table for the models
stargazer(
  models_list_impacts,
  type = "text", 
  title = "Panel Data Models for IMPACT Variables (PC)",
  align = TRUE,
  omit.stat = c("F", "ser"),
  header = FALSE,
  dep.var.caption = "Dependent Variable: RPL_THEMES"
)

# Extract coefficients for each model
coefficients_impacts <- lapply(models_list_impacts, tidy)

# Filter coefficients for selected IMPACTS_vars
selected_coefficients_impacts <- lapply(coefficients_impacts, function(model_coef) {
  model_coef %>% filter(term %in% IMPACTS_vars)
})

# Combine results into a single data frame
selected_coefficients_df_impacts <- bind_rows(selected_coefficients_impacts, .id = "Model")

# Recode term names for better readability
selected_coefficients_df_impacts$term <- factor(
  selected_coefficients_df_impacts$term,
  levels = IMPACTS_vars,
  labels = c(
    "Extreme Heat",
    "Flooding",
    "Saltwater Intrusion",
    "Sea Level Rise",
    "Storm Surge"
  )
)

# Custom color palette for the plot
custom_colors_impacts <- c("#17a2b8", "#76d7c4", "#20c997", "#ffdd44", "#ffcc00")

# Plot for IMPACTS Vars
plot_impacts <- ggplot(selected_coefficients_df_impacts, aes(x = Model, y = estimate, color = term, shape = term)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
       x = "", 
       y = "Coefficient Estimate", 
       color = "Legend", 
       shape = "Legend") +
  scale_color_manual(values = custom_colors_impacts) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(face = "bold")
  )

print(plot_impacts)

library(gridExtra)

grid.arrange(
  plot_features, plot_com, 
  plot_plan_types, plot_impacts, 
  nrow = 2, ncol = 2
)
