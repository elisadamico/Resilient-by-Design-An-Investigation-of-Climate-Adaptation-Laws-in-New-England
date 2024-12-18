## FINAL MODELS

library(plm)
library(broom)
library(ggplot2)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)

rbd_data <- read.csv("RBD.csv")

rbd_data <- pdata.frame(rbd_data, index = c("stcty", "year"))

# Log transformation of variables
rbd_data <- rbd_data %>%
  mutate(
    E_TOTPOP_log = log(E_TOTPOP + 0.0000000001),
    M_SNGPNT_POP_log = log(M_SNGPNT_POP + 0.0000000001),
    M_HBURD_POP_log = log(M_HBURD_POP + 0.0000000001),
    EPL_POV150_log = log(EPL_POV150 + 0.0000000001),
    EPL_UNEMP_log = log(EPL_UNEMP + 0.0000000001),
    M_MINRTY_POP_log = log(M_MINRTY_POP + 0.0000000001),
    M_MOBILE_POP_log = log(M_MOBILE_POP + 0.0000000001),
    M_LIMENG_POP_log = log(M_LIMENG_POP + 0.0000000001), 
    crsi_NaturalEnvironment_log = log(crsi_NaturalEnvironment + 0.0000000001)
  )

#################
#### FEATURE ####
#################

FEATURE_vars <- c(
  "FEATURE_economicresilience",
  "FEATURE_ecosystemsnaturalresources",
  "FEATURE_governmentbylawsordinancescodes",
  "FEATURE_infrastructurebuilt",
  "FEATURE_socialenvironmentaljustice"
)

control_vars <- c(
  "PC1_imp", "PC2_imp", "PC3_imp", "PC4_imp", "PC1_plan", 
  "PC2_plan", "PC3_plan", "PC4_plan", "PC5_plan", 
  "COMMUNITY_town", "COMMUNITY_organization", "COMMUNITY_tribe", "COMMUNITY_state",
  "EPL_POV150_log", "EPL_UNEMP_log", "M_HBURD_POP_log", 
  "M_MINRTY_POP_log", "M_SNGPNT_POP_log", "M_MOBILE_POP_log", 
  "M_LIMENG_POP_log", "crsi_NaturalEnvironment_log"
  )

# Loop through FEATURE_vars and fit models for features of interest
models_list <- list()
clustered_se_list <- list()  # List to store clustered standard errors

for (feature in FEATURE_vars) {
  formula <- as.formula(paste("RPL_THEMES ~", paste(c(control_vars, feature), collapse = " + ")))
  model <- plm(formula, data = rbd_data, model = "within", index = c("stcty", "year"))  # Specify index for panel data
    models_list[[feature]] <- model
  # Calculate clustered standard errors using vcovHC
  clustered_se <- sqrt(diag(vcovHC(model, type = "HC1", cluster = "group")))  # Use "group" for clustering by stcty
  clustered_se_list[[feature]] <- clustered_se
}

for (feature in FEATURE_vars) {
  cat("Model for feature:", feature, "\n")
  model_summary <- summary(models_list[[feature]])
  print(model_summary)  
  
  cat("\nClustered Standard Errors:\n")
  print(clustered_se_list[[feature]])
  cat("\n\n")
}

# Stargazer table for "Feature" models
stargazer(
  models_list,
  title = "Panel Data Models with Fixed Effects and Clustered Standard Errors",
  header = FALSE,
  omit = c("^(PC|COMMU)")
  )


# Extract coefficients for each model
coefficients <- lapply(models_list, tidy)
selected_coefficients <- lapply(coefficients, function(model_coef) {
  model_coef %>% filter(term %in% FEATURE_vars)
})
selected_coefficients_df <- bind_rows(selected_coefficients, .id = "Model")
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

# Plot for FEATURE Vars

custom_colors <- c("#17a2b8", "#76d7c4", "#20c997", "#ffdd44", "#ffcc00")

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

###################
#### PLOT TYPE ####
###################

PLAN_TYPE_vars <- c(
  "PLAN_TYPE_adaptationplan", 
  "PLAN_TYPE_casestudy",
  "PLAN_TYPE_climatemitigationdocument",
  "PLAN_TYPE_disasterrecoveryplan",
  "PLAN_TYPE_resilienceplan"
)

control_vars_plan <- c(
  "PC1_imp", "PC2_imp", "PC3_imp", "PC4_imp", 
  "COMMUNITY_town", "COMMUNITY_organization", "COMMUNITY_tribe", "COMMUNITY_state",
  "PC1_fea", "PC2_fea", "PC3_fea", "PC4_fea", "EPL_POV150_log", "EPL_UNEMP_log", 
  "M_HBURD_POP_log", "M_MINRTY_POP_log", "M_SNGPNT_POP_log", "M_MOBILE_POP_log", 
  "M_LIMENG_POP_log", "crsi_NaturalEnvironment_log"
)

# Fit models for each PLAN_TYPE variable and calculate clustered standard errors
models_list_plan <- list()
clustered_se_list_plan <- list()  

for (plan in PLAN_TYPE_vars) {
  formula <- as.formula(paste("RPL_THEMES ~", paste(c(control_vars_plan, plan), collapse = " + ")))
  model <- plm(formula, data = rbd_data, model = "within", index = c("stcty", "year"))  # Specify index for panel data
  models_list_plan[[plan]] <- model
  
  # Calculate clustered standard errors
  clustered_se <- sqrt(diag(vcovHC(model, type = "HC1", cluster = "group")))  # Cluster by stcty
  clustered_se_list_plan[[plan]] <- clustered_se
}


stargazer(
  models_list_plan,
  title = "Panel Data Models with Fixed Effects and Clustered Standard Errors",
  header = FALSE,
  omit = c("^(PC|COMMU)"))

# Extract coefficients and filter for PLAN_TYPE variables
coefficients_plan <- lapply(models_list_plan, tidy)
selected_coefficients_plan <- lapply(coefficients_plan, function(model_coef) {
  model_coef %>% filter(term %in% PLAN_TYPE_vars)
})
selected_coefficients_df_plan <- bind_rows(selected_coefficients_plan, .id = "Model")

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

# Plot for PLAN_TYPE Vars
custom_colors_plan <- c("#17a2b8", "#76d7c4", "#20c997", "#ffdd44", "#ffcc00")

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

##################
###  COMMUNITY ###
##################

model_com <- plm(RPL_THEMES ~ PC1_imp + PC2_imp + PC3_imp + PC4_imp + PC1_plan + 
                   PC2_plan + PC3_plan + PC4_plan + PC5_plan + 
                   COMMUNITY_town + COMMUNITY_organization + COMMUNITY_tribe + 
                   COMMUNITY_state + COMMUNITY_watershed +
                   PC1_fea + PC2_fea + PC3_fea + PC4_fea +
                   EPL_POV150_log + EPL_UNEMP_log + M_HBURD_POP_log + M_MINRTY_POP_log + 
                   M_SNGPNT_POP_log + M_MOBILE_POP_log + M_LIMENG_POP_log + crsi_NaturalEnvironment_log,
                 
                 data = rbd_data, model = "within", random.method = "swar")

# Calculate clustered standard errors around 'stcty'
clustered_se <- vcovHC(model_com, type = "HC1", cluster = "group")
model_com_summary <- coeftest(model_com, vcov = clustered_se)
print(model_com_summary)

# Create stargazer table with clustered standard errors
stargazer(
  model_com_summary,
  title = "Panel Data Models with Fixed Effects and Clustered Standard Errors",
  header = FALSE,
  omit = c("^(PC)"))


# Extract coefficients
coefficients_com <- tidy(model_com)
community_terms <- c("COMMUNITY_town", "COMMUNITY_organization", "COMMUNITY_tribe", "COMMUNITY_state", "COMMUNITY_watershed")
filtered_community_coefficients <- coefficients_com %>%
  filter(term %in% community_terms)

# Plot for COMMUNITY Vars

custom_colors_com <- c("#17a2b8", "#76d7c4", "#20c997", "#ffdd44", "#f39c12")

legend_labels <- c("COMMUNITY_town" = "Town", 
                   "COMMUNITY_organization" = "Organization", 
                   "COMMUNITY_tribe" = "Tribe", 
                   "COMMUNITY_state" = "State", 
                   "COMMUNITY_watershed" = "Watershed")

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



###############
### IMPACTS ###
###############

IMPACTS_vars <- c(
  "IMPACTS_extremeheat",
  "IMPACTS_flooding",
  "IMPACTS_saltwaterintrusion",
  "IMPACTS_sealevelrise",
  "IMPACTS_stormsurge"
)

control_vars_impacts <- c(
  "PC1_plan", "PC2_plan", "PC3_plan", "PC4_plan", "PC5_plan", 
  "COMMUNITY_town", "COMMUNITY_organization", "COMMUNITY_tribe",
  "PC1_fea", "PC2_fea", "PC3_fea", "PC4_fea", "EPL_POV150_log", "EPL_UNEMP_log", 
  "M_HBURD_POP_log", "M_MINRTY_POP_log", "M_SNGPNT_POP_log", "M_MOBILE_POP_log", 
  "M_LIMENG_POP_log", "crsi_NaturalEnvironment_log"
)

# Loop through IMPACTS_vars and fit models for impacts of interest
models_list_impacts <- list()
for (impact in IMPACTS_vars) {
  formula <- as.formula(paste("RPL_THEMES ~", paste(c(control_vars_impacts, impact), collapse = " + ")))
    model <- plm(formula, data = rbd_data, model = "within", index = c("stcty"), effect = "individual")
  models_list_impacts[[impact]] <- model
}
print(models_list_impacts)

# Create stargazer table
stargazer(
  models_list_impacts,
  title = "Panel Data Models with Fixed Effects and Clustered Standard Errors",
  header = FALSE,
  omit = c("^(PC|COMMU)")
)

# Extract coefficients for each model
coefficients_impacts <- lapply(models_list_impacts, tidy)
selected_coefficients_impacts <- lapply(coefficients_impacts, function(model_coef) {
  model_coef %>% filter(term %in% IMPACTS_vars)
})

selected_coefficients_df_impacts <- bind_rows(selected_coefficients_impacts, .id = "Model")

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

# Plot for IMPACTS Vars
custom_colors_impacts <- c("#17a2b8", "#76d7c4", "#20c997", "#ffdd44", "#ffcc00")

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


