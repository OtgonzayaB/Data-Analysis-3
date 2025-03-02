######## Load libraries ########
library(ggplot2)   # Visualization
library(dplyr)     # Data manipulation
library(car)       # Multicollinearity check
library(caret)     # Cross-validation
library(MASS)      # BIC calculation

# Define file paths
data_in <- "raw/"
data_out <- "clean/"

######## Import dataset ########
data_all <- read.csv(paste0(data_in, "morg2014.csv"), stringsAsFactors = FALSE)

# Select relevant variables
data_all <- data_all[, c("hhid", "intmonth", "stfips", "weight", "earnwke", "uhourse",  
                         "grade92", "race", "ethnic", "age", "sex", "marital", "ownchild", 
                         "chldpres", "prcitshp", "state", "ind02", "occ2012", "class94", 
                         "unionmme", "unioncov", "lfsr94")]

# Rename columns
colnames(data_all)[colnames(data_all) == 'uhourse'] <- 'uhours'
colnames(data_all)[colnames(data_all) == 'class94'] <- 'class'

# Sample Selection
data_all <- subset(data_all, data_all$uhours != 0 & !is.na(data_all$uhours))
data_all <- subset(data_all, data_all$earnwke != 0 & !is.na(data_all$earnwke))
data_all <- subset(data_all, data_all$age >= 16 & data_all$age <= 64)

######## Filter for Pharmacists ########
pharmacists <- subset(data_all, data_all$occ2012 == 3050)

# Convert categorical variables
pharmacists$sex <- as.factor(pharmacists$sex)
pharmacists$grade92 <- as.numeric(pharmacists$grade92)
pharmacists$class <- as.factor(pharmacists$class)

######## Create earnings per hour variable ########
pharmacists$earn_per_hour <- pharmacists$earnwke / pharmacists$uhours

# Remove infinite/missing values
pharmacists <- pharmacists[!is.infinite(pharmacists$earn_per_hour) & !is.na(pharmacists$earn_per_hour), ]

# Drop unnecessary columns
pharmacists <- pharmacists[, !names(pharmacists) %in% c("occ2012", "lfsr94")]

# Check near-zero variance predictors
nzv_vars <- nearZeroVar(pharmacists, saveMetrics = TRUE)
print(nzv_vars[nzv_vars$nzv, ])  # Show problematic variables

# Drop near-zero variance variables
pharmacists <- pharmacists[, !names(pharmacists) %in% rownames(nzv_vars[nzv_vars$nzv, ])]

####### MODEL ESTIMATIONS #######

##### Model 1: Baseline (Age & Sex)
model1 <- lm(earn_per_hour ~ age + sex, data = pharmacists)

summary(model1)

##### Model 2: Add Education & Employment Class

pharmacists <- droplevels(subset(pharmacists, class != "Government - Local"))

model2 <- lm(earn_per_hour ~ age + sex + grade92 + class, data = pharmacists)

summary(model2)

##### Model 3: Add State, Citizenship, and Industry

pharmacists <- pharmacists[, !names(pharmacists) %in% c("region")]


pharmacists$prcitshp <- factor(ifelse(pharmacists$prcitshp %in% c("Native, Born Abroad Of US Parent(s)", 
                                                                  "Native, Born in PR or US Outlying Area"), 
                                      "Other", pharmacists$prcitshp))

model3 <- lm(earn_per_hour ~ age + sex + grade92 + class + prcitshp, data = pharmacists)

summary(model3)

#####  Model 4: Optimized Model
# Selectively add only the most significant industry categories

selected_industries <- c("Grocery stores (4451)", "Justice, public order, and safety activities (922, pt. 92115)",  
                         "State 92", "State 82", "Pharmacies and drug stores (44611)")

pharmacists_filtered <- pharmacists[pharmacists$ind02 %in% selected_industries, ]

# Convert categorical variables
pharmacists_filtered$ind02 <- as.factor(pharmacists_filtered$ind02)
pharmacists_filtered$class <- as.factor(pharmacists_filtered$class)
pharmacists_filtered$sex <- as.factor(pharmacists_filtered$sex)

# Create dummy for "Grocery Stores"
pharmacists_filtered$ind02_GroceryStores <- ifelse(pharmacists_filtered$ind02 == "Grocery stores (4451)", 1, 0)

# Center age to reduce collinearity
pharmacists_filtered$age_centered <- pharmacists_filtered$age - mean(pharmacists_filtered$age, na.rm = TRUE)
pharmacists_filtered$age2_centered <- pharmacists_filtered$age_centered^2

# Drop categories with insufficient observations
pharmacists_filtered <- droplevels(subset(pharmacists_filtered, class != "Government - Local"))
pharmacists_filtered <- subset(pharmacists_filtered, ind02_GroceryStores != 1)

# Model 4 Final
model4 <- lm(earn_per_hour ~ age_centered + age2_centered + sex + grade92 + class, data = pharmacists_filtered)

summary(model4)

####### Model Performance Metrics #######

# Define RMSE function
rmse <- function(model, data) {
  sqrt(mean((data$earn_per_hour - predict(model, data))^2))
}

# Calculate RMSE and BIC for all models
rmse_values <- c(rmse(model1, pharmacists), rmse(model2, pharmacists), rmse(model3, pharmacists), rmse(model4, pharmacists_filtered))
bic_values <- c(BIC(model1), BIC(model2), BIC(model3), BIC(model4))

# Perform 10-fold cross-validation
set.seed(123)
cv_control <- trainControl(method = "cv", number = 10)

cv_model1 <- train(earn_per_hour ~ age + sex, data = pharmacists, method = "lm", trControl = cv_control)

cv_model2 <- train(earn_per_hour ~ age + sex + grade92 + class, 
                   data = pharmacists, 
                   method = "lm", 
                   trControl = cv_control, 
                   na.action = na.omit)

cv_model3 <- train(earn_per_hour ~ age + sex + grade92 + class + prcitshp,  # Removed `region`
                   data = pharmacists, 
                   method = "lm", 
                   trControl = cv_control, 
                   na.action = na.omit)



cv_model4 <- train(earn_per_hour ~ age_centered + age2_centered + sex + grade92 + class, data = pharmacists_filtered, method = "lm", trControl = cv_control, na.action = na.omit)

