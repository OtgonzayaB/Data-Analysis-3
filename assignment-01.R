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




