###################################################################################################
##  The code is adapted from Lowe, et al., Lancet Planetary Health https://github.com/drrachellowe/hydromet_dengue
# 
## Written by Qu Cheng
## Last updated: March 21 th, 2023
###################################################################################################

# R script to run INLA models of increasing complexity


# STEP 1: load packages and pre-processed data
source("0_packages_data_functions.R")

# STEP 2: choose to use Tmax, since it has the lowest correlation with P9wk.noself
cor(GZ.daily[, c("Tave", "Tmax", "Tmin", "P8wk.noself")], use = "complete.obs", method = "spearman")
cor(GZ.daily.subset[, c("Tave", "Tmax", "Tmin", "P8wk.noself")], use = "complete.obs", method = "spearman")



# Step 3: no interaction
baseformula0 <- CaseCount ~ 1 + ns(Time, year.n*7) + dow + basis_tmax + basis_rain
model <- fitmodel(baseformula0, GZ.daily.subset)
model$dic$dic


# Step 4: add interaction, centered at high (model1.1), medium (model1.2) and low (model1.3) level of prior water availability measured
# by the cumulative rainfall in the previous 8 weeks
baseformula0 <- CaseCount ~ 1 + ns(Time, year.n*7) + dow + basis_tmax + basis_rain  + P8wk.noself
formula3.1 <- update.formula(baseformula0, ~. + basis_rain_water_low)
formula3.2 <- update.formula(baseformula0, ~. + basis_rain_water_mid)
formula3.3 <- update.formula(baseformula0, ~. + basis_rain_water_high)

# create a list of formulas
formulas <- list(formula3.1, formula3.2, formula3.3)

# create model label string
lab <- c("model_low", "model_mid", "model_high")


models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- fitmodel(formulas[[i]], GZ.daily.subset)
                   save(model, file = paste0("output/", lab[i],".RData"))})

# create table to store DIC
table1 <- data.frame(Model  = c("low prior water", "medium prior water", "high prior water"), 
                     DIC = NA)

for(i in 1:length(formulas))
{
  load(paste0("output/",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table1





