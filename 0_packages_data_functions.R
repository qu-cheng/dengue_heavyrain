###################################################################################################
##  The code is adapted from Lowe, et al., Lancet Planetary Health https://github.com/drrachellowe/hydromet_dengue
# 
## Written by Qu Cheng
## Last updated: March 21, 2023
###################################################################################################

# R script to load packages and prepare data for DLNM

library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)
library(ggsci)
library(dlnm)
library(INLA)
library(tsModel)
library(data.table)
library(splines)


## STEP 1. load and process data 
# full dataset - used for generate lagged data as the input for cross-basis function
GZ.daily <- read_csv("Data/DF_case.csv") %>%
  mutate(OnsetDate = as.Date(OnsetDate), Year = year(OnsetDate), Month = month(OnsetDate), dow = weekdays(OnsetDate),
         P8wk = zoo::rollsum(Precipitation, 8*7, align = "right", na.pad = TRUE), P8wk.noself = lag(P8wk, 1),
         HeavyRain = Precipitation > quantile(Precipitation[Precipitation > 0], 0.95),   
         P8wk.low = P8wk.noself - quantile(P8wk.noself, 0.05, na.rm = TRUE),
         P8wk.mid = P8wk.noself - quantile(P8wk.noself, 0.5, na.rm = TRUE),
         P8wk.high = P8wk.noself - quantile(P8wk.noself, 0.95, na.rm = TRUE)) %>%
  mutate(Subset = ifelse(Year %in% c(2006, 2012, 2013, 2014, 2016, 2017, 2018) & Month %in% c(7:12), 1, 0))  # data used in the analysis



# subset - used for fitting model
GZ.daily.subset <- GZ.daily %>%
  filter(Subset == 1) %>%
  mutate(Time = 1:n())

year.n <- length(unique(GZ.daily.subset$Year))




## STEP 2. generate lagged climatic variables since we only use a subset of them for generating cross-basis matrix
lag.t <- 20*7   # 20 weeks
lag_tave <- tsModel::Lag(GZ.daily$Tave, k = 0:lag.t)[GZ.daily$Subset == 1, ]
lag_tmin <- tsModel::Lag(GZ.daily$Tmin, k = 0:lag.t)[GZ.daily$Subset == 1, ]
lag_tmax <- tsModel::Lag(GZ.daily$Tmax, k = 0:lag.t)[GZ.daily$Subset == 1, ]
lag_rain <- tsModel::Lag(GZ.daily$HeavyRain, k = 0:lag.t)[GZ.daily$Subset == 1, ]



## STEP 3. define cross-basis matrix (combining nonlinear and lag functions)
# Tave
var.current <- lag_tave
basis_tave <- crossbasis(var.current, 
                         argvar = list(fun = "ns", knots = equalknots(var.current[1,], 2)),  
                         arglag=list(knots= logknots(lag.t, 2)))

# Tmin
var.current <- lag_tmin
basis_tmin <- crossbasis(var.current, 
                         argvar = list(fun = "ns", knots = equalknots(var.current[1,], 2)),  
                         arglag=list(knots= logknots(lag.t, 2)))

# Tmax
var.current <- lag_tmax
basis_tmax <- crossbasis(var.current, 
                         argvar = list(fun = "ns", knots = equalknots(var.current[1,], 2)),  
                         arglag=list(knots= logknots(lag.t, 2)))

# Rainfall
var.current <- lag_rain
basis_rain <- crossbasis(var.current, 
                         argvar = list(fun = "strata", breaks = 0.5),  
                         arglag=list(knots= logknots(lag.t, 2)))


# Interaction for Rainfall and prior water availability
basis_rain_water_low <- basis_rain * GZ.daily.subset$P8wk.low
basis_rain_water_mid <- basis_rain * GZ.daily.subset$P8wk.mid
basis_rain_water_high <- basis_rain * GZ.daily.subset$P8wk.high

# assign names for cross basis
colnames(basis_tave) <- paste0("basis_tave.", colnames(basis_tave))
colnames(basis_tmin) <- paste0("basis_tmin.", colnames(basis_tmin))
colnames(basis_tmax) <- paste0("basis_tmax.", colnames(basis_tmax))
colnames(basis_rain) <- paste0("basis_rain.", colnames(basis_rain))

colnames(basis_rain_water_low) <- paste0("basis_rain_water_low.", colnames(basis_rain_water_low))
colnames(basis_rain_water_mid) <- paste0("basis_rain_water_mid.", colnames(basis_rain_water_mid))
colnames(basis_rain_water_high) <- paste0("basis_rain_water_high", colnames(basis_rain_water_high))


# INLA MODEL FUNCTION ========= STARTED FROM HERE!!!!
# include formula and set defaults for data, family (to allow other prob dist models e.g. Poisson) and config (to allow for sampling)
fitmodel <- function(formula, data = GZ.daily.subset, family = "nbinomial")
  
{
  model <- inla(formula = formula, data = data, family = family,
                control.compute = list(dic = TRUE),
                control.fixed = list(correlation.matrix = TRUE, 
                                     prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE), 
                verbose = FALSE)
  model <- inla.rerun(model)
  return(model)
}
