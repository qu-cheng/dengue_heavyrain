###################################################################################################
##  The code is adapted from Lowe, et al., Lancet Planetary Health https://github.com/drrachellowe/hydromet_dengue
# and the supplementary of Armstrong B, Sera F, Vicedo-Cabrera AM, Abrutzky R, Åström DO, Bell ML, Chen BY, de Sousa Zanotti 
# Stagliorio Coelho M, Correa PM, Dang TN, Diaz MH. The role of humidity in associations of high temperature with mortality: a 
# multicountry, multicity study. Environmental health perspectives. 2019 Sep 25;127(9):097007.
#
## Written by Qu Cheng
## Last updated: June 15th, 2023
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
# population data by year
GZ.YearEndPop <- read_csv("Data/GZ_pop.csv") %>%
  mutate(OnsetDate = as.Date(paste(Year, "-12-31", sep = "")))

GZ.YearEndPop.Day <- data.frame(OnsetDate = seq(as.Date("2004-12-31"), as.Date("2018-12-31"), by = "day")) %>%
  left_join(GZ.YearEndPop[, c("OnsetDate", "YearEndPopulation")]) # data frame for store the interpolated daily population size data

GZ.YearEndPop.Day$YearEndPopulation <- round(approx(GZ.YearEndPop.Day$OnsetDate[!is.na(GZ.YearEndPop.Day$YearEndPopulation)], GZ.YearEndPop.Day$YearEndPopulation[!is.na(GZ.YearEndPop.Day$YearEndPopulation)], GZ.YearEndPop.Day$OnsetDate)$y *10000) # interpolation and round; population*10000, since the original unit was 10,000 persons 


# full dataset - used for generate lagged data as the input for cross-basis function
GZ.daily <- read_csv("Data/DF_case.csv") %>%
  mutate(OnsetDate = as.Date(OnsetDate), Year = year(OnsetDate), Month = month(OnsetDate), dow = weekdays(OnsetDate), 
         DOY = yday(OnsetDate)) %>%
  left_join(GZ.YearEndPop.Day)





## STEP 2.  INLA model function
# include formula and set defaults for data and family (to allow other prob dist models e.g. Poisson) 
fitmodel <- function(formula, data = GZ.daily.subset, family = "nbinomial")  # GZ.daily.subset is a subset of GZ.daily in dengue transmission years and months
{
  model <- inla(formula = formula, data = data, family = family, offset = log(YearEndPopulation), 
                control.compute = list(dic = TRUE),
                control.fixed = list(correlation.matrix = TRUE, 
                                     prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE), 
                verbose = FALSE,
                safe = TRUE)
  model <- inla.rerun(model)
  return(model)
}




## STEP 3. generate crossbasis and fit model
# we set several parameters for sensitivity analysis
DengueModel <- function(water.wks = 8,  # number of weeks for estimating prior water availability
                        water.pt = 0.5, # percentile of the prior water availability for extracting the effect of heavy rain (HR)
                        HR.pt = 0.95, # threshold for defining heavy rain
                        trend.method = c("ns_time", "ns_year_doy"),
                        time.df = 7, # degree of freedom for the spline to represent secular temporal trend
                        n.knots = 2, # number of knots for the crossbasis
                        year.include = c(2006, 2012, 2013, 2014, 2016, 2017, 2018) # years to included in the analysis
                        )
{
  GZ.daily <- GZ.daily %>%
    mutate(P8wk = zoo::rollsum(Precipitation/1000, water.wks*7, align = "right", na.pad = TRUE),    # change mm to m to have smaller values for better estimation by INLA
           P8wk.noself = lag(P8wk, 1),
           HeavyRain = Precipitation > quantile(Precipitation[Precipitation > 0], HR.pt),   
           P8wk.current = P8wk.noself - quantile(P8wk.noself, water.pt, na.rm = TRUE)) %>%
    mutate(Subset = ifelse(Year %in% year.include & Month %in% c(7:12), 1, 0))  # data used in the analysis
  
  # subset - used for fitting model
  GZ.daily.subset <- GZ.daily %>%
    filter(Subset == 1) %>%
    mutate(Time = 1:n())
  
  # cor(GZ.daily[, c("Tave", "Tmax", "Tmin", "P8wk.noself")], use = "complete.obs", method = "spearman")
  # cor(GZ.daily.subset[, c("Tave", "Tmax", "Tmin", "P8wk.noself")], use = "complete.obs", method = "spearman")
  
  year.n <- length(unique(GZ.daily.subset$Year))
  
  
  ## generate lagged climatic variables since we only use a subset of them for generating cross-basis matrix
  lag.t <- 20*7   # 20 weeks
  lag_tmax <- tsModel::Lag(GZ.daily$Tmax, k = 0:lag.t)[GZ.daily$Subset == 1, ]
  lag_rain <- tsModel::Lag(GZ.daily$HeavyRain, k = 0:lag.t)[GZ.daily$Subset == 1, ]
  lag_P8wk.current <- tsModel::Lag(GZ.daily$P8wk.current, k = 0:lag.t)[GZ.daily$Subset == 1, ]
  
  
  ## define cross-basis matrix (combining nonlinear and lag functions)
  # Tmax
  var.current <- lag_tmax
  basis_tmax <- crossbasis(var.current, 
                           argvar = list(fun = "ns", knots = equalknots(var.current[,1], n.knots)),  
                           arglag=list(knots= logknots(lag.t, n.knots)))
  
  # Rainfall
  var.current <- lag_rain
  basis_rain <- crossbasis(var.current, 
                           argvar = list(fun = "strata", breaks = 0.5),  
                           arglag=list(knots= logknots(lag.t, n.knots)))
  
  # P8wk
  var.current <- lag_P8wk.current
  basis_PriorWater <- crossbasis(var.current, 
                               argvar = list(fun = "lin"),  
                               arglag=list(knots= logknots(lag.t, n.knots)))
  
  
  # Rain * P8wk
  var.current <- tsModel::Lag(GZ.daily$P8wk.current * GZ.daily$HeavyRain, k = 0:lag.t)[GZ.daily$Subset == 1, ]
  basis_rain_PriorWater <- crossbasis(var.current, 
                                     argvar = list(fun = "lin"),  
                                     arglag=list(knots= logknots(lag.t, 2)))
  
  # assign names for cross basis
  colnames(basis_tmax) <- paste0("basis_tmax.", colnames(basis_tmax))
  colnames(basis_rain) <- paste0("basis_rain.", colnames(basis_rain))
  
  colnames(basis_PriorWater) <- paste0("basis_pwater.", colnames(basis_PriorWater))
  
  colnames(basis_rain_PriorWater) <- paste0("basis_rain_pwater.", colnames(basis_rain_PriorWater))
  
  
  if(trend.method == "ns_time")
  {
    formula <- CaseCount ~ 1 + ns(Time, year.n*time.df) + dow + basis_tmax + basis_rain + basis_PriorWater + basis_rain_PriorWater
  } else if(trend.method == "ns_year_doy") {
    ns.Year <- ns(GZ.daily.subset$Year, 4)
    colnames(ns.Year) <- paste0("ns_year", colnames(ns.Year))
    
    ns.DOY <- ns(GZ.daily.subset$DOY, time.df)
    colnames(ns.DOY) <- paste0("ns_doy", colnames(ns.DOY))
    
    formula <- CaseCount ~ 1 + ns.Year + ns.DOY + dow + basis_tmax + basis_rain + basis_PriorWater + basis_rain_PriorWater
  }
  model <- fitmodel(formula, GZ.daily.subset)

  # output
  list(model.result = model, 
       basis_tmax = basis_tmax, 
       basis_rain = basis_rain, 
       basis_PriorWater = basis_PriorWater, 
       basis_rain_PriorWater = basis_rain_PriorWater)
}



         

         
         








