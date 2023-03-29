###################################################################################################
##  The code is adapted from Lowe, et al., Lancet Planetary Health https://github.com/drrachellowe/hydromet_dengue
# 
## Written by Qu Cheng
## Last updated: March 8th, 2023
###################################################################################################

# R script to examine the sensitivity of the results to various model parameters
# 1. centering value for prior water availability: 15, 25, 35, ..., 85 percentile
# 2. definition of heavy rainfall
# 3. number of weeks for prior water availability: 8 or 9
# 4. ns(Time, df = 8 or 9)
# 5. number of knots for the exposure- or lag-response relationships: 3 or 4
# 6. remove 2014




# STEP 1: centering value for prior water availability
source("0_packages_data_functions.R")    # for loading packages and functions

thres <- seq(0.05, 0.95, 0.1)

for(thres.i in thres)
{
  print(thres.i)
  
  GZ.daily <- read_csv("Data/DF_case.csv") %>%
    mutate(OnsetDate = as.Date(OnsetDate), Year = year(OnsetDate), Month = month(OnsetDate), dow = weekdays(OnsetDate),
           P8wk = zoo::rollsum(Precipitation, 8*7, align = "right", na.pad = TRUE), P8wk.noself = lag(P8wk, 1),
           HeavyRain = Precipitation > quantile(Precipitation[Precipitation > 0], 0.95),   
           P8wk.low = P8wk.noself - quantile(P8wk.noself, thres.i, na.rm = TRUE)) %>%
    mutate(Subset = ifelse(Year %in% c(2006, 2012, 2013, 2014, 2016, 2017, 2018) & Month %in% c(7:12), 1, 0))  # data used in the analysis
  
  # subset - used for fitting model
  GZ.daily.subset <- GZ.daily %>%
    filter(Subset == 1) %>%
    mutate(Time = 1:n())
  
  year.n <- length(unique(GZ.daily.subset$Year))
  
  lag.t <- 20*7   # 20 weeks
  lag_tmax <- tsModel::Lag(GZ.daily$Tmax, k = 0:lag.t)[GZ.daily$Subset == 1, ]
  lag_rain <- tsModel::Lag(GZ.daily$HeavyRain, k = 0:lag.t)[GZ.daily$Subset == 1, ]
  
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
  
  # assign names for cross basis
  colnames(basis_tmax) <- paste0("basis_tmax.", colnames(basis_tmax))
  colnames(basis_rain) <- paste0("basis_rain.", colnames(basis_rain))
  
  colnames(basis_rain_water_low) <- paste0("basis_rain_water_low.", colnames(basis_rain_water_low))
  
  baseformula0 <- CaseCount ~ 1 + ns(Time, year.n*7) + dow + basis_tmax + basis_rain  + P8wk.noself + basis_rain_water_low
  model <- fitmodel(baseformula0, GZ.daily.subset)
  save(model, file = paste0("output/model_p8wthres_", thres.i,".RData"))
}









# STEP 2: heavy rain defined as above the 90th percentile
source("0_packages_data_functions.R")  

GZ.daily <- read_csv("Data/DF_case.csv") %>%
  mutate(OnsetDate = as.Date(OnsetDate), Year = year(OnsetDate), Month = month(OnsetDate), dow = weekdays(OnsetDate),
         P8wk = zoo::rollsum(Precipitation, 8*7, align = "right", na.pad = TRUE), P8wk.noself = lag(P8wk, 1),
         HeavyRain = Precipitation > quantile(Precipitation[Precipitation > 0], 0.9),   
         P8wk.low = P8wk.noself - quantile(P8wk.noself, 0.05, na.rm = TRUE),
         P8wk.mid = P8wk.noself - quantile(P8wk.noself, 0.5, na.rm = TRUE),
         P8wk.high = P8wk.noself - quantile(P8wk.noself, 0.95, na.rm = TRUE)) %>%
  mutate(Subset = ifelse(Year %in% c(2006, 2012, 2013, 2014, 2016, 2017, 2018) & Month %in% c(7:12), 1, 0))  # data used in the analysis


# subset - used for fitting model
GZ.daily.subset <- GZ.daily %>%
  filter(Subset == 1) %>%
  mutate(Time = 1:n())

year.n <- length(unique(GZ.daily.subset$Year))

lag.t <- 20*7   # 20 weeks
lag_tmax <- tsModel::Lag(GZ.daily$Tmax, k = 0:lag.t)[GZ.daily$Subset == 1, ]
lag_rain <- tsModel::Lag(GZ.daily$HeavyRain, k = 0:lag.t)[GZ.daily$Subset == 1, ]

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
colnames(basis_tmax) <- paste0("basis_tmax.", colnames(basis_tmax))
colnames(basis_rain) <- paste0("basis_rain.", colnames(basis_rain))

colnames(basis_rain_water_low) <- paste0("basis_rain_water_low.", colnames(basis_rain_water_low))
colnames(basis_rain_water_mid) <- paste0("basis_rain_water_mid.", colnames(basis_rain_water_mid))
colnames(basis_rain_water_high) <- paste0("basis_rain_water_high", colnames(basis_rain_water_high))

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
                   save(model, file = paste0("output/", lab[i],"_HR_thres.RData"))})

# create table to store DIC
table1 <- data.frame(Model  = c("low prior water", "medium prior water", "high prior water"), 
                     DIC = NA)

for(i in 1:length(formulas))
{
  load(paste0("output/",lab[i],"_HR_thres.RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table1








# STEP 3: number of weeks for prior water availability
source("0_packages_data_functions.R")    # for loading packages and functions


## run models 
# full dataset - used for generate lagged data as the input for cross-basis function
GZ.daily <- read_csv("Data/DF_case.csv") %>%
  mutate(OnsetDate = as.Date(OnsetDate), Year = year(OnsetDate), Month = month(OnsetDate), dow = weekdays(OnsetDate),
         P8wk = zoo::rollsum(Precipitation, 7*7, align = "right", na.pad = TRUE), P8wk.noself = lag(P8wk, 1),
         HeavyRain = Precipitation > quantile(Precipitation[Precipitation > 0], 0.95),   
         P8wk.low = P8wk.noself - quantile(P8wk.noself, 0.05, na.rm = TRUE),
         P8wk.mid = P8wk.noself - quantile(P8wk.noself, 0.5, na.rm = TRUE),
         P8wk.high = P8wk.noself - quantile(P8wk.noself, 0.95, na.rm = TRUE)) %>%
  mutate(Subset = ifelse(Year %in% c(2006, 2012, 2013, 2014, 2016, 2017, 2018) & Month %in% c(7:12), 1, 0))  # data used in the analysis

GZ.daily.subset <- GZ.daily %>%
  filter(Subset == 1) %>%
  mutate(Time = 1:n())
year.n <- length(unique(GZ.daily.subset$Year))

lag.t <- 20*7   # 20 weeks
lag_tmax <- tsModel::Lag(GZ.daily$Tmax, k = 0:lag.t)[GZ.daily$Subset == 1, ]
lag_rain <- tsModel::Lag(GZ.daily$HeavyRain, k = 0:lag.t)[GZ.daily$Subset == 1, ]

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
colnames(basis_tmax) <- paste0("basis_tmax.", colnames(basis_tmax))
colnames(basis_rain) <- paste0("basis_rain.", colnames(basis_rain))

colnames(basis_rain_water_low) <- paste0("basis_rain_water_low.", colnames(basis_rain_water_low))
colnames(basis_rain_water_mid) <- paste0("basis_rain_water_mid.", colnames(basis_rain_water_mid))
colnames(basis_rain_water_high) <- paste0("basis_rain_water_high", colnames(basis_rain_water_high))

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
                   save(model, file = paste0("output/", lab[i],"_P7w.RData"))})

# create table to store DIC
table1 <- data.frame(Model  = c("low prior water", "medium prior water", "high prior water"), 
                     DIC = NA)

for(i in 1:length(formulas))
{
  load(paste0("output/", lab[i],"_P7w.RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table1




## previous 9 weeks 
# full dataset - used for generate lagged data as the input for cross-basis function
GZ.daily <- read_csv("Data/DF_case.csv") %>%
  mutate(OnsetDate = as.Date(OnsetDate), Year = year(OnsetDate), Month = month(OnsetDate), dow = weekdays(OnsetDate),
         P8wk = zoo::rollsum(Precipitation, 9*7, align = "right", na.pad = TRUE), P8wk.noself = lag(P8wk, 1),
         HeavyRain = Precipitation > quantile(Precipitation[Precipitation > 0], 0.95),   
         P8wk.low = P8wk.noself - quantile(P8wk.noself, 0.05, na.rm = TRUE),
         P8wk.mid = P8wk.noself - quantile(P8wk.noself, 0.5, na.rm = TRUE),
         P8wk.high = P8wk.noself - quantile(P8wk.noself, 0.95, na.rm = TRUE)) %>%
  mutate(Subset = ifelse(Year %in% c(2006, 2012, 2013, 2014, 2016, 2017, 2018) & Month %in% c(7:12), 1, 0))  # data used in the analysis
GZ.daily.subset <- GZ.daily %>%
  filter(Subset == 1) %>%
  mutate(Time = 1:n())
year.n <- length(unique(GZ.daily.subset$Year))

lag.t <- 20*7   # 20 weeks
lag_tmax <- tsModel::Lag(GZ.daily$Tmax, k = 0:lag.t)[GZ.daily$Subset == 1, ]
lag_rain <- tsModel::Lag(GZ.daily$HeavyRain, k = 0:lag.t)[GZ.daily$Subset == 1, ]

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
colnames(basis_tmax) <- paste0("basis_tmax.", colnames(basis_tmax))
colnames(basis_rain) <- paste0("basis_rain.", colnames(basis_rain))

colnames(basis_rain_water_low) <- paste0("basis_rain_water_low.", colnames(basis_rain_water_low))
colnames(basis_rain_water_mid) <- paste0("basis_rain_water_mid.", colnames(basis_rain_water_mid))
colnames(basis_rain_water_high) <- paste0("basis_rain_water_high", colnames(basis_rain_water_high))

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
                   save(model, file = paste0("output/", lab[i],"_P9w.RData"))})

# create table to store DIC
table1 <- data.frame(Model  = c("low prior water", "medium prior water", "high prior water"), 
                     DIC = NA)

for(i in 1:length(formulas))
{
  load(paste0("output/", lab[i],"_P9w.RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table1











# STEP 4: df of ns(Time)
# full dataset - used for generate lagged data as the input for cross-basis function
source("0_packages_data_functions.R")

df <- c(8, 9)

for(df.i in 1:length(df))
{
  print(df[df.i])

  baseformula0 <- CaseCount ~ 1 + ns(Time, year.n*df[df.i]) + dow + basis_tmax + basis_rain  + P8wk.noself
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
                     save(model, file = paste0("output/", lab[i],"_df_",df[df.i],".RData"))})

  # create table to store DIC
  table1 <- data.frame(Model  = c("low prior water", "medium prior water", "high prior water"),
                       DIC = NA)

  for(i in 1:length(formulas))
  {
    load(paste0("output/", lab[i],"_df_",df[df.i],".RData"))
    table1$DIC[i] <- round(model$dic$dic, 0)
  }

  # view table
  print(table1)
}







# STEP 5: number of knots for the exposure- or lag-response relationship
source("0_packages_data_functions.R")
nknots = c(3, 4)

## STEP 3. define cross-basis matrix (combining nonlinear and lag functions)
for(knots.i in nknots)
{
  print(knots.i)
  # Tmax
  var.current <- lag_tmax
  basis_tmax <- crossbasis(var.current, 
                           argvar = list(fun = "ns", knots = equalknots(var.current[1,], 2)),  
                           arglag=list(knots= logknots(lag.t, 2)))
  
  # Rainfall
  var.current <- lag_rain
  basis_rain <- crossbasis(var.current, 
                           argvar = list(fun = "strata", breaks = 0.5),  
                           arglag=list(knots= logknots(lag.t, knots.i)))
  
  
  # Interaction for Rainfall and prior water availability
  basis_rain_water_low <- basis_rain * GZ.daily.subset$P8wk.low
  basis_rain_water_mid <- basis_rain * GZ.daily.subset$P8wk.mid
  basis_rain_water_high <- basis_rain * GZ.daily.subset$P8wk.high
  
  # assign names for cross basis
  colnames(basis_tmax) <- paste0("basis_tmax.", colnames(basis_tmax))
  colnames(basis_rain) <- paste0("basis_rain.", colnames(basis_rain))
  
  colnames(basis_rain_water_low) <- paste0("basis_rain_water_low.", colnames(basis_rain_water_low))
  colnames(basis_rain_water_mid) <- paste0("basis_rain_water_mid.", colnames(basis_rain_water_mid))
  colnames(basis_rain_water_high) <- paste0("basis_rain_water_high", colnames(basis_rain_water_high))
  
  
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
                     save(model, file = paste0("output/", lab[i],"_", knots.i,"knots.RData"))})
}










# STEP 6: remove 2014
source("0_packages_data_functions.R")    # for loading packages and functions


## run models 
# full dataset - used for generate lagged data as the input for cross-basis function
GZ.daily <- read_csv("Data/DF_case.csv") %>%
  mutate(OnsetDate = as.Date(OnsetDate), Year = year(OnsetDate), Month = month(OnsetDate), dow = weekdays(OnsetDate),
         P8wk = zoo::rollsum(Precipitation, 8*7, align = "right", na.pad = TRUE), P8wk.noself = lag(P8wk, 1),
         HeavyRain = Precipitation > quantile(Precipitation[Precipitation > 0], 0.95),   
         P8wk.low = P8wk.noself - quantile(P8wk.noself, 0.05, na.rm = TRUE),
         P8wk.mid = P8wk.noself - quantile(P8wk.noself, 0.5, na.rm = TRUE),
         P8wk.high = P8wk.noself - quantile(P8wk.noself, 0.95, na.rm = TRUE)) %>%
  mutate(Subset = ifelse(Year %in% c(2006, 2012, 2013, 2016, 2017, 2018) & Month %in% c(7:12), 1, 0))  # data used in the analysis

GZ.daily.subset <- GZ.daily %>%
  filter(Subset == 1) %>%
  mutate(Time = 1:n())
year.n <- length(unique(GZ.daily.subset$Year))

lag.t <- 20*7   # 20 weeks
lag_tmax <- tsModel::Lag(GZ.daily$Tmax, k = 0:lag.t)[GZ.daily$Subset == 1, ]
lag_rain <- tsModel::Lag(GZ.daily$HeavyRain, k = 0:lag.t)[GZ.daily$Subset == 1, ]

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
colnames(basis_tmax) <- paste0("basis_tmax.", colnames(basis_tmax))
colnames(basis_rain) <- paste0("basis_rain.", colnames(basis_rain))

colnames(basis_rain_water_low) <- paste0("basis_rain_water_low.", colnames(basis_rain_water_low))
colnames(basis_rain_water_mid) <- paste0("basis_rain_water_mid.", colnames(basis_rain_water_mid))
colnames(basis_rain_water_high) <- paste0("basis_rain_water_high", colnames(basis_rain_water_high))

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
                   save(model, file = paste0("output/", lab[i],"_no2014.RData"))})

# create table to store DIC
table1 <- data.frame(Model  = c("low prior water", "medium prior water", "high prior water"), 
                     DIC = NA)

for(i in 1:length(formulas))
{
  load(paste0("output/",lab[i],"_no2014.RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table1







