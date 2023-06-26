###################################################################################################
##  Code for running the main model and the sensitivity analyses
# 
## Written by Qu Cheng
## Last updated: Jun 15th, 2023
###################################################################################################

# load packages and pre-processed data
source("0_packages_data_functions.R")

# R script to run INLA models 
#=============  Main model ================
model.fit <- DengueModel(water.pt = 0.05)
save(model.fit, file = "output/Main_low.Rdata")

model.fit <- DengueModel(water.pt = 0.5)
save(model.fit, file = "output/Main_mid.Rdata")

model.fit <- DengueModel(water.pt = 0.95)
save(model.fit, file = "output/Main_high.Rdata")



#=======Sensitivity: centering values for prior water ================
thres <- seq(0.05, 0.95, 0.1)
for(thres.i in thres)
{
  print(thres.i)
  
  model.fit <- DengueModel(water.pt = thres.i)
  save(model.fit, file = paste0("output/Sens_p8wthres_", thres.i,".RData"))
}



#=======Sensitivity: heavy rainfall defined as >90th percentile  ================
model.fit <- DengueModel(water.pt = 0.05, HR.pt = 0.9)
save(model.fit, file = "output/Sens_HRthres_low.Rdata")

model.fit <- DengueModel(water.pt = 0.5, HR.pt = 0.9)
save(model.fit, file = "output/Sens_HRthres_mid.Rdata")

model.fit <- DengueModel(water.pt = 0.95, HR.pt = 0.9)
save(model.fit, file = "output/Sens_HRthres_high.Rdata")





#=======Sensitivity: number of weeks for prior water availability  ================
# set to 7 weeks
model.fit <- DengueModel(water.pt = 0.05, water.wks = 7)
save(model.fit, file = "output/Sens_p7w_low.Rdata")

model.fit <- DengueModel(water.pt = 0.5, water.wks = 7)
save(model.fit, file = "output/Sens_p7w_mid.Rdata")

model.fit <- DengueModel(water.pt = 0.95, water.wks = 7)
save(model.fit, file = "output/Sens_p7w_high.Rdata")

# set to 9 weeks
model.fit <- DengueModel(water.pt = 0.05, water.wks = 9)
save(model.fit, file = "output/Sens_p9w_low.Rdata")

model.fit <- DengueModel(water.pt = 0.5, water.wks = 9)
save(model.fit, file = "output/Sens_p9w_mid.Rdata")

model.fit <- DengueModel(water.pt = 0.95, water.wks = 9)
save(model.fit, file = "output/Sens_p9w_high.Rdata")





#=======Sensitivity: df of ns(Time)  ================
# set to 8
model.fit <- DengueModel(water.pt = 0.05, time.df = 8)
save(model.fit, file = "output/Sens_timedf8_low.Rdata")

model.fit <- DengueModel(water.pt = 0.5, time.df = 8)
save(model.fit, file = "output/Sens_timedf8_mid.Rdata")

model.fit <- DengueModel(water.pt = 0.95, time.df = 8)
save(model.fit, file = "output/Sens_timedf8_high.Rdata")

# set to 9
model.fit <- DengueModel(water.pt = 0.05, time.df = 9)
save(model.fit, file = "output/Sens_timedf9_low.Rdata")

model.fit <- DengueModel(water.pt = 0.5, time.df = 9)
save(model.fit, file = "output/Sens_timedf9_mid.Rdata")

model.fit <- DengueModel(water.pt = 0.95, time.df = 9)
save(model.fit, file = "output/Sens_timedf9_high.Rdata")


#=======Sensitivity: number of knots for the exposure- or lag-response relationship  ================
# set to 3
model.fit <- DengueModel(water.pt = 0.05, n.knots = 3)
save(model.fit, file = "output/Sens_3knots_low.Rdata")

model.fit <- DengueModel(water.pt = 0.5, n.knots = 3)
save(model.fit, file = "output/Sens_3knots_mid.Rdata")

model.fit <- DengueModel(water.pt = 0.95, n.knots = 3)
save(model.fit, file = "output/Sens_3knots_high.Rdata")

# set to 4
model.fit <- DengueModel(water.pt = 0.05, n.knots = 4)
save(model.fit, file = "output/Sens_4knots_low.Rdata")

model.fit <- DengueModel(water.pt = 0.5, n.knots = 4)
save(model.fit, file = "output/Sens_4knots_mid.Rdata")

model.fit <- DengueModel(water.pt = 0.95, n.knots = 4)
save(model.fit, file = "output/Sens_4knots_high.Rdata")





#=======Sensitivity: remove 2014  ================
model.fit <- DengueModel(water.pt = 0.05, year.include = c(2006, 2012, 2013, 2016, 2017, 2018))
save(model.fit, file = "output/Sens_no2014_low.Rdata")

model.fit <- DengueModel(water.pt = 0.5, year.include = c(2006, 2012, 2013, 2016, 2017, 2018))
save(model.fit, file = "output/Sens_no2014_mid.Rdata")

model.fit <- DengueModel(water.pt = 0.95, year.include = c(2006, 2012, 2013, 2016, 2017, 2018))
save(model.fit, file = "output/Sens_no2014_high.Rdata")