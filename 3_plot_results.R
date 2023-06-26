###################################################################################################
##  The code is adapted from Lowe, et al., Lancet Planetary Health https://github.com/drrachellowe/hydromet_dengue
# 
## Written by Qu Cheng
## Last updated: March 21, 2023
###################################################################################################

# R script to plot the results from INLA model



#================== Main model ===========================
# STEP 1: load packages and pre-processed data
source("0_packages_data_functions.R")


# STEP 2: process model outputs for temperature and rainfall
#=== When prior water level is low ===
load("output/Main_low.Rdata")
model <- model.fit$model.result
basis_tmax <- model.fit$basis_tmax
basis_rain <- model.fit$basis_rain

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

quantile(GZ.daily$Tmax, seq(0, 1, 0.1))

# get RR for temperature
indt <- grep("basis_t", model$names.fixed)
predTemp <- crosspred(basis_tmax, coef = coef[indt], vcov=vcov[indt,indt],
                      model.link = "log", bylag = 1, cen = 28)   # ref = median(max Temp)
Temp_low <- data.table(WaterLevel = "Low", 
                       VariableValue = rownames(predTemp$matRRfit),
                       Lag = rep(0:140, each = nrow(predTemp$matRRfit)), 
                       RR = as.numeric(predTemp$matRRfit), 
                       RR.low = as.numeric(predTemp$matRRlow), 
                       RR.high = as.numeric(predTemp$matRRhigh))

# get RR for rainfall
indt <- grep("basis_rain.v", model$names.fixed)
pred.Rain <- crosspred(basis_rain, coef = coef[indt], vcov=vcov[indt,indt],
                      model.link = "log", bylag = 1, cen = 0, by = 1)   # ref = no rain
Rain_low <- data.table(WaterLevel = "Low", 
                     VariableValue = rownames(pred.Rain$matRRfit),
                     Lag = rep(0:140, each = nrow(pred.Rain$matRRfit)), 
                     RR = as.numeric(pred.Rain$matRRfit), 
                     RR.low = as.numeric(pred.Rain$matRRlow), 
                     RR.high = as.numeric(pred.Rain$matRRhigh))


#=== When prior water level is medium ===
load("output/Main_mid.Rdata")
model <- model.fit$model.result
basis_tmax <- model.fit$basis_tmax
basis_rain <- model.fit$basis_rain

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# get RR for temperature
indt <- grep("basis_t", model$names.fixed)
predTemp <- crosspred(basis_tmax, coef = coef[indt], vcov=vcov[indt,indt],
                      model.link = "log", bylag = 1, cen = 28)   # ref = median(max Temp)
Temp_mid <- data.table(WaterLevel = "Medium", 
                        VariableValue = rownames(predTemp$matRRfit),
                        Lag = rep(0:140, each = nrow(predTemp$matRRfit)), 
                        RR = as.numeric(predTemp$matRRfit), 
                        RR.low = as.numeric(predTemp$matRRlow), 
                        RR.high = as.numeric(predTemp$matRRhigh))

# get RR for rainfall
indt <- grep("basis_rain.v", model$names.fixed)
pred.Rain <- crosspred(basis_rain, coef = coef[indt], vcov=vcov[indt,indt],
                       model.link = "log", bylag = 1, cen = 0, by = 1)   # ref = no rain
Rain_mid <- data.table(WaterLevel = "Medium", 
                        VariableValue = rownames(pred.Rain$matRRfit),
                        Lag = rep(0:140, each = nrow(pred.Rain$matRRfit)), 
                        RR = as.numeric(pred.Rain$matRRfit), 
                        RR.low = as.numeric(pred.Rain$matRRlow), 
                        RR.high = as.numeric(pred.Rain$matRRhigh))


#=== When prior water level is high ===
load("output/Main_high.Rdata")
model <- model.fit$model.result
basis_tmax <- model.fit$basis_tmax
basis_rain <- model.fit$basis_rain

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# get RR for temperature
indt <- grep("basis_t", model$names.fixed)
predTemp <- crosspred(basis_tmax, coef = coef[indt], vcov=vcov[indt,indt],
                      model.link = "log", bylag = 1, cen = 28)   # ref = median(max Temp)
Temp_high <- data.table(WaterLevel = "High", 
                       VariableValue = rownames(predTemp$matRRfit),
                       Lag = rep(0:140, each = nrow(predTemp$matRRfit)), 
                       RR = as.numeric(predTemp$matRRfit), 
                       RR.low = as.numeric(predTemp$matRRlow), 
                       RR.high = as.numeric(predTemp$matRRhigh))

# get RR for rainfall
indt <- grep("basis_rain.v", model$names.fixed)
pred.Rain <- crosspred(basis_rain, coef = coef[indt], vcov=vcov[indt,indt],
                       model.link = "log", bylag = 1, cen = 0,  by = 1)   # ref = no rain
Rain_high <- data.table(WaterLevel = "High", 
                       VariableValue = rownames(pred.Rain$matRRfit),
                       Lag = rep(0:140, each = nrow(pred.Rain$matRRfit)), 
                       RR = as.numeric(pred.Rain$matRRfit), 
                       RR.low = as.numeric(pred.Rain$matRRlow), 
                       RR.high = as.numeric(pred.Rain$matRRhigh))

Temp_RR <- rbindlist(list(Temp_low, Temp_mid, Temp_high))
Rain_RR <- rbindlist(list(Rain_low, Rain_mid, Rain_high))







# STEP 3: make plots for rainfall
# manuscript Figure 4
Rain_RR %>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel) %>%
  top_n(1, RR)

Rain_RR %>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel) %>%
  top_n(1, -RR)

scenario_names <- c(
  'Low'="A. Low prior water",
  'Medium' = "B. Medium prior water",
  'High'="C. High prior water"
)

col.pal <- c("#6baed6", "#2171b5", "#08519c")
Rain_RR %>%
  mutate(VariableValue = as.numeric(VariableValue),
         WaterLevel = factor(WaterLevel, levels = c("Low", "Medium", "High"))) %>%
  filter(VariableValue == 1) %>%
  ggplot(aes(x = Lag, y = RR, col = WaterLevel, fill = WaterLevel, group = 1)) + 
  geom_ribbon(aes(ymin = RR.low, ymax = RR.high, fill = WaterLevel), alpha = 0.3, col = NA) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  scale_color_manual(values = col.pal) +
  scale_fill_manual(values = col.pal) +
  labs(x = "Lag (days)", 
       y = "RR",
       col = "",
       fill = "")+
  facet_wrap(~WaterLevel, labeller = as_labeller(scenario_names)) +
  guides(fill = "none", color = "none") +
  theme(strip.background = element_blank(), 
        strip.text = element_text(hjust = 0, size = 15, face = "bold"))

# ggsave("Figures/Fig4_HeavyRain.pdf", width = 10, height = 4)







# STEP 4: make plots for temperature
# For main manuscript
Temp_RR %>%
  filter(VariableValue == 35) %>%
  group_by(WaterLevel) %>%
  top_n(1, RR)

Temp_RR %>%
  filter(VariableValue == 18) %>%
  group_by(WaterLevel) %>%
  top_n(1, -RR)

Fig.Temp1 <- Temp_RR %>%
  filter(WaterLevel == "Medium") %>%
  mutate(VariableValue = as.numeric(VariableValue)) %>%
  ggplot(aes(x = Lag, y = VariableValue, fill = RR)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 1, low = "#1b7837", high = "#762a83") +
  theme_cowplot() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.border = element_rect(size = 1, colour = "black"),
        axis.line = element_blank(),
        legend.key.height = unit(1.5, "cm"),
        legend.text = element_text(angle = 90,
                                   hjust = 0.5)) +
  xlab("Lag (days)") +
  ylab(bquote(T[max]~' (°C)'))  +
  geom_hline(yintercept =  28, linetype = "dashed") +
  annotate("text", x = 125, y = 29.5, label = "ref.")


col.pal <- pal_aaas()(2)
Fig.Temp2 <- Temp_RR %>%
  mutate(VariableValue = as.numeric(VariableValue)) %>%
  filter(WaterLevel == "Medium", VariableValue %in% c(18, 35)) %>%
  ggplot(aes(x = Lag, y = RR, col = as.factor(VariableValue), fill = as.factor(VariableValue))) + 
  geom_ribbon(aes(ymin = RR.low, ymax = RR.high), alpha = 0.3, col = NA) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  scale_color_manual(values = col.pal, labels = c(bquote(T[max]~' = 18°C'), bquote(T[max]~' = 35°C'))) +
  scale_fill_manual(values = col.pal, labels = c(bquote(T[max]~' = 18°C'), bquote(T[max]~' = 35°C'))) +
  theme(legend.position = c(0.5, 0.2)) +
  labs(x = "Lag (days)", 
       y = "RR",
       col = "",
       fill = "")

Fig.Temp <- plot_grid(Fig.Temp1, Fig.Temp2, labels = c("A", "B"), rel_widths = c(0.55, 0.48))
# save_plot("Figures/Fig5_temp.pdf", Fig.Temp, base_height = 3.8, base_width = 8)



























#======================== Sensitivity analyses =================================
# STEP 1: centering value for prior water availability
source("0_packages_data_functions.R")
filenames <- data.frame(filenames = paste("output/Sens_p8wthres_", seq(0.05, 0.95, 0.1),".RData", sep = ""),
                        WaterLevel = seq(0.05, 0.95, 0.1))

Rain.all <- NULL
for(i in 1:nrow(filenames))
{
  print(i)
  
  load(filenames$filenames[i])
  model <- model.fit$model.result
  basis_rain <- model.fit$basis_rain
  
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # get RR for rainfall
  indt <- grep("basis_rain.v", model$names.fixed)
  pred.Rain <- crosspred(basis_rain, coef = coef[indt], vcov=vcov[indt,indt],
                         model.link = "log", bylag = 1, cen = 0, by = 1)   # ref = 29 degree C
  current.Rain <- data.table(WaterLevel = filenames$WaterLevel[i], 
                             VariableValue = rownames(pred.Rain$matRRfit),
                             Lag = rep(0:140, each = nrow(pred.Rain$matRRfit)), 
                             RR = as.numeric(pred.Rain$matRRfit), 
                             RR.low = as.numeric(pred.Rain$matRRlow), 
                             RR.high = as.numeric(pred.Rain$matRRhigh))
  
  Rain.all <- rbind(Rain.all, current.Rain)
}

Rain.all%>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel) %>%
  top_n(1, RR)

Rain.all %>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel) %>%
  top_n(1, -RR)


# RR at 50 mm
scenario_names_b <- c(
  '0.05'="A. 5th percentile",
  '0.15'="B. 15th percentile",
  '0.25'="C. 25th percentile",
  '0.35'="D. 35th percentile",
  '0.45'="E. 45th percentile",
  '0.55'="F. 55th percentile",
  '0.65'="G. 65th percentile",
  '0.75'="H. 75th percentile",
  '0.85'="I. 85th percentile",
  '0.95'="J. 95th percentile"
)



Rain.all %>%
  mutate(VariableValue = as.numeric(VariableValue)) %>%
  filter(VariableValue %in% c(1)) %>%
  ggplot(aes(x = Lag, y = RR)) + 
  geom_ribbon(aes(ymin = RR.low, ymax = RR.high), alpha = 0.3, col = NA) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  theme(legend.position = c(0.05, 0.97),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15)) +
  labs(x = "Lag (days)", 
       y = "RR",
       col = "",
       fill = "")+
  facet_wrap(~WaterLevel, labeller = as_labeller(scenario_names_b), nrow = 4)


# ggsave("Figures/FigS4_HeavyRain_rainCenter.pdf", height = 12, width = 9)









# STEP 2: change the threshold for defining heavy rainfall events
lab <- c("low", "mid", "high")
filenames <- data.frame(filenames = c(paste("output/Sens_HRthres_", lab,".RData", sep = "")),
                        WaterLevel = c("Low", "Medium", "High"))

Rain.all <- NULL
for(i in 1:nrow(filenames))
{
  print(i)
  
  load(filenames$filenames[i])
  model <- model.fit$model.result
  basis_rain <- model.fit$basis_rain
  
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # get RR for rainfall
  indt <- grep("basis_rain.v", model$names.fixed)
  pred.Rain <- crosspred(basis_rain, coef = coef[indt], vcov=vcov[indt,indt],
                         model.link = "log", bylag = 1, cen = 0, by = 1)   # ref = 29 degree C
  current.Rain <- data.table(WaterLevel = filenames$WaterLevel[i], 
                             VariableValue = rownames(pred.Rain$matRRfit),
                             Lag = rep(0:140, each = nrow(pred.Rain$matRRfit)), 
                             RR = as.numeric(pred.Rain$matRRfit), 
                             RR.low = as.numeric(pred.Rain$matRRlow), 
                             RR.high = as.numeric(pred.Rain$matRRhigh))
  
  Rain.all <- rbind(Rain.all, current.Rain)
}


Rain.all%>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel) %>%
  top_n(1, RR)

Rain.all %>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel) %>%
  top_n(1, -RR)

scenario_names <- c(
  'Low'="A. Low prior water",
  'Medium' = "B. Medium prior water",
  'High'="C. High prior water"
)

col.pal <- c("#6baed6", "#2171b5", "#08519c")
Rain.all %>%
  mutate(VariableValue = as.numeric(VariableValue),
         WaterLevel = factor(WaterLevel, levels = c("Low", "Medium", "High"))) %>%
  filter(VariableValue == 1) %>%
  ggplot(aes(x = Lag, y = RR, col = WaterLevel, fill = WaterLevel, group = 1)) + 
  geom_ribbon(aes(ymin = RR.low, ymax = RR.high, fill = WaterLevel), alpha = 0.3, col = NA) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  scale_color_manual(values = col.pal) +
  scale_fill_manual(values = col.pal) +
  labs(x = "Lag (days)", 
       y = "RR",
       col = "",
       fill = "")+
  facet_wrap(~WaterLevel, labeller = as_labeller(scenario_names)) +
  guides(fill = "none", color = "none") +
  theme(strip.background = element_blank(), 
        strip.text = element_text(hjust = 0, size = 15, face = "bold"))

ggsave("Figures/FigS5_HeavyRain_90th_percentile.pdf", width = 10, height = 4)









# STEP 3: number of weeks for prior water availability
lab <- c("low", "mid", "high")
filenames <- data.frame(filenames = c(paste("output/Sens_p7w_", lab,".RData", sep = ""),
                                      paste("output/Sens_p9w_", lab,".RData", sep = "")),
                        pweek = rep(c(7,9), each = 3),
                        WaterLevel = rep(c("Low", "Medium", "High"), 2))

Rain.all <- NULL
for(i in 1:nrow(filenames))
{
  print(i)
  
  load(filenames$filenames[i])
  model <- model.fit$model.result
  basis_rain <- model.fit$basis_rain
  
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # get RR for rainfall
  indt <- grep("basis_rain.v", model$names.fixed)
  pred.Rain <- crosspred(basis_rain, coef = coef[indt], vcov=vcov[indt,indt],
                         model.link = "log", bylag = 1, cen = 0, by = 1) 
  current.Rain <- data.table(WaterLevel = filenames$WaterLevel[i], 
                             pweek = filenames$pweek[i],
                             VariableValue = rownames(pred.Rain$matRRfit),
                             Lag = rep(0:140, each = nrow(pred.Rain$matRRfit)), 
                             RR = as.numeric(pred.Rain$matRRfit), 
                             RR.low = as.numeric(pred.Rain$matRRlow), 
                             RR.high = as.numeric(pred.Rain$matRRhigh))
  
  Rain.all <- rbind(Rain.all, current.Rain)
}

Rain.all%>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel, pweek) %>%
  top_n(1, RR)

Rain.all %>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel, pweek) %>%
  top_n(1, -RR)



#==== make plots ======
scenario_names <- c(
  'Low 7'="A. Low, previous 7 wks",
  'Medium 7'="B. Medium, previous 7 wks",
  'High 7'="C. High, previous 7 wks",
  'Low 9'="D. Low, previous 9 wks",
  'Medium 9'="E. Medium, previous 9 wks",
  'High 9'="F. High, previous 9 wks"
)

col.pal <- c("#6baed6", "#2171b5", "#08519c")
Rain.all %>%
  mutate(VariableValue = as.numeric(VariableValue),
         WaterLevel = factor(WaterLevel, levels = c("Low", "Medium", "High")),
         panel = paste(WaterLevel, pweek),
         panel = factor(panel, levels = c("Low 7", "Medium 7", "High 7", "Low 9", "Medium 9", "High 9"))) %>%
  filter(VariableValue == 1) %>%
  ggplot(aes(x = Lag, y = RR, col = WaterLevel, fill = WaterLevel, group = 1)) + 
  geom_ribbon(aes(ymin = RR.low, ymax = RR.high, fill = WaterLevel), alpha = 0.3, col = NA) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  scale_color_manual(values = col.pal) +
  scale_fill_manual(values = col.pal) +
  labs(x = "Lag (days)", 
       y = "RR",
       col = "",
       fill = "")+
  facet_wrap(~panel, labeller = as_labeller(scenario_names), nrow = 2) +
  guides(fill = "none", color = "none") +
  theme(strip.background = element_blank(), 
        strip.text = element_text(hjust = 0, size = 15, face = "bold"))

ggsave("Figures/FigS6_HeavyRain_cumrain_wks.pdf", width = 10, height = 8)










# STEP 4: df of ns(Time)
#==== process data ======
df <- c(8, 9)
lab <- c("low", "mid", "high")
filenames <- data.frame(filenames = c(paste("output/Sens_timedf8_", lab,".RData", sep = ""),
                                      paste("output/Sens_timedf9_", lab,".RData", sep = "")),
                        df = rep(df, each = 3),
                        WaterLevel = rep(c("Low", "Medium", "High"), 2))

Rain.all <- NULL
for(i in 1:nrow(filenames))
{
  print(i)
  
  load(filenames$filenames[i])
  model <- model.fit$model.result
  basis_rain <- model.fit$basis_rain
  
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # get RR for rainfall
  indt <- grep("basis_rain.v", model$names.fixed)
  pred.Rain <- crosspred(basis_rain, coef = coef[indt], vcov=vcov[indt,indt],
                         model.link = "log", bylag = 1, cen = 0, by = 1)  
  current.Rain <- data.table(WaterLevel = filenames$WaterLevel[i], 
                             time.df = filenames$df[i],
                             VariableValue = rownames(pred.Rain$matRRfit),
                             Lag = rep(0:140, each = nrow(pred.Rain$matRRfit)), 
                             RR = as.numeric(pred.Rain$matRRfit), 
                             RR.low = as.numeric(pred.Rain$matRRlow), 
                             RR.high = as.numeric(pred.Rain$matRRhigh))

  Rain.all <- rbind(Rain.all, current.Rain)
}

Rain.all%>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel, time.df) %>%
  top_n(1, RR)

Rain.all %>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel, time.df) %>%
  top_n(1, -RR)


Rain.all %>%
  filter(VariableValue == "1") %>%
  group_by(time.df, WaterLevel) %>%
  filter(RR == ifelse(WaterLevel == "High", min(RR), max(RR))) %>%
  mutate(format = paste(round(RR, 2), ", ", round(RR.low, 2), "-", round(RR.high, 2), sep = ""))


#==== make plots ======
scenario_names <- c(
  'Low 8'="A. Low, ns(Time, df = 8)",
  'Medium 8'="B. Medium, ns(Time, df = 8)",
  'High 8'="C. High, ns(Time, df = 8)",
  'Low 9'="D. Low, ns(Time, df = 9)",
  'Medium 9'="E. Medium, ns(Time, df = 9)",
  'High 9'="F. High, ns(Time, df = 9)"
)

col.pal <- c("#6baed6", "#2171b5", "#08519c")
Rain.all %>%
  mutate(VariableValue = as.numeric(VariableValue),
         WaterLevel = factor(WaterLevel, levels = c("Low", "Medium", "High")),
         panel = paste(WaterLevel, time.df),
         panel = factor(panel, levels = c("Low 8", "Medium 8", "High 8", "Low 9", "Medium 9", "High 9"))) %>%
  filter(VariableValue == 1) %>%
  ggplot(aes(x = Lag, y = RR, col = WaterLevel, fill = WaterLevel, group = 1)) + 
  geom_ribbon(aes(ymin = RR.low, ymax = RR.high, fill = WaterLevel), alpha = 0.3, col = NA) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  scale_color_manual(values = col.pal) +
  scale_fill_manual(values = col.pal) +
  labs(x = "Lag (days)", 
       y = "RR",
       col = "",
       fill = "")+
  facet_wrap(~panel, labeller = as_labeller(scenario_names), nrow = 2) +
  guides(fill = "none", color = "none") +
  theme(strip.background = element_blank(), 
        strip.text = element_text(hjust = 0, size = 15, face = "bold"))

ggsave("Figures/FigS7_HeavyRain_time_df.pdf", width = 10, height = 8)






















# STEP 5: number of knots for the exposure- or lag-response relationship
source("0_packages_data_functions.R")
lab <- c("low", "mid", "high")
filenames <- data.frame(filenames = c(paste("output/Sens_3knots_", lab,".RData", sep = ""), 
                                      paste("output/Sens_4knots_", lab,".RData", sep = "")),
                        knots = rep(c(3,4), each = 3),
                        WaterLevel = rep(c("Low", "Medium", "High"), 2))

Rain.all <- NULL
for(i in 1:nrow(filenames))
{
  print(i)
  
  load(filenames$filenames[i])
  model <- model.fit$model.result
  basis_rain <- model.fit$basis_rain
 
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # get RR for rainfall
  indt <- grep("basis_rain.v", model$names.fixed)
  pred.Rain <- crosspred(basis_rain, coef = coef[indt], vcov=vcov[indt,indt],
                         model.link = "log", bylag = 1, cen = 0, by = 1)   # ref = 29 degree C
  current.Rain <- data.table(WaterLevel = filenames$WaterLevel[i], 
                             nknots = filenames$knots[i],
                             VariableValue = rownames(pred.Rain$matRRfit),
                             Lag = rep(0:140, each = nrow(pred.Rain$matRRfit)), 
                             RR = as.numeric(pred.Rain$matRRfit), 
                             RR.low = as.numeric(pred.Rain$matRRlow), 
                             RR.high = as.numeric(pred.Rain$matRRhigh))
  
  Rain.all <- rbind(Rain.all, current.Rain)
}

Rain.all%>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel, nknots) %>%
  top_n(1, RR)

Rain.all %>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel, nknots) %>%
  top_n(1, -RR)

Rain.all %>%
  filter(VariableValue == "1") %>%
  group_by(nknots, WaterLevel) %>%
  filter(RR == ifelse(WaterLevel == "High", min(RR), max(RR))) %>%
  mutate(format = paste(round(RR, 2), ", ", round(RR.low, 2), "-", round(RR.high, 2), sep = ""))


scenario_names <- c(
  'Low 3'="A. Low, 3 knots",
  'Medium 3'="B. Medium, 3 knots",
  'High 3'="C. High, 3 knots",
  'Low 4'="D. Low, 4 knots",
  'Medium 4'="E. Medium, 4 knots",
  'High 4'="F. High, 4 knots"
)

Rain.all %>%
  mutate(VariableValue = as.numeric(VariableValue),
         WaterLevel = factor(WaterLevel, levels = c("Low", "Medium", "High")),
         panel = paste(WaterLevel, nknots),
         panel = factor(panel, levels = c("Low 3", "Medium 3", "High 3", "Low 4", "Medium 4", "High 4"))) %>%
  filter(VariableValue == 1) %>%
  ggplot(aes(x = Lag, y = RR, col = WaterLevel, fill = WaterLevel, group = 1)) + 
  geom_ribbon(aes(ymin = RR.low, ymax = RR.high, fill = WaterLevel), alpha = 0.3, col = NA) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  scale_color_manual(values = col.pal) +
  scale_fill_manual(values = col.pal) +
  labs(x = "Lag (days)", 
       y = "RR",
       col = "",
       fill = "")+
  facet_wrap(~panel, labeller = as_labeller(scenario_names)) +
  guides(fill = "none", color = "none") +
  theme(strip.background = element_blank(), 
        strip.text = element_text(hjust = 0, size = 15, face = "bold"))

ggsave("Figures/FigS8_HeavyRain_nknots.pdf", width = 10, height = 8)













# STEP 6: remove 2014
source("0_packages_data_functions.R")
lab <- c("low", "mid", "high")
filenames <- data.frame(filenames = c(paste("output/Sens_no2014_", lab,".RData", sep = "")),
                        WaterLevel = c("Low", "Medium", "High"))

Rain.all <- NULL
for(i in 1:nrow(filenames))
{
  print(i)
  
  load(filenames$filenames[i])
  model <- model.fit$model.result
  basis_rain <- model.fit$basis_rain
  
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # get RR for rainfall
  indt <- grep("basis_rain.v", model$names.fixed)
  pred.Rain <- crosspred(basis_rain, coef = coef[indt], vcov=vcov[indt,indt],
                         model.link = "log", bylag = 1, cen = 0, by = 1)   # ref = 29 degree C
  current.Rain <- data.table(WaterLevel = filenames$WaterLevel[i], 
                             VariableValue = rownames(pred.Rain$matRRfit),
                             Lag = rep(0:140, each = nrow(pred.Rain$matRRfit)), 
                             RR = as.numeric(pred.Rain$matRRfit), 
                             RR.low = as.numeric(pred.Rain$matRRlow), 
                             RR.high = as.numeric(pred.Rain$matRRhigh))
  
  Rain.all <- rbind(Rain.all, current.Rain)
}



Rain.all%>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel) %>%
  top_n(1, RR)

Rain.all %>%
  filter(VariableValue == 1) %>%
  group_by(WaterLevel) %>%
  top_n(1, -RR)




Rain.all %>%
  filter(VariableValue == "1") %>%
  group_by(WaterLevel) %>%
  filter(RR == ifelse(WaterLevel == "High", min(RR), max(RR))) %>%
  mutate(format = paste(round(RR, 2), ", ", round(RR.low, 2), "-", round(RR.high, 2), sep = ""))



scenario_names <- c(
  'Low'="A. Low prior water",
  'Medium' = "B. Medium prior water",
  'High'="C. High prior water"
)

col.pal <- c("#6baed6", "#2171b5", "#08519c")
Rain.all %>%
  mutate(VariableValue = as.numeric(VariableValue),
         WaterLevel = factor(WaterLevel, levels = c("Low", "Medium", "High"))) %>%
  filter(VariableValue == 1) %>%
  ggplot(aes(x = Lag, y = RR, col = WaterLevel, fill = WaterLevel, group = 1)) + 
  geom_ribbon(aes(ymin = RR.low, ymax = RR.high, fill = WaterLevel), alpha = 0.3, col = NA) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  scale_color_manual(values = col.pal) +
  scale_fill_manual(values = col.pal) +
  labs(x = "Lag (days)", 
       y = "RR",
       col = "",
       fill = "")+
  facet_wrap(~WaterLevel, labeller = as_labeller(scenario_names)) +
  guides(fill = "none", color = "none") +
  theme(strip.background = element_blank(), 
        strip.text = element_text(hjust = 0, size = 15, face = "bold"))

ggsave("Figures/FigS9_HeavyRain_no2014.pdf", width = 10, height = 4)










