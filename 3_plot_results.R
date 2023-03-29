###################################################################################################
##  The code is adapted from Lowe, et al., Lancet Planetary Health https://github.com/drrachellowe/hydromet_dengue
# 
## Written by Qu Cheng
## Last updated: March 21, 2023
###################################################################################################

# R script to plot the results from INLA model

# STEP 1: load packages and pre-processed data
source("0_packages_data_functions.R")

load("output/model_low.Rdata")
model_low <- model

load("output/model_mid.Rdata")
model_mid <- model

load("output/model_high.Rdata")
model_high <- model






# STEP 2: process model outputs for temperature and rainfall
#=== When prior water level is low ===
model <- model_low

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
model <- model_mid

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
model <- model_high

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












# STEP 3: make plots for temperature
# For main manuscript
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
# save_plot("Figures/Fig4_temp.pdf", Fig.Temp, base_height = 3.8, base_width = 8)

  

# for supplementary
#===== with low as the ref =====
Fig.Temp1 <- Temp_RR %>%
  filter(WaterLevel == "Low") %>%
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
  ylab(bquote(T[max]~' (°C)')) +
  ggtitle("A. Low prior water")+
  geom_hline(yintercept =  28, linetype = "dashed")+
  annotate("text", x = 125, y = 29.5, label = "ref.")



Fig.Temp2 <- Temp_RR %>%
  mutate(VariableValue = as.numeric(VariableValue)) %>%
  filter(WaterLevel == "Low", VariableValue %in% c(18, 35)) %>%
  ggplot(aes(x = Lag, y = RR, col = as.factor(VariableValue), fill = as.factor(VariableValue))) + 
  geom_ribbon(aes(ymin = RR.low, ymax = RR.high), alpha = 0.3, col = NA) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  scale_color_manual(values = col.pal, labels = c(bquote(T[max]~' = 18°C'), bquote(T[max]~' = 35°C'))) +
  scale_fill_manual(values = col.pal, labels = c(bquote(T[max]~' = 18°C'),bquote(T[max]~' = 35°C'))) +
  theme(legend.position = c(0.5, 0.2)) +
  labs(x = "Lag (days)", 
       y = "RR",
       col = "",
       fill = "")+
  ggtitle("B. Low prior water")

#===== with high as the ref =====
Fig.Temp3 <- Temp_RR %>%
  filter(WaterLevel == "High") %>%
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
  ylab(bquote(T[max]~' (°C)')) +
  ggtitle("C. High prior water")+
  geom_hline(yintercept =  28, linetype = "dashed")+
  annotate("text", x = 125, y = 29.5, label = "ref.")



Fig.Temp4 <- Temp_RR %>%
  mutate(VariableValue = as.numeric(VariableValue)) %>%
  filter(WaterLevel == "High", VariableValue %in% c(18, 35)) %>%
  ggplot(aes(x = Lag, y = RR, col = as.factor(VariableValue), fill = as.factor(VariableValue))) + 
  geom_ribbon(aes(ymin = RR.low, ymax = RR.high), alpha = 0.3, col = NA) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  scale_color_manual(values = col.pal, labels = c(bquote(T[max]~' = 18°C'), bquote(T[max]~' = 35°C'))) +
  scale_fill_manual(values = col.pal, labels = c(bquote(T[max]~' = 18°C'),  bquote(T[max]~' = 35°C'))) +
  theme(legend.position = c(0.5, 0.2)) +
  labs(x = "Lag (days)", 
       y = "RR",
       col = "",
       fill = "")+
  ggtitle("D. High prior water")

Fig.Temp <- plot_grid(Fig.Temp1, Fig.Temp2, Fig.Temp3, Fig.Temp4, rel_widths = c(0.55, 0.48), nrow = 2)
save_plot("Figures/FigS4_temp_low_high.pdf", Fig.Temp, base_height = 8, base_width = 8)







# STEP 4: make plots for rainfall
# manuscript Figure 5
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

ggsave("Figures/Fig5_HeavyRain.pdf", width = 10, height = 4)

