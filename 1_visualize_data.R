###################################################################################################
##  The code is adapted from Lowe, et al., Lancet Planetary Health https://github.com/drrachellowe/hydromet_dengue
# 
## Written by Qu Cheng
## Last updated: March 13th, 2023
###################################################################################################

# R script to plot dengue and climate data in Guangzhou
source("0_packages_data_functions.R")




# Manuscript Fig 2
Fig2A <- GZ.daily %>%
  group_by(Month) %>%
  dplyr::summarize(Mean = mean(Tave), Maximum = mean(Tmax), Minimum = mean(Tmin)) %>%
  gather("TempType", "Temp", Mean:Minimum) %>%
  ggplot(aes(x = Month, y = Temp, col = TempType)) +
  geom_line(size = 1) +
  scale_color_aaas(labels = c("Max", "Mean", "Min")) +
  theme_cowplot() +
  scale_x_continuous(labels=month.abb, breaks = 1:12) +
  ylab("Temperature (°C)") +
  xlab("") +
  theme(legend.position = c(0.05, 0.9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(col = "")

Fig2B <- GZ.daily %>%
  group_by(Year, Month) %>%
  dplyr::summarize(Rain = sum(Precipitation)) %>%
  group_by(Month) %>%
  dplyr::summarize(Rain = mean(Rain)) %>%
  ggplot(aes(x = Month)) +
  geom_bar(aes(y = Rain), stat  = "identity", fill = "lightblue") +
  theme_cowplot() +
  scale_x_continuous(labels=month.abb, breaks = 1:12) +
  xlab("") +
  scale_y_continuous(name = "Avg. cum. rainfall (mm)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

Fig2C <- GZ.daily %>%
  group_by(Year, Month) %>%
  dplyr::summarize(CaseCount = sum(CaseCount)) %>%
  group_by(Year) %>%
  mutate(CaseCountTotal = sum(CaseCount)) %>%
  mutate(Prop = CaseCount/CaseCountTotal) %>%
  group_by(Month) %>%
  dplyr::summarize(Prop = mean(Prop, na.rm = TRUE))%>%
  ggplot(aes(x = Month, y = Prop*100, shape = Month %in% 7:12, col = Month %in% 7:12, group = 1)) +
  geom_line(col = "black") +
  geom_point(size = 2) +
  theme_cowplot() +
  scale_x_continuous(labels=month.abb, breaks = 1:12) +
  ylab("Cases in each month (%)") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_shape_manual(values = c(4, 15)) +
  scale_color_aaas() +
  guides(shape = "none", col = "none")


Fig2D <- GZ.daily %>%
  mutate(Year = year(OnsetDate)) %>%
  group_by(Year) %>%
  dplyr::summarize(CaseCount = sum(CaseCount)) %>%
  ggplot(aes(x = Year, y = CaseCount, shape = CaseCount >= 100, col = CaseCount >= 100, group = 1)) +
  geom_line(col = "black") + 
  geom_point(size = 2) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  labs(x = "", y = "Number of cases") +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(2006, 2018, 2)) +
  geom_hline(yintercept = 100, col = "gray", linetype = "dashed") +
  scale_shape_manual(values = c(4, 15)) +
  scale_color_aaas() +
  guides(shape = "none", col = "none")




Fig2.row1 <- plot_grid(Fig2A, Fig2B, labels = "AUTO", nrow = 1, rel_widths = c(0.5, 0.5), label_size = 16)
Fig2.row2 <- plot_grid(Fig2C, Fig2D, labels = c("C", "D"), nrow = 1, rel_widths = c(0.5, 0.5), label_size = 16)
Fig2 <- plot_grid(Fig2.row1, Fig2.row2, nrow = 2)
# save_plot("Fig2.pdf", Fig2, base_height = 7, base_width = 8)










# Manuscript Fig 3
Fig3A <- GZ.daily%>%
  filter(Month %in% 7:12) %>%
  filter(Year %in% c(2006, 2012, 2013, 2014, 2016, 2017, 2018))%>%
  ggplot(aes(x = OnsetDate, y = CaseCount)) +
  geom_rect(xmin = ymd("2012-07-01"),xmax = ymd("2012-12-31"),
            ymin = -Inf,ymax = Inf, fill = "gray90") +
  geom_rect(xmin = ymd("2014-07-01"),xmax = ymd("2014-12-31"),
            ymin = -Inf,ymax = Inf, fill = "gray90") +
  geom_rect(xmin = ymd("2017-07-01"),xmax = ymd("2017-12-31"),
            ymin = -Inf,ymax = Inf, fill = "gray90") +
  geom_point(size = 0.8) + 
  scale_y_sqrt(breaks = c(100, 500, 1000, 1500)) +
  xlab("") +
  ylab("Number of reported cases") +
  theme_cowplot() +
  facet_wrap(~Year, scales = "free_x", nrow = 1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold")) 


Fig3B <- GZ.daily %>%
  # filter(Month %in% 7:12) %>%
  filter(Year %in% c(2006, 2012, 2013, 2014, 2016, 2017, 2018))%>%
  ggplot(aes(x = OnsetDate, y = Tmax, group = Year))+
  geom_rect(xmin = ymd("2012-01-01"),xmax = ymd("2012-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gray90") +
  geom_rect(xmin = ymd("2014-01-20"),xmax = ymd("2014-12-31"),
            ymin = -Inf,ymax = Inf, fill = "gray90") +
  geom_rect(xmin = ymd("2017-01-20"),xmax = ymd("2017-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gray90") +
  geom_rect(xmin = ymd("2006-07-01"),xmax = ymd("2006-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2012-07-01"),xmax = ymd("2012-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2013-07-01"),xmax = ymd("2013-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2014-07-01"),xmax = ymd("2014-12-31"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2016-07-01"),xmax = ymd("2016-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2017-07-01"),xmax = ymd("2017-12-10"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2018-07-01"),xmax = ymd("2018-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_point(size = 0.8) +
  theme_cowplot() +
  scale_color_manual(values = c("NA", "blue")) +
  labs(x = "", y = "Daily max temperature (°C)") +
  guides(col = "none")+
  facet_wrap(~Year, scales = "free_x", nrow = 1)+
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold")) 


rainfall.threshold <- 51

GZ.dat <- GZ.daily %>%
  mutate(HeavyRain = Precipitation > rainfall.threshold
  ) 
  

Cum.Rain.Thre <- quantile(GZ.dat$P8wk.noself, c(0.1, 0.5, 0.9), na.rm = TRUE)


Fig3C <- GZ.dat %>%
  # filter(Month %in% 7:12) %>%
  filter(Year %in% c(2006, 2012, 2013, 2014, 2016, 2017, 2018))%>%
  ggplot(aes(x = OnsetDate, y = P8wk.noself, group = Year))+
  geom_rect(xmin = ymd("2012-01-01"),xmax = ymd("2012-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gray90") +
  geom_rect(xmin = ymd("2014-01-20"),xmax = ymd("2014-12-31"),
            ymin = -Inf,ymax = Inf, fill = "gray90") +
  geom_rect(xmin = ymd("2017-01-20"),xmax = ymd("2017-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gray90") +
  geom_rect(xmin = ymd("2006-07-01"),xmax = ymd("2006-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2012-07-01"),xmax = ymd("2012-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2013-07-01"),xmax = ymd("2013-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2014-07-01"),xmax = ymd("2014-12-31"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2016-07-01"),xmax = ymd("2016-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2017-07-01"),xmax = ymd("2017-12-10"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2018-07-01"),xmax = ymd("2018-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_line() +
  geom_point(aes(col = HeavyRain)) +
  # geom_hline(yintercept = Cum.Rain.Thre, col = "gray", linetype = "dashed") +
  theme_cowplot() +
  scale_color_manual(values = c("NA", "blue")) +
  labs(x = "", y = "8-wk cumulative prec. (mm)") +
  guides(col = "none")+
  facet_wrap(~Year, scales = "free_x", nrow = 1)+
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold")) 

Fig3 <- plot_grid(Fig3A, Fig3B, Fig3C, labels = c("A", "B", "C"), nrow = 3, label_size = 16)
#   save_plot("Figures/Fig3.pdf", Fig3, base_height = 10.5, base_width = 8)








# Supplementary Fig S1
FigS1A <- GZ.daily%>%
  ggplot(aes(x = OnsetDate, y = CaseCount)) +
  geom_point(size = 0.8) + 
  scale_y_sqrt(breaks = c(100, 500, 1000, 1500)) +
  xlab("") +
  ylab("Number of reported cases") +
  theme_cowplot() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold")) +
  geom_vline(xintercept = seq(as.Date("2007-01-01"), as.Date("2018-01-01"), "1 year"), col = "gray80") 

FigS1B <- GZ.dat %>%
  ggplot(aes(x = OnsetDate, y = Tmax, group = Year))+
  geom_rect(xmin = ymd("2006-07-01"),xmax = ymd("2006-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2012-07-01"),xmax = ymd("2012-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2013-07-01"),xmax = ymd("2013-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2014-07-01"),xmax = ymd("2014-12-31"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2016-07-01"),xmax = ymd("2016-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2017-07-01"),xmax = ymd("2017-12-10"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2018-07-01"),xmax = ymd("2018-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_point(size = 0.8) +
  theme_cowplot() +
  scale_color_manual(values = c("NA", "blue")) +
  labs(x = "", y = "Daily max temperature (°C)") +
  guides(col = "none")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold")) +
  geom_vline(xintercept = seq(as.Date("2007-01-01"), as.Date("2018-01-01"), "1 year"), col = "gray80") 


GZ.dat <- GZ.daily %>%
  mutate(HeavyRain = Precipitation > 51
  )

FigS1C <- GZ.dat %>%
  ggplot(aes(x = OnsetDate, y = P8wk, group = Year))+
  geom_rect(xmin = ymd("2006-07-01"),xmax = ymd("2006-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2012-07-01"),xmax = ymd("2012-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2013-07-01"),xmax = ymd("2013-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2014-07-01"),xmax = ymd("2014-12-31"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2016-07-01"),xmax = ymd("2016-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2017-07-01"),xmax = ymd("2017-12-10"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_rect(xmin = ymd("2018-07-01"),xmax = ymd("2018-12-13"),
            ymin = -Inf,ymax = Inf, fill = "gold2") +
  geom_line() +
  geom_point(aes(col = HeavyRain)) +
  theme_cowplot() +
  scale_color_manual(values = c("NA", "blue")) +
  labs(x = "", y = "8-wk cumulative prec. (mm)") +
  guides(col = "none")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold")) +
  geom_vline(xintercept = seq(as.Date("2007-01-01"), as.Date("2018-01-01"), "1 year"), col = "gray80") 




FigS1 <- plot_grid(FigS1A, FigS1B, FigS1C, labels = c("A", "B", "C"), nrow = 3, label_size = 16)
# save_plot("Figures/FigS1 descriptive results all years.pdf", FigS1, base_height = 11.5, base_width = 8)







# Supplementary Figure S2--- heavy rain
FigS2A <- GZ.daily %>%
  mutate(HeavyRain = Precipitation > 51) %>%
  group_by(Year) %>%
  dplyr::summarize(HeavyRain = sum(HeavyRain), CaseCount = sum(CaseCount)) %>%
  ggplot(aes(x = Year, y = HeavyRain, shape = CaseCount >= 100, col = CaseCount >= 100, group = 1))+
  geom_line(col = "black") + 
  geom_point(size = 2) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  labs(x = "", y = "No. of heavy rainfall events") +
  scale_x_continuous(breaks = seq(2006, 2018, 2)) +
  scale_y_continuous(breaks = seq(2, 14, 2)) +
  scale_shape_manual(values = c(4, 15)) +
  scale_color_aaas() +
  guides(shape = "none", col = "none")



dat1 <- GZ.daily %>%
  mutate(HeavyRain = Precipitation > 51) %>%
  group_by(Month) %>%
  dplyr::summarize(HeavyRain = sum(HeavyRain)/13) %>%
  mutate(type = "All years")

dat2 <- GZ.daily %>%
  filter(Year %in% c(2006, 2012, 2013, 2014, 2016, 2017, 2018)) %>%
  mutate(HeavyRain = Precipitation > 51) %>%
  group_by(Month) %>%
  dplyr::summarize(HeavyRain = sum(HeavyRain)/7) %>%
  mutate(type = "Included")


dat <- rbind(dat1, dat2) %>%
  mutate(type = factor(type, levels = c("Included", "All years")))

FigS2B <- ggplot()+
  geom_line(data = dat, aes(x = Month, y = HeavyRain, linetype = type)) +
  geom_point(data = dat, aes(x = Month, y = HeavyRain, shape = Month %in% 7:12, col = Month %in% 7:12), size = 2) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = c(0.68, 0.95)) +
  labs(x = "", y = "Mean no. of heavy rainfall events") +
  scale_x_continuous(labels=month.abb, breaks = 1:12) +
  scale_shape_manual(values = c(4, 15)) +
  scale_color_aaas() +
  guides(shape = "none", col = "none") +
  labs(lty = "")

# save_plot("Figures/FigS2 mean hr events by year and month.pdf", plot_grid(FigS2A, FigS2B, labels = c("A", "B")), base_width = 8, base_height = 4)








# Supplementary Figure S3
FigS3A <- GZ.daily %>%
  group_by(Year) %>%
  dplyr::summarize(P8wk.noself = mean(P8wk.noself, na.rm = TRUE), CaseCount = sum(CaseCount)) %>%
  ggplot(aes(x = Year, y = P8wk.noself, shape = CaseCount >= 100, col = CaseCount >= 100, group = 1))+
  geom_line(col = "black") + 
  geom_point(size = 2) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  labs(x = "", y = "Mean 8-wk cum. rainfall (mm)") +
  scale_x_continuous(breaks = seq(2006, 2018, 2)) +
  scale_shape_manual(values = c(4, 15)) +
  scale_color_aaas() +
  guides(shape = "none", col = "none")


  
dat1 <- GZ.daily %>%
  group_by(Month) %>%
  dplyr::summarize(P8wk.noself = mean(P8wk.noself, na.rm = TRUE)) %>%
  mutate(type = "All years")

dat2 <- GZ.daily %>%
  filter(Year %in% c(2006, 2012, 2013, 2014, 2016, 2017, 2018)) %>%
  group_by(Month) %>%
  dplyr::summarize(P8wk.noself = mean(P8wk.noself, na.rm = TRUE)) %>%
  mutate(type = "Included")

dat <- rbind(dat1, dat2) %>%
  mutate(type = factor(type, levels = c("Included", "All years")))

FigS3B <- ggplot()+
  geom_line(data = dat, aes(x = Month, y = P8wk.noself, linetype = type)) +
  geom_point(data = dat, aes(x = Month, y = P8wk.noself, shape = Month %in% 7:12, col = Month %in% 7:12), size = 2) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = c(0.68, 0.95)) +
  labs(x = "", y = "Mean 8-wk cum. rainfall (mm)") +
  scale_x_continuous(labels=month.abb, breaks = 1:12) +
  scale_shape_manual(values = c(4, 15)) +
  scale_color_aaas() +
  guides(shape = "none", col = "none") +
  labs(lty = "")

save_plot("Figures/FigS3 mean 8wk cum rain by year and month.pdf", plot_grid(FigS3A, FigS3B, labels = c("A", "B")), base_width = 8, base_height = 4)








