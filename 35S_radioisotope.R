library(tidyverse)
library(readxl)
library(openxlsx)
library(scales)
library(repr)


#Read the raw data and subset - Oxyrrhis marina (Oxy)
raw_data_Oxy_0<- read_excel("data/raw_counts/Radio/Radio_all.xlsx", sheet=6)
Oxy_uptake_0 = subset(raw_data_Oxy_0, Analysis == "Uptake")

#Read the raw data and subset - Gyrodinium dominans (Gyro)
raw_data_Gyro_0<- read_excel("data/raw_counts/Radio/Radio_all.xlsx", sheet=7)
Gyro_uptake_0 = subset(raw_data_Gyro_0, Analysis == "Uptake")

#Read the raw data and subset - Karlodinium armiger (Karlo)
raw_data_Karlo_0<- read_excel("data/raw_counts/Radio/Radio_all.xlsx", sheet=8)
Karlo_uptake_0 = subset(raw_data_Karlo_0, Analysis == "Uptake")


#Try ggplot fitting:Michaelis - Menten
plot_M<-ggplot() +  
  geom_point(data=Oxy_uptake_0, aes(x=Incubation_h,y=Mean_AB), color="#006B8C", size=5, alpha=0.7) +   
  geom_errorbar(data=Oxy_uptake_0, mapping=aes(y = Mean_AB , x=Incubation_h, ymin=Mean_AB-SD_AB, ymax=Mean_AB+SD_AB), color="#006B8C",width=.05)+
  geom_smooth(data=Oxy_uptake_0, aes(x=Incubation_h,y=Mean_AB), size=1, color="#006B8C",method = "nls", 
              formula = y ~ Vmax * x / (Km + x), 
              start = list(Vmax = 0.0012, Km = 6),
              se = F, size = 0.5)+
  geom_point(data=Karlo_uptake_0, aes(x=Incubation_h,y=Mean_AB), color="#238F29",size=5, alpha=0.7) + 
  geom_errorbar(data=Karlo_uptake_0, mapping=aes(y = Mean_AB , x=Incubation_h, ymin=Mean_AB-SD_AB, ymax=Mean_AB+SD_AB), color="#238F29",width=.05)+
  geom_smooth(data=Karlo_uptake_0, aes(x=Incubation_h,y=Mean_AB), size=1, color="#238F29", method = "nls", 
              formula = y ~ Vmax * x / (Km + x), 
              start = list(Vmax = 0.0012, Km = 6),
              se = F, size = 0.5)+
  geom_point(data=Gyro_uptake_0, aes(x=Incubation_h,y=Mean_AB), color="#784374", size=5, alpha=0.7) +    
  geom_errorbar(data=Gyro_uptake_0, mapping=aes(y = Mean_AB , x=Incubation_h, ymin=Mean_AB-SD_AB, ymax=Mean_AB+SD_AB), color="#784374",width=.05)+
  geom_smooth(data=Gyro_uptake_0, aes(x=Incubation_h,y=Mean_AB), method="lm", se=FALSE, color="#784374")+
  labs(y = "DPM/μm3",
       x = "Time(hr)")+
  theme(
    #panel.grid.major = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())

plot_M

