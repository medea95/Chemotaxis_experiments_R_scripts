library(tidyverse)
library(readxl)
library(openxlsx)
library(scales)
library(tidyr)
library(gridExtra)
library(cowplot)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(ggpubr)
library(plotrix)
library(outliers)
library(nlme)
library(rstatix)

#Load data:INSIDE
Ic_all_meanIN<- read_excel("results/Review/Ic_sub_vs_ctrl/mean.xlsx", sheet=1) %>%
  select(8:13) 

Ic_all_meanIN$substrate_f = factor(Ic_all_meanIN$substrate, levels=c('DMSP','DMS','ACRY'))
Ic_all_meanIN$organism_f = factor(Ic_all_meanIN$organism, levels=c('Karlo','Oxy','Gyro'))
Ic_all_meanIN$concentration_f = factor(Ic_all_meanIN$concentration, levels=c('2','20','200'))

#Plot:
plotIC_1<-ggplot(data=Ic_all_meanIN) +
  geom_bar(aes(x = concentration_f, y = Mean),alpha=0.3, fill="black", color="black",stat='identity')+
  stat_summary(aes(x = concentration_f, y = Mean,label=round(..y..,1)), fun=mean,geom="text", size=3, vjust = -2)+
  #geom_errorbar(aes(ymin=Mean-sd_mean, ymax=Mean+sd_mean), width=0.05, alpha=1, position=position_dodge(.9), stat='identity', color="black")+
  #scale_size(limits = c(0.9, 12.5), range=c(1,10))+
  geom_point(aes(x=concentration_f, y=S_C, color=S_C==1),alpha=1, size=2,fill="black",stat='identity')+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 22), breaks=c(0,4,8,12,16))+
  facet_grid(organism_f ~ substrate_f)+
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  geom_hline(yintercept = 1, size=0.5, alpha=0.5, color="red", linetype="dashed")+
  theme(legend.position="none", panel.grid.minor = element_blank(), panel.grid.major=element_blank(), axis.title.y = element_blank(), 
        axis.title.x = element_blank())
plotIC_1

#Load data: OUTSIDE
Ic_all_meanOUT<- read_excel("results/Review/Ic_sub_vs_ctrl/mean.xlsx", sheet=3) %>%
  select(8:13) 

Ic_all_meanOUT$substrate_f = factor(Ic_all_meanOUT$substrate, levels=c('DMSP','DMS','ACRY'))
Ic_all_meanOUT$organism_f = factor(Ic_all_meanOUT$organism, levels=c('Karlo','Oxy','Gyro'))
Ic_all_meanOUT$concentration_f = factor(Ic_all_meanOUT$concentration, levels=c('2','20','200'))

#Plot:
plotIC_2<-ggplot(data=Ic_all_meanOUT) +
  geom_bar(aes(x = concentration_f, y = Mean), alpha=1, fill="white", color="black", stat='identity')+
  stat_summary(aes(x = concentration_f, y = Mean,label=round(..y..,1)), fun=mean,geom="text", size=3, vjust = -2)+
  #geom_errorbar(aes(ymin=mean_Ic_out-err_Ic_out, ymax=mean_Ic_out+err_Ic_out), width=0.05, alpha=0.7, position=position_dodge(.9), stat='identity', color="black")+
  #scale_size(limits = c(0.9, 12.5), range=c(1,10))+
  geom_point(aes(x=concentration_f, y=S_C, fill=S_C==1),shape=21, alpha=1, size=2,color="black",stat='identity')+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 22), breaks=c(0,4,8,12,16))+
  facet_grid(organism_f ~ substrate_f)+
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red")) +
  geom_hline(yintercept = 1, size=0.5, alpha=0.8, color="red", linetype="dashed")+
  theme(legend.position="none", panel.grid.minor = element_blank(), panel.grid.major=element_blank(), axis.title.y = element_blank(), 
        axis.title.x = element_blank())

plotIC_2
