library(tidyverse)
library(readxl)
library(openxlsx)
library(scales)

#Load your data:
#K_DMSP, G_DMSP, O_DMSP
#K_DMS, G_DMS, O_DMS
#K_ACRY, G_ACRY, O_ACRY

#Bring together the substrates:
DMSP_all3<-rbind(K_DMSP, G_DMSP, O_DMSP)
DMS_all3<-rbind(K_DMS, G_DMS, O_DMS)
ACRY_all3<-rbind(K_ACRY, G_ACRY, O_ACRY)

#Plot
p_DMSP<-ggplot(data=DMSP_all3)+
  geom_area(mapping = aes(x=time_s, y = Total_control, fill=replicate), position="dodge",alpha=0.3,stat="identity",size=0.5)+
  geom_smooth(mapping = aes(x=time_s, y = Total_substrate, color=replicate),stat="identity", size=0.5, linetype="dashed")+
  facet_grid(organism ~ concentration)+
  scale_x_continuous(limits=c(0, 600))+
  scale_y_continuous(limits=c(0, 35))+
  scale_color_manual(values=c("#000000","#717573", "#AEB5B1"))+
  scale_fill_manual(values=c("#000000","#717573", "#AEB5B1"))+
  theme_classic()+
  theme(legend.position = 'bottom', axis.text=element_text(size=7))

p_DMS<-ggplot(data=DMS_all3)+
  geom_area(mapping = aes(x=time_s, y = Total_control, fill=replicate), position="dodge",alpha=0.3,stat="identity",size=0.5)+
  geom_smooth(mapping = aes(x=time_s, y = Total_substrate, color=replicate),stat="identity", size=0.5, linetype="dashed")+
  facet_grid(organism ~ concentration)+
  scale_x_continuous(limits=c(0, 600))+
  scale_y_continuous(limits=c(0, 35))+
  scale_color_manual(values=c("#000000","#717573", "#AEB5B1"))+
  scale_fill_manual(values=c("#000000","#717573", "#AEB5B1"))+
  theme_classic()+
  theme(legend.position = 'bottom', axis.text=element_text(size=7))

p_DMS

p_ACRY<-ggplot(data=ACRY_all3)+
  geom_area(mapping = aes(x=time_s, y = Total_control, fill=replicate), position="dodge",alpha=0.3,stat="identity",size=0.5)+
  geom_smooth(mapping = aes(x=time_s, y = Total_substrate, color=replicate),stat="identity", size=0.5, linetype="dashed")+
  facet_grid(organism ~ concentration)+
  scale_x_continuous(limits=c(0, 600))+
  scale_y_continuous(limits=c(0, 35))+
  scale_color_manual(values=c("#000000","#717573", "#AEB5B1"))+
  scale_fill_manual(values=c("#000000","#717573", "#AEB5B1"))+
  theme_classic()+
  theme(legend.position = 'bottom', axis.text=element_text(size=7))

p_ACRY
