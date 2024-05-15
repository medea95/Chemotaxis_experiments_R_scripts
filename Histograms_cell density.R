library(tidyverse)
library(readxl)
library(openxlsx)
library(scales)

#Load your data and reassemble
#Plot one replicate of one compound and concentration:
plot1<-ggplot() +
  geom_bar(data=G_D200_R1_IN, mapping = aes(x=bin_edge_neg, y = counts_substrate),stat="identity", fill='#784374', alpha=0.8)+
  geom_bar(data=G_D200_R1_IN, mapping = aes(x=bin_edge_neg, y = counts_control),stat="identity", fill='black', alpha=0.6)+
  #geom_point(data=G_D200_R1_IN_f, mapping = aes(x=bin_edge_neg, y=fit_line), size=1.5, color="white")+ 
  geom_line(data=G_D200_R1_IN_f, mapping = aes(x=bin_edge_neg, y=fit_line), size=1, alpha=0.6,color="#F74A55")+ 
  geom_bar(data=G_D200_R1_OUT, mapping = aes(x=bin_edge_left, y = counts_substrate),stat="identity", fill='#784374', alpha=0.8)+
  geom_bar(data=G_D200_R1_OUT, mapping = aes(x=bin_edge_left, y = counts_control),stat="identity", fill='black', alpha=0.6)+
  #geom_point(data=G_D200_R1_OUT_f, mapping = aes(x=bin_edge_um, y=fit_line), size=1.5, color="white")+ 
  geom_line(data=G_D200_R1_OUT_f, mapping = aes(x=bin_edge_um, y=fit_line), size=1,  alpha=0.6, color="#F74A55")+ 
  scale_x_continuous(limits=c(-2000, 500))+
  scale_y_continuous(limits=c(0, 3))+
  geom_vline(xintercept = -315.46, size=0.7, alpha=0.8, color="#F74A55", linetype="dashed")+
  geom_vline(xintercept = 115.57, size=0.7, alpha=0.8, color="#F74A55", linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #panel.background = element_blank())
plot1



#Load your data and reassemble
#Plot all the replicates of each compound and concentration together:

plot_D<-ggplot() +
  geom_area(DMSP_all3_in, mapping = aes(x=bin_edge_neg, y = counts_control, fill=replicate), position="dodge",alpha=0.3,stat="identity",size=0.5)+
  geom_area(DMSP_all3_out, mapping = aes(x=bin_edge_um, y = counts_control, fill=replicate),position="dodge", alpha=0.3, stat="identity",size=0.5)+
  geom_smooth(DMSP_all3_in, mapping = aes(x=bin_edge_neg, y = counts_substrate, color=replicate),stat="identity", size=0.5, linetype="dashed")+
  #geom_smooth(DMSP_all3_in, mapping = aes(x=bin_edge_neg, y = counts_control, color=replicate),size=0.5, stat="identity")+
  geom_smooth(DMSP_all3_out, mapping = aes(x=bin_edge_um, y = counts_substrate, color=replicate),stat="identity", size=0.5, linetype="dashed")+
  #geom_smooth(DMSP_all3_out, mapping = aes(x=bin_edge_um, y = counts_control, color=replicate), size=0.5, stat="identity")+
  facet_grid(organism ~ concentration)+
  scale_x_continuous(limits=c(-1000, 1000))+
  scale_color_manual(values=c("#000000","#717573", "#AEB5B1"))+
  scale_fill_manual(values=c("#000000","#717573", "#AEB5B1"))+
  theme_classic()+
  theme(legend.position = 'bottom', axis.text=element_text(size=7))

plot_D
