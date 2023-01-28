#regression for NY biomass
library(ggplot2)
library("gridExtra")
library(ggpubr)
library(plotly)
library(Metrics)
library(corrplot)
library(Hmisc)
library(hydrogeo)
library(Metrics)

# setwd("D:/Box Sync/Box Sync/UrbanHeatEcostress/Rcode")
# setwd("G:/QinPCbackup_2022_3_26/New Volume/Box Sync/UrbanHeatEcostress/Rcode")
setwd("C:/Users/maqin/Nutstore/1/我的坚果云/NYtrees/code")
library('foreign')

# Do = Dall[,c(10,2,3,4:7,13:15)]
# write.csv(Do,'./NYTrees_FID_Region_Att.csv')
Do = read.csv('./NYTrees_FID_Region_Att.csv')
D2 = read.csv('./Carbon_CHM_CC_model_D278.csv')
fitm =lm(CarbonDensity~ 0+ CHMmean,data = D2 )

Du = Do[,c(1:7)]
Du$CHMmean = Du$Hmean
predict_lm = predict(fitm, newdata = Du, interval = 'confidence')
CD = as.data.frame(predict_lm)
Du$CDf = CD$fit
Du$Carbonf = Du$CDf/10000*Du$Area
Du$CDl = CD$lwr
Du$Carbonlwr = Du$CDl/10000*Du$Area
Du$CDu = CD$upr
Du$Carbonupr = Du$CDu/10000*Du$Area
Dall = Du[,c(2:8,10,12,14)]

SumCarb = read.csv('./SumCarbUncertainty.csv')
SumCarb[1,c(2:8)]=c(min(Dall$Carbonf),max(Dall$Carbonf),mean(Dall$Carbonf),
                    sd(Dall$Carbonf),quantile(Dall$Carbonf,0.25),
                    quantile(Dall$Carbonf,0.5),quantile(Dall$Carbonf,0.75))
SumCarb[2,c(2:8)]=c(min(Dall$Carbonlwr),max(Dall$Carbonlwr),mean(Dall$Carbonlwr),
                    sd(Dall$Carbonlwr),quantile(Dall$Carbonlwr,0.25),
                    quantile(Dall$Carbonlwr,0.5),quantile(Dall$Carbonlwr,0.75))
SumCarb[3,c(2:8)]=c(min(Dall$Carbonupr),max(Dall$Carbonupr),mean(Dall$Carbonupr),
                    sd(Dall$Carbonupr),quantile(Dall$Carbonupr,0.25),
                    quantile(Dall$Carbonupr,0.5),quantile(Dall$Carbonupr,0.75))

write.csv(SumCarb,'./CarbonUncertainty.csv')


ts = 14
p= ggplot(D2, aes(x=CHMmean, y=CarbonDensity)) +
  geom_point(size=3, color='grey20')+
  stat_smooth(method = 'lm',formula = y ~0+ x,se= TRUE,level = 0.95, color='blue')+
  theme_bw()+theme( legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_text(size=ts),axis.title.y=element_text(size=ts), 
        axis.text.x=element_text(size=ts), axis.text.y=element_text(size=ts))+
  scale_x_continuous('LiDAR CHMmean,m')+
  scale_y_continuous('Carbon Density,ton/ha',
                     # breaks = c(0, 20,40,60,80),
                     limits=c(0,150))
p

 ggsave(plot = p, width = 6, height = 6, dpi = 600, 
        filename = "Carbon_CHMm_Se_D278.jpg")


# Basic scatter plot
{
  ly1 = 165
  ly2 = 155
  ts =14
  ls = 6
  
 pcc = ggplot(D2, aes(x=TCC, y=CarbonDensity)) + geom_point(size=3)+
   stat_smooth(method = 'lm',formula = y ~0+ x,se= FALSE,color='black')+
   
   stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
   geom_smooth(method = "nls", 
               formula = y ~  k2 *exp(k1 * x)-1,
               se = FALSE,color='blue',
               method.args = list(start = list(k1 = 0.01, k2 = 1)))+
  theme_bw()+theme( legend.position="none")+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title.x=element_text(size=ts),axis.title.y=element_text(size=ts), 
    axis.text.x=element_text(size=ts), axis.text.y=element_text(size=ts))+
  scale_x_continuous('Canopy Cover,%')+
  scale_y_continuous('Carbon Density,mg/ha',
                     limits=c(0,150))

pcc

 

pCmax = ggplot(D2, aes(x=CHMmax, y=CarbonDensity)) +geom_point(size=3)+
  stat_smooth(method = 'lm',formula = y ~0+ x,se= FALSE,color='black')+
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_smooth(method = "nls", 
              formula = y ~  k2 *exp(k1 * x)-1,
              se = FALSE,color='blue',
              method.args = list(start = list(k1 = 0.01, k2 = 1)))+

  theme_bw()+theme( legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title.x=element_text(size=ts),axis.title.y=element_text(size=ts), 
    axis.text.x=element_text(size=ts), axis.text.y=element_text(size=ts))+
  scale_x_continuous('CHMmax,m')+
  scale_y_continuous('Carbon Density,mg/ha',
                     # breaks = c(0, 20,40,60,80),
                     limits=c(0,150))

pCmax

pCmean = ggplot(D2, aes(x=CHMmean, y=CarbonDensity)) +geom_point(size=3)+
  stat_smooth(method = 'lm',formula = y ~0+ x,se= FALSE,color='black')+
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_smooth(method = "nls", 
              formula = y ~  k2 *exp(k1 * x)-1,
              se = FALSE,color='blue',
              method.args = list(start = list(k1 = 0.4, k2 = 1)))+
  theme_bw()+theme( legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title.x=element_text(size=ts),axis.title.y=element_text(size=ts), 
    axis.text.x=element_text(size=ts), axis.text.y=element_text(size=ts))+
  scale_x_continuous('CHMmean,m')+
  scale_y_continuous('Carbon Density,mg/ha',
                     # breaks = c(0, 20,40,60,80),
                     limits=c(0,150))

pCmean



PNmean = ggplot(D2, aes(x= NDVImean, y=CarbonDensity)) +geom_point(size=3)+
  stat_smooth(method = 'lm',formula = y ~0+ x,se= FALSE,color='black')+
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_smooth(method = "nls", 
              formula = y ~  k2 *exp(k1 * x)-1,
              se = FALSE,color='blue',
              method.args = list(start = list(k1 = 0.01, k2 = 1)))+
  theme_bw()+theme( legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title.x=element_text(size=ts),axis.title.y=element_text(size=ts), 
    axis.text.x=element_text(size=ts), axis.text.y=element_text(size=ts))+
  scale_x_continuous('NDVImean')+
  scale_y_continuous('Carbon Density,mg/ha',
                     # breaks = c(0, 20,40,60,80),
                     limits=c(0,150))
PNmean

PNmax = ggplot(D2, aes(x= NDVImax, y=CarbonDensity)) + 
 geom_point(size=3)+
  stat_smooth(method = 'lm',formula = y ~0+ x,se= FALSE,color='black')+
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_smooth(method = "nls", 
              formula = y ~  k2 *exp(k1 * x)-1,
              se = FALSE,color='blue',
              method.args = list(start = list(k1 = 0.01, k2 = 1)))+
  theme_bw()+theme( legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title.x=element_text(size=ts),axis.title.y=element_text(size=ts), 
    axis.text.x=element_text(size=ts), axis.text.y=element_text(size=ts))+
  scale_x_continuous('NDVImax')+
  scale_y_continuous('Carbon Density,mg/ha',
                     # breaks = c(0, 20,40,60,80),
                     limits=c(0,150))
PNmax


p=grid.arrange(pCmax,pCmean,pcc,PNmean,ncol=2, nrow=2)

ggsave(plot = p, width = 10, height = 10, dpi = 600, 
       filename = "Carbon_CHMs_CC_NDVI_D278.jpg")

}

 
