library(dplyr)
library(foreign)
library(Metrics)
# setwd("C:/Users/maqin/Nutstore/1/我的坚果云/NYtrees")

ss= read.csv('./Plot_TreeSeg_S.csv')



err <- matrix(0,nrow=3,ncol=6)
err = as.data.frame(err)
colnames(err) = c('V','R2','RMSE','bia','MAE','Rbia')
err[,'V']=c('NT','Hmn','Hmax')

err[1,'R2']=round((cor(ss$NF,ss$NL))^2,2)
err[2,'R2']=round((cor(ss$HFmn,ss$HLmn))^2,2)
err[3,'R2']=round((cor(ss$HFmax,ss$HLmax))^2,2)

err[1,'RMSE']=round(rmse(ss$NF,ss$NL),2)
err[2,'RMSE']=round(rmse(ss$HFmn,ss$HLmn),2)
err[3,'RMSE']=round(rmse(ss$HFmax,ss$HLmax),2)

err[1,'bia']=round(mean(ss$NL)-mean(ss$NF),2)
err[2,'bia']=round(mean(ss$HLmn)-mean(ss$HFmn),2)
err[3,'bia']=round(mean(ss$HLmax)-mean(ss$HFmax),2)

err[1,'Rbia']=round( (mean(ss$NL)-mean(ss$NF))/mean(ss$NF)*100,2)
err[2,'Rbia']=round( (mean(ss$HLmn)-mean(ss$HFmn))/mean(ss$HFmn)*100,2)
err[3,'Rbia']=round( (mean(ss$HLmax)-mean(ss$HFmax))/mean(ss$HFmax)*100,2)

err[1,'MAE']= round(mean(abs(ss$NL-ss$NF)),2)
err[2,'MAE']=round(mean(abs(ss$HFmn-ss$HLmn)),2)
err[3,'MAE']=round(mean(abs(ss$HFmax-ss$HLmax)),2)
write.csv(err,'./err.csv')


library(ggplot2)
library("gridExtra")
# Basic scatter plot
pN = ggplot(ss, aes(x=ss$NL, y=ss$NF)) + geom_point(size=1.5)+
  # stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= TRUE)+
  theme_bw()+theme( legend.position="none")+
  scale_x_continuous('LiDAR NT', limits=c(0,40))+
  scale_y_continuous('Field NT', limits=c(0,40))+
  geom_abline(intercept = 0, slope = 1, color="grey",linetype="dashed", size=0.5)

pN
ggsave(plot = pN, width = 4, height = 4, dpi = 600,
       filename = "./NT.jpg")

pHm = ggplot(ss, aes(x=ss$HLmn, y=ss$HFmn)) + geom_point(size=1.5)+
  # stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= TRUE)+
  theme_bw()+theme( legend.position="none")+
  scale_x_continuous('LiDAR mean height,m', limits=c(0,25))+
  scale_y_continuous('Field mean height,m', limits=c(0,25))+
  geom_abline(intercept = 0, slope = 1, color="grey",linetype="dashed", size=0.5)

pHm
ggsave(plot = pHm, width = 4, height = 4, dpi = 600,
       filename = "./Hmn.jpg")


pHmax = ggplot(ss, aes(x=ss$HLmax, y=ss$HFmax)) + geom_point(size=1.5)+
  # stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= TRUE)+
  theme_bw()+theme( legend.position="none")+
  scale_x_continuous('LiDAR max height,m', limits=c(0,30))+
  scale_y_continuous('Field max height,m', limits=c(0,30))+
  geom_abline(intercept = 0, slope = 1, color="grey",linetype="dashed", size=0.5)

pHmax
ggsave(plot = pHmax, width = 4, height = 4, dpi = 600,
       filename = "./Hmax.jpg")






























