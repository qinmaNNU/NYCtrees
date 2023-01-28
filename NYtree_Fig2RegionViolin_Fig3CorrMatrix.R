
# setwd("C:/Users/maqin/Nutstore/1/我的坚果云/NYtrees/code/gitups")
library('foreign')
library('ggplot2')
#summarize dataset by regions and attributes 

#plot tree distribution over regions
#Dall: individual tree dataset
{
#Plot tree attributes by regions
Dall = read.csv('./NYTrees_FID_Region_Att.csv')
  
Dall$reg = as.factor(Dall$reg)
ts=14
set <- theme(axis.text.x=element_text(angle=ts,hjust = 1,colour="black",
                                      family="Times",size=ts),   
             axis.text.y=element_text(family="Times",size=ts,face="plain"),  
             axis.title.y=element_text(family="Times",size = ts,face="plain"),  
             panel.border = element_blank(),
             axis.line = element_line(colour = "black",size=1),  
             legend.position="none",
             panel.grid.major = element_blank(),    
             panel.grid.minor = element_blank()) 

#Tree top height
{
p = ggplot(Dall, aes(x=reg, y=Hmax,fill=reg)) + 
  geom_violin(trim=TRUE,color="white") +  
  geom_boxplot(width=0.2,position=position_dodge(0.9))+ 
  theme_bw()+
  set+
  ylab("Tree top height,m")+xlab("")  

p

jpeg(file = "Hmax_reg.jpg",width =5,height = 4,
     units = "in",res =300)  
print(p)
dev.off()
}

#Tree mean height
{
p = ggplot(Dall, aes(x=reg, y=Hmean,fill=reg)) + 
  geom_violin(trim=TRUE,color="white") +  
  geom_boxplot(width=0.2,position=position_dodge(0.9))+ 
  theme_bw()+
  set+
  ylab("Tree mean height,m")+xlab("")  

p

jpeg(file = "Hmean_reg.jpg",width =5,height = 4,
     units = "in",res =300)  
print(p)
dev.off()
}

#Tree volume
{
  Dv = subset(Dall,Dall$Volume<=500)
  p = ggplot(Dv, aes(x=reg, y=Volume,fill=reg)) + 
    geom_violin(trim=TRUE,color="white") +  
    geom_boxplot(width=0.2,position=position_dodge(0.9))+ 
    theme_bw()+ 
    set+
    ylab("Tree volume,m3")+xlab("")  
  
  p
  
  jpeg(file = "Vol_reg.jpg",width =5,height = 4,
       units = "in",res =300)  
  print(p)
  dev.off()
}

#Tree Area
{
  Dv = subset(Dall,Dall$Area<=80)
  p = ggplot(Dv, aes(x=reg, y=Area,fill=reg)) + 
    geom_violin(trim=TRUE,color="white") +  
    geom_boxplot(width=0.2,position=position_dodge(0.9))+ 
    theme_bw()+ 
    set+
    ylab("Crown area,m2")+xlab("")  
  
  p
  
  jpeg(file = "Area_reg.jpg",width =5,height = 4,
       units = "in",res =300)  
  print(p)
  dev.off()
}
#Tree carbon
{
  Dv = subset(Dall,Dall$Carbonf<=1)
  p = ggplot(Dv, aes(x=reg, y=Carbonf,fill=reg)) + 
    geom_violin(trim=TRUE,color="white") +  
    geom_boxplot(width=0.2,position=position_dodge(0.9))+ 
    theme_bw()+ 
    set+
    ylab("Carbon storage,ton")+xlab("")  
  
  p
  
  jpeg(file = "Carbon_reg.jpg",width =5,height = 4,
       units = "in",res =300)  
  print(p)
  dev.off()
}
}

#plot The correlation matrix among five tree structure variables over the six million trees in NYC.

{

  library(corrplot)
  library(Hmisc)
  library(dplyr)
  #show correlations and p-values
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }

    #Trees
    {


    # write.csv(sg,'./Tree_Bg.csv')
    Md=D[, c("Hmean","Hmax","AREA","Vol","Carbon")]
      Md=D[, c("Area","Hmean","Hmax","Volume","Carbonf")]
      #NT:number of tree; Hm:mean tree top height, Cm:mean carbon stock in tree; 
    #Cs: total carbon stock; Am:average crown area; As: total crown area
    Mp <- rcorr(as.matrix(Md))
    Tcm = flattenCorrMatrix(Mp$r, Mp$P)
    Mdr <- cor(Md)
    corrplot.mixed(Mdr)
    # dev.off()
      
    }
 
  }
   
  
