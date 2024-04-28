
rm(list=ls())
gc()
library(stringr)
library(dplyr)
library(smatr)
library(ggplot2)

############   计算总体性状的相关 #############
##了解数据和函数
#### 数据路径
dir.path <- 'G:/Papers/Garden.final4/'

#################  (1) 数据  ##############
#####数据
field <- read.csv(str_c(dir.path, "Data/clean_data/TRY_climtesp_anger.csv"))
field <- rename(field, Climate=Climate100)
head(field)

###计算性状对共同截距所需的模型 ,每次更换数据集
#野外数据集
dataset <- field
#植物园数据集，不同气候阈值划分
#dataset <- data

###计算性状对共同斜率所需的模型
model1 <- alist(sma(LNC~LCC*Climate,    data=dataset, log = 'xy'),
                sma(LDMC~LCC*Climate, data=dataset, log = 'xy'),
                sma(SLA~LCC*Climate,  data=dataset, log = 'xy'),
                sma(LDMC~LNC*Climate, data=dataset, log = 'xy'),
                sma(SLA~LNC*Climate,  data=dataset, log = 'xy'),
                sma(SLA~LDMC*Climate,   data=dataset, log = 'xy'))
###计算性状对共同截距所需的模型
model2 <- alist(sma(LNC~LCC+Climate,    data=dataset, log = 'xy'),
                sma(LDMC~LCC+Climate, data=dataset, log = 'xy'),
                sma(SLA~LCC+Climate,  data=dataset, log = 'xy'),
                sma(LDMC~LNC+Climate, data=dataset, log = 'xy'),
                sma(SLA~LNC+Climate,  data=dataset, log = 'xy'),
                sma(SLA~LDMC+Climate,   data=dataset, log = 'xy'))




##################### （1）不同气候带物种变异系数和斜率 #############
#保存数据的表
trait.result <- list()

#### 开始计算性状间相关
for (i in 1: length(model1)) {
  #i=2
  ##计算相关SMA
  sma.r1 <- eval(model1[[i]])
  sma.r2 <- eval(model2[[i]]) 
  #公式
  formala.r <- as.character(sma.r1$formula)
  
  #汇总性状相关性的结果，r2，斜率和相关
  sum.r1 <- sma.r1$groupsummary
  sum.r2 <- sma.r2$groupsummary  
  #添加共斜率斜率检验和截距检验
  sum.r1$sloptelet <- sma.r1$commoncoef$p
  sum.r2$elet <-  as.vector(sma.r2$gtr$p)
  
  #添加性状对信息
  sum.r1$yt <- rep(formala.r[2], nrow(sum.r2))
  sum.r1$xt <- rep(str_extract(formala.r[3], '[A-Z]+'), nrow(sum.r1))
  sum.r1$trait <- rep(str_c(formala.r[2], '_', str_extract(formala.r[3], '[A-Z]+')), nrow(sum.r1))
  
  ###根据斜率和截距检验结果来选择数据
  trait.result[[i]] <- data.frame(sum.r1[,c(1:10,18)], comslope.t=sum.r1$slopt, comint.t=sum.r2$elet, 
                                  comslope=sum.r2$Slope, comintint=sum.r2$Int)
}


## 最终结果
sma.final <- do.call(rbind, trait.result)

#保存
write.csv(sma.final, str_c(dir.path, '/Results/8.field_trade_offs100.csv'), row.names = F)






