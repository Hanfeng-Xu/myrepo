
library(stringr)
library(dplyr)
library(smatr)
library(ggplot2)

rm(list=ls())
gc()

############################################
###########  1. 数据准备  #############
#### 数据路径
dir.path <- 'G:/Papers/Garden.final4/'


#################  (1) 数据  ##############
##植物园
garden <- read.csv(str_c(dir.path, 'Data/clean_data/garden_data.csv'))  %>% na.omit() %>% dplyr::select(Species:SLA, Type70:Type90)
head(garden)

#每次更换阈值
garden <- dplyr::select(garden, Species:SLA, Type=Type100)
table(garden$Type)

#本地与外来数据
native_d <- subset(garden, Type=='native')

###########  2. 计算本地物种的性状对权衡关系  #############
###计算性状对共同截距所需的模型
model1 <- alist(sma(LNC~LCC,    data=native_d , log = 'xy'),
                sma(LDMC~LCC, data=native_d , log = 'xy'),
                sma(SLA~LCC,  data=native_d , log = 'xy'),
                sma(LDMC~LNC, data=native_d , log = 'xy'),
                sma(SLA~LNC,  data=native_d , log = 'xy'),
                sma(SLA~LDMC,   data=native_d, log = 'xy'))


#保存数据的表
trait.result <- list()

#### 开始计算性状间相关
for (i in 1: length(model1)) {
  #  i=4
  ##计算相关SMA
  sma.r1 <- eval(model1[[i]])
  
  #公式
  formala.r <- as.character(sma.r1$formula)
  
  #汇总性状相关性的结果，r2，斜率和相关
  sum.r1 <- sma.r1$groupsummary
  
  
  #添加性状对信息
  sum.r1$yt <- rep(formala.r[2], nrow(sum.r1))
  sum.r1$xt <- rep(formala.r[3], nrow(sum.r1))
  sum.r1$trait <- rep(str_c(formala.r[2], '_', formala.r[3]), nrow(sum.r1))
  
  #保存结果
  trait.result[[i]] <- data.frame(sum.r1[,c(1:10, 15:17)])
}

## 最终结果
native.final <- do.call(rbind, trait.result)

#保存
write.csv(native.final, str_c(dir.path, 'Results/4.native_trade.csv'), row.names = F)


###########  3. 计算外来物种与本地物种的性状对权衡关系  #############
###计算性状对共同截距所需的模型
model2 <- alist(sma(LNC~LCC*Type,    data=ne.data ,log = 'xy'),
                sma(LDMC~LCC*Type, data=ne.data , log = 'xy'),
                sma(SLA~LCC*Type,  data=ne.data , log = 'xy'),
                sma(LDMC~LNC*Type, data=ne.data , log = 'xy'),
                sma(SLA~LNC*Type,  data=ne.data , log = 'xy'),
                sma(SLA~LDMC*Type, data=ne.data , log = 'xy'))
###计算性状对共同斜率所需的模型
model3 <- alist(sma(LNC~LCC+Type,    data=ne.data , log = 'xy'),
                sma(LDMC~LCC+Type, data=ne.data , log = 'xy'),
                sma(SLA~LCC+Type,  data=ne.data , log = 'xy'),
                sma(LDMC~LNC+Type, data=ne.data , log = 'xy'),
                sma(SLA~LNC+Type,  data=ne.data , log = 'xy'),
                sma(SLA~LDMC+Type,   data=ne.data, log = 'xy'))

#保存数据的表
trait.result <- list()


 #### 开始计算性状间相关
for (climate in c("Tropical", "Temperate", "Widely-distributed")) {
  #climate='Tropical'
  ne.data <- subset(garden, Type=='native'|Type==climate)
  
  #中途保存结果
  temp.result <- list()
  
  #循环计算不同性状对权衡
  for (i in 1: length(model2)){
    #i=4
    ##计算相关SMA
    sma.r1 <- eval(model2[[i]])
    sma.r2 <- eval(model3[[i]]) 
    #公式
    formala.r <- as.character(sma.r1$formula)
    
    #汇总性状相关性的结果，r2，斜率和相关
    sum.r1 <- sma.r1$groupsummary
    sum.r2 <- sma.r2$groupsummary  
    #添加共斜率斜率检验和截距检验
    sum.r1$slopt <- sma.r1$commoncoef$p
    sum.r2$elet <-  as.vector(sma.r2$gtr$p)
    
    #添加性状对信息
    sum.r1$yt <- rep(formala.r[2], nrow(sum.r1))
    sum.r1$xt <- rep(str_extract(formala.r[3], '[A-Z]+'), nrow(sum.r1))
    sum.r1$trait <- rep(str_c(formala.r[2], '_', str_extract(formala.r[3], '[A-Z]+'), climate), nrow(sum.r1))
    
    ##去掉本地物种
    #sum.r1 <- subset(sum.r1, !group=='native')
    
    #保存结果
    temp.result[[i]] <- data.frame(sum.r1[,c(1:10, 16:18)], comslope.t=sum.r1$slopt, comint.t=unique(sum.r2$elet),
                                   comslope=sum.r2$Slope, comintint=sum.r2$Int)
  } #性状对循环
  
  trait.result[[climate]] <- do.call(rbind, temp.result)
  
}#气候带循环


## 最终结果
final.r <- do.call(rbind, trait.result)

#保存
write.csv(final.r, str_c(dir.path, 'Results/4.garden_exotic_trade100.csv'), row.names = F)



