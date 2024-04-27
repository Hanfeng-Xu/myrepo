
library(stringr)
library(dplyr)
library(smatr)
library(ggplot2)

rm(list=ls())
gc()

############################################
###########  1. 数据准备  #############
#### 数据路径
dir.path <- 'G:/Papers/Garden.final2/'


#################  (1) 数据  ##############
##植物园，每次需要修改
garden <- read.csv(str_c(dir.path, 'Data/clean_data/garden_data.csv'))  %>% na.omit() %>% 
          dplyr::select(Species, LCC, LNC, LDMC, SLA, Climate70, Climate80, Climate90, 
                        Climate100, Type70, Type80, Type90, Type100)
head(garden)

##野外，每次需要修改
field <- read.csv(str_c(dir.path, 'Data/clean_data/TRY_climtesp_diaz_anger.csv'))
head(field)

#本地物种
native <- garden %>% dplyr::select(Species, LCC, LNC, LDMC, SLA, Climate70, Climate80, Climate90, 
                                   Climate100, Type70, Type80, Type90, Type100) %>% filter(Type70=='native')

### 本地+野外
nf <- native %>% dplyr::select(Climate=Type70, Species, LCC, LNC, LDMC, SLA) %>%
             rbind(field)

### 本地+植物园
g1 <- garden %>% dplyr::select(Climate=Climate70, Species, LCC, LNC, LDMC, SLA)
ng <- native %>% dplyr::select(Climate=Type70, Species, LCC, LNC, LDMC, SLA) %>% rbind(g1)

### 本地+外来物种不同气候
nn <- garden %>% dplyr::select(Climate=Type70, Species, LCC, LNC, LDMC, SLA)

###计算性状对共同截距所需的模型 ,每次更换数据集
#野外数据集
dataset <- nn

###计算性状对共同斜率所需的模型
model1 <- alist(sma(LNC~LCC*Climate,    data=dataset, log = 'xy'),
                sma(LDMC~LCC*Climate, data=dataset, log = 'xy'),
                sma(SLA~LCC*Climate,  data=dataset, log = 'xy'),
                sma(LDMC~LNC*Climate, data=dataset, log = 'xy'),
                sma(SLA~LNC*Climate,  data=dataset, log = 'xy'),
                sma(SLA~LDMC*Climate,   data=dataset, log = 'xy'))

##################### （1）不同气候带物种变异系数和斜率 #############
#保存数据的表
trait.result <- list()

#### 开始计算性状间相关
for (i in 1: length(model1)) {
   #i=4
  ##计算相关SMA
  sma.r1 <- eval(model1[[i]]) 
  #公式
  formala.r <- as.character(sma.r1$formula)
  
  #汇总性状相关性的结果，r2，斜率和相关
  sum.r1 <- sma.r1$groupsummary
 
  #添加共斜率斜率检验和截距检验
  sum.r1$slopt <- sma.r1$commoncoef$p
  
  #添加性状对信息
  sum.r1$yt <- rep(formala.r[2], nrow(sum.r1))
  sum.r1$xt <- rep(str_extract(formala.r[3], '[A-Z]+'), nrow(sum.r1))
  sum.r1$trait <- rep(str_c(formala.r[2], '_', str_extract(formala.r[3], '[A-Z]+')), nrow(sum.r1))
  
  #保存结果
  trait.result[[i]] <- data.frame(sum.r1[,c(1:10,18)], comslope.t=round(sum.r1$slopt, 4))
}


## 最终结果
sma.final <- do.call(rbind, trait.result)
#保存数据
#write.csv(sma.final, str_c(dir.path, '/Results/6.slope_native_field.csv'), row.names = F)
#write.csv(sma.final, str_c(dir.path, '/Results/6.slope_native_garden.csv'), row.names = F)
write.csv(sma.final, str_c(dir.path, '/Results/6.slope_native_nonative.csv'), row.names = F)





