rm(list = ls())
gc()

library(raster)
library(tidyverse)
library(plyr)
###########################################  一 加载数据 ###############################################################
#加载原始下载数据信息
download.inf <- read.csv('G:/Papers/Garden/Data_cleaning/Field/step1.Data/Field_download_all_inf.csv') %>% 
  dplyr::select(usagekey, original_sciname) 


################# 匹配气候带信息
#加载柯本气候带分布tif
clim <- raster('G:/R/climit_type/Beck_KG_V1_present_0p0083.tif')

#气候带分布类型
classification <- read.table('G:\\数据\\全球气候带划分\\Beck_KG_V1\\climate_classification.txt',
                             header =F)
c_no <- str_extract(classification$V1, '[0-9]+')
c_type <- str_extract(classification$V1, '\\p{L}+')
c_detail <- classification$V2
Climtype <- data.frame(classNo=as.double(c_no), ClimType=c_type, ClimDetail=c_detail)


###自编函数extract_mode-用于提取众数
extract_mode=function(x){
  if (sum(x)==0) (0) else{t1=x[x>0]
  as.numeric(names(table(t1)))[if (sum(table(t1) == max(table(t1)))==1){table(t1) == max(table(t1))
  } else {which((table(t1) == max(table(t1)))==TRUE)[1]}]}}


##### 添加气候大类分类信息
replace_clim <- c("Af"='Tropical',  "Am"='Tropical',"Aw"='Tropical', 
                  "BWh"='Arid',  "BWk"='Arid', "BSh"='Arid', "BSk"='Arid',
                  "Csa"='Temperate', "Csb"='Temperate',  "Csc"='Temperate', 
                  "Cwa"='Temperate',  "Cwb"='Temperate',  "Cwc"='Temperate', 
                  "Cfa"='Temperate',  "Cfb"='Temperate',  "Cfc"='Temperate', 
                  "Dsa"='Cold',  "Dsb"='Cold',  "Dsc"='Cold', 
                  "Dsd"='Cold',  "Dwa"='Cold',  "Dwb"='Cold', 
                  "Dwc"='Cold',  "Dwd"='Cold',  "Dfa"='Cold', 
                  "Dfb"='Cold',  "Dfc"='Cold',  "Dfd"='Cold', 
                  "ET"='Polar',  "EF"='Polar') 


########################################### 二 循环计算气候带 ###############################################################
library(data.table)
# 设置CSV文件路径
file_path <- "H:/0054946-240321170329656/0054946-240321170329656.csv"

# 设置每次读取的行数
chunk_size <- 1000000  # 每次读取100万行

# 计算需要分块读取的次数
num_chunks <- ceiling(34724346 / chunk_size)

# 初始化一个空的数据表用于存储结果
result <- data.table()

# 使用while循环分块读取数据并进行分析
chunk_num <- 0


while (chunk_num < num_chunks) {
  chunk_num <- chunk_num + 1
  start_row <- (chunk_num - 1) * chunk_size + 1
  end_row <- min(chunk_num * chunk_size, 34724346)
  
  #看运行到第几个块
  print(chunk_num)
  
  # 读取数据块
  if (chunk_num == 1) {
    # 如果是第一个数据块，则读取时自动提取列名
    chunk_data <- fread(file_path, skip = start_row - 1, nrows = chunk_size, quote='')
    # 存储列名
    col_names <- colnames(chunk_data)
  } else {
    # 如果不是第一个数据块，则手动指定列名，并跳过第一行
    chunk_data <- fread(file_path, skip = start_row - 1, nrows = chunk_size, col.names = col_names, quote='')
  }
  
  # 进行数据分析
  ############################# GIBF数据处理
  ### 加载gbif数据
  gbif.inf <- chunk_data
  
  #筛选变量
  gbif_sp <- gbif.inf[, c("gbifID",'taxonKey', 'speciesKey', "species",'verbatimScientificName', 
                          "decimalLatitude","decimalLongitude")]
  
  ### 匹配原始下载物种数据信息
  gbif_sp <- left_join(gbif_sp, download.inf, by=c('taxonKey'='usagekey'))
  
  #筛选NA值
  gbif_sp <- filter(gbif_sp, !is.na(original_sciname))
  
  ############################# GIBF数据添加经纬度
  #加载数据
  gbif_sp <- gbif_sp %>%  dplyr::select(taxonKey, original_sciname, decimalLongitude, decimalLatitude) 
  #gbif_sp$decimalLongitude <- as.numeric(gbif_sp$decimalLongitude)
  gbif_sp <-  na.omit(gbif_sp)
  
  
  ################# 4. 提取数据气候信息
  #提取数据
  site  <- dplyr::select(gbif_sp, long=decimalLongitude, lat=decimalLatitude) 
  
  ####第一次提取
  site_climtype <- raster::extract(clim, site[, 1:2])
  
  #加入数据
  site$climtype <- site_climtype
  gbif_sp$climtype <- site_climtype
  
  #保存第一次提的和剩余的
  site1 <- gbif_sp[!site$climtype==0, ]
  site_rest <- gbif_sp[site$climtype==0, ]
  
  #### 第二次提取
  clim_1000 <- raster::extract(clim, dplyr::select(site_rest, long=decimalLongitude, lat=decimalLatitude), 
                               buffer=1000, fun=extract_mode)
  
  site_rest$climtype <- clim_1000
  
  ###所有提取结果
  banna_clim <- rbind(site1, site_rest)
  
  #### 根据气候分类匹配气候带信息
  final_clim <- left_join(banna_clim, Climtype, by=c('climtype'='classNo'))
  
  
  ################# 6. 物种匹配气候带类型
  #添加
  final_clim$Climatic_zones <- str_replace_all(final_clim$ClimType, replace_clim)
  final_clim <-  dplyr::select(final_clim, taxonKey:climtype, Climatic_zones)
  
  # 将结果存储到结果数据表中
  result <- rbind(result, final_clim)
}

#保存匹配好位点的数据
#fwrite(result, 'G:/Papers/Garden.final3/Data/fieldclimate/field_climate_fwrite.csv')
#write.csv(result, 'G:/Papers/Garden.final3/Data/fieldclimate/field_climate.csv', row.names = F)



###########################################  三 气候带划分 ###############################################################
####### 基于gbif的野外物种全球分布点数据气候带信息
head(result)

#删除没有气候带信息的条
field.sp.gbif <- filter(result, !climtype==0)
head(field.sp.gbif)

#####根据需要修改物种名
colnames(field.sp.gbif)[2] <- 'species'

#每个物种记录数
field.gbif.30 <- ddply(field.sp.gbif, .(species), summarise, nsp=length(species))
head(field.gbif.30)

#点小于30的物种-less than
field.gbif.lr30 <- filter(field.gbif.30, nsp < 30)
#点大于30的物种-more than
field.gbif.mr30 <- filter(field.gbif.30, nsp > 29)


##保存划分气候带的数据
list70 <- list()
list80 <- list()
list90 <- list()
list100 <- list()

#### 筛选不同阈值阈值threshold
#进度条
pb <- txtProgressBar(style=3)

for (sp in unique(field.gbif.mr30$species)) {
#  sp="Couratari guianensis" #Couratari guianensis Andira inermis
  sp1.gbif <- filter(field.sp.gbif, species==sp)
  sp1.clim <- data.frame(table(sp1.gbif$Climatic_zones))
  sp1.clim$prop <- sp1.clim$Freq/sum(sp1.clim$Freq) 
  sp1.clim <- arrange(sp1.clim, desc(prop))
  sp1.clim$Var1 <- as.vector(sp1.clim$Var1)
  
  #阈值threshold
  list100[[sp]] <- c(sp, ifelse(sp1.clim[1, 3]>=0.99, sp1.clim[1, 1], 'Widely_spread'), 0.99)
  list90[[sp]] <- c(sp, ifelse(sp1.clim[1, 3]>=0.9, sp1.clim[1, 1], 'Widely_spread'), 0.9) 
  list80[[sp]] <- c(sp, ifelse(sp1.clim[1, 3]>=0.8, sp1.clim[1, 1], 'Widely_spread'), 0.8) 
  list70[[sp]] <- c(sp, ifelse(sp1.clim[1, 3]>=0.7, sp1.clim[1, 1], 'Widely_spread'), 0.7) 
  
  ## 第二个位置：实时反映进度
  setTxtProgressBar(pb, which(unique(field.gbif.mr30$species)==sp)/4140)
}

sp.clim.100 <- do.call(rbind, list100)
sp.clim.90 <- do.call(rbind, list90)
sp.clim.80 <- do.call(rbind, list80)
sp.clim.70 <- do.call(rbind, list70)

sp.clim.100 <- as.data.frame(sp.clim.100)
sp.clim.90 <- as.data.frame(sp.clim.90)
sp.clim.80 <- as.data.frame(sp.clim.80)
sp.clim.70 <- as.data.frame(sp.clim.70)

colnames(sp.clim.100) <- c('species', 'Climatic_zones', 'threshold')
colnames(sp.clim.90) <- c('species', 'Climatic_zones', 'threshold')
colnames(sp.clim.80) <- c('species', 'Climatic_zones', 'threshold')
colnames(sp.clim.70) <- c('species', 'Climatic_zones', 'threshold')

#保存
head(sp.clim.70)
write.csv(sp.clim.100, 'G:/Papers/Garden.final3/Data/fieldclimate//field.clim.100.inf.csv', row.names=F)
write.csv(sp.clim.90, 'G:/Papers/Garden.final3/Data/fieldclimate//field.clim.90.inf.csv', row.names=F)
write.csv(sp.clim.80, 'G:/Papers/Garden.final3/Data/fieldclimate//field.clim.80.inf.csv', row.names=F)
write.csv(sp.clim.70, 'G:/Papers/Garden.final3/Data/fieldclimate//field.clim.70.inf.csv', row.names=F)






