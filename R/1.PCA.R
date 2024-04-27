
library(ggplot2)
library(stringr)
library(plyr)
library(dplyr)
library(patchwork)

rm(list=ls())
gc()


#### 数据路径
dir.path <- 'G:/Papers/Garden.final4/'


#################  (1) 数据处理  ##############
#####数据
field.d <- read.csv(str_c(dir.path, "Data/clean_data/TRY_climtesp_anger.csv"))
head(field.d)

##植物园
garden <- read.csv(str_c(dir.path, 'Data/clean_data/garden_data.csv'))  %>% na.omit()
head(garden)


#####PCA数据
##野外
field_pca <- na.omit(field.d)
field_pca$data <- rep('field', nrow(field_pca))
field_pca <- dplyr::select(field_pca, Climate=Climate70, Species, LCC, LNC, LDMC, SLA, data)
head(field_pca)

##植物园
#选择不同阈值的Climate进行计算
garden_pca <- dplyr::select(garden, Climate=Climate70, Species, LCC, LNC, LDMC, SLA)
#garden_pca <- dplyr::select(garden, Climate=Climate80, Species, LCC, LNC, LDMC, SLA)
#garden_pca <- dplyr::select(garden, Climate=Climate90, Species, LCC, LNC, LDMC, SLA)
#garden_pca <- dplyr::select(garden, Climate=Climate100, Species, LCC, LNC, LDMC, SLA)


garden_pca$data <- rep('garden', nrow(garden_pca))
head(garden_pca)

##合并植物园和野外
pca.d <- do.call(rbind, list(field_pca, garden_pca))
head(pca.d)


#############每次输入不同数据，运行不一样的PCA ##############
#######运行前标准化输入数据，统一为性状+分组
pca.f <- function(pca.in){
  #######运行PCA
  pca <- prcomp(pca.in[,-1], center = TRUE, scale. = TRUE)
  summary(pca)
  
  ##提取PC得分，添加分组信息
  df <- pca$x %>% as.data.frame()
  head(df)
  
  #添加分组
  df_pca <- data.frame(df, group=pca.in[, 1])
  head(df_pca)
  
  #提取贡献
  sum <- summary(pca)
  xlab <- paste(' PC1 (', round(sum$importance[2,1]*100, 2), '%)')
  ylab <- paste(' PC1 (', round(sum$importance[2,2]*100, 2), '%)')
  
  #提取载荷得分
  sum1 <- sum$rotation %>% as.data.frame()
  sum1$traits <- rownames(sum1)
  
  
  ##### 范围大小
  xmin <- range(df_pca$PC1)[1]
  xlength <- range(df_pca$PC1)[2]-range(df_pca$PC1)[1]
  ymin <- range(df_pca$PC2)[1]
  ylength <- range(df_pca$PC2)[2]-range(df_pca$PC2)[1]
  
  ##### 提取作图数据
  pca.r <- list()
  pca.r$df <- df_pca
  pca.r$loading <- sum1
  pca.r$range <- data.frame(xmin, xlength, ymin, ylength, xlab, ylab)
  #返回结果
  return(pca.r)
}
###############


############  运行不同分组的PCA #############
##PCA结果保存在loading.all中
loading.all <- list()
head(pca.d)

###### 1.野外与植物园总体权衡
#######每次PCA分析，按照固定的格式，第一列为分组信息，后面为性状数据
dt=dplyr::select(pca.d, group = data, LCC, LNC, LDMC, SLA)

##运行pca
result <- pca.f(dt)
head(result$df)
loading.all[[1]] <- data.frame(result$loading, type=rep('fieldVSgarden', 4))


p1 <- ggplot(result$df, aes(x=PC1, y=PC2))+
  geom_point(aes(color=group))+
  geom_segment(data=result$loading, aes(x=0, y=0, xend=PC1*4, yend=PC2*4), 
               arrow =arrow(angle = 30, length = unit(0.25, "cm")), size=1)+
  geom_text(data= result$loading, aes(x=PC1*4.3, y=PC2*4.3, label=traits))+
  geom_hline(yintercept=0, linetype='dashed', color='gray40')+
  geom_vline(xintercept=0, linetype='dashed', color='gray40')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.margin = margin(20, 10, 10, 10), 
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position=c(0.8, 0.15),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8))+
  scale_color_manual(values=c('field'='gray60', 'garden'='#6F99ADFF'))+
  xlim(-8, 8)+
  labs(x = result$range$xlab, y = result$range$ylab, color='Group')



###### 2.野外权衡
head(pca.d)
##输入数据，固定格式
dt <- dplyr::filter(pca.d, data=='field') %>% dplyr::select(group = Climate, LCC, LNC, LDMC, SLA)

##运行pca
result <- pca.f(dt)
head(result$df)
loading.all[[2]] <- data.frame(result$loading, type=rep('Field', 4))

p2 <- ggplot(result$df, aes(x=PC1, y=PC2))+
  geom_point(aes(color=group))+
  labs(x = result$range$xlab, y = result$range$ylab)+
  geom_segment(data=result$loading, aes(x=0, y=0, xend=PC1*4, yend=PC2*4), 
               arrow =arrow(angle = 30, length = unit(0.25, "cm")), size=1)+
  geom_text(data= result$loading, aes(x=PC1*4.3, y=PC2*4.3, label=traits))+
  geom_hline(yintercept=0, linetype='dashed', color='gray40')+
  geom_vline(xintercept=0, linetype='dashed', color='gray40')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.margin = margin(20, 10, 10, 10), 
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position=c(0.8, 0.2),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8))+
  scale_color_manual(values=c('Widely_spread'="#E18727FF",'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  xlim(-6, 5)+
  labs(color='Field climate:')


###### 3.植物园不同气候带权衡
##输入数据，固定格式
dt <- dplyr::filter(pca.d, data=='garden') %>% dplyr::select(group = Climate, LCC, LNC, LDMC, SLA)

##运行pca
result <- pca.f(dt)
head(result$df)
loading.all[[3]] <- data.frame(result$loading, type=rep('Garden', 4))

p3 <- ggplot(result$df, aes(x=PC1, y=PC2))+
  geom_point(aes(color=group))+
  labs(x = result$range$xlab, y = result$range$ylab)+
  geom_segment(data=result$loading, aes(x=0, y=0, xend=PC1*4, yend=PC2*4), 
               arrow =arrow(angle = 30, length = unit(0.25, "cm")), size=1)+
  geom_text(data= result$loading, aes(x=PC1*4.3, y=PC2*4.3, label=traits))+
  geom_hline(yintercept=0, linetype='dashed', color='gray40')+
  geom_vline(xintercept=0, linetype='dashed', color='gray40')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.margin = margin(20, 10, 10, 10), 
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position=c(0.8, 0.2),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8))+
  scale_color_manual(values=c('Widely-distributed'="#E18727FF", 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  xlim(-5,6)+
  labs(color='Garden climate:')



#####合并图片
library(patchwork)
p.all <- p1+p2+p3

#载荷得分汇总
p.r <- do.call(rbind, loading.all)


#保存
ggsave(str_c(dir.path, 'Results/1.pca_all_70.pdf'), p.all, width=14, height=4)
#ggsave(str_c(dir.path, 'Results/1.pca_all_100.pdf'), p.all, width=14, height=4)

#PCA结果
write.csv(p.r, str_c(dir.path, 'Results//1.pca_loading.csv'), row.names = F)










