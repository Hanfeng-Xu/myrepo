
rm(list=ls())
gc()
library(stringr)
library(dplyr)
library(ggplot2)

############   计算总体性状的相关 #############
##了解数据和函数
#### 数据路径
dir.path <- 'G:/Papers/Garden.final2/'

#### 显著性转换函数
p.sig <- function(x){if(x>0.05) ('ns') else ('sig')}

star.p <- function(x){as.character(symnum(x,  
                                          cutpoints = c(0,  0.05, 1), 
                                          symbols = c("*", "ns")))}
#野外性状权衡
native_field <- read.csv(str_c(dir.path, '/Results/6.slope_native_field.csv'))
head(native_field)

#植物园性状权衡
native_garden <- read.csv(str_c(dir.path, '/Results/6.slope_native_garden.csv'))
head(native_garden)

#本地与外来物种不同气候带权衡
native_alien <- read.csv(str_c(dir.path, '/Results/6.slope_native_nonative.csv'))
head(native_alien)


######################################## 不同气候带与本地物种的比较 ############################
########不同气候带
###数据处理
line.field <- dplyr::select(native_garden, group, pval, trait, Slope, Slopelow=Slope_lowCI, Slopehigh=Slope_highCI, Slopet=comslope.t)
head(line.field)

#合并数据
line.dat1 <- line.field
line.dat1$psig <- sapply(line.dat1$pval, p.sig)
line.dat1$ssig <- sapply(line.dat1$Slopet, star.p)

#分组因子
line.dat1$group <- factor(line.dat1$group, levels = c('native', 'Tropical', 'Temperate', 'Widely-distributed'), 
                          labels = c('Native', 'Tropical', 'Temperate', 'Widespread'))

p1 <- ggplot(data=subset(line.dat1, trait=='LNC_LCC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh), size=1, width=0.3, color='gray70', linetype='dashed')+
  geom_point(color='gray70', size=5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-7.0, 7.0)+
  annotate('text_npc', label='LCC-LNC', npcx =0.5, npcy=0.95, color='black', size=4)


p2 <- ggplot(data=subset(line.dat1, trait=='LDMC_LCC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF", 'Widespread'="#E18727FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(1.5, 4)+
  annotate('text_npc', label=paste('LCC-LDMC: ', 'ns', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)


p3 <- ggplot(data=subset(line.dat1, trait=='SLA_LCC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group, linetype=psig), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_linetype_manual(values = c('ns'='dashed', 'sig'='solid'))+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="gray70", 'Temperate'="#0072B5FF", 'Widespread'="gray70"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-7.0, -3)+
  annotate('text_npc', label=paste('LCC-SLA', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p4 <- ggplot(data=subset(line.dat1, trait=='LDMC_LNC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF", 'Widespread'="#E18727FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-0.85, -0.35)+
  annotate('text_npc', label=paste('LNC-LDMC: ', 'ns', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)



p5 <- ggplot(data=subset(line.dat1, trait=='SLA_LNC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF", 'Widespread'="#E18727FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(0.9, 1.5)+
  annotate('text_npc', label=paste('LNC-SLA: ', 'ns', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p6 <- ggplot(data=subset(line.dat1, trait=='SLA_LDMC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF", 'Widespread'="#E18727FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-2.5, -1.4)+
  annotate('text_npc', label=paste('LDMC-SLA: ', 'ns', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)


####图片组合
library(cowplot)
p.list <- list(p1, p2, p3, p4, p5, p6)
p.all <- plot_grid(plotlist = p.list, ncol = 3, align = 'hv',  labels = "auto",
                   label_x = 0.05, label_y = 1.05, label_colour='black', label_size=14)

#图例
p.legend <- get_legend(p2+theme(legend.position = 'top'))

#最终保存
p.finall <- plot_grid(p.legend, p.all, ncol = 1, rel_heights = c(0.1, 1))+theme(plot.margin = margin(20, 10, 10, 10))
save_plot(str_c(dir.path, 'Results/7.garden_native_slope.pdf'), p.finall, base_width=14, base_height=7, dpi = 300)


######################################## 外来物种与本地物种的比较 ############################
########不同气候带
###数据处理
line.field <- dplyr::select(native_alien, group, pval, trait, Slope, Slopelow=Slope_lowCI, Slopehigh=Slope_highCI, Slopet=comslope.t)
head(line.field)

#合并数据
line.dat1 <- line.field
line.dat1$psig <- sapply(line.dat1$pval, p.sig)
line.dat1$ssig <- sapply(line.dat1$Slopet, star.p)

#分组因子
line.dat1$group <- factor(line.dat1$group, levels = c('native', 'Tropical', 'Temperate', 'Widely-distributed'), 
                          labels = c('Native', 'Tropical', 'Temperate', 'Widespread'))

p1 <- ggplot(data=subset(line.dat1, trait=='LNC_LCC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh), size=1, width=0.3, color='gray70', linetype='dashed')+
  geom_point(color='gray70', size=5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-7.0, 7.0)+
  annotate('text_npc', label=paste('LCC-LNC',sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p2 <- ggplot(data=subset(line.dat1, trait=='LDMC_LCC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group, linetype=psig), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_linetype_manual(values = c('ns'='dashed', 'sig'='solid'))+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF", 'Widespread'="#E18727FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(1, 4.5)+
  annotate('text_npc', label=paste('LCC-LDMC: ', 'ns', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p3 <- ggplot(data=subset(line.dat1, trait=='SLA_LCC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group, linetype=psig), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_linetype_manual(values = c('ns'='dashed', 'sig'='solid'))+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="gray70", 'Temperate'="#0072B5FF", 'Widespread'="gray70"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-9.0, -2)+
  annotate('text_npc', label=paste('LCC-SLA', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p4 <- ggplot(data=subset(line.dat1, trait=='LDMC_LNC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group, linetype=psig), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_linetype_manual(values = c('ns'='dashed', 'sig'='solid'))+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="gray70", 'Widespread'="gray70"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-0.9, -0.3)+
  annotate('text_npc', label=paste('LNC-LDMC', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p5 <- ggplot(data=subset(line.dat1, trait=='SLA_LNC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group, linetype=psig), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_linetype_manual(values = c('ns'='dashed', 'sig'='solid'))+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF", 'Widespread'="#E18727FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(0.5, 2)+
  annotate('text_npc', label=paste('LNC-SLA: ', 'ns', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p6 <- ggplot(data=subset(line.dat1, trait=='SLA_LDMC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group, linetype=psig), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_linetype_manual(values = c('ns'='dashed', 'sig'='solid'))+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF", 'Widespread'="gray70"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-2.8, -1.3)+
  annotate('text_npc', label=paste('LDMC-SLA', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

####图片组合
library(cowplot)
p.list <- list(p1, p2, p3, p4, p5, p6)
p.all <- plot_grid(plotlist = p.list, ncol = 3, align = 'hv',  labels = "auto",
                   label_x = 0.05, label_y = 1.05, label_colour='black', label_size=14)

#图例
p.legend <- get_legend(p2+theme(legend.position = 'top'))

#最终保存
p.finall <- plot_grid(p.legend, p.all, ncol = 1, rel_heights = c(0.1, 1))+theme(plot.margin = margin(20, 10, 10, 10))
save_plot(str_c(dir.path, 'Results/7.exotic_native_slope.pdf'), p.finall, base_width=14, base_height=7, dpi = 300)


######################################## 野外不同气候带与本地物种的比较 ############################
########不同气候带
###数据处理
line.field <- dplyr::select(native_field, group, pval, trait, Slope, Slopelow=Slope_lowCI, Slopehigh=Slope_highCI, Slopet=comslope.t)
head(line.field)

#合并数据
line.dat1 <- line.field
line.dat1$psig <- sapply(line.dat1$pval, p.sig)
line.dat1$ssig <- sapply(line.dat1$Slopet, star.p)

#分组因子
line.dat1$group <- factor(line.dat1$group, levels = c('native', 'Tropical', 'Temperate'), 
                          labels = c('Native', 'Tropical', 'Temperate'))

p1 <- ggplot(data=subset(line.dat1, trait=='LNC_LCC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group, linetype=psig), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_linetype_manual(values = c('ns'='dashed', 'sig'='solid'))+
  scale_color_manual(values = c('Native'='gray70', 'Tropical'="#BC3C29FF", 'Temperate'="gray70"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-5.5, -2.5)+
  annotate('text_npc', label=paste('LCC-LNC',  sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p2 <- ggplot(data=subset(line.dat1, trait=='LDMC_LCC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group, linetype=psig), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_linetype_manual(values = c('ns'='dashed', 'sig'='solid'))+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(1.5, 4)+
  annotate('text_npc', label=paste('LCC-LDMC: ', '*', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p3 <- ggplot(data=subset(line.dat1, trait=='SLA_LCC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group, linetype=psig), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_linetype_manual(values = c('ns'='dashed', 'sig'='solid'))+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="gray70", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-8.0, -3)+
  annotate('text_npc', label=paste('LCC-SLA',  sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p4 <- ggplot(data=subset(line.dat1, trait=='LDMC_LNC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-1.0, -0.35)+
  annotate('text_npc', label=paste('LNC-SLA: ', '*', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p5 <- ggplot(data=subset(line.dat1, trait=='SLA_LNC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(0.9, 1.6)+
  annotate('text_npc', label=paste('LNC-SLA: ', '*', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

p6 <- ggplot(data=subset(line.dat1, trait=='SLA_LDMC'), aes(x=group, y= Slope))+
  geom_errorbar(aes(ymin=Slopelow, ymax=Slopehigh, color=group), size=1, width=0.3)+
  geom_point(aes(color=group), size=5)+
  scale_color_manual(values = c('Native'='gray30', 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  labs(x='Climates')+
  ylim(-2.5, -1.0)+
  annotate('text_npc', label=paste('LDMC-SLA: ', '*', sep = ''), npcx =0.5, npcy=0.95, color='black', size=4)

####图片组合
library(cowplot)
p.list <- list(p1, p2, p3, p4, p5, p6)
p.all <- plot_grid(plotlist = p.list, ncol = 3, align = 'hv',  labels = "auto",
                   label_x = 0.05, label_y = 1.05, label_colour='black', label_size=14)
#图例
p.legend <- get_legend(p2+theme(legend.position = 'top'))

#最终保存
p.finall <- plot_grid(p.legend, p.all, ncol = 1, rel_heights = c(0.1, 1))+theme(plot.margin = margin(20, 10, 10, 10))
save_plot(str_c(dir.path, 'Results/7.field_native_slope.pdf'), p.finall, base_width=14, base_height=7, dpi = 300)


