
rm(list=ls())
gc()
library(stringr)
library(dplyr)
library(smatr)
library(ggplot2)
library(ggpmisc)

############   权衡作图  #############
##了解数据和函数
#### 数据路径
dir.path <- 'G:/Papers/Garden.final4/'

#################  (1) 数据  ##############
#####数据
field <- read.csv(str_c(dir.path, "Data/clean_data/TRY_climtesp_diaz_anger.csv"))
head(field)

#野外权衡
field.trade <- read.csv(str_c(dir.path, '/Results/2.Field_trade_offs.csv'))
head(field.trade)


##植物园,每次更改气候变量
garden <- read.csv(str_c(dir.path, 'Data/clean_data/garden_data.csv'))  %>% na.omit() %>% dplyr::rename(Climate=Climate70)
head(garden)

##性状权衡
#植物园,每次更改气候变量
garden.trade <- read.csv(str_c(dir.path, '/Results/2.Garden_trade_offs70.csv'))
head(garden.trade)


###################### 作图 ######################
#### 显著性转换函数
p.sig <- function(x){if(x>0.05) ('NS') else ('sig')}

star.p <- function(x){as.character(symnum(x,  
                                          cutpoints = c(0, 0.05,  1), 
                                          symbols = c("*", "ns")))}

#标注函数
xytext <- function(data=NULL, x='LCC', y='LNC'){
  #x,y最大最小值
  xmin <- min(log10(data[, x]), na.rm=T)
  xmax <- max(log10(data[, x]), na.rm=T)
  ymin <- min(log10(data[, y]), na.rm=T)
  ymax <- max(log10(data[, y]), na.rm=T) 
  
  range.x <- xmax-xmin #修改
  range.y <- ymax-ymin#修改
  #结果
  data.frame(xtext=xmin, xrang=range.x, ytext=ymin, yrang=range.y)
}


#####################################    野外性状权衡     #################################################
#############筛选数据
###画线数据
dat <- subset(field.trade, select = c(group, n:Slope, Int, trait, comslope.t, comint.t, comslope, comintint))
dat$r2 <- round(dat$r2, 3)
dat$psig <- sapply(dat$pval,  FUN=p.sig)

########## 1. LCC-LNC
lined.dat <- subset(dat, trait==str_c('LNC', '_', 'LCC'))
xy <- xytext(data = field, x='LCC', y='LNC')

pair1 <- ggplot(data=field, aes(x=log10(LCC), y=log10(LNC)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
  scale_color_manual(values = c('Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(2.4, 2.85)+ 
  ylim(0.5, 2.0)+
  labs(x=expression(paste(log[10], '(LCC)', ' (mg ', g^{-1}, ')')), y=expression(paste(log[10], '(LNC)',' (mg ', g^{-1}, ')')))


########## 2. LCC-LDMC
lined.dat <- subset(dat, trait==str_c('LDMC', '_', 'LCC'))
xy <- xytext(data = field, x='LCC', y='LDMC')

pair2 <- ggplot(data=field, aes(x=log10(LCC), y=log10(LDMC)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
  geom_abline(data=lined.dat, aes(slope=Slope, intercept=Int, color=group), size=0.5)+
  scale_color_manual(values = c('Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(2.4, 2.85)+ #LCC
  ylim(-0.9, -0.1)+ #LDMC
  annotate('text_npc', npcx =0.2, npcy=0.05, label=paste('Slope: ', star.p(unique(lined.dat$comslope.t))), size=4)+
  labs(x=expression(paste(log[10], '(LCC)',' (mg ', g^{-1}, ')')), y=expression(paste(log[10], '(LDMC)',' (g ', g^{-1}, ')')))

########## 3. LCC-SLA
lined.dat <- subset(dat, trait==str_c('SLA', '_', 'LCC'))
xy <- xytext(data = field, x='LCC', y='SLA')

pair3 <- ggplot(data=field, aes(x=log10(LCC), y=log10(SLA)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
#  geom_abline(data=lined.dat, aes(slope=Slope, intercept=Int, color=group), size=0.5)+
  scale_color_manual(values = c('Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(2.4, 2.85)+
  ylim(0.2, 1.8)+
  labs(x=expression(paste(log[10], '(LCC)',' (mg ', g^{-1}, ')')), y=expression(paste(log[10], '(SLA) (', m^2, ' ', kg^{-1}, ')')))


########## 4. LNC-LDMC
lined.dat <- subset(dat, trait==str_c('LDMC', '_', 'LNC'))
xy <- xytext(data = field, x='LNC', y='LDMC')

pair4 <- ggplot(data=field, aes(x=log10(LNC), y=log10(LDMC)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
  geom_abline(data=lined.dat, aes(slope=Slope, intercept=Int, color=group), size=0.5)+
  scale_color_manual(values = c('Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(0.5, 2.0)+ #LNC
  ylim(-0.9, -0.1)+ #LDMC
  annotate('text_npc', npcx =0.2, npcy=0.05, label=paste('Slope: ', star.p(unique(lined.dat$comslope.t))), size=4)+
  labs(x=expression(paste(log[10], '(LNC)',' (mg ', g^{-1}, ')')), y=expression(paste(log[10], '(LDMC)',' (g ', g^{-1}, ')')))


########## 5. LNC-SLA
lined.dat <- subset(dat, trait==str_c('SLA', '_', 'LNC'))
xy <- xytext(data = field, x='LNC', y='SLA')

pair5 <- ggplot(data=field, aes(x=log10(LNC), y=log10(SLA)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
  geom_abline(data=lined.dat, aes(slope=comslope, intercept=comintint, color=group), size=0.5)+
  scale_color_manual(values = c('Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(0.5, 2.0)+
  ylim(0.2, 1.8)+
  annotate('text_npc', npcx =0.2, npcy=0.12, label=paste('Slope: ', star.p(unique(lined.dat$comslope.t))), size=4)+
  annotate('text_npc', npcx =0.2, npcy=0.05, label=paste('Ele: ', star.p(unique(lined.dat$comint.t))), size=4)+
  labs(x=expression(paste(log[10], '(LNC)',' (mg ', g^{-1}, ')')), y=expression(paste(log[10], '(SLA) (', m^2, ' ', kg^{-1}, ')')))


########## 6. LDMC-SLA
lined.dat <- subset(dat, trait==str_c('SLA', '_', 'LDMC'))
xy <- xytext(data = field, x='LDMC', y='SLA')

pair6 <- ggplot(data=field, aes(x=log10(LDMC), y=log10(SLA)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
  geom_abline(data=lined.dat, aes(slope=Slope, intercept=Int, color=group), size=0.5)+
  scale_color_manual(values = c('Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(-0.9, -0.1)+
  ylim(0.2, 1.8)+
  annotate('text_npc', npcx =0.2, npcy=0.05, label=paste('Slope: ', star.p(unique(lined.dat$comslope.t))), size=4)+
  labs(x=expression(paste(log[10], '(LDMC)',' (g ', g^{-1}, ')')), y=expression(paste(log[10],'(SLA) (', m^2, ' ', kg^{-1}, ')')))



#####################################    植物园性状权衡     #################################################
#############筛选数据
###画线数据
dat <- subset(garden.trade, select = c(group, n:Slope, Int, trait, comslope.t:comintint))
dat$r2 <- round(dat$r2, 3)
dat$psig <- sapply(dat$pval,  FUN=p.sig)


########## 1. LCC-LNC
lined.dat <- subset(dat, trait==str_c('LNC', '_', 'LCC'))
xy <- xytext(data = garden, x='LCC', y='LNC')

pair11 <- ggplot(data=garden, aes(x=log10(LCC), y=log10(LNC)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
  scale_color_manual(values = c('Widely-distributed'="#E18727FF", 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(2.55, 2.75)+
  ylim(0.9, 1.7)+
  labs(x=expression(paste(log[10], '(LCC)', ' (mg ', g^{-1}, ')')), y=expression(paste(log[10], '(LNC)',' (mg ', g^{-1}, ')')))


########## 2. LCC-LDMC
lined.dat <- subset(dat, trait==str_c('LDMC', '_', 'LCC'))
xy <- xytext(data = garden, x='LCC', y='LDMC')

pair21 <- ggplot(data=garden, aes(x=log10(LCC), y=log10(LDMC)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
  geom_abline(data=lined.dat, aes(slope=comslope, intercept=comintint, color=group), size=0.5)+
  scale_color_manual(values = c('Widely-distributed'="#E18727FF", 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(2.55, 2.75)+
  ylim(-0.7, -0.2)+
  annotate('text_npc', npcx =0.2, npcy=0.05, label=paste('Slope: ', star.p(unique(lined.dat$comslope.t))), size=4)+
  labs(x=expression(paste(log[10], '(LCC)',' (mg ', g^{-1}, ')')), y=expression(paste(log[10], '(LDMC)',' (g ', g^{-1}, ')')))


########## 3. C-SLA
lined.dat <- subset(dat, trait==str_c('SLA', '_', 'LCC'))
xy <- xytext(data = garden, x='LCC', y='SLA')

pair31 <- ggplot(data=garden, aes(x=log10(LCC), y=log10(SLA)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
#  geom_abline(data=lined.dat, aes(slope=Slope, intercept=Int, color=group), size=0.5)+
  scale_color_manual(values = c('Widely-distributed'="#E18727FF", 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(2.55, 2.75)+
  ylim(0.5, 1.6)+
  labs(x=expression(paste(log[10], '(LCC)',' (mg ', g^{-1}, ')')), y=expression(paste(log[10], '(SLA) (', m^2, ' ', kg^{-1}, ')')))


########## 4. N-LDMC
lined.dat <- subset(dat, trait==str_c('LDMC', '_', 'LNC'))
xy <- xytext(data = garden, x='LNC', y='LDMC')

pair41 <- ggplot(data=garden, aes(x=log10(LNC), y=log10(LDMC)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
  geom_abline(data=lined.dat, aes(slope=comslope, intercept=comintint, color=group), size=0.5)+
  scale_color_manual(values = c('Widely-distributed'="#E18727FF", 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(0.9, 1.7)+
  ylim(-0.7, -0.2)+
  annotate('text_npc', npcx =0.2, npcy=0.05, label=paste('Slope: ', star.p(unique(lined.dat$comslope.t))), size=4)+
  labs(x=expression(paste(log[10], '(LNC)',' (mg ', g^{-1}, ')')), y=expression(paste(log[10], '(LDMC)',' (g ', g^{-1}, ')')))


########## 5. N-SLA
lined.dat <- subset(dat, trait==str_c('SLA', '_', 'LNC'))
xy <- xytext(data = garden, x='LNC', y='SLA')

pair51 <- ggplot(data=garden, aes(x=log10(LNC), y=log10(SLA)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
  geom_abline(data=lined.dat, aes(slope=comslope, intercept=comintint, color=group), size=0.5)+
  scale_color_manual(values = c('Widely-distributed'="#E18727FF", 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(0.9, 1.7)+
  ylim(0.5, 1.6)+
  annotate('text_npc', npcx =0.2, npcy=0.12, label=paste('Slope: ', star.p(unique(lined.dat$comslope.t))), size=4)+
  annotate('text_npc', npcx =0.2, npcy=0.05, label=paste('Ele: ', star.p(unique(lined.dat$comint.t))), size=4)+
  labs(x=expression(paste(log[10], '(LNC)',' (mg ', g^{-1}, ')')), y=expression(paste(log[10], '(SLA) (', m^2, ' ', kg^{-1}, ')')))


########## 6. LDMC-SLA
lined.dat <- subset(dat, trait==str_c('SLA', '_', 'LDMC'))
xy <- xytext(data = garden, x='LDMC', y='SLA')

pair61 <- ggplot(data=garden, aes(x=log10(LDMC), y=log10(SLA)))+
  geom_point(aes(color=Climate), alpha=0.9, size=1)+
  geom_abline(data=lined.dat, aes(slope=comslope, intercept=comintint, color=group), size=0.5)+
  scale_color_manual(values = c('Widely-distributed'="#E18727FF", 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="plain", size=14),
        axis.text.x = element_text(face="plain", color='black', size=12),
        axis.title.y=element_text(face="plain", size=14),
        axis.text.y = element_text(face="plain", color='black', size=12),
        legend.position = "none")+
  xlim(-0.7, -0.2)+
  ylim(0.5, 1.6)+
  annotate('text_npc', npcx =0.2, npcy=0.12, label=paste('Slope: ', star.p(unique(lined.dat$comslope.t))), size=4)+
  annotate('text_npc', npcx =0.2, npcy=0.05, label=paste('Ele: ', star.p(unique(lined.dat$comint.t))), size=4)+
  labs(x=expression(paste(log[10], '(LDMC)',' (g ', g^{-1}, ')')), y=expression(paste(log[10],'(SLA) (', m^2, ' ', kg^{-1}, ')')))



####################################
library(cowplot)
p.list <- list(pair5, pair51, pair6, pair61, pair4, pair41, pair2, pair21, pair1, pair11, pair3, pair31)
#p.list <- list(pair1, pair11, pair2, pair21, pair3, pair31, pair4, pair41, pair5, pair51, pair6, pair61)
p.all <- plot_grid(plotlist = p.list, ncol = 4, align = 'hv', labels = "auto",
                   label_x = 0.05, label_y = 1, label_colour='black', label_size=14)

#图例
p.legend <- get_legend(pair51+theme(legend.position = 'top'))

#图片
p.finall <- plot_grid(p.legend, p.all, ncol = 1, rel_heights = c(0.1,1))

###保存
#save_plot(str_c(dir.path, 'Results/3.garden_field100.pdf'), p.finall, base_height=10, base_width=15, dpi=300)
save_plot(str_c(dir.path, 'Results/3.garden_field11.pdf'), p.finall, base_height=10, base_width=15, dpi=300)
save_plot(str_c(dir.path, 'Results/3.tiaogarden_field70.jpeg'), p.finall, base_height=10, base_width=15, dpi=300)






