
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
##植物园，每次需要修改
garden <- read.csv(str_c(dir.path, 'Data/clean_data/garden_data.csv'))  %>% na.omit() %>% dplyr::rename(Type=Type70)
head(garden)

#权衡结果，每次需要修改
native_exotic <- read.csv(str_c(dir.path, 'Results/4.garden_exotic_trade70.csv'))
#native_exotic <- read.csv(str_c(dir.path, 'Results/4.garden_exotic_trade100.csv'))
head(native_exotic)


###################### 自定义函数 ######################
############# 作图函数显著性转换函数p.sig, star.p
p.sig <- function(x){if(x>0.05) ('NS') else ('sig')}

star.p <- function(x){as.character(symnum(x,  
                                          cutpoints = c(0, 0.05, 1), 
                                          symbols = c("*", "ns")))}

############# 作图函数native.plot
native.plot <- function(p.data=garden, line=native_exotic, mapping=aes(x=log10(LNC), y=log10(LDMC)), 
                        x='LNC', y='LDMC', climate='Tropical'){
  
  #需要画图的权衡
  lined.dat1 <- subset(line, trait==str_c(y, '_', x, climate))
  
  ##本地与外来气候带的权衡作图数据
  #拟合数据
  lined.dat2 <- subset(lined.dat1, group=='native'|group==climate)
  #作图数据
  trait.d <- subset(p.data, Type=='native'|Type==climate)
  
  ##线条和点的颜色color.point
  if (climate=='Tropical') {
    color.point="#BC3C29FF"
    color.line = c('native'='gray60', 'Tropical'="#BC3C29FF")} else if (climate=='Temperate') {
      color.point="#0072B5FF" 
      color.line = c('native'='gray60', 'Temperate'="#0072B5FF")} else if (climate=='Widely-distributed')  {
        color.point="#E18727FF"
        color.line = c('native'='gray60', 'Widely-distributed'="#E18727FF")}
  
  ##画图x,y的限制大小
  #x限制
  if (x=='LCC')(xrang=c(2.55, 2.75)) else if (x=="LNC")(xrang=c(0.9, 1.7)) else if (x=='LDMC') (xrang=c(-0.7, -0.2))
  #y限制
  if (y=="LNC")(yrang=c(0.9, 1.7)) else if (y=='LDMC') (yrang=c(-0.7, -0.2)) else if (y=='SLA') (yrang=c(0.5, 1.6))
  
  ##坐标轴
  if (x=='LCC' & y=='LNC') {xlab=expression(paste(log[10], '(LCC)', ' (mg ', g^{-1}, ')'))
  ylab=expression(paste(log[10], '(LNC)',' (mg ', g^{-1}, ')'))
  } else if (x=='LCC' & y=='LDMC')  {xlab=expression(paste(log[10], '(LCC)',' (mg ', g^{-1}, ')')) 
  ylab=expression(paste(log[10], '(LDMC)',' (g ', g^{-1}, ')'))
  } else if (x=='LCC' & y=='SLA')  {xlab=expression(paste(log[10], '(LCC)',' (mg ', g^{-1}, ')')) 
  ylab=expression(paste(log[10], '(SLA) (', m^2, ' ', kg^{-1}, ')'))
  } else if (x=='LNC' & y=='LDMC')  {xlab=expression(paste(log[10], '(LNC)',' (mg ', g^{-1}, ')')) 
  ylab=expression(paste(log[10], '(LDMC)',' (g ', g^{-1}, ')'))
  } else if (x=='LNC' & y=='LDMC')  {xlab=expression(paste(log[10], '(LNC)',' (mg ', g^{-1}, ')')) 
  ylab=expression(paste(log[10], '(LDMC)',' (g ', g^{-1}, ')'))
  } else if (x=='LNC' & y=='SLA')   {xlab=expression(paste(log[10], '(LNC)',' (mg ', g^{-1}, ')')) 
  ylab=expression(paste(log[10], '(SLA) (', m^2, ' ', kg^{-1}, ')'))
  } else if (x=='LDMC' & y=='SLA')  {xlab=expression(paste(log[10], '(LDMC)',' (g ', g^{-1}, ')')) 
  ylab=expression(paste(log[10],'(SLA) (', m^2, ' ', kg^{-1}, ')'))}
  
  
  ### 根据两种类型是否相关划线
  
  if ('NS' %in% lined.dat2$psig) {
    #只有一个相关或都不相关
    p.trait <- ggplot(data=trait.d, mapping)+
      geom_point(data=subset(trait.d, Type=='native'), color='gray60',  size=1.0)+
      geom_point(data=subset(trait.d, Type==climate), color=color.point,  size=1.0, shape=17)+
      theme_bw()+
      theme(panel.grid = element_blank(),
            axis.title.x=element_text(face="plain", size=12),
            axis.text.x = element_text(face="plain", color='black', size=10),
            axis.title.y=element_text(face="plain", size=12),
            axis.text.y = element_text(face="plain", color='black', size=10),
            legend.position = "none")+
      xlim(xrang)+ 
      ylim(yrang)+
      labs(x=xlab, y=ylab)
  } else { 
    if (unique(lined.dat2$comslope.t)>0.05){#都相关的时候    
      p.trait <- ggplot(data=trait.d, mapping)+
        geom_point(data=subset(trait.d, Type=='native'), color='gray60',  size=1.0)+
        geom_point(data=subset(trait.d, Type==climate), color=color.point,  size=1.0, shape=17)+
        geom_abline(data=lined.dat2, aes(slope=comslope, intercept=comintint, color=group), size=0.5)+
        scale_color_manual(values = color.line)+
        theme_bw()+
        theme(panel.grid = element_blank(),
              axis.title.x=element_text(face="plain", size=12),
              axis.text.x = element_text(face="plain", color='black', size=10),
              axis.title.y=element_text(face="plain", size=12),
              axis.text.y = element_text(face="plain", color='black', size=10),
              legend.position = "none")+
        xlim(xrang)+ 
        ylim(yrang)+
        annotate('text_npc', npcx =0.2, npcy=0.05, label=paste('Slope: ', unique(lined.dat2$slopet)), size=4)+
        labs(x=xlab, y=ylab)} else {
          #都相关的时候    
          p.trait <- ggplot(data=trait.d, mapping)+
            geom_point(data=subset(trait.d, Type=='native'), color='gray60',  size=1.0)+
            geom_point(data=subset(trait.d, Type==climate), color=color.point,  size=1.0, shape=17)+
            geom_abline(data=lined.dat2, aes(slope=Slope, intercept=Int, color=group), size=0.5)+
            scale_color_manual(values = color.line)+
            theme_bw()+
            theme(panel.grid = element_blank(),
                  axis.title.x=element_text(face="plain", size=12),
                  axis.text.x = element_text(face="plain", color='black', size=10),
                  axis.title.y=element_text(face="plain", size=12),
                  axis.text.y = element_text(face="plain", color='black', size=10),
                  legend.position = "none")+
            xlim(xrang)+ 
            ylim(yrang)+
            annotate('text_npc', npcx =0.2, npcy=0.05, label=paste('Slope: ', unique(lined.dat2$slopet)), size=4)+
            labs(x=xlab, y=ylab)
        }
  }
  
  #函数返回
  return(p.trait)
}
############################################


###画线数据
native_exotic$psig <- sapply(native_exotic$pval,  FUN=p.sig)
native_exotic$slopet <- sapply(native_exotic$comslope.t,  FUN=star.p)
native_exotic$intt <- sapply(native_exotic$comint.t,  FUN=star.p)
native_exotic <- subset(native_exotic, select = c(group, n:Slope, Int, trait, comslope.t:intt))

#####################################    本地与外来性状权衡比较     #################################################
########## 1. LCC-LNC
p11 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LCC), y=log10(LNC)), 
                   x='LCC', y='LNC', climate='Tropical')

p12 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LCC), y=log10(LNC)), 
                   x='LCC', y='LNC', climate='Temperate')

p13 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LCC), y=log10(LNC)),  
                   x='LCC', y='LNC', climate='Widely-distributed')

########## 2. LCC-LDMC
p21 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LCC), y=log10(LDMC)), 
                   x='LCC', y='LDMC', climate='Tropical')

p22 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LCC), y=log10(LDMC)), 
                   x='LCC', y='LDMC', climate='Temperate')

p23 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LCC), y=log10(LDMC)),  
                   x='LCC', y='LDMC', climate='Widely-distributed')

########## 3. LCC-SLA
p31 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LCC), y=log10(SLA)), 
                   x='LCC', y='SLA', climate='Tropical')

p32 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LCC), y=log10(SLA)), 
                   x='LCC', y='SLA', climate='Temperate')

p33 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LCC), y=log10(SLA)),  
                   x='LCC', y='SLA', climate='Widely-distributed')

########## 4. LNC-LDMC
p41 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LNC), y=log10(LDMC)), 
                   x='LNC', y='LDMC', climate='Tropical')

p42 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LNC), y=log10(LDMC)),  
                   x='LNC', y='LDMC', climate='Temperate')

p43 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LNC), y=log10(LDMC)),  
                   x='LNC', y='LDMC', climate='Widely-distributed')


########## 5. LNC-SLA
p51 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LNC), y=log10(SLA)), 
                   x='LNC', y='SLA', climate='Tropical')

p52 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LNC), y=log10(SLA)),  
                   x='LNC', y='SLA', climate='Temperate')

p53 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LNC), y=log10(SLA)),  
                   x='LNC', y='SLA', climate='Widely-distributed')


########## 6. LDMC-SLA
p61 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LDMC), y=log10(SLA)), 
                   x='LDMC', y='SLA', climate='Tropical')

p62 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LDMC), y=log10(SLA)),  
                   x='LDMC', y='SLA', climate='Temperate')

p63 <- native.plot(p.data=garden, line=native_exotic, mapping=aes(x=log10(LDMC), y=log10(SLA)),  
                   x='LDMC', y='SLA', climate='Widely-distributed')




#图片
library(cowplot)

#p.list <- list(p11,p12,p13,p21,p22,p23,p31,p32,p33, p41, p42, p43, p51,p52, p53, p61, p62, p63)
p.list <- list(p51,p52,p53,p61,p62,p63,p41,p42,p43, p21, p22, p23, p11,p12, p13, p31, p32, p33)

p.all <- plot_grid(plotlist = p.list, ncol = 3, align = 'hv', labels = "auto",
                   label_x = 0.05, label_y = 1.06, label_colour='black', label_size=14)



#图例
p.legend <- get_legend(ggplot(data=garden, aes(x=log10(LNC), y=log10(SLA)))+
                         geom_point(aes(color=Type, shape=Type),  size=1)+
                         geom_abline(data=subset(native_exotic, trait==str_c('LDMC', '_', 'LNC')), aes(slope=Slope, intercept=Int, color=group), size=0.5)+
                         scale_shape_manual(values = c('native'=16, 'Widely-distributed'=17, 'Tropical'=17, 'Temperate'=17))+
                         scale_color_manual(values = c('native'='gray60', 'Widely-distributed'="#E18727FF", 'Tropical'="#BC3C29FF", 'Temperate'="#0072B5FF"))+
                         theme_bw()+
                         theme(legend.position = "top")+
                         xlim(0.9, 1.7)+ 
                         ylim(0.5, 1.6))

#图片
p.finall <- plot_grid(p.legend, p.all, ncol = 1, rel_heights = c(0.05,1))+
  theme(plot.margin = margin(10, 10, 10,10))
#保存
#save_plot(str_c(dir.path, 'Results/5.garden_native70.jpeg'), p.finall, base_height=16, base_width=10, dpi=300)
#save_plot(str_c(dir.path, 'Results/5.garden_native70.pdf'), p.finall, base_height=16, base_width=10, dpi=300)
save_plot(str_c(dir.path, 'Results/5.tiaogarden_native70.jpeg'), p.finall, base_height=16, base_width=10, dpi=300)

