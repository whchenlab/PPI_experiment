# plot.bar
library(ggplot2)
library(ggpubr)
library(cowplot)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(xlsx2dfs)
library(ggsignif)


# figure_name <- 'ALT'
# y_label0 <- 'ALT (U/L)'
# file_name <- '02 - table/ALT.xlsx'
# figure_number <- 'C'


plot_bar_signif <- function(figure_number,figure_name,y_label0,file_name){
  data_plot0 <- read.xlsx(file_name)
  data_plot1 <- gather(data_plot0,'Group','num')
  if(sum(is.na(data_plot1$num))>0){
    data_plot1 <- data_plot1[-(which(is.na(data_plot1$num))),]
  }
  data_plot <- data_plot1 %>%
    group_by(Group) %>%
    summarise(n=n(),mean=mean(num),sd=sd(num),se=sd/sqrt(n))
  length0 <- max(data_plot$mean+data_plot$se)
  # level0 <- c("Control","CCl4","CCl4(PPI+Bac.)","CCl4(PPI)","CCl4(Bac.)")
  level0 <- colnames(data_plot0)
  stat.test <- compare_means(
    num~Group,data = data_plot1,
    method = "wilcox.test"
  )%>%mutate(y.position = seq(from=length0*1.05, to=length0*1.5,length.out=10))
  x=stat.test$p
  stat.test$p.signif<-ifelse(x<0.1, ifelse(x<0.05, ifelse(x<0.01, ifelse(x<0.001, ifelse(x<=0.0001, '****','***'),'**'),'*'),'.'),'ns')
  stat.test$p.side <- 1
  stat.test$p.side.method <- 'two.sided'
  for (i in 1:nrow(stat.test)) {
    x_t <- data_plot0[,stat.test$group1[i]]
    if(sum(is.na(x_t))>0){
      x_t <- x_t[-which(is.na(x_t))]
    }
    y_t <- data_plot0[,stat.test$group2[i]]
    if(sum(is.na(y_t))>0){
      y_t <- y_t[-which(is.na(y_t))]
    }
    gp <- wilcox.test(x_t,y_t,alternative='greater')
    gp[["p.value"]]
    lp <- wilcox.test(x_t,y_t,alternative='less')
    lp[["p.value"]]
    stat.test[i,'p.side'] <- as.numeric(ifelse(gp[["p.value"]]<lp[["p.value"]],gp[["p.value"]],lp[["p.value"]]))
    stat.test[i,'p.side.method'] <- ifelse(gp[["p.value"]]<lp[["p.value"]],'greater','less')
  }
  x=stat.test$p.side
  stat.test$p.side.signif<-ifelse(x<0.1, ifelse(x<0.05, ifelse(x<0.01, ifelse(x<0.001, ifelse(x<=0.0001, '****','***'),'**'),'*'),'.'),'ns')

  data_plot$Group <- factor(data_plot$Group,levels = level0)
  data_plot1$Group <- factor(data_plot1$Group,levels = level0)


  p <- ggplot(data_plot, aes(Group, weight = mean, fill = Group)) +
    # geom_hline(yintercept = seq(10, 50, 10), color = 'gray') +
    geom_bar(color = "white", width = .65, position = 'dodge') +
    scale_fill_manual(values = c('#90BFF9','#05BE78','#FF0000','#B07FC0','#C5944E'))+ #palette ="Set3"
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, size = 0.3, position = position_dodge(0.7)) +
    labs( y = y_label0,x='') +
    # ylim(NA,length0*1.55)+
    scale_y_continuous(expand = c(0,0),limits = c(0, length0*1.55)) +
    ggtitle(figure_number,figure_name)+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x=element_text(angle=45, hjust=1,face = 'plain',size=10),
          axis.text.y=element_text(face = 'plain',size=10),
          text = element_text(size=15,face = 'plain',family ="sans",colour = 'black'))+
    theme(legend.position="none",
          plot.title = element_text(hjust = -0.4, vjust=2.2,size=18),
          plot.margin = margin(t = 0, r = 2, b = 0, l = 0, unit = "cm"))


  p <- p+stat_pvalue_manual(stat.test,label = "p.side.signif",inherit.aes = F)
  print(p)
  ggsave(filename = paste0('01 - Figure/',figure_name,'.pdf'),width = 2.3,height = 4.5)
  write.csv(stat.test,file = paste0('01 - Figure/',figure_name,'.csv'),row.names = F)
  return(p)
}
pC <- plot_bar_signif('C','AST','AST (U/L)','02 - table/AST.xlsx')
pD <- plot_bar_signif('D','ALT','ALT (U/L)','02 - table/ALT.xlsx')
pE <- plot_bar_signif('E','Streptococcus salivarius','Fold change','02 - table/liver_Streptococcus salivarius.xlsx')
pF <- plot_bar_signif('F','Streptococcus parasanguinis','Fold change','02 - table/liver_Streptococcus parasanguinis.xlsx')
pG <- plot_bar_signif('G','Occludin','Fold change','02 - table/Occludin.xlsx')
pH <- plot_bar_signif('H','Claudin-1','Fold change','02 - table/Claudin-1.xlsx')
pI <- plot_bar_signif('I','Total bacterial load','Total bacterial load','02 - table/Total bacterial load.xlsx')
pJ <- plot_bar_signif('J','IL1B','Fold change','02 - table/IL1B.xlsx')
pK <- plot_bar_signif('K','CXCI-1','Fold change','02 - table/CXCI-1.xlsx')

P <- ggarrange(pC,pD,pE,pF,pF,pG,pH,pI,pJ,pK,ncol = 5,nrow = 2)
P
ggsave(filename = paste0('01 - Figure/','all','.pdf'),width = 16,height = 8)


pSA <- plot_bar_signif('A', 'Streptococcus parasanguinis','Fold change','02 - table/mc_Streptococcus parasanguinis.xlsx')
pSB<- plot_bar_signif('B', 'Veillonella atypica','Fold change','02 - table/mc_Veillonella atypica.xlsx')
pSC <- plot_bar_signif('C', 'Streptococcus salivarius','Fold change','02 - table/mc_Streptococcus salivarius.xlsx')
pSD <- plot_bar_signif('D', 'Veillonella parvula','Fold change','02 - table/mc_Veillonella parvula.xlsx')
pSE <- plot_bar_signif('E', 'Veillonella atypica','Fold change','02 - table/liver_Veillonella atypica.xlsx')
pSF <- plot_bar_signif('F', 'Veillonella parvula','Fold change','02 - table/liver_Veillonella parvula.xlsx')
pSG <- plot_bar_signif('G','TLR-2','Fold change','02 - table/TLR-2.xlsx')
pSH <- plot_bar_signif('H', 'TNF-a','Fold change','02 - table/TNF-a.xlsx')

PS <- ggarrange(pSA,pSB,pSC,pSD,pSE,pSF,pSG,pSH,ncol = 4,nrow = 2)
PS
ggsave(filename = paste0('01 - Figure/','sumplementary','.pdf'),width = 11.5,height = 9)
