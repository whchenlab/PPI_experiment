# plot ----
load('00 - data/marker.heatmap.RData')
load('00 - data/marker.heatmap.index.RData')
# [5] "LC_PRJNA431746(16S)"                
marker.heatmap <- marker.heatmap[,c("LC_PRJNA471972(16S)",
                                    "LC_PRJEB6337.dis(WGS)",              
                                    "LC_PRJEB6337.val(WGS)",
                                    "NAFLD_PRJNA246121(16S)",             
                                    "NAFLD_PRJNA540738(16S)",
                                    "VH_PRJNA540738(16S)",
                                    "Cholestasis_PRJNA540738(16S)",
                                    "ALD_PRJNA540738(16S)",
                                    "LC_control_non-PPI_PRJNA431746(16S)",
                                    "LC_control_PPI_PRJNA431746(16S)",
                                    "PPI_LC_PRJNA431746(16S)",
                                    "PPI_CLD_Bonn_cirrhosis(16S)",        
                                    "PPI_CLD_Bonn_non-cirrhosis(16S)",   
                                    "PPI_Health_self(WGS)"
                                    )]
marker.heatmap.index <- marker.heatmap.index[,colnames(marker.heatmap)]
# colnames(marker.heatmap)
marker.heatmap.index[marker.heatmap.index==0] <- 'NANA'
bk <- c(seq(-5,-0.1,by=0.01),seq(0,5,by=0.01))

P <- pheatmap(marker.heatmap,border_color=NA,
               scale = "none",
               color = c(colorRampPalette(colors = c("#4575B4","white"))(length(bk)/2),colorRampPalette(colors = c("white","#D9352A"))(length(bk)/2)),
               legend_breaks=seq(-5,5,1),
               breaks=bk,cluster_rows = F,cluster_cols = F,angle_col = '90',
              display_numbers = marker.heatmap.index,
              fontsize = 10,
              fontsize_row = 10,
              fontsize_col = 10,
              fontsize_number = 14
              )
P

pdf(file = '02 - Figure/Fig1A_index_control_name_pailie_col.pdf',width = 6,height = 4)
P
dev.off()

