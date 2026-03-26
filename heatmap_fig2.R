rm(list =ls())
library(psych)
library(corrplot)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(openxlsx)
library(pheatmap)
library(readxl)
setwd("D:\\UserData\\Desktop\\新冠项目")
data <- read_excel("D:\\UserData\\Desktop\\新冠项目\\新冠蛋白表达矩阵.xlsx" )
df_vaccine <- data[data$`various vaccine doses` == "ThreeVx", ]
df_no_vaccine <- data[data$`various vaccine doses` == "NoVx", ]
df_vaccine <- df_vaccine[, -c(1:3,8)]
df_no_vaccine <- df_no_vaccine[, -c(1:3,8)]
#custom_order <- c("D614G", "Beta", "Gamma", "Kappa", "Delta","Alpha","Omicron BA.1","Omicron BA.2","Omicron BA.3","Omicron BA.4","Omicron BA.5","Omicron BF.7","Omicron BQ.1.1","Omicron XBB","Omicron BA.2.75","Omicron XBB.1.5","Omicron EG.5.1","Omicron XBB.1.16","Omicron XBB.1.9.1","Omicron JN.1") 
#df_vaccine <- df_vaccine[, custom_order]
cor_matrix <- corr.test(df_no_vaccine,use ="pairwise",method ="spearman",adjust = "fdr", alpha = 0.05)

r<- cor_matrix$r
p <- cor_matrix$p
#write.xlsx(r,"两组比较相关性系数.xlsx",rowNames=TRUE)
#write.xlsx(p,"两组比较相关性pvalue.xlsx",rowNames=TRUE)
if(!is.null(p)){
  sssmt <- p<0.001
  p[sssmt]<-'***'
  ssmt <- p>0.001&p<0.01
  p[ssmt] <-'**'
  smt <- p >0.01&p<0.05
  p[smt] <- '*'
  p[!sssmt&!ssmt&!smt] <- ''
}else{
  p <-F
}
p_formatted <- round(r, 2)  
pheatmap(r,scale = "none",
         cluster_rows = T,
         cluster_cols = T,
         border_color = NA,
         display_numbers = p_formatted,
         fontsize_number = 9,
         fontsize_row = 12,              
         fontsize_col = 12,
         fontface_row = "bold",  
         fontface_col = "bold",  
         number_color = "black",
         cellwidth = 30,
         cellheight = 30)
#color=colorRampPalette(c("#0000FF","#FFFFFF","#FF0000"))(50))
