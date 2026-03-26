rm(list =ls())
library(psych)
library(corrplot)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(openxlsx)
library(pheatmap)
library(readxl)

file.path <- ("D:\\UserData\\Desktop\\新冠项目\\未接种疫苗IRT.xlsx")
data_sheet1 <- read_excel(file.path, sheet = "5months")
data_sheet2 <- read_excel(file.path, sheet = "6months")
data_sheet3 <- read_excel(file.path, sheet = "2months")

data1 <- data_sheet1[, -c(1:3,8)]
data2 <- data_sheet2[, -c(1:3,8)]
data3 <- data_sheet3[, -c(1:3,8)]

cor_matrix <- corr.test(data2,use ="pairwise",method ="spearman",adjust = "fdr", alpha = 0.05)
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
         cellheight = 30,
color=colorRampPalette(c("#0000FF","#FFFFFF","#FF0000"))(50))

