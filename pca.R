rm(list = ls())
library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(RColorBrewer)

data <- read_excel("neutralization inhibition rate_data.xlsx" )
df_vaccine <- data[data$`various vaccine doses` == "ThreeVx", ]
df_no_vaccine <- data[data$`various vaccine doses` == "NoVx", ]

new_rownames <- df_no_vaccine$`Sample ID`
new_data <- as.data.frame(df_no_vaccine[, -c(1:3,8)]) 
rownames(new_data) <- new_rownames
group <- df_vaccine$`Intrauterine recovery time (from infection to delivery)`

pca_result <- prcomp(new_data,center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x)
pca_data$group <- group

var_explained <- pca_result$sdev^2/ sum(pca_result$sdev^2)
ggplot(pca_data, aes(x = PC1, y = PC2, color = group)) + 
  geom_point(aes(shape = group), size = 3) +  
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") + 
  labs(x = paste0("(PC1: ", round(var_explained[1] * 100, 2), "%)"),  
       y = paste0("(PC2: ", round(var_explained[2] * 100, 2), "%)")) +  
  stat_ellipse(aes(fill = group, color = group), alpha = 0.2, geom = "polygon") + 
  scale_color_manual(values = c("#66B3FF", "#FF6347","#67C1A5")) +  
  scale_fill_manual(values = c("#66B3FF", "#FF6347","#67C1A5")) + 
  theme_bw() +  
  theme(
    panel.grid = element_blank(),  # No grid lines
    axis.title = element_text(face = "bold", colour = "black", size = 14),  
    axis.text = element_text(face = "bold", colour = "black", size = 12), 
    plot.title = element_text(face = "bold", colour = "black", size = 16, hjust = 0.5),  
    legend.text = element_text(face = "bold", size = 12, color = "black"),  
    legend.title = element_blank(),  
    legend.key.size = unit(1.5, "lines")  
  )

