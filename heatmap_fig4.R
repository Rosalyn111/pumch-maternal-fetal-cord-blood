
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)


samples_groups_file <- "sample group.xlsx"
peptide_data_file <- "peptides IgG_data.xlsx"

all_data <- read_excel(peptide_data_file)
all_data <- melt(all_data)

{

group_df <- read_excel(samples_groups_file, sheet = "Sheet1") %>%
  select(`Sample ID`, `various vaccine doses`) %>%
  rename(sample_id = `Sample ID`, group = `various vaccine doses`) %>%
  filter(!is.na(sample_id))


peptide_data <- read_excel(peptide_data_file) %>%

  mutate(peptide_group = sub("-.*", "", Peptide))


extract_group_data <- function(data, group_df, target_group) {

  target_samples <- group_df %>%
    filter(group == target_group) %>%
    pull(sample_id)
  

  existing_samples <- intersect(target_samples, names(data))
  
  if (length(existing_samples) == 0) {
    warning(paste("没有找到匹配", target_group, "组的样本"))
    return(NULL)
  }
  

  result <- data %>%
    select(Peptide, peptide_group, all_of(existing_samples))
  
  return(result)
}
}


target_peptide_groups <- c("nsp1", "nsp2", "nsp3", "nsp4", "nsp5", "nsp6", 
                           "nsp7", "nsp8", "nsp9", "nsp10", "nsp12", 
                           "nsp13", "nsp14", "nsp15", "nsp16","ORF3a","ORF6","E",
                           "N","S","M","ORF8","ORF10","ORF7a"
)


for (group in target_peptide_groups) {
  cat("正在处理:", group, "\n")
  

  Vx_data <- extract_group_data(peptide_data, group_df, "NoVx") %>%
    filter(peptide_group %in% group)
  
  # 检查数据是否为空
  if (nrow(Vx_data) == 0) {
    cat("警告: ", group, " 组数据为空，跳过\n")
    next
  }
  

  melted_data <- melt(Vx_data, id.vars = c("Peptide", "peptide_group"))
  colnames(melted_data) <- c("Peptide", "PeptideGroup", "Sample", "Value")
  

  num_peptides <- length(unique(melted_data$Peptide))
  num_samples <- length(unique(melted_data$Sample))
  

  p <- ggplot(melted_data, aes(x = Peptide, y = Sample, fill = Value)) +
    geom_tile() +
    coord_fixed(ratio = 0.8) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      limits = c(-2.0, 4),
      oob = scales::squish
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  
    ) +
    ggtitle(paste("NoVx -", group)) + 
    xlab("Peptide") +
    ylab("Sample")
  

  print(p)
  

  output_file <- paste0("landscape_map1/NoVx_", group, ".pdf")
  
  ggsave(output_file,
         plot = p,
         width = max(8, num_peptides * 0.4 + 1), 
         height = max(6, num_samples * 0.2 + 1),  
         units = "in",
         limitsize = FALSE)
  
  cat("已保存: ", output_file, "\n\n")
}

cat("所有热图已保存完成！\n")
