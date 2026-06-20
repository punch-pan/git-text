# ============================================================================
# 复现 Christensen et al. (2018) - 可复现性检验完整代码
# 遵循《心理学院R编程语言课程可重复检验指南(2025版)》
# 作者：潘安祺 | 单人作业
# 日期：2025-06
# ============================================================================

# ----------------------------- 0. 环境准备 ----------------------------------
rm(list = ls())
# 工作目录（根据你实际存放数据的位置修改，当前为示例路径）
setwd("~/Desktop/osfstorage-archive")

# 加载所需包
library(psych)
library(NetworkToolbox)
library(igraph)
library(SemNeT)
library(SemNetCleaner)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# 设置随机种子，保证结果可复现
set.seed(2025)

# ----------------------------- 1. 数据加载 ----------------------------------
load("Data/Saved R Cleaning Files/FINAL Cleaning File.RData")
openness <- read.csv("Data/Cleaned FINAL/FINAL open.csv", stringsAsFactors = FALSE)

# 数据格式转换
fluency <- as.matrix(con)
mode(fluency) <- "numeric"
latent_var <- openness$no_int

# 校验被试数量一致性
stopifnot(nrow(fluency) == length(latent_var))

# ----------------------------- 2. 分组 --------------------------------------
order_idx <- order(latent_var)
low_idx <- order_idx[1:258]
high_idx <- order_idx[259:516]
low_resp <- fluency[low_idx, ]
high_resp <- fluency[high_idx, ]

# ----------------------------- 3. 描述性统计 --------------------------------
# 每组总反应数
low_sum <- rowSums(low_resp)
high_sum <- rowSums(high_resp)
mean_low <- mean(low_sum); sd_low <- sd(low_sum)
mean_high <- mean(high_sum); sd_high <- sd(high_sum)

# 相关性分析
total_sum <- c(low_sum, high_sum)
total_latent <- c(latent_var[low_idx], latent_var[high_idx])
cor_test <- cor.test(total_sum, total_latent)

# 独立样本t检验 + Cohen's d效应量
t_test <- t.test(high_sum, low_sum, var.equal = TRUE)
pooled_sd <- sqrt(((257)*var(low_sum) + 257*var(high_sum)) / 514)
cohen_d <- (mean_high - mean_low)

# McNemar检验（独特反应数）
low_present <- colSums(low_resp) > 0
high_present <- colSums(high_resp) > 0
present_mat <- data.frame(low=low_present, high=high_present)
present_mat <- present_mat[low_present | high_present, ]
tab <- table(present_mat$low, present_mat$high)
mcnemar_test <- mcnemar.test(tab)
phi <- sqrt(mcnemar_test$statistic / nrow(present_mat))

# ------------------- 4. 原文献方法：语义网络指标 ---------------------------
# 过滤低频词汇
low_final <- low_resp[, colSums(low_resp)>=2, drop=F]
high_final <- high_resp[, colSums(high_resp)>=2, drop=F]
common_cols <- intersect(colnames(low_final), colnames(high_final))
low_eq <- low_final[, common_cols, drop=F]
high_eq <- high_final[, common_cols, drop=F]

# 自定义余弦相似度函数
cosine_sim <- function(mat, addConstant=0.01){
  mat <- as.matrix(mat) + addConstant
  cp <- tcrossprod(mat)
  nrm <- sqrt(rowSums(mat^2))
  cp / outer(nrm, nrm)
}

# 计算余弦相似度矩阵
cos_low <- cosine_sim(t(low_eq))
cos_high <- cosine_sim(t(high_eq))

# 【修复TMFG报错】兼容新旧版本包
net_low <- NetworkToolbox::TMFG(cos_low, normal = FALSE)
net_high <- NetworkToolbox::TMFG(cos_high, normal = FALSE)
if(is.list(net_low)) net_low <- net_low$A
if(is.list(net_high)) net_high <- net_high$A

# 邻接矩阵二值化
net_low <- ifelse(net_low > 0, 1, 0)
net_high <- ifelse(net_high > 0, 1, 0)

# 转为网络图对象
g_low <- graph_from_adjacency_matrix(net_low, "undirected")
g_high <- graph_from_adjacency_matrix(net_high, "undirected")

# 提取最大连通分量（处理不连通网络）
if(!is_connected(g_low)){
  cl <- components(g_low); g_low <- induced_subgraph(g_low, which(cl$membership==which.max(cl$csize)))
}
if(!is_connected(g_high)){
  cl <- components(g_high); g_high <- induced_subgraph(g_high, which(cl$membership==which.max(cl$csize)))
}

# 计算网络核心指标
aspl_low <- mean_distance(g_low)
aspl_high <- mean_distance(g_high)
cc_low <- transitivity(g_low, "global")
cc_high <- transitivity(g_high, "global")
q_low <- modularity(cluster_louvain(g_low))
q_high <- modularity(cluster_louvain(g_high))

# ------------------- 5. 创新方法：最小生成树MST（无需额外包，零报错）------------------------
g_mst_low <- mst(g_low)
g_mst_high <- mst(g_high)

aspl_ebic_low <- mean_distance(g_mst_low)
aspl_ebic_high <- mean_distance(g_mst_high)
q_ebic_low <- modularity(cluster_louvain(g_mst_low))
q_ebic_high <- modularity(cluster_louvain(g_mst_high))

# ------------------- 6. 随机网络检验 ---------------------------------------
rand_aspl_low <- replicate(1000, {
  g <- sample_gnm(vcount(g_low), ecount(g_low), F)
  mean_distance(g)
})
z_low <- (aspl_low - mean(rand_aspl_low))/sd(rand_aspl_low)

rand_aspl_high <- replicate(1000, {
  g <- sample_gnm(vcount(g_high), ecount(g_high), F)
  mean_distance(g)
})
z_high <- (aspl_high - mean(rand_aspl_high))/sd(rand_aspl_high)

# ------------------- 7. 论文数值 & 复现数值 -------------------------------
paper <- list(
  lowM=16.41, lowSD=4.49, highM=17.83, highSD=4.63,
  cor=0.17, d=0.24, mcn=16.91, phi=0.22,
  low_aspl=3.19, low_cc=1.03, low_q=0.590,
  high_aspl=2.84, high_cc=1.05, high_q=0.521
)

repro <- list(
  lowM=mean_low, lowSD=sd_low, highM=mean_high, highSD=sd_high,
  cor=cor_test$est, d=cohen_d, mcn=mcnemar_test$stat, phi=phi,
  low_aspl=aspl_low, low_cc=cc_low, low_q=q_low,
  high_aspl=aspl_high, high_cc=cc_high, high_q=q_high
)

# 百分误差计算函数 & 评级函数
pe <- function(r,p) abs(r-p)/abs(p)*100
grade <- function(x) ifelse(x==0,"完全一致",ifelse(x<10,"次要偏差","主要偏差"))

# ------------------- 8. 生成8张标准表格（中文命名，路径统一）-------------------
# 结果保存文件夹（固定路径，自动创建）
results_dir <- "~/Desktop/My_Replication_Results"
dir.create(results_dir, showWarnings = FALSE)

# 表2 描述性统计结果对比
tab2 <- data.frame(
  指标 = c("低组M","低组SD","高组M","高组SD"),
  原文 = c(paper$lowM,paper$lowSD,paper$highM,paper$highSD),
  复现 = c(repro$lowM,repro$lowSD,repro$highM,repro$highSD),
  PE = round(c(pe(repro$lowM,paper$lowM),pe(repro$lowSD,paper$lowSD),
               pe(repro$highM,paper$highM),pe(repro$highSD,paper$highSD)),2),
  评级 = sapply(c(pe(repro$lowM,paper$lowM),pe(repro$lowSD,paper$lowSD),
                pe(repro$highM,paper$highM),pe(repro$highSD,paper$highSD)),grade)
)

# 表3 原方法推断性统计对比
tab3 <- data.frame(
  指标 = c("相关r","Cohen's d","McNemarχ2","φ","低ASPL","高ASPL","低Q","高Q"),
  原文 = c(paper$cor,paper$d,paper$mcn,paper$phi,
         paper$low_aspl,paper$high_aspl,paper$low_q,paper$high_q),
  复现 = c(repro$cor,repro$d,repro$mcn,repro$phi,
         repro$low_aspl,repro$high_aspl,repro$low_q,repro$high_q),
  PE = round(c(pe(repro$cor,paper$cor),pe(repro$d,paper$d),
               pe(repro$mcn,paper$mcn),pe(repro$phi,paper$phi),
               pe(repro$low_aspl,paper$low_aspl),pe(repro$high_aspl,paper$high_aspl),
               pe(repro$low_q,paper$low_q),pe(repro$high_q,paper$high_q)),2),
  评级 = sapply(c(pe(repro$cor,paper$cor),pe(repro$d,paper$d),
                pe(repro$mcn,paper$mcn),pe(repro$phi,paper$phi),
                pe(repro$low_aspl,paper$low_aspl),pe(repro$high_aspl,paper$high_aspl),
                pe(repro$low_q,paper$low_q),pe(repro$high_q,paper$high_q)),grade)
)

# 表4 创新方法推断性统计
tab4 <- data.frame(
  指标 = c("低ASPL","高ASPL","低Q","高Q"),
  复现值 = c(aspl_ebic_low,aspl_ebic_high,q_ebic_low,q_ebic_high),
  方向一致性 = c(ifelse(aspl_ebic_high<aspl_ebic_low,"一致","不一致"),
            ifelse(q_ebic_high<q_ebic_low,"一致","不一致"))
)

# 表5 结果可复现性评估
all_grades <- c(tab2$评级, tab3$评级)
tab5 <- as.data.frame(table(all_grades)) %>% 
  rename(评级 = all_grades, 数量 = Freq) %>%
  mutate(占比 = round(数量/sum(数量)*100,1))

# 表6 原方法推论一致性
tab6 <- data.frame(
  假设 = c("H1:高组ASPL更低","H2:高组CC更高","H3:高组Q更低"),
  原文结论 = c("支持","支持","支持"),
  复现结论 = c(ifelse(aspl_high<aspl_low,"支持","不支持"),
           ifelse(cc_high>cc_low,"支持","不支持"),
           ifelse(q_high<q_low,"支持","不支持")),
  一致性 = c(ifelse(aspl_high<aspl_low,"一致","不一致"),
          ifelse(cc_high>cc_low,"一致","不一致"),
          ifelse(q_high<q_low,"一致","不一致"))
)

# 表7 创新方法推论一致性
tab7 <- data.frame(
  假设 = c("H1","H3"),
  复现结论 = c(ifelse(aspl_ebic_high<aspl_ebic_low,"支持","不支持"),
           ifelse(q_ebic_high<q_ebic_low,"支持","不支持")),
  一致性 = c(ifelse(aspl_ebic_high<aspl_ebic_low,"一致","不一致"),
          ifelse(q_ebic_high<q_ebic_low,"一致","不一致"))
)

# 统一保存表格（文件名和后续读取严格对应，杜绝路径错误）
write.csv(tab2, file.path(results_dir, "table2_desc_compare.csv"), row.names = FALSE)
write.csv(tab3, file.path(results_dir, "table3_infer_compare.csv"), row.names = FALSE)
write.csv(tab4, file.path(results_dir, "table4_new_method.csv"), row.names = FALSE)
write.csv(tab5, file.path(results_dir, "table5_reproducibility.csv"), row.names = FALSE)
write.csv(tab6, file.path(results_dir, "table6_consistency_old.csv"), row.names = FALSE)
write.csv(tab7, file.path(results_dir, "table7_consistency_new.csv"), row.names = FALSE)


# ============================================================================
# 全英文绘图
# ============================================================================
if (!require("ggtext")) install.packages("ggtext")
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggtext)

# 固定路径
results_dir <- "~/Desktop/My_Replication_Results"
fig_dir <- file.path(results_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# 配色
color_paper <- "#4A4A4A"
color_repro <- "#2E86AB"
color_major <- "#D64933"
color_minor <- "#F4A261"
color_consistent <- "#2E8B57"
color_inconsistent <- "#D64933"

# 主题
theme_nature <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(color = "black", size = 11),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

# 读取文件（自动适配你的表格，不做任何列名筛选！）
desc <- read.csv(file.path(results_dir, "table2_desc_compare.csv"), stringsAsFactors = FALSE)
infer <- read.csv(file.path(results_dir, "table3_infer_compare.csv"), stringsAsFactors = FALSE)
repro_df <- read.csv(file.path(results_dir, "table5_reproducibility.csv"), stringsAsFactors = FALSE)
consistency <- read.csv(file.path(results_dir, "table6_consistency_old.csv"), stringsAsFactors = FALSE)

# 强制统一英文列名 —— 彻底解决报错
colnames(desc) <- c("Variable", "Paper", "Repro", "PE", "Grade")
colnames(infer) <- c("Variable", "Paper", "Repro", "PE", "Grade")
colnames(repro_df) <- c("Grade", "Count", "Percent")
colnames(consistency) <- c("Hypothesis", "Original", "Replication", "Consistency")

# 1. 描述统计对比图
desc_long <- desc %>%
  pivot_longer(cols = c(Paper, Repro), names_to = "Source", values_to = "Value")

p1 <- ggplot(desc_long, aes(x = Variable, y = Value, fill = Source)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_text(aes(label = round(Value, 2)), position = position_dodge(0.8), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("Paper" = color_paper, "Repro" = color_repro),
                    labels = c("Original Study", "Replication")) +
  labs(title = "Descriptive Statistics Comparison", x = "", y = "Value") +
  theme_nature() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# 2. 百分误差图（已删除报错行！）
p2 <- ggplot(infer, aes(x = reorder(Variable, PE), y = PE, fill = Grade)) +
  geom_col(width = 0.7, color = "black") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray30") +
  geom_text(aes(label = paste0(round(PE, 1), "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("完全一致" = "#2E8B57",
                               "次要偏差" = color_minor,
                               "主要偏差" = color_major)) +
  labs(title = "Percentage Error (PE)", x = "", y = "PE (%)") +
  theme_nature()

# 3. 可复现性环形图
repro_df <- repro_df %>%
  mutate(ymax = cumsum(Count), ymin = c(0, head(ymax, n = -1)))

p3 <- ggplot(repro_df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2, fill = Grade)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  geom_text(x = 3, aes(y = (ymin + ymax)/2, label = paste0(round(Percent, 0), "%")),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("完全一致" = "#2E8B57",
                               "次要偏差" = color_minor,
                               "主要偏差" = color_major)) +
  labs(title = "Reproducibility Rating") +
  theme_nature() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

# 4. 推论一致性图
p4 <- ggplot(consistency, aes(x = Hypothesis, y = 1)) +
  geom_point(aes(color = Consistency), size = 8, shape = 16) +
  geom_text(aes(label = ifelse(Consistency == "一致", "✓", "✗")),
            color = "white", size = 5) +
  scale_color_manual(values = c("一致" = color_consistent,
                                "不一致" = color_inconsistent)) +
  labs(title = "Inferential Consistency", x = "", y = "") +
  theme_nature() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none")

# 组合图
combined <- (p1 + p2) / (p3 + p4) +
  plot_annotation(tag_levels = "A", title = "Reproducibility Analysis Summary") &
  theme(plot.tag = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))

# 保存图片
ggsave(file.path(fig_dir, "descriptive_plot.png"), p1, width = 8, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "pe_plot.png"), p2, width = 8, height = 6, dpi = 300)
ggsave(file.path(fig_dir, "reproducibility_donut.png"), p3, width = 5, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "consistency_plot.png"), p4, width = 8, height = 3, dpi = 300)
ggsave(file.path(fig_dir, "combined_figure.png"), combined, width = 14, height = 10, dpi = 300)

