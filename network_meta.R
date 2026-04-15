# =========================================
# HPV 网络Meta分析（最终稳定版｜RR｜真实节点大小）
# =========================================

# 1️⃣ 清空环境（必须）
rm(list = ls())
cat("\014")

# 2️⃣ 加载包
packages <- c("readxl", "netmeta")
for(p in packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

# 3️⃣ 读取数据
data <- read_excel("C:/Users/高磊/OneDrive/桌面/HPV网状.xlsx")

# 4️⃣ 基础处理
data$study <- as.character(data$study)
data$treatment <- as.character(data$treatment)

# =========================================
# 5️⃣ 合并对照组
# =========================================
data$treatment[data$treatment %in% c("安慰剂", "placebo", "自然随访", "随访", "observation")] <- "NC"

# =========================================
# 6️⃣ 确保event是“转阴人数”
# =========================================
# 如果你填的是未转阴，请取消注释👇
# data$event <- data$n - data$event

# 防止误填百分比
if(max(data$event, na.rm = TRUE) > max(data$n, na.rm = TRUE)){
  data$event <- round(data$event / 100 * data$n)
}

# =========================================
# 7️⃣ 两两比较（RR）
# =========================================
pw <- pairwise(
  treat = treatment,
  event = event,
  n = n,
  studlab = study,
  data = data,
  sm = "RR"
)

# =========================================
# 8️⃣ 网络Meta分析
# =========================================
nma <- netmeta(
  TE = pw$TE,
  seTE = pw$seTE,
  treat1 = pw$treat1,
  treat2 = pw$treat2,
  studlab = pw$studlab,
  sm = "RR",
  random = TRUE
)

# =========================================
# 9️⃣ ⭐计算真实节点大小（关键修复）
# =========================================
trt_n <- tapply(data$n, data$treatment, sum)

# 对齐顺序（非常重要）
trt_n <- trt_n[nma$trts]

# =========================================
# 🔟 📊 网络图（真实比例版）
# =========================================
pdf("C:/Users/高磊/OneDrive/桌面/HPV_网络图_RR_最终.pdf",
    width = 8, height = 6)

netgraph(nma,
         plastic = TRUE,
         cex = 1.4,
         cex.points = sqrt(trt_n),   # ⭐关键：真实样本量
         thickness = "number.of.studies",
         col.points = "darkred",
         col = "gray40")

dev.off()

# =========================================
# 1️⃣1️⃣ 🌲 森林图
# =========================================
pdf("C:/Users/高磊/OneDrive/桌面/HPV_森林图_RR_最终.pdf",
    width = 10, height = 7)

forest(nma,
       fontsize = 11,
       squaresize = 1.2,
       lwd = 2,
       col.square = "black",
       col.diamond = "darkblue",
       col.refline = "red",
       xlab = "Risk Ratio",
       main = "Network Meta-analysis (RR)")

dev.off()

# =========================================
# 1️⃣2️⃣ 导出结果
# =========================================
result <- as.data.frame(summary(nma)$random)

write.csv(result,
          "C:/Users/高磊/OneDrive/桌面/HPV_RR结果_最终.csv",
          row.names = TRUE)

# =========================================
# 1️⃣3️⃣ 排名（新版）
# =========================================
rank <- netrank(nma, small.values = "bad")

rank_df <- data.frame(
  treatment = names(rank$ranking.random),
  ranking = round(rank$ranking.random, 4)
)

write.csv(rank_df,
          "C:/Users/高磊/OneDrive/桌面/HPV_RR排序_最终.csv",
          row.names = FALSE)

# =========================================
# 完成提示
# =========================================
cat("=====================================\n")
cat("✅ 分析完成（RR｜节点大小已修复）\n")
cat("📄 所有结果已输出到桌面\n")
cat("=====================================\n")