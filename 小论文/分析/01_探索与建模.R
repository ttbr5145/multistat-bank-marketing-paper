# 多元统计分析小论文 —— 银行营销数据（UCI222）探索性分析与建模
# 数据编号 11 / 吴桐 2024213023
rm(list = ls())
options(stringsAsFactors = FALSE)

dir_out  <- "D:/AI/多统上机/小论文/分析"
dir_fig  <- file.path(dir_out, "图")
dir_tab  <- file.path(dir_out, "表")
for (p in c(dir_out, dir_fig, dir_tab)) if (!dir.exists(p)) dir.create(p, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl); library(openxlsx); library(pROC); library(ggplot2); library(dplyr)
  library(cluster)
})

## ---------- 1. 读取与清洗 ----------
f <- "D:/AI/多统上机/小论文/小论文数据/11-银行营销数据_UCI222_BankMarketing.xlsx"
d0 <- read_excel(f, sheet = 1)
stopifnot(ncol(d0) == 17)
names(d0) <- c("age","job","marital","edu","default","balance","housing","loan",
               "contact","day","month","duration","campaign","pdays","previous",
               "poutcome","y")

lab <- c(age="年龄", job="职业", marital="婚姻状况", edu="教育程度", default="信用违约",
         balance="年均余额", housing="住房贷款", loan="个人贷款", contact="联系方式",
         day="联系日", month="联系月份", duration="联系时长", campaign="本次联系次数",
         pdays="距上次联系天数", previous="此前联系次数", poutcome="上次营销结果",
         y="是否认购定期存款", ageg="年龄组")

d <- d0
d$y <- factor(d$y, levels = c("否","是"))
for (v in c("job","marital","edu","default","housing","loan","contact","month","poutcome"))
  d[[v]] <- factor(d[[v]])
d$ageg <- cut(d$age, breaks = c(-Inf,30,40,50,60,Inf),
              labels = c("29岁及以下","30-39","40-49","50-59","60岁及以上"))
d$durmin <- d$duration / 60

sink(file.path(dir_out, "01_分析日志.txt"), split = TRUE)
cat("数据规模：", nrow(d), "行 x", ncol(d), "列\n")
cat("认购分布：\n"); print(table(d$y)); cat("认购率：", round(mean(d$y=="是")*100,2), "%\n")
cat("缺失值总数：", sum(is.na(d0)), "\n")
cat("从未联系过（pdays = -1）：", sum(d$pdays == -1), "（", round(mean(d$pdays==-1)*100,2), "%）\n\n")

## ---------- 2. 描述统计 ----------
num_v <- c("age","balance","day","duration","campaign","pdays","previous")
desc <- do.call(rbind, lapply(num_v, function(v){
  x <- d[[v]]
  data.frame(变量 = lab[v], 均值 = mean(x), 标准差 = sd(x), 最小值 = min(x),
             下四分位 = quantile(x,.25), 中位数 = median(x), 上四分位 = quantile(x,.75),
             最大值 = max(x))
}))
desc[ ,-1] <- lapply(desc[ ,-1], function(z) round(z, 3))
cat("== 数值变量描述统计 ==\n"); print(desc, row.names = FALSE); cat("\n")

cat_v <- c("job","marital","edu","default","housing","loan","contact","month","poutcome")
catres <- do.call(rbind, lapply(cat_v, function(v){
  tb <- table(d[[v]])
  data.frame(变量 = lab[v], 类别 = names(tb), 频数 = as.integer(tb),
             占比 = round(as.numeric(prop.table(tb))*100, 2))
}))
cat("== 分类变量分布（前 20 行）==\n"); print(head(catres, 20), row.names = FALSE); cat("\n")

rate_by <- function(v, nm){
  tb <- d %>% group_by(lv = .data[[v]]) %>%
    summarise(n = n(), 认购数 = sum(y=="是"), 认购率 = round(mean(y=="是")*100, 2), .groups="drop")
  names(tb)[1] <- "类别"
  tb <- as.data.frame(tb); tb$变量 <- nm; tb[ ,c("变量","类别","n","认购数","认购率")]
}
rates <- do.call(rbind, lapply(c("job","marital","edu","default","housing","loan",
                                 "contact","month","poutcome","ageg"),
                               function(v) rate_by(v, lab[v])))
cat("== 各分类水平认购率（前 20 行）==\n"); print(head(rates, 20), row.names = FALSE); cat("\n")

## ---------- 3. 卡方检验与 Cramer's V ----------
cv <- function(tb){
  chi <- suppressWarnings(chisq.test(tb))
  k <- min(dim(tb)) - 1; n <- sum(tb)
  data.frame(卡方 = round(as.numeric(chi$statistic),2), 自由度 = as.integer(chi$parameter),
             p值 = ifelse(chi$p.value < 2.2e-16, "< 2.2e-16", formatC(chi$p.value, format="g", digits=4)),
             CramerV = round(sqrt(as.numeric(chi$statistic)/(n*k)), 4))
}
chi_tab <- do.call(rbind, lapply(c(cat_v,"ageg"), function(v) cbind(变量 = lab[v], cv(table(d[[v]], d$y)))))
names(chi_tab)[1] <- "变量"; rownames(chi_tab) <- NULL
cat("== 卡方独立性检验（与是否认购）==\n"); print(chi_tab, row.names = FALSE); cat("\n")

## ---------- 4. Logistic 回归：两个模型 ----------
# 模型A：营销前即可获得的信息（不含通话时长，可用于筛选拨打名单）
# 模型B：加入通话时长，用于解释“沟通质量”，但只能在通话结束后获得
varsA <- c("age","job","marital","edu","default","balance","housing","loan",
           "campaign","previous","poutcome","month")
fmlA <- as.formula(paste("y ~", paste(varsA, collapse = " + ")))
fmlB <- update(fmlA, . ~ . + duration)

mA <- glm(fmlA, data = d, family = binomial)
mB <- glm(fmlB, data = d, family = binomial)

fit_tab <- data.frame(
  模型 = c("A 营销前可用", "B 含通话时长"),
  参数个数 = c(length(coef(mA)), length(coef(mB))),
  AIC = round(c(AIC(mA), AIC(mB)), 2),
  偏差 = round(c(deviance(mA), deviance(mB)), 2),
  McFaddenR2 = round(c(1 - deviance(mA)/mA$null.deviance, 1 - deviance(mB)/mB$null.deviance), 4)
)
cat("== 模型整体比较 ==\n"); print(fit_tab, row.names = FALSE); cat("\n")

ORtab <- function(m){
  s <- summary(m)$coefficients; ci <- confint.default(m)
  out <- data.frame(变量 = rownames(s), 系数 = round(s[,1],4), 标准误 = round(s[,2],4),
                    沃尔德z = round(s[,3],3),
                    p值 = ifelse(s[,4] < 2.2e-16, "< 2.2e-16", formatC(s[,4], format="g", digits=4)),
                    OR = round(exp(s[,1]),4),
                    OR下限 = round(exp(ci[,1]),4), OR上限 = round(exp(ci[,2]),4))
  rownames(out) <- NULL; out
}
OR_A <- ORtab(mA); OR_B <- ORtab(mB)
cat("== 模型 A 系数（营销前可用）==\n"); print(OR_A, row.names = FALSE); cat("\n")
cat("== 模型 B 关键项（通话时长）==\n")
print(OR_B[OR_B$变量 == "duration", ], row.names = FALSE); cat("\n")

## 预测、AUC、判别能力
eva <- function(m, nm){
  p <- fitted(m)
  r <- roc(d$y, p, levels = c("否","是"), direction = "<", quiet = TRUE)
  a <- as.numeric(auc(r)); ci <- as.numeric(ci.auc(r))
  bt <- coords(r, "best", ret = c("threshold","sensitivity","specificity"), transpose = FALSE)
  pred <- ifelse(p >= bt$threshold[1], "是", "否")
  cm <- table(实际 = d$y, 预测 = pred)
  acc <- sum(diag(cm))/sum(cm)
  data.frame(模型 = nm, AUC = round(a,4), AUC下限 = round(ci[1],4), AUC上限 = round(ci[3],4),
             阈值 = round(bt$threshold[1],4), 灵敏度 = round(cm["是","是"]/sum(cm["是",])*100,2),
             特异度 = round(cm["否","否"]/sum(cm["否",])*100,2), 准确率 = round(acc*100,2),
             预测阳性数 = sum(pred == "是"))
}
eva_tab <- rbind(eva(mA, "A 营销前可用"), eva(mB, "B 含通话时长"))
cat("== 判别能力（Youden 最佳阈值）==\n"); print(eva_tab, row.names = FALSE); cat("\n")
cat("模型 A 混淆矩阵：\n"); print(table(实际 = d$y, 预测 = ifelse(fitted(mA) >= eva(mA,"A")$阈值, "是", "否"))); cat("\n")

## Hosmer-Lemeshow（十分组，模型 A）
hl <- function(y, p, g = 10){
  br <- unique(quantile(p, probs = seq(0, 1, length.out = g + 1)))
  grp <- cut(p, breaks = br, include.lowest = TRUE, labels = FALSE)
  o1 <- tapply(as.integer(y == "是"), grp, sum); e1 <- tapply(p, grp, sum); n <- tapply(p, grp, length)
  g2 <- length(unique(grp)); ch <- sum((o1-e1)^2/(e1*(1-e1/n)))
  c(卡方 = round(ch,3), 自由度 = g2-2, p值 = formatC(pchisq(ch, g2-2, lower.tail = FALSE), format="g", digits=4))
}
cat("Hosmer-Lemeshow（模型 A）：", paste(hl(d$y, fitted(mA)), collapse="  "), "\n")
cat("注：样本量 45211，HL 检验对微小偏离极敏感，p 值偏小属预期，需结合校准图解读。\n\n")

## 逐步变量选择（AIC，模型 A）
stepA <- tryCatch(step(mA, direction = "both", trace = 0), error = function(e) NULL)
if (!is.null(stepA)) {
  cat("逐步回归（AIC 双向）入选变量：\n"); print(names(coef(stepA))[-1])
  cat("简化后 AIC =", round(AIC(stepA),2), " 相对全模型 ΔAIC =", round(AIC(stepA)-AIC(mA),2), "\n\n")
}

## ---------- 5. K-means 客户分群 ----------
km_v <- c("age","balance","duration")
X <- scale(d[, km_v])
set.seed(20261122)
wss <- sapply(1:8, function(k) kmeans(X, k, nstart = 25, iter.max = 50)$tot.withinss)
set.seed(20261122)
km <- kmeans(X, 4, nstart = 50, iter.max = 100)
idx <- sample(nrow(X), 5000)
sil <- tryCatch(mean(silhouette(km$cluster[idx], dist(X[idx, ]))[, 3]), error = function(e) NA)
cat("== K-means 分群（k=4，", paste(lab[km_v], collapse="、"), "）==\n")
cat("平均轮廓系数（5000 抽样）：", round(sil, 4), "\n")
prof <- d %>% mutate(簇 = factor(km$cluster)) %>% group_by(簇) %>%
  summarise(人数 = n(), 年龄均值 = round(mean(age),1), 余额均值 = round(mean(balance),1),
            时长均值秒 = round(mean(duration),1), 认购率 = round(mean(y=="是")*100,2), .groups="drop") %>%
  as.data.frame()
prof$人数占比 <- round(prof$人数/nrow(d)*100, 2)
cat("各簇画像：\n"); print(prof, row.names = FALSE); cat("\n")
cat("WSS（k=1..8）：", paste(round(wss,1), collapse=", "), "\n\n")

## ---------- 6. 图形 ----------
zh <- theme_bw(base_family = "SimSun") +
  theme(plot.title = element_text(hjust = .5, size = 11),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8))

ggsave(file.path(dir_fig, "图1_上次营销结果与认购.png"),
       ggplot(d, aes(x = poutcome, fill = y)) + geom_bar(position = "fill") +
         scale_y_continuous(labels = scales::percent) +
         labs(x = "上次营销结果", y = "构成比例", title = "图1  上次营销结果与本次认购构成", fill = "是否认购") + zh,
       width = 6, height = 3.4, dpi = 300)

ggsave(file.path(dir_fig, "图2_年龄组与认购.png"),
       ggplot(d, aes(x = ageg, fill = y)) + geom_bar(position = "fill") +
         scale_y_continuous(labels = scales::percent) +
         labs(x = "年龄组", y = "构成比例", title = "图2  年龄组与认购构成", fill = "是否认购") + zh,
       width = 6, height = 3.4, dpi = 300)

ggsave(file.path(dir_fig, "图3_联系时长箱线图.png"),
       ggplot(d, aes(x = y, y = durmin, fill = y)) + geom_boxplot(show.legend = FALSE) +
         coord_cartesian(ylim = c(0, 8)) +
         labs(x = "是否认购", y = "联系时长（分钟）", title = "图3  联系时长与认购结果") + zh,
       width = 5, height = 3.6, dpi = 300)

png(file.path(dir_fig, "图4_ROC曲线对比.png"), width = 1800, height = 1500, res = 300, family = "SimSun")
rA <- roc(d$y, fitted(mA), levels = c("否","是"), direction = "<", quiet = TRUE)
rB <- roc(d$y, fitted(mB), levels = c("否","是"), direction = "<", quiet = TRUE)
plot(rA, col = "#2E5A88", lwd = 2, legacy.axes = TRUE,
     main = "图4  两个 Logistic 模型的 ROC 曲线", xlab = "1 - 特异度", ylab = "灵敏度", cex.main = 1.05)
plot(rB, col = "#C0392B", lwd = 2, add = TRUE)
abline(0, 1, lty = 2, col = "gray50")
legend("bottomright", bty = "n", cex = .95,
       legend = c(sprintf("模型A（营销前）AUC = %.3f", as.numeric(auc(rA))),
                  sprintf("模型B（含时长）AUC = %.3f", as.numeric(auc(rB)))),
       col = c("#2E5A88", "#C0392B"), lwd = 2)
dev.off()

png(file.path(dir_fig, "图5_聚类WSS.png"), width = 2100, height = 1500, res = 300, family = "SimSun")
plot(1:8, wss, type = "b", pch = 19, col = "#2E5A88",
     main = "图5  K-means 组内平方和随聚类数变化", xlab = "聚类数 k", ylab = "组内平方和 WSS")
dev.off()

## ---------- 7. 导出 ----------
wb <- createWorkbook()
for (nm in c("数值描述统计","分类分布","分类认购率","卡方检验","模型比较","模型A系数","模型B系数","判别能力","分群画像")) {
  addWorksheet(wb, nm)
  writeData(wb, nm, switch(nm,
    "数值描述统计" = desc, "分类分布" = catres, "分类认购率" = rates, "卡方检验" = chi_tab,
    "模型比较" = fit_tab, "模型A系数" = OR_A, "模型B系数" = OR_B,
    "判别能力" = eva_tab, "分群画像" = prof))
}
saveWorkbook(wb, file.path(dir_tab, "01_分析结果表.xlsx"), overwrite = TRUE)
write.csv(eva_tab, file.path(dir_tab, "01_判别能力.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("图片：", length(list.files(dir_fig)), " 张；表格：", length(list.files(dir_tab)), " 个\n")
cat("会话信息：R", R.version$major, R.version$minor, "\n")
sink()
