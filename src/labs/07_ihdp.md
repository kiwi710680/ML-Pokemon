---
id: 07_ihdp
nav: 7 IHDP
title: Lab 7　IHDP：只有知道真值，才能談 coverage
lead: NHEFS 和 RHC 只能看「方法之間像不像」，看不到「對不對」。IHDP 是半合成資料：共變數來自真實的早產兒介入試驗，結果是照公式模擬的——所以每一份都知道真正的 ATE。這是檢查 bias、RMSE、95% CI 覆蓋率的地方，也是 Lab 4 的 double robustness 和 cross-fitting 在真實共變數上的考試。
toolbox: estimators, realdata, plot
files: data/ihdp/ihdp_npci_1.csv, data/ihdp/ihdp_npci_2.csv, data/ihdp/ihdp_npci_3.csv, data/ihdp/ihdp_npci_4.csv, data/ihdp/ihdp_npci_5.csv, data/ihdp/ihdp_npci_6.csv, data/ihdp/ihdp_npci_7.csv, data/ihdp/ihdp_npci_8.csv, data/ihdp/ihdp_npci_9.csv, data/ihdp/ihdp_npci_10.csv
packages: randomForest
time: 第一次要裝 randomForest；全部約 2–3 分鐘，最慢的是第 04 格
---
| 格 | 在看什麼 | 投影片 |
|---|---|---|
| 01–02 | 一份資料：真值、直接相減、overlap | 56–59、93 |
| 03 | 同一份資料，四種組合 vs 真值 | 81、83–84 |
| 04–05 | 10 份資料：bias、RMSE、coverage | 87、92 |

::: note
資料：Hill (2011) 建構的 IHDP 半合成 benchmark，使用 CEVAE 公開的 10 份（ihdp_npci_1–10；每份 747 人、25 個共變數）。Karim (2026) 用 50 份，所以這裡的覆蓋率蒙地卡羅誤差比較大（約 ±7%），看方向，不要看小數點。真值定義為這份資料的 E[μ₁(X) − μ₀(X)]。
:::

## 01 一份資料：真值與直接相減
```r
d <- load_ihdp(1)
c(n = length(d$A), treated = sum(d$A), truth = d$truth, naive_difference = mean(d$Y[d$A == 1]) - mean(d$Y[d$A == 0]))
```

::: look
真值約 4.0，直接相減約 4.0——這份剛好偏得不多。但「剛好」不能靠：其他份差多少，第 04 格會看到。這份資料檔裡同時存了 μ₀(X)、μ₁(X)（兩個世界的期望值），真實研究永遠不會有這兩欄。
:::

## 02 overlap | 投影片 93
```r
nu <- nuisance(d, "glm", "full")
round(overlap_summary(nu$g_raw, d$A), 3)
overlap_plot(nu$g_raw, d$A, labels = c("treated", "control"))
```

::: look
介入組只有 139 人，ĝ 最大只有 0.60；約 4% 的人 ĝ 小於 0.025 被截斷，最大權重約 30，介入組的有效樣本數剩 83（原本 139）。overlap 比 NHEFS 差、比 LaLonde 好很多——樣本又小，等一下 CI 的寬度會反映這件事。
:::

## 03 同一份資料，四種組合 vs 真值 | 投影片 81、83–84
```r
res1 <- do.call(rbind, lapply(c("glm", "rf"), function(lr) do.call(rbind, lapply(c("full", "crossfit"), function(sc)
  panel(d, nuisance(d, lr, sc, seed = 1, ntree = 100), paste(lr, sc))))))
res1$error <- res1$est - d$truth
res1$covers <- res1$lo <= d$truth & d$truth <= res1$hi
res1
```

::: look
- 這一份資料上偏最多的是 **RF 的 g-computation**（約 −0.3 到 −0.4）：random forest 把 A 的效果平滑掉了（Lab 6 看過的那個問題）。GLM 的 g-computation 這一份只偏 −0.09——但第 04 格會看到，平均起來它偏得最多。
- RF full sample：AIPW 偏低、TMLE 偏高，CI 卻只有 ±0.1，兩個都沒蓋到真值（covers = FALSE）。又錯、又很有自信。
- RF cross-fit：誤差回到 0.05 以內，CI 寬回到 ±0.27，都蓋到。
:::

## 04 10 份資料：誰對準了？CI 真的有 95% 嗎？ | 投影片 87、92
每一份都跑一次 {GLM, RF} × {full, cross-fit}。瀏覽器約 2 分鐘。把 `R` 改成 5 可以更快。

```r
R <- 10
runs <- do.call(rbind, lapply(seq_len(R), function(r) { dr <- load_ihdp(r)
  do.call(rbind, lapply(c("glm", "rf"), function(lr) do.call(rbind, lapply(c("full", "crossfit"), function(sc) {
    p <- panel(dr, nuisance(dr, lr, sc, seed = r, ntree = 100), sc); p$learner <- lr; p$r <- r; p$truth <- dr$truth; p })))) }))
runs$err <- runs$est - runs$truth
runs$cover <- runs$lo <= runs$truth & runs$truth <= runs$hi
grade <- aggregate(cbind(bias = err, mse = err^2, coverage = cover, ci_width = hi - lo) ~ learner + scheme + estimator,
                   data = runs, FUN = mean, na.action = na.pass)
grade$rmse <- sqrt(grade$mse); grade$mse <- NULL
grade <- grade[order(grade$estimator, grade$learner, grade$scheme), ]
print(grade, digits = 3, row.names = FALSE)
```

## 05 畫成 Karim 的三格圖：bias、RMSE、coverage
```r
g <- grade[grade$estimator %in% c("AIPW", "TMLE"), ]
lab <- paste(g$learner, g$scheme, g$estimator)
col <- ifelse(g$scheme == "full", COL$red, COL$teal)
par(mfrow = c(1, 3), mar = c(4.5, 1, 2, 1), oma = c(0, 12, 0, 0), las = 1, bty = "n")
y <- nrow(g):1
plot(g$bias, y, pch = 19, col = col, cex = 1.6, xlim = range(c(0, g$bias)) * 1.2, yaxt = "n", xlab = "bias", main = "Bias (0 is best)")
abline(v = 0, lty = 2); axis(2, at = y, labels = lab, outer = TRUE, tick = FALSE, col.axis = COL$ink, cex.axis = 1.15)
plot(g$rmse, y, pch = 19, col = col, cex = 1.6, xlim = c(0, max(g$rmse) * 1.1), yaxt = "n", xlab = "RMSE", main = "RMSE (lower is better)")
plot(g$coverage, y, pch = 19, col = col, cex = 1.6, xlim = c(0, 1.05), yaxt = "n", xlab = "coverage", main = "95% CI coverage")
abline(v = 0.95, lty = 2)
```

::: look
- 右圖最重要：RF full sample（紅點）的 CI 只蓋到真值 2–3 次（10 次裡），因為過擬合讓 SE 被低估；cross-fit（藍點）回到 10/10，平均 CI 寬從約 0.53 變成約 1.27。
- 中圖右下角那個點：RF full TMLE 的 RMSE 約 1.3，比別人大三倍——10 份裡有一份，TMLE 拿過擬合的 Q̂ 去 fluctuate，整個飛掉。平均 bias 看起來不大，是因為有正有負互相抵掉；這就是為什麼要同時看 bias 和 RMSE。
- GLM 的 full 和 cross-fit 覆蓋率都是 1.0：低容量的模型背不起來，所以 full sample 也沒事——和 Lab 6 的 GLM 一樣。但 GLM cross-fit 的 RMSE 和 CI 寬度（約 2.7）反而比 full 大：747 人切 5 折，每次只用 600 人去配 25 個變數的 logistic，ĝ 更不穩、極端權重更多。**cross-fitting 不是免費的**——樣本小、模型本來就不會背答案的時候，它的代價可能大於好處（Lab 4 第 05 格的「想一想」）。
- 覆蓋率 1.0 不是「剛好 95%」，而是偏保守（CI 偏寬）；Karim 用 50 份資料看到 DML／CV-TMLE 0.99，也是保守的。
:::

::: think 只有 10 份資料，覆蓋率 0.3 和 1.0 的差別可信嗎？
覆蓋率的蒙地卡羅標準誤約 √(p(1−p)/10)：p = 0.95 時約 0.07。0.3 和 1.0 差了 0.7，遠超過這個誤差，方向可信；但 0.9 和 1.0 的差別就不能下結論。Karim 也提醒：MCSE 要以「獨立的資料份數」為單位算，同一份資料換 20 種切法不會變成 20 份獨立資料。
:::

::: think 為什麼 GLM 的 g-computation bias 那麼大（約 −0.7），AIPW 卻小很多？
IHDP 的結果是用非線性公式生成的，主效應線性模型的 Q̂ 系統性地錯了（first-order bias）。AIPW 的修正項用 ĝ 加權的殘差把它補回來——這就是 Lab 4 第 03 格「Q 錯、g 大致對」那一格的 double robustness，在真實共變數上親眼看到。
:::

::: punch
在不知道真值的資料上，「幾個方法一致」只是安心；在知道真值的資料上，才看得到誰的 CI 在說謊。
:::
