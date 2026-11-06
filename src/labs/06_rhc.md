---
id: 06_rhc
nav: 6 RHC
title: Lab 6　RHC：learner 太會背答案的時候（二元結果）
lead: 加護病房的病人做右心導管（RHC），30 天死亡風險會差多少？5,735 人、66 個共變數、二元結果。Lab 4 在神奇糖果裡用 k = 1 的 kNN 看到「自己改考卷」；這裡在真實資料上用 random forest 再看一次——而且看得更清楚。最後一格看投影片 82 的「結果範圍」：二元結果時，TMLE 為什麼一定落在合法範圍。
toolbox: estimators, realdata, plot
files: data/rhc.csv
packages: randomForest
time: 第一次要裝 randomForest；全部約 2–4 分鐘，最慢的是第 06 格
---
| 格 | 在看什麼 | 投影片 |
|---|---|---|
| 01–03 | 資料、兩組差在哪、overlap | 56–59、91 |
| 04 | GLM：full sample 和 cross-fit 幾乎一樣 | 83–85 |
| 05–06 | random forest：full sample 的殘差被低估 → AIPW、CI 都歪掉；cross-fit 救回來 | 63、83–85、91 |
| 07–08 | 四種組合畫在一起；TMLE 的範圍優勢 | 82 |

::: note
資料：Connors et al. (1996) SUPPORT 研究的 RHC 資料，取自 Vanderbilt Biostatistics 公開資料庫（hbiostat.org/data）。共變數處理和 Karim (2026) 相同：拿掉治療、結果、日期與 ID，以及缺失太多的 cat2、adld3p、urin1，類別變數展開成 0/1。這是觀察資料，**沒有真值**。
:::

## 01 資料與問題
```r
d <- load_rhc("data/rhc.csv")
c(n = length(d$A), covariates = ncol(d$X), had_RHC = sum(d$A), death_rate = mean(d$Y),
  unadjusted_RD = mean(d$Y[d$A == 1]) - mean(d$Y[d$A == 0]))
```

::: look
做 RHC 的人 30 天死亡率高 7.4 個百分點。這是 RHC 害的，還是做 RHC 的人本來就比較重？
:::

## 02 做 RHC 的人，本來就比較重
```r
s <- sapply(d$X, smd, A = d$A)
top <- head(sort(abs(s), decreasing = TRUE), 10)
round(data.frame(SMD = s[names(top)]), 2)
```

::: look
前幾名是 APACHE 分數（aps1）、血壓（meanbp1）、PaO₂/FiO₂（pafi1）、診斷類別——做 RHC 的病人病得比較重。這就是 confounding，也是 Connors 這篇當年引起爭議的原因。
:::

## 03 overlap | 投影片 66、91
```r
nu_glm_full <- nuisance(d, "glm", "full")
round(overlap_summary(nu_glm_full$g_raw, d$A), 3)
overlap_plot(nu_glm_full$g_raw, d$A, labels = c("RHC", "no RHC"))
```

::: look
兩組有明顯位移（做 RHC 的偏右），但大部分範圍都有重疊；被截斷的人不到 2%，治療組的有效樣本數還有約 1,200（原本 2,184）。overlap 夠用，問題不在這裡。
:::

## 04 GLM：full sample 和 cross-fit | 投影片 83–85
先用主效應 logistic regression 當 Q 和 g。這種模型「容量」小，就算看過自己的資料，也背不起來。

```r
nu_glm_cf <- nuisance(d, "glm", "crossfit", seed = 1)
rbind(full = residual_check(d, nu_glm_full), crossfit = residual_check(d, nu_glm_cf))
rbind(panel(d, nu_glm_full, "GLM full"), panel(d, nu_glm_cf, "GLM cross-fit"))
```

::: look
殘差的 RMS，樣本內 0.417、out-of-fold 0.423，幾乎一樣——GLM 沒在背答案。所以 full sample 和 cross-fit 的 AIPW 都約 0.056–0.058，CI 也差不多。
:::

## 05 Random forest，full sample：自己改考卷 | 投影片 63、91
換成 random forest（100 棵樹）。full sample 的意思是：用 5,735 人訓練，再預測同一批 5,735 人。瀏覽器約 20–40 秒。

```r
nu_rf_full <- nuisance(d, "rf", "full", ntree = 100)
residual_check(d, nu_rf_full)
panel(d, nu_rf_full, "RF full")
```

::: look
殘差 RMS 掉到 0.16——每個病人的預測幾乎就是他自己的結果，因為訓練時看過他。後果：

- AIPW 的修正項 H(Y − Q̂<sub>A</sub>) 平均只剩 0.007，AIPW（約 0.022）被拉回 g-computation（約 0.015）。
- EIF 的 SE 也跟著被低估，CI（約 0.016–0.028）窄得不像話——**不是比較準，是假裝比較準。**
- TMLE 的非線性更新對同一個過擬合的 Q̂ 反應不同，跑到 0.085。兩個「號稱漸近相同」的估計器，在這裡差了四倍。
:::

## 06 Random forest，cross-fit：預測和打分數分開 | 投影片 83–85
同樣的 random forest，但每個人的預測都來自沒看過他的另外 4 折。要訓練 10 個森林，瀏覽器約 1–3 分鐘。

```r
nu_rf_cf <- nuisance(d, "rf", "crossfit", seed = 1, ntree = 100)
rbind(full = residual_check(d, nu_rf_full), crossfit = residual_check(d, nu_rf_cf))
panel(d, nu_rf_cf, "RF cross-fit")
```

::: look
out-of-fold 殘差回到 0.42（跟 GLM 一樣），修正項從 0.007 變成約 0.02，AIPW 和 TMLE 重新靠在一起（約 0.033–0.040），CI 變寬到合理的大小（±0.025）。random forest 本身有隨機性，瀏覽器和本機 R 的數字會差一點（例如 AIPW 0.033 vs 0.037），機制一樣。Karim (2026) 用 SuperLearner 看到一模一樣的機制：樣本內殘差 0.30、out-of-fold 0.42；full-sample AIPW 0.033、DML 0.043。
:::

::: think random forest 的 g-computation 在 full 和 cross-fit 都只有 0.014–0.015，為什麼比 GLM 的 0.055 小那麼多？
random forest 把 A 當成 66 個變數中的一個，分裂時不一定會用到它——治療的效果被「平滑」掉了。這就是 Lab 2 第 05 格的原因 2：ML 故意偏一點，對預測 loss 划算，對 ATE 不划算。AIPW／TMLE 的修正項就是在補這一塊；但修正要有效，前提是殘差是誠實的（cross-fit）。
:::

## 07 四種組合畫在一起
```r
all <- rbind(panel(d, nu_glm_full, "GLM full"), panel(d, nu_glm_cf, "GLM cross-fit"),
             panel(d, nu_rf_full, "RF full"), panel(d, nu_rf_cf, "RF cross-fit"))
sub <- all[all$estimator %in% c("AIPW", "TMLE"), ]
karim <- data.frame(lab = c("Karim AIPW full", "Karim TMLE full", "Karim DML (r=100)", "Karim CV-TMLE (r=100)"),
                    est = c(0.033, 0.053, 0.043, 0.047), lo = c(0.021, 0.041, 0.019, 0.023), hi = c(0.045, 0.065, 0.067, 0.071))
forest_plot(c(paste(sub$scheme, sub$estimator), karim$lab), c(sub$est, karim$est), c(sub$lo, karim$lo), c(sub$hi, karim$hi),
            col = c(ifelse(grepl("full", sub$scheme), COL$red, COL$teal), rep(COL$muted, 4)), ref = 0, xlab = "30-day mortality risk difference")
```

::: look
紅色（full sample）裡，RF 的兩條線又窄又互相打架；藍色（cross-fit）裡，GLM（約 0.058）和 RF（約 0.033–0.040）的 CI 大幅重疊，都是 3–6 個百分點左右的增加。灰色是 Karim 的結果。**cross-fitting 不改變 estimand，卻可以改變有限樣本的 estimate 和 CI。**
:::

## 08 二元結果：TMLE 一定落在合法範圍 | 投影片 82
```r
pseudo <- with(nu_rf_cf, Q1 - Q0 + (d$A / g - (1 - d$A) / (1 - g)) * (d$Y - ifelse(d$A == 1, Q1, Q0)))
c(AIPW_contribution_min = min(pseudo), AIPW_contribution_max = max(pseudo))
tm <- est_tmle(list(A = d$A, Y = d$Y), nu_rf_cf, ybounds = c(0, 1))
tm
```

::: look
AIPW 是把一堆個人貢獻平均，這份資料的個人貢獻從約 −24 到 +13（權重乘殘差），所以有限樣本裡答案不保證落在 [−1, 1]。TMLE 是兩個更新後的機率 Q̂*₁、Q̂*₀ 相減，每一項都在 [0, 1]，風險差一定在 [−1, 1]。這是 Lab 3 第 07 格那張表「結果範圍」那一列的意思——這份資料兩者都在範圍內，但罕見結果、極端權重時差別就出來了。
:::

::: punch
樣本內殘差小，不代表模型準；可能只是它看過答案。
:::

RHC 沒有真值：cross-fit 之後幾個方法一致，是內部一致性的檢查，不能證明沒有未測量混雜。Connors 原研究報的是 odds ratio，這裡是風險差，數值不能直接比。
