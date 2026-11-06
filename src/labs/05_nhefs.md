---
id: 05_nhefs
nav: 5 NHEFS
title: Lab 5　一家人上場：戒菸會讓人胖多少？AIPW、TMLE、DML、CV-TMLE
lead: Lab 2–4 在模擬資料上看懂了每一塊誤差。這裡換成真實資料，復刻 Karim (2026) Three Routes to One Answer 的 NHEFS 主線：同一組 nuisance，接成 AIPW、TMLE；加上 cross-fitting，就是 DML 和 CV-TMLE。沒有真值，只能靠診斷（overlap、balance、敏感度）判斷答案可不可信。對應投影片 56–57、77、81–84、87、93–95。
toolbox: estimators, nhefs, realdata, plot
files: data/nhefs.csv
time: 全部約 2 分鐘，最慢的是第 06、10 格
---
| 格 | Karim (2026) 的步驟 | 投影片 |
|---|---|---|
| 01–02 | Step 0：固定題目；看兩組差在哪 | 56–59 |
| 03–04 | Step 1：先診斷 overlap 與 balance，再選 estimator | 67、77、93 |
| 05–07 | Step 2–4：共用 nuisance，full sample vs cross-fit | 79–84、94–95 |
| 08–10 | Step 6：敏感度（截斷、learner、切分） | 87、93 |

::: note
**一家人怎麼分？**兩個問題就分完了：(1) 怎麼把 EIF 變成一個數字——加回去（AIPW 家族）或推 Q̂（TMLE 家族）；(2) nuisance 怎麼訓練——full sample、cross-fit、double cross-fit。AIPW + cross-fit = DML，TMLE + cross-fit = CV-TMLE（投影片 94 那張 2×3 表）。

與原論文的差異：原文的 SuperLearner 用 5 個 learner（mean、GLM、glmnet、ranger、earth）；這裡用 base R 寫的 **sl_lite**：mean、主效應 GLM、Hernán–Robins GLM（加平方項）、樣條 GLM 的凸組合，權重由內層 5 折 CV 決定。數字不會和論文逐位相同，但應落在同一範圍（論文 3.32–3.42 kg）。
:::

## 01 資料與問題 | 投影片 56–57
A = 1971–1982 間戒菸（qsmk），Y = 同期間體重變化（kg），X = 性別、種族、年齡、教育、吸菸強度、菸齡、運動、活動量、1971 年體重。只用 Y 沒缺的 1,566 人（complete case），所以 estimand 是「這 1,566 人」的 ATE，不是全美國人。

```r
dat <- load_nhefs("data/nhefs.csv")
c(n = nrow(dat), quit = sum(dat$A), unadjusted_diff_kg = mean(dat$Y[dat$A == 1]) - mean(dat$Y[dat$A == 0]))
```

## 02 戒菸的人跟沒戒菸的人，本來就不一樣
標準化平均差（SMD）＝兩組平均差除以合併標準差。|SMD| &gt; 0.1 通常就算「兩組不像」。

```r
X <- nhefs_X(dat)
grp <- data.frame(mean_quit = sapply(X[dat$A == 1, ], mean), mean_no_quit = sapply(X[dat$A == 0, ], mean),
                  SMD = sapply(X, smd, A = dat$A))
round(grp[order(-abs(grp$SMD)), ], 2)
```

::: look
戒菸的人平均年紀比較大（SMD 0.28）、每天抽的菸比較少、白人比例比較高。年紀和體重變化本身有關——不調整就比，2.5 kg 這個數字混進了「年紀」的效果。
:::

## 03 先診斷 overlap，再選 estimator | 投影片 67、77、93
```r
set.seed(1)
nf <- nhefs_nuisance(dat, dat, "hr")        # start with the Hernan-Robins GLM: fastest
round(overlap_summary(nf$g_raw, dat$A), 3)
overlap_plot(nf$g_raw, dat$A, xlab = "g-hat(X) = P(quit | X)", labels = c("quit (403)", "did not quit (1163)"))
```

::: look
ĝ 從 0.05 到 0.78，兩組廣泛重疊；截斷在 [0.025, 0.975] 一個人都沒有被截到（論文：0%）。最大權重 17、戒菸組有效樣本數約 326（原本 403）——權重健康。這是 Karim 選 NHEFS 當「乾淨教學案例」的原因。
:::

## 04 加權之後，兩組像了嗎？（balance）
用 1/ĝ（戒菸組）、1/(1−ĝ)（未戒菸組）加權，再算一次 SMD。紅點是加權前，藍點是加權後——這張圖叫 love plot。

```r
bt <- balance_table(X, dat$A, nf$g)
bt <- bt[order(abs(bt$before)), ]
lab_par(mar = c(4.5, 9, 1, 1))
plot(NA, xlim = c(0, 0.32), ylim = c(1, nrow(bt)), axes = FALSE, xlab = "|SMD|", ylab = "")
abline(v = 0.1, lty = 2, col = "grey60")
segments(abs(bt$after_IPW), seq_len(nrow(bt)), abs(bt$before), seq_len(nrow(bt)), col = "grey80")
points(abs(bt$before), seq_len(nrow(bt)), pch = 19, col = COL$red)
points(abs(bt$after_IPW), seq_len(nrow(bt)), pch = 19, col = COL$teal)
axis(1); axis(2, at = seq_len(nrow(bt)), labels = bt$variable, tick = FALSE, col.axis = COL$ink)
legend("bottomright", c("before weighting", "after IPW"), pch = 19, col = c(COL$red, COL$teal), bty = "n")
```

::: look
紅點有 8 個超過 0.1（年紀最大，0.28），加權後藍點全部縮到 0.04 以下。balance 是 ĝ「有沒有把兩組變像」的檢查——它只看得到量到的變數，看不到沒量到的混雜（exchangeability 還是假設）。
:::

## 05 Full sample：同一組 nuisance，四種接法 | 投影片 81、83–84
```r
p_full <- nhefs_panel(dat, nf, "full sample")
p_full[, -2]
plug <- est_plugin(nf)
c(plugin = plug, AIPW_correction = est_aipw(dat, nf)[["est"]] - plug, TMLE_epsilon = est_tmle(dat, nf)[["eps"]])
```

::: look
AIPW 和 TMLE 幾乎一樣（差在小數點第三位）：AIPW 直接把修正項加上去，TMLE 先把 Q̂ 往 H 的方向推 ε 再 plug in。Q̂ 本來就不差（Hernán–Robins 的模型），所以修正項和 ε 都很小——和 Lab 3 的糖果是同一個故事。
:::

## 06 換成 sl_lite，並做 cross-fitting（= DML / CV-TMLE） | 投影片 94–95
每個人的 Q̂、ĝ 都由沒看過他的四折資料訓練；SuperLearner 的內層 CV 也放在外層的訓練折裡面。瀏覽器裡約 1 分鐘。

```r
nc <- nhefs_crossfit(dat, learner = "sl_lite", K = 5, seed = 1)
p_cf <- nhefs_panel(dat, nc, "cross-fit K=5")
p_cf[, -2]
set.seed(1); w <- attr(sl_lite(dat, dat[1:2, ], "Q"), "weights"); round(w, 3)   # sl_lite weights for Q
```

::: look
DML 和 CV-TMLE 約 3.4 kg，CI 約 2.5–4.3。sl_lite 把 7 成權重給 Hernán–Robins 模型（hr）——資料自己挑出了教科書的模型。瀏覽器和本機 R 可能差 0.04 左右：sl_lite 內層 CV 的隨機切法不完全一樣，第 10 格會看到這種「換切法」的晃動本來就有 0.1 左右。
:::

## 07 畫在一起，和論文對照
```r
ours <- rbind(transform(p_full, src = "ours full"), transform(p_cf, src = "ours cross-fit"))
karim <- data.frame(estimator = c("g-computation", "IPTW (Hajek)", "AIPW", "TMLE", "AIPW", "TMLE"),
                    est = c(3.27, 3.19, 3.32, 3.33, 3.38, 3.39), lo = c(NA, NA, 2.50, 2.50, 2.41, 2.41),
                    hi = c(NA, NA, 4.14, 4.15, 4.36, 4.36),
                    src = c(rep("Karim full", 4), "Karim DML (r=100)", "Karim CV-TMLE (r=100)"))
lab <- c(paste(ours$src, ours$estimator), paste(karim$src, ifelse(grepl("r=100", karim$src), "", karim$estimator)))
forest_plot(lab, c(ours$est, karim$est), c(ours$lo, karim$lo), c(ours$hi, karim$hi),
            col = c(rep(COL$teal, 8), rep(COL$red, 6)), ref = 3.4, ref_lab = "Hernan-Robins ~ 3.4", xlab = "difference in weight change (kg)")
```

::: look
藍色是我們、紅色是論文。所有雙重穩健的估計都在 3.3–3.5 kg 之間；它們之間的差距（約 0.1）遠小於 CI 的半寬（約 0.9）。**選哪條路，沒有選對題目和食材重要。**投影片 84 說 AIPW 和 TMLE 的差別在有限樣本——在 overlap 好、Q̂ 不差的資料上，差別小到看不出來。
:::

## 08 敏感度一：propensity 截斷門檻
Karim 的練習題：把 [0.025, 0.975] 換成更鬆或更緊的門檻，看幾個人被截到、答案動了多少。

```r
tr <- t(sapply(list(c(.01, .99), c(.025, .975), c(.05, .95), c(.10, .90)), function(b) {
  nu <- nhefs_crossfit(dat, "hr", 5, 1, bounds = b); a <- est_aipw(dat, nu)
  c(lower = b[1], upper = b[2], pct_clipped = 100 * mean(nu$g_raw < b[1] | nu$g_raw > b[2]), AIPW = a[["est"]], SE = a[["se"]])
}))
round(tr, 3)
```

::: look
門檻 0.025 以內沒有人被截到，0.05 也只截到 2 個人（0.13%），答案不動；收到 [0.10, 0.90] 才截到 5% 的人，答案也只動 0.02 kg。overlap 好的資料，截斷只是保險，不會改變結論——Lab 8 會看到相反的情況。
:::

## 09 敏感度二：nuisance 用哪個 learner
```r
lr <- t(sapply(c("mean", "main", "hr", "spline"), function(l) { a <- est_aipw(dat, nhefs_crossfit(dat, l, 5, 1)); c(AIPW = a[["est"]], SE = a[["se"]]) }))
round(lr, 3)
```

::: look
「mean」那列 Q̂ 和 ĝ 都只有截距（兩個模型都錯），AIPW 就退回未調整的 2.5 kg——和 Lab 4 第 03 格右下角那格一樣：雙重穩健不是兩個都錯也能救。主效應、Hernán–Robins、樣條三個都在 3.38–3.43：只要模型夠用，答案就穩。
:::

## 10 敏感度三：換一個隨機切法，答案晃多少？ | 投影片 87、93
同一份資料、不同的 fold。Karim 用 100 次取中位數；這裡先跑 10 次（可以自己改大）。

```r
reps <- 10
r <- sapply(seq_len(reps), function(s) { nu <- nhefs_crossfit(dat, "hr", K = 5, seed = 100 + s)
  c(DML = est_aipw(dat, nu)[["est"]], CV_TMLE = est_tmle(dat, nu)[["est"]]) })
round(r, 3)
c(median_DML = median(r["DML", ]), sd_DML = sd(r["DML", ]))
lab_par(); hist(r["DML", ], breaks = 8, col = adjustcolor(COL$teal, .6), border = "white", main = "", xlab = "DML estimate (kg); one value per random split")
abline(v = median(r["DML", ]), lwd = 3, col = COL$red)
```

::: look
10 種切法從 3.25 到 3.59，標準差約 0.1 kg（Karim：0.08），比抽樣 SE（約 0.5）小很多。但如果只比兩個任意的 seed，你可能是在比切法、不是在比方法——所以要預先寫好要重複幾次、怎麼彙整。
:::

## 11 和論文對帳
| Estimator（Karim 2026, Fig. 4） | r | 估計值 (kg) | 95% CI |
|---|---|---|---|
| g-computation, full | – | 3.27 | 未報 |
| IPTW, full | – | 3.19 | 未報 |
| AIPW, full | – | 3.32 | 2.50–4.14 |
| TMLE, full | – | 3.33 | 2.50–4.15 |
| DML | 100 | 3.38 | 2.41–4.36 |
| CV-TMLE | 100 | 3.39 | 2.41–4.36 |

::: punch
NHEFS 沒有真值：幾個方法接近，代表答案對方法選擇不敏感，不代表識別假設成立。
:::

那方法什麼時候會不一樣？Lab 6（RHC）看 learner 太會背答案、而且結果是二元的時候；Lab 7（IHDP）在知道真值的地方檢查 CI；Lab 8（LaLonde）看 overlap 不見的時候。
