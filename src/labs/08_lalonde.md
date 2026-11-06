---
id: 08_lalonde
nav: 8 LaLonde
title: Lab 8　LaLonde：當橋不存在
lead: 職業訓練能讓收入增加多少？同一批受訓者，配上兩種對照組：一種是隨機分派沒被選上的人（實驗），一種是從一般族群 PSID 抓來的人（觀察）。實驗給了一個基準答案；觀察資料上，所有估計器會怎樣？這是 Lab 4 第 06 格 positivity 旋鈕轉到底的真實版本。
toolbox: estimators, realdata, plot
files: data/lalonde/nswre74_treated.txt, data/lalonde/nswre74_control.txt, data/lalonde/psid_controls.txt
packages: randomForest
time: 第一次要裝 randomForest；全部約 1 分鐘
---
| 格 | 在看什麼 | 投影片 |
|---|---|---|
| 01 | 隨機分派：直接相減就有答案 | 56–58 |
| 02–04 | 換成 PSID 對照：兩組差到不能比，propensity 幾乎分開 | 66、75、91 |
| 05–06 | 八個估計值各說各話；權重集中在少數人身上 | 66–67、82 |
| 07 | trimming：答案變了，因為題目變了 | 56、58 |

::: note
資料：LaLonde (1986) 的 NSW 職訓實驗，使用 Dehejia &amp; Wahba 整理的版本（users.nber.org/~rdehejia/data）：NSW 受訓者 185 人、NSW 隨機對照 260 人、PSID 對照 2,490 人。結果是 1978 年收入（美元），共變數：年齡、教育、黑人、西語裔、已婚、無學位、1974 與 1975 年收入。
:::

## 01 隨機分派：基準答案
```r
ex <- load_lalonde("experimental")
c(trainees = sum(ex$A), randomized_controls = sum(1 - ex$A), naive_difference = ex$benchmark,
  regression_adjusted = unname(coef(lm(ex$Y ~ ex$A + as.matrix(ex$X)))[2]))
```

::: look
隨機分派時，直接相減就是受訓者族群的效果：約 1,794 美元。加上共變數調整是 1,676，差不多——設計本身把兩組變得可以比。
:::

## 02 換成 PSID 對照組
```r
d <- load_lalonde("psid")
grp <- data.frame(mean_trainees = sapply(d$X[d$A == 1, ], mean), mean_PSID = sapply(d$X[d$A == 0, ], mean), SMD = sapply(d$X, smd, A = d$A))
round(grp, 2)
c(trainees = sum(d$A), PSID_controls = sum(1 - d$A), naive_difference = mean(d$Y[d$A == 1]) - mean(d$Y[d$A == 0]))
```

::: look
受訓者的 1974、1975 年收入只有 PSID 的零頭，年輕、沒結婚、沒學位的比例高很多——SMD 動輒超過 1。直接相減變成 −15,205 美元：不是訓練害的，是兩群人根本不是同一種人。
:::

## 03 overlap：橋在哪裡？ | 投影片 91
```r
nu <- nuisance(d, "glm", "crossfit", seed = 1)
round(overlap_summary(nu$g_raw, d$A), 3)
overlap_plot(nu$g_raw, d$A, labels = c("NSW trainees (185)", "PSID controls (2,490)"))
```

::: look
大部分 PSID 對照的 ĝ 擠在 0 附近，受訓者分散在右邊——兩座山幾乎沒有交集。約 80% 的人 ĝ &lt; 0.025，被截斷規則碰到。這不是估計器的問題，是資料裡**根本沒有**「長得像受訓者、卻沒受訓」的人可以比。
:::

## 04 和 NHEFS 對照一下
| | NHEFS（Lab 5） | LaLonde × PSID |
|---|---|---|
| ĝ 範圍 | 0.05–0.78 | ≈ 0–0.96 |
| 被截斷的比例 | 0% | 約 80% |
| 治療組有效樣本數 / 人數 | 約 326 / 403 | 約 36 / 185 |

有效樣本數（ESS）掉到 36：權重加總之後，185 個受訓者只相當於 36 個人的資訊量——少數幾個人扛著整個答案。

## 05 八個估計值，各說各話 | 投影片 82
```r
allp <- do.call(rbind, lapply(c("glm", "rf"), function(lr) do.call(rbind, lapply(c("full", "crossfit"), function(sc)
  panel(d, nuisance(d, lr, sc, seed = 1, ntree = 100), paste(lr, sc))))))
allp[, c("est", "lo", "hi")] <- round(allp[, c("est", "lo", "hi")]); allp$se <- round(allp$se)
allp
forest_plot(paste(allp$scheme, allp$estimator), allp$est, allp$lo, allp$hi, ref = ex$benchmark,
            ref_lab = "experimental benchmark 1,794 (NSW population)", xlab = "difference in 1978 earnings (US$)",
            col = ifelse(grepl("IPTW", allp$estimator), COL$gold, ifelse(grepl("full", allp$scheme), COL$red, COL$teal)))
```

::: look
從 −14,000（IPTW）到 +800，AIPW 和 TMLE 在同一組 nuisance 下還能差 3,700–4,400 美元（GLM cross-fit：AIPW −12、TMLE −4,422）。RF full sample 的 CI 只有 ±300，又是「又錯、又很有自信」。Karim 的 Table 9 一樣：−733 到 −14,629。這時候「方法之間一致」完全不成立——這本身就是一個警訊。Moccia et al. (2024) 說 AIPW 在 near-positivity violation 下不穩，在這裡看得最清楚。
:::

::: think 那 1,794 是不是這裡的真值？誰離它最近誰就對？
不是。1,794 是 **NSW 受訓者族群**的效果（接近 ATT）。這裡估的是「受訓者 + PSID」合併族群的 ATE，那是另一個 estimand，大部分人是根本不會去參加職訓的一般人。1,794 只能當方向和量級的參考，不能拿來算 coverage（Karim 2026 也這樣提醒）。
:::

## 06 權重集中在誰身上？ | 投影片 66–67
```r
w <- ifelse(d$A == 1, 1 / nu$g, 1 / (1 - nu$g))
wt <- w[d$A == 1]
c(max_trainee_weight = max(wt), n_at_cap = sum(wt >= 39.99),
  top10_share_of_weight = sum(sort(wt, decreasing = TRUE)[1:10]) / sum(wt), ESS = sum(wt)^2 / sum(wt^2))
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 2.5, 1), las = 1, bty = "n")
hist(log10(wt), breaks = 30, col = adjustcolor(COL$red, .7), border = "white", main = "trainee weights 1 / g-hat",
     xaxt = "n", xlab = "weight (log scale; capped at 40)", ylab = "trainees")
axis(1, at = log10(c(1, 2, 5, 10, 20, 40)), labels = c(1, 2, 5, 10, 20, 40))
cs <- cumsum(sort(wt, decreasing = TRUE)) / sum(wt)            # cumulative share of total weight, heaviest first
plot(seq_along(cs), cs, type = "s", lwd = 3, col = COL$red, ylim = c(0, 1),
     xlab = "trainees, heaviest weight first", ylab = "share of total weight", main = "who carries the answer?")
abline(0, 1 / length(cs), lty = 2, col = COL$muted)
points(10, cs[10], pch = 19, cex = 1.6); text(10, cs[10], sprintf("  top 10 = %.0f%%", 100 * cs[10]), adj = 0)
legend("bottomright", c("actual", "if every trainee counted equally"), lty = c(1, 2), lwd = c(3, 1), col = c(COL$red, COL$muted), bty = "n")
```

::: look
- 左圖：大部分受訓者的權重在 1–1.5 之間——他們的 ĝ 接近 0.9，一看就「不像 PSID 的人」。但最右邊有 6 個人 ĝ 掉到 0.025 以下，權重被截在上限 40——他們長得太像 PSID 的一般人，模型幾乎不相信他們會受訓。
- 右圖：**前 10 個人（185 人裡的 5%）扛了將近一半（約 49%）的總權重**，有效樣本數只剩 36。
- 這 10 個人 1978 年的收入多 1,000 或少 1,000，答案就跟著跑。AIPW 的修正項是「權重 × 殘差」，所以同樣受影響（Moccia et al. 2024：AIPW has the same weaknesses of IPW when it comes to positivity；投影片 75 的 V_eff 也說明了為什麼）。
:::

## 07 Trimming：答案變了，因為題目變了 | 投影片 56、58
只保留 ĝ 在 [ε, 1 − ε] 的人，再做一次 cross-fit AIPW（= DML）。

```r
trim <- do.call(rbind, lapply(c(0, 0.05, 0.10), function(eps) {
  keep <- nu$g_raw >= eps & nu$g_raw <= 1 - eps
  dk <- list(X = d$X[keep, ], A = d$A[keep], Y = d$Y[keep], binary = FALSE)
  a <- est_aipw(dk, nuisance(dk, "glm", "crossfit", seed = 1))
  data.frame(rule = if (eps == 0) "keep all" else sprintf("keep g-hat in [%.2f, %.2f]", eps, 1 - eps),
             n_kept = sum(keep), trainees_kept = sum(dk$A), share_kept = round(mean(keep), 2),
             DML = round(a[["est"]]), lo = round(a[["lo"]]), hi = round(a[["hi"]]))
}))
trim
```

::: look
修到 [0.05, 0.95] 只剩 420 人（16%），[0.10, 0.90] 剩 304 人（11%）——和 Karim 的 16%、11% 一樣。估計值跳到正的，CI 寬到 ±2,500–2,900。這不是「修好了原本的問題」：被丟掉的 84% 大部分是 PSID 的一般人，剩下的是「長得有點像受訓者」的子族群。estimand 從合併族群的 ATE 變成了 ψ<sub>ε</sub> = E[Q₁ − Q₀ | ε ≤ g ≤ 1−ε]。縮小題目可以是合理的選擇，但要明講。
:::

::: punch
AIPW、TMLE、DML 像三條導航路線。跨河的橋不存在時，換導航 App 不會把橋生出來。
:::

positivity 有兩層：**識別**（資料裡有沒有可比的人）和 **precision**（有的話夠不夠多）。Lab 4 第 06 格是第二層；這裡是第一層壞掉的樣子。處理方式是回到設計：換對照組、重新定義目標族群（例如改估 ATT）、或明講外推的假設——不是換一個更厲害的 learner。
