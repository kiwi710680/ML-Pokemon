---
id: 04_remainder
nav: 4 剩下什麼
title: Lab 4　神奇糖果（下）：修完之後還剩什麼？R₂、double robustness、cross-fitting
lead: Lab 3 把一階誤差扣掉了，答案從 84.2 回到 80.3。但修正不是魔法：泰勒展開還剩一個二階項 R₂，還有「自己出題自己改」留下的一塊。這個 Lab 把剩下的東西一塊一塊拆開，看它們長什麼樣、什麼時候會變大。對應投影片 85–94。
toolbox: estimators, candy, plot
files: 
time: 全部約 3–5 分鐘，最慢的是第 01、07 格
---
| 格 | 投影片 | 這格回答的問題 |
|---|---|---|
| 01–02 | 85–86、89 | 修完剩下哪幾塊？R₂ 為什麼那麼小？ |
| 03 | 88 | double robustness：兩張保險，賠一張就夠？ |
| 04 | 90–91 | Neyman orthogonality 長什麼樣？ |
| 05 | 87、93 | cross-fitting 救的是什麼？ |
| 06 | 77、93 | positivity 差的時候，代價是什麼？ |
| 07 | 87、92 | 一份資料可能是運氣：CI 真的有 95% 嗎？ |

## 01 修完之後剩下什麼：拆開 AIPW 的誤差 | 投影片 85–86
`ψ̂_AIPW − ψ = (ℙn − P)D + R₂ + (ℙn − P)(D̂ − D)`，其中 `R₂ = E[(ĝ − g)/ĝ·(Q̂₁ − Q₁)] + E[(ĝ − g)/(1 − ĝ)·(Q̂₀ − Q₀)]`。把 plug-in、full-sample AIPW、cross-fit AIPW 各拆一次（瀏覽器約 1 分鐘）。每一根長條是一塊誤差，菱形是總誤差。

```r
truth <- candy_truth()
d   <- simulate_candy(1500, seed = 2026)
pop <- candy_population(8000)
dec    <- decompose_candy(d, pop, k = 10, scheme = "full")
dec_cf <- decompose_candy(d, pop, k = 10, scheme = "crossfit")
tab <- rbind(plugin_full = dec$plugin, AIPW_full = dec$aipw, AIPW_crossfit = dec_cf$aipw)
colnames(tab) <- c("total", "sampling", "first_order_or_R2", "empirical_process")
round(tab, 2)
round(rbind(full = dec$L2, crossfit = dec_cf$L2), 3)   # L2 error of g-hat, of Q-hat, and their product
cols <- c(COL$teal, COL$red, COL$plum)
lab_par(mar = c(4.5, 9, 1, 1))
bp <- barplot(t(tab[3:1, 2:4]), beside = TRUE, horiz = TRUE, col = cols, border = NA,
              names.arg = c("AIPW\ncross-fit", "AIPW\nfull", "plug-in\nfull"),
              xlim = c(-2.5, 5.5), xlab = "error of the estimate (CP)")
abline(v = 0, col = COL$ink)
points(tab[3:1, "total"], colMeans(bp), pch = 23, bg = "white", cex = 2.2, lwd = 2)
legend("bottomright", c("sampling: (Pn - P)D", "first-order bias (plug-in) / R2 (AIPW)", "empirical process", "total"),
       fill = c(cols, NA), border = NA, pch = c(NA, NA, NA, 23), pt.bg = "white", pt.cex = 1.6, bty = "n")
```

::: look
- 最上面那組（plug-in）的紅色長條約 4.8，是 first-order bias；換到 AIPW，紅色變成 R₂，只剩約 −1——**一階項變成「ĝ 誤差 × Q̂ 誤差」，加法變乘法。**
- L2 表：‖ĝ − g‖ 只有 0.04，‖Q̂ − Q‖ 約 19，乘起來不到 1：兩個都不完美，乘起來就小了。
- 紫色（empirical process）在 full sample 約 0.9，cross-fit 只剩約 −0.3：預測和打分數用不同的人，這一塊就壓下去了。
- 藍色（抽樣誤差）在兩種 AIPW 一模一樣——它是真 EIF 的樣本平均，跟 nuisance 怎麼估無關，也就是最後 CI 要描述的那一塊。
- 投影片 86 只放了上面兩組（plug-in 和 full-sample AIPW）；cross-fit 那組留在這裡看。
- 你可能會想說：cross-fit 的菱形（約 −2.1）怎麼反而比 full sample（約 −0.5）離 0 更遠？因為這只是一份資料，R₂ 和抽樣誤差剛好同號疊在一起。cross-fitting 保證的不是「每一份都比較準」，而是誤差的結構乾淨、CI 誠實——第 07 格重複很多次才看得到。
:::

## 02 R₂ 為什麼那麼小？每一隻波波攤開來看 | 投影片 89
把母體裡每一隻波波的誤差畫出來：左邊是 propensity 的誤差，中間是 outcome 的誤差，右邊把每一隻對 plug-in 的貢獻（紅）和對 R₂ 的貢獻（藍）疊在一起。

```r
u <- dec$units                               # full-sample models evaluated on the population
p <- u[u$species == "Pidgey", ]
pad <- function(y, top = 0.35) { r <- range(y); r + c(0, top * diff(r)) }   # extra room on top for the legend
# averages: Pidgey only (what the dots show) vs the whole population (what enters the table above)
means <- rbind(Pidgey = colMeans(p[, c("first_order_unit", "R2_unit")]),
               all_species = colMeans(u[, c("first_order_unit", "R2_unit")]))
round(means, 2)
round(c(first_order = unname(dec$plugin["first_order"]), R2 = unname(dec$aipw["R2"])), 2)   # same as all_species
par(mfrow = c(1, 3), mar = c(5, 5.8, 2.5, 1), mgp = c(3.6, .8, 0), las = 1, bty = "n", col.axis = COL$muted)
plot(p$cp, p$g_err, pch = 16, cex = .45, col = adjustcolor(COL$plum, .4),
     xlab = "Original CP (Pidgey)", ylab = "g-hat(X) - g(X)", main = "propensity error")
abline(h = 0)
plot(p$cp, p$Q1_err, pch = 16, cex = .45, col = adjustcolor(COL$red, .4), ylim = pad(c(p$Q1_err, p$Q0_err), 0.2),
     xlab = "Original CP (Pidgey)", ylab = "Q-hat - Q", main = "outcome error")
points(p$cp, p$Q0_err, pch = 16, cex = .45, col = adjustcolor(COL$teal, .4))
abline(h = 0)
legend("topright", c("Q1-hat - Q1", "Q0-hat - Q0"), pch = 16, col = c(COL$red, COL$teal), bty = "n")
plot(p$cp, p$first_order_unit, pch = 16, cex = .45, col = adjustcolor(COL$red, .35),
     ylim = pad(c(p$first_order_unit, p$R2_unit)), xlab = "Original CP (Pidgey)",
     ylab = "contribution per Pokemon (CP)", main = "plug-in term vs R2 term")
points(p$cp, p$R2_unit, pch = 16, cex = .45, col = adjustcolor(COL$teal, .6))
abline(h = 0)
abline(h = means["Pidgey", "first_order_unit"], col = COL$red, lwd = 3, lty = 2)
abline(h = means["Pidgey", "R2_unit"], col = COL$teal, lwd = 3, lty = 2)
legend("topright", c("(Q1-hat - Q1) - (Q0-hat - Q0)", "R2 integrand", "Pidgey means"),
       pch = c(16, 16, NA), lty = c(NA, NA, 2), lwd = c(NA, NA, 3), col = c(COL$red, COL$teal, COL$ink), bty = "n")
```

::: look
- 左圖：ĝ 的誤差最多約 7 個百分點，是一條平滑的曲線——低 CP 低估、高 CP 高估。中圖：Q̂ 的誤差大很多，兩端更糟：CP 最低那端 Q̂₁ 高估到 +100（Lab 2 第 04 格那個問題），最高那端兩條都往下掉——kNN 在邊界只能找到一邊的鄰居。
- 右圖：紅點是每一隻波波對 plug-in 偏差的貢獻，散得很開，波波的平均（紅虛線）約 6.9；藍點是對 R₂ 的貢獻，要先乘上 (ĝ − g)/ĝ 這個小數字，所以中間一大段都被壓在 0 附近，平均（藍虛線）約 −2.2。全部物種一起平均，剛好是上一格表裡的 4.81 和 −1.03。
- 藍點只在最左邊（CP &lt; 60）一路掉到 −80：那裡 Q̂₁ 錯了將近 100，ĝ 也低估、而且 ĝ 本身就小。**兩個 nuisance 同一個地方一起錯，乘積才會大。**
:::

## 03 Double robustness：兩張保險，賠一張就夠？ | 投影片 88
R₂ 是乘積，所以只要 ĝ 或 Q̂ 其中一個是對的，它就是 0。真實研究沒辦法驗證這件事，但模擬可以：我們手上有**真的** g 和 Q。故意弄錯一個——Q 錯 = 一條直線 `Y ~ A + species + cp`（投影片 62 那條），g 錯 = 每隻寶可夢都給一樣的機率（完全不調整）——在 100,000 隻的大母體上算 AIPW 的偏差，抽樣誤差幾乎可以忽略。

```r
big <- candy_population(1e5)
psi_big <- mean(big$Q1 - big$Q0)
Q_right <- data.frame(Q1 = big$Q1, Q0 = big$Q0)
Q_wrong <- data.frame(Q1 = lin_Q(big, big, 1), Q0 = lin_Q(big, big, 0))
g_right <- big$g0
g_wrong <- rep(mean(big$A), nrow(big))
aipw_bias <- function(Q, g) est_aipw(big, data.frame(Q, g = g))[["est"]] - psi_big
dr <- matrix(c(aipw_bias(Q_right, g_right), aipw_bias(Q_wrong, g_right),
               aipw_bias(Q_right, g_wrong), aipw_bias(Q_wrong, g_wrong)), 2,
             dimnames = list(c("Q right", "Q wrong"), c("g right", "g wrong")))
round(dr, 2)                                                     # AIPW bias in each cell
c(plugin_with_Q_wrong = mean(Q_wrong$Q1 - Q_wrong$Q0) - psi_big,
  IPTW_with_g_wrong = est_ipw(big, data.frame(g = g_wrong)) - psi_big)   # one insurance only
```

::: look
- 2×2 表的三格接近 0（只剩一點點抽樣雜訊），只有「兩個都錯」那格偏了約 −7。
- 下面兩個數字是「只買一張保險」：plug-in 用錯的 Q 偏約 −7；IPTW 用錯的 g 偏到 +130 以上（等於沒調整）。
- 而且 AIPW 在「Q 錯、g 對」那格，是把 Q 的 −7 整個救回來——這就是 Lab 3 的修正項在做的事。
:::

::: think 兩個都錯的那格，AIPW 偏了約 −7，跟 plug-in 用錯的 Q 一樣多。這是巧合嗎？
不是。g 錯成「每隻都一樣」時，修正項只是把兩組的殘差平均相減；直線模型 `Y ~ A + species + cp` 裡有 A，最小平方法會讓兩組的殘差各自加總剛好是 0，所以修正項整個是 0、什麼都沒修，AIPW 就等於 plug-in。雙重穩健不是「兩個都錯也能救」——投影片 88 那張表右下角是 ✗ 的原因。
:::

## 04 Neyman orthogonality 長什麼樣 | 投影片 90–91
讓 nuisance 從真值往旁邊偏 t：`g_t = g + t(ḡ − g)`、`Q_t = Q + t(Q̄ − Q)`。plug-in 的 moment 是 `E[Q_{1t} − Q_{0t}] − ψ`；EIF 的 moment 是 `E[D(O; ψ, g_t, Q_t)]`。用真的 g、Q 精確算出來（不靠抽樣），畫在同一張圖上。

```r
oc <- orthogonality_curves(pop)
lab_par()
plot(oc$t, oc$plugin_Q, type = "l", lwd = 3, col = COL$red, ylim = c(-20, 20),
     xlab = "t: how far the nuisances move away from the truth", ylab = "moment (0 is ideal)")
lines(oc$t, oc$eif_both, lwd = 3, col = COL$teal)
lines(oc$t, oc$eif_Q_only, lwd = 3, lty = 2, col = COL$gold)
lines(oc$t, oc$eif_g_only, lwd = 3, lty = 3, col = COL$plum)
abline(h = 0, v = 0, col = "grey80")
legend("topleft", c("plug-in: perturb Q", "EIF: perturb g and Q", "EIF: perturb Q only (g correct)", "EIF: perturb g only (Q correct)"),
       col = c(COL$red, COL$teal, COL$gold, COL$plum), lwd = 3, lty = c(1, 1, 2, 3), bty = "n")
round(oc[oc$t %in% c(-1, -0.5, -0.1, 0, 0.1, 0.5, 1), ], 3)
```

::: look
- 紅線（plug-in）是斜線：Q 偏一點，答案就偏一點——一階。
- 藍線（EIF，兩個都偏）在 t = 0 是平的，離開 0 才慢慢彎上去：t 很小的時候是 t² 的量級。這就是 orthogonality（讀微分）。
- 兩條虛線剛好是 0：只要 g 對或 Q 對，不管另一個偏多少，moment 都是 0——這就是上一格的 double robustness（讀極限）。**同一個乘積，兩種讀法。**
:::

## 05 Cross-fitting：不要自己出題自己改 | 投影片 87、93
把 kNN 的 k 改成 1：每隻寶可夢最近的鄰居常常就是自己，樣本內殘差被低估，修正項和 SE 都被低估。換成 out-of-fold 預測（每一隻的預測都來自沒看過牠的另外 4 折）就恢復了。也試試 k = 3、5、10。

```r
k <- 1
f  <- nuisance_full(d, k = k)
cf <- nuisance_crossfit(d, K = 5, seed = 1, k = k)
rms <- function(nu) sqrt(mean((d$Y - ifelse(d$A == 1, nu$Q1, nu$Q0))^2))
c(residual_RMS_in_sample = rms(f), residual_RMS_out_of_fold = rms(cf))
rbind(full_sample = est_aipw(d, f), cross_fit_DML = est_aipw(d, cf), cross_fit_TMLE = est_tmle(d, cf)[1:4])
```

::: look
k = 1 時樣本內殘差 22.8、out-of-fold 43.4，差了將近一倍。full-sample 的 SE（約 1.6）比 cross-fit（約 2.4）小很多——不是比較準，是**假裝比較準**。在本機重複 500 次：k = 1 時 full-sample CI 的覆蓋率只有 85%，cross-fit 是 95%；k = 10 時差距縮小成 93% vs 95%——learner 越會背答案，差距越大。
:::

::: think 那 cross-fitting 有沒有代價？
有。每個 nuisance 只用 4/5 的資料訓練，Q̂ 的誤差變大一點（看第 01 格的 L2 表：cross-fit 的 ‖Q̂ − Q‖ 比較大），R₂ 也就大一點。它用一點點估計精度，換掉「自己改考卷」那一塊。資料少的時候這個代價會更明顯——Lab 7（IHDP）的 GLM 就是例子。
:::

## 06 Positivity：訓練師越偏心，SE 越大 | 投影片 77、93
`strength` 控制訓練師有多偏心：越大，g 越靠近 0 或 1。看最大權重、cross-fit AIPW 的 SE，以及理論上最小的 SE `√(V_eff / n)`（效率界）。

```r
pos <- t(sapply(c(0.5, 1, 2, 3), function(s) {
  ds <- simulate_candy(1500, seed = 2026, strength = s); ps <- candy_population(4000, strength = s)
  nc <- nuisance_crossfit(ds, K = 5, seed = 1, k = 10); a <- est_aipw(ds, nc)
  c(strength = s, min_g = min(ds$g0), max_g = max(ds$g0), max_weight = unname(weight_summary(ds, nc)["max_weight"]),
    AIPW = a[["est"]], SE = a[["se"]], SE_bound = candy_se_bound(ps, 1500))
}))
round(pos, 3)
lab_par()
plot(pos[, "strength"], pos[, "SE"], type = "b", pch = 19, lwd = 2, col = COL$red, ylim = c(0, max(pos[, "SE"]) * 1.1),
     xlab = "confounding strength (trainer favouritism)", ylab = "standard error")
lines(pos[, "strength"], pos[, "SE_bound"], type = "b", pch = 19, lwd = 2, col = COL$teal)
legend("topleft", c("SE of cross-fit AIPW", "efficiency bound sqrt(V_eff / n)"), col = c(COL$red, COL$teal), lwd = 2, bty = "n")
```

::: look
strength = 3 時，最小的 g 只剩 0.006，SE 從 2 暴增到 10。藍線是任何估計器都打不破的下界，它也在變大：`V_eff = E[σ²/g + σ²/(1−g)] + Var(τ)`，g 靠近 0 或 1，第一項就爆。紅線比藍線漲得更快，因為截斷在 [0.025, 0.975]、kNN 在稀疏處學得更差。**overlap 差的代價是 precision，換估計器救不回來**——Lab 8（LaLonde）是真實資料的版本。
:::

## 07 一份資料可能是運氣：重複 30 次 | 投影片 87、92
同一個世界抽 30 份資料（每份 1,500 隻），每份都估一次。看平均有沒有對準真值、95% CI 有沒有大約 95% 蓋到真值。（約 1–2 分鐘）

```r
reps <- 30; k <- 10
set.seed(1)
sim <- lapply(seq_len(reps), function(r) suppressWarnings(all_estimates(simulate_candy(1500), k = k)))
est <- sapply(sim, function(e) e[, "est"])
cover <- sapply(sim, function(e) e[, "lo"] < truth & e[, "hi"] > truth)
summ <- data.frame(mean = rowMeans(est), bias = rowMeans(est) - truth, SD = apply(est, 1, sd), coverage = rowMeans(cover))
round(summ, 2)
ord <- rownames(est)[-1]
lab_par(mar = c(4.5, 9, 1, 1))
stripchart(lapply(ord, function(r) est[r, ]), method = "jitter", pch = 19, col = adjustcolor(COL$teal, .5),
           group.names = ord, xlab = "ATE estimate", las = 1)
abline(v = truth, lty = 2, lwd = 2)
```

::: look
plug-in 整團往右偏（約 +3 到 +4）；AIPW、TMLE 對準虛線。30 次只能看大概：覆蓋率的蒙地卡羅誤差約 ±4%（√(0.95×0.05/30)）。如果跑出來 full sample 0.97、cross-fit 0.90，看起來好像 full 比較好——但兩個都在 95% ± 2 個 MCSE 之內，這是雜訊，不是結論。本機 500 次的版本（k = 10）是 full 93%、cross-fit 95%；想看大差距，把 `k` 改成 1 再跑。
:::

## 08 帶走三件事
::: punch
plug-in 的誤差跟著 Q̂ 走；用 D 修正之後，只剩 ĝ 誤差 × Q̂ 誤差。
:::

- 第 01–02 格：first-order bias（約 4.8）被換成 R₂（約 −1.0）——加法變乘法。
- 第 03–04 格：double robustness（只錯一個 → 0）和 orthogonality（兩個都偏一點 → t²）是同一個乘積的兩種讀法。
- 第 05–06 格：cross-fitting 處理自己改考卷；positivity 差的代價是 SE，任何估計器都救不了。

接下來換真實資料：沒有真值、沒有真的 g 和 Q，只能靠診斷。Lab 5 從 NHEFS 開始。
