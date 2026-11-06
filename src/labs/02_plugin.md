---
id: 02_plugin
nav: 2 plug-in
title: Lab 2　神奇糖果（上）：把 regression 變成因果效果，然後看它歪掉
lead: 訓練師在進化前，會餵一些寶可夢吃「神奇糖果」（A = 1）。糖果到底讓進化後 CP 平均多多少？Lab 1 學會的 regression 怎麼變成這個答案（plug-in）？用了最會預測的 learner，答案為什麼還是歪的？對應投影片 55–68。
toolbox: estimators, candy, plot
files: 
time: 全部約 1–2 分鐘，最慢的是第 05、06 格
---
| 格 | 投影片 | 這格回答的問題 |
|---|---|---|
| 01–02 | 56–59、62 | 誰吃到糖？直接相減錯在哪？ |
| 03–04 | 60–62 | 因果問題怎麼變成 regression？plug-in 是什麼？Q 該用哪個模型？ |
| 05–06 | 63–65 | 為什麼預測很準，ATE 還是歪？歪的那一塊有多大？ |
| 07 | 67–68 | 另一條路 IPTW：只信 g 會怎樣？ |

::: warn
**虛構情境、模擬資料。**因為資料是我們自己生成的，**真值 ψ ≈ 80.8**，真的 Q 和 g 也都知道——所以可以做真實研究做不到的事：直接看答案錯了多少、錯在哪裡。Lab 5–8 換成真實資料，就沒有這個奢侈了。
:::

## 01 誰吃到糖？ | 投影片 58–59
X = (物種, 進化前 CP)，A = 有沒有吃糖，Y = 進化後 CP。訓練師偏心：CP 高的、伊布，比較容易吃到糖。CP 本身又決定進化後 CP。X 同時影響 A 和 Y——這就是 confounding。

```r
truth <- candy_truth()                     # population truth psi = E[Y(1) - Y(0)]
d <- simulate_candy(1500, seed = 2026)
head(d[, c("species", "cp", "A", "Y")])
table(species = d$species, Candy = d$A)
round(tapply(d$cp, d$A, mean), 1)           # Pokemon that got candy start with higher CP
pg <- d[d$species == "Pidgey", ]
lab_par()
plot(pg$cp, pg$Y, pch = 19, col = ifelse(pg$A == 1, COL$red, COL$teal), xlab = "Original CP (Pidgey)", ylab = "CP after evolution")
legend("topleft", c("Candy A = 1", "No Candy A = 0"), pch = 19, col = c(COL$red, COL$teal), bty = "n")
```

::: look
紅點（吃糖）集中在右上——吃糖的本來就比較強。直接比較兩組，會把「本來就比較強」算成糖果的功勞。
:::

## 02 直覺做法：直接相減、一條直線 | 投影片 59、62
```r
c(truth = truth,
  naive_difference = mean(d$Y[d$A == 1]) - mean(d$Y[d$A == 0]),
  linear_adjusted = unname(coef(lm(Y ~ A + species + cp, data = d))["A"]))
```

::: look
直接相減 229，差了快三倍（confounding）。用一條直線同時調整物種和 CP，得到 74.8：好很多，但還是往另一邊偏了 6（model misspecification：關係會彎，糖果效果還跟著 CP 變大，一個 A 的係數放不下）。
:::

## 03 Plug-in：同一隻寶可夢，預測兩次 | 投影片 60–61
識別假設成立時，`ψ = E[Q₁(X) − Q₀(X)]`，Q 就是 Lab 1 的 regression。先學 Q̂（這裡用 kNN：同物種、同一組裡找 CP 最接近的 10 隻取平均），對每一隻「假裝吃糖」和「假裝沒吃」各預測一次，相減、平均，就結束了。這叫 g-computation，也叫 plug-in：`ψ̂ = (1/n) Σ {Q̂₁(Xᵢ) − Q̂₀(Xᵢ)}`。順便也把另一個 nuisance——propensity score `g(X) = P(A = 1 | X)`——估起來，第 07 格會用到。

```r
nf <- nuisance_full(d, k = 10)               # Q1 = Q-hat(1, X), Q0 = Q-hat(0, X) via kNN; g = g-hat(X) via logistic regression
head(cbind(d[, c("species", "cp", "A", "Y")], Q1 = round(nf$Q1, 1), Q0 = round(nf$Q0, 1), diff = round(nf$Q1 - nf$Q0, 1)))
c(truth = truth, plugin_kNN = est_plugin(nf))
```

::: look
每一列都有 Q1、Q0：吃糖的那隻，Q0 是「如果牠沒吃」的預測；沒吃的那隻，Q1 是「如果牠吃了」。diff 欄平均起來就是答案 84.2——比一條直線（74.8，少了 6.0）近一些，但還是比真值多了 3.4。
:::

## 04 那 Q 要用哪一個模型？kNN 在哪裡猜錯 | 投影片 62
低 CP 的波波很少吃糖。要猜「低 CP 波波**如果**吃了糖」，kNN 找不到夠近的鄰居，只好拿比較強的來充數。

```r
xs <- data.frame(species = factor("Pidgey", levels = species_levels), cp = 10:460, A = 1, Y = 0)
q_knn  <- knn_Q(d, xs, a = 1, k = 10)
q_true <- 1.85 * xs$cp * (1 - xs$cp / 2600) + 20 + 0.30 * xs$cp
lab_par()
plot(pg$cp[pg$A == 1], pg$Y[pg$A == 1], pch = 19, col = adjustcolor(COL$red, .5), xlim = c(0, 470),
     xlab = "Original CP (Pidgey)", ylab = "CP after evolution if eat Candy")
lines(xs$cp, q_true, lwd = 3, lty = 2); lines(xs$cp, q_knn, lwd = 3, col = COL$red, type = "s")
rug(pg$cp[pg$A == 1], col = COL$red)
legend("topleft", c("true Q1(x)", "kNN Q1-hat(x)"), lty = c(2, 1), lwd = 3, col = c(COL$ink, COL$red), bty = "n")
```

::: look
下方短線是吃糖的波波，左邊很稀疏。那裡紅色階梯（Q̂₁）明顯高於虛線（Q₁）。這種寶可夢不多，預測 loss 幾乎感覺不到；但 plug-in 要替**每一隻**預測另一個世界，偏差就一路傳到 ψ̂。
:::

## 05 千萬不要說：「預測 loss 很低，plug-in 就準」 | 投影片 63
Lab 1 的做法是：挑預測 loss 最小的模型。那就把它用在這裡——另外抽 2,000 隻寶可夢當測試資料，比較一條直線和不同 k 的 kNN：誰預測最準？誰的 ATE 最準？

```r
test <- simulate_candy(2000, seed = 99)                       # fresh Pokemon, never used for training
learners <- list(linear = 0, kNN_k1 = 1, kNN_k3 = 3, kNN_k10 = 10, kNN_k30 = 30, kNN_k100 = 100)
cmp <- t(sapply(learners, function(k) {
  Qa <- function(nd, a) if (k == 0) lin_Q(d, nd, a) else knn_Q(d, nd, a, k)
  pred <- ifelse(test$A == 1, Qa(test, 1), Qa(test, 0))       # predict the outcome we can see
  plug <- mean(Qa(d, 1) - Qa(d, 0))                           # plug-in ATE on the study sample
  c(test_RMSE = sqrt(mean((test$Y - pred)^2)), plugin_ATE = plug, ATE_error = plug - truth)
}))
round(cmp, 2)
best <- which.min(cmp[, "test_RMSE"])                         # what Lab 1's rule would pick
lab_par(mar = c(5.2, 6, 1.5, 1), mgp = c(4, .8, 0))
plot(cmp[, "test_RMSE"], abs(cmp[, "ATE_error"]), pch = 19, cex = 2, col = COL$red, log = "y", xlim = c(30, 125),
     xlab = "prediction error on new Pokemon (RMSE)", ylab = "|ATE error| (log scale)")
points(cmp[best, "test_RMSE"], abs(cmp[best, "ATE_error"]), cex = 4, lwd = 2)
text(cmp[, "test_RMSE"], abs(cmp[, "ATE_error"]), sub("_", " ", rownames(cmp)), pos = 4, offset = 1, col = COL$ink)
legend("topleft", "lowest prediction error", pch = 1, pt.cex = 2.5, pt.lwd = 2, bty = "n")
```

::: look
- 預測最準的是 kNN k = 10（RMSE 約 33，圖上畫圈的那一點）——Lab 1 的規則會選它。可是它的 ATE 偏了 3.4。
- k = 1、k = 3 預測比較差（RMSE 42、36），ATE 反而只偏 0.1–0.2。
- 圖上的點沒有排成一條線：**預測 loss 的排名，不是 ATE 誤差的排名。**
:::

::: think 為什麼會這樣？投影片 63 的三個原因，各對應這張圖的哪個地方？
原因 1（考的範圍不一樣）：RMSE 平均的是「看得到的那一臂」，ATE 要預測「另一個世界」，而且偏偏落在吃糖很少的低 CP 那一區。原因 2（ML 故意偏一點）：k 越大越平滑，預測的 variance 小了，但平滑造成的偏差會原封不動傳進 ψ̂——k = 30、100 就是這樣。原因 3（自己出題自己改）：這一格的 Q̂ 是用同一份資料訓練又預測；k = 1 時最近的鄰居常常就是自己，Lab 4 第 05 格會看到它怎麼騙過 SE。所以「挑 loss 最小的 learner」是對預測的最佳化，不是對 ψ 的最佳化。
:::

## 06 把 plug-in 的誤差拆成三塊 | 投影片 64–65
那 k = 10 偏掉的 3.4 是從哪裡來的？投影片 64 的式子：

`ψ̂_plug − ψ = (ℙn − P){Q₁ − Q₀}　+　P{(Q̂₁ − Q₁) − (Q̂₀ − Q₀)}　+　(ℙn − P){(Q̂₁ − Q₁) − (Q̂₀ − Q₀)}`

真實研究裡這三塊都看不到。但模擬資料知道真的 Q，「母體平均 P」可以用另外 8,000 隻同分布的寶可夢近似——所以三塊都算得出來（約 20 秒）。

```r
pop <- candy_population(8000)
dec <- decompose_candy(d, pop, k = 10, scheme = "full")
round(dec$plugin, 2)
lab_par(mar = c(4.5, 11, 1, 1))
barplot(rev(dec$plugin), horiz = TRUE, names.arg = rev(c("total error", "sampling error", "first-order bias", "empirical process")),
        col = rev(c(COL$ink, COL$teal, COL$red, COL$plum)), border = NA, xlab = "error of the estimate (CP)")
abline(v = 0)
```

::: look
- 總誤差約 3.4，最大的一塊是紅色的 first-order bias（約 4.8）——Q̂ 估錯、平均之後跟著偏的那一塊，它的大小跟 ‖Q̂ − Q‖ 同一個量級。
- 藍色的抽樣誤差（約 0.2）本來就該有，是 CI 要描述的東西。
- 紫色（約 −1.6）是「同一份資料又訓練又評分」留下來的那一塊。
- 所以「把預測做準」不夠：ML 學得多慢，first-order bias 就多大。怎麼辦？Lab 3 會把這一塊估出來、扣掉。
:::

## 07 另一條路：IPTW | 投影片 67–68
如果不信 Q̂，只信 ĝ 呢？很少吃糖的那類，少數吃了糖的要代表更多同類：權重 1/ĝ。HT 版本直接乘權重除以 n；Hájek（stabilized）版本除以各組權重總和。

```r
c(truth = truth, IPTW_HT = est_ipw_ht(d, nf), IPTW_Hajek = est_ipw(d, nf))
round(weight_summary(d, nf), 1)        # largest weight and effective sample size (ESS) per arm
overlap_plot(nf$g, d$A, labels = c("candy", "no candy"))
```

::: look
ĝ 從 0.09 到 0.87，兩組廣泛重疊；最大權重約 10，有效樣本數掉了一些但還健康。這次 IPTW（約 77.5）往另一邊偏了 3。plug-in 只信 Q̂，IPTW 只信 ĝ——兩個都是「只買一張保險」。
:::

## 08 帶走一句話
::: punch
預測最準的 learner，不等於 ATE 最準；plug-in 的偏差跟著 Q̂ 的誤差走。
:::

兩張保險各自都會歪。那能不能兩張一起買，而且讓 Q̂ 的錯誤不要一路傳進答案？Lab 3：用 EIF 把一階誤差估出來、扣掉。
