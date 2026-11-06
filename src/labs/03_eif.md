---
id: 03_eif
nav: 3 EIF 修正
title: Lab 3　神奇糖果（中）：用 EIF 把一階誤差扣掉——AIPW 與 TMLE
lead: Lab 2 的 plug-in 偏了 3.4，而且偏差跟著 Q̂ 走。怎麼辦？先問一個更基本的問題：資料多一個人，答案會動多少？把這個「動多少」寫成公式，就是 efficient influence function（EIF）。有了它，修正方法就自然冒出來了：加回去（AIPW），或者推 Q̂（TMLE）。對應投影片 68–82。
toolbox: estimators, candy, plot
files: 
time: 全部約 30 秒
---
| 格 | 投影片 | 這格回答的問題 |
|---|---|---|
| 01 | 60、77 | 同一份糖果資料、同一組 nuisance |
| 02–03 | 68–70 | 多一個人，平均動多少？分組平均動多少？（EIF 的兩塊積木） |
| 04 | 71–75 | 拼出 D(O)：它真的平均為 0 嗎？它的變異數是什麼？ |
| 05–06 | 76–81 | 修法一：加回去（AIPW）；修法二：推 Q̂（TMLE） |
| 07 | 82 | 兩個修法解的是同一個方程式 |

::: warn
**以下有一點數學，但每一步都有數字可以對。**看不懂公式也沒關係，只要記得一句話：EIF 告訴你「每一個人把答案往哪邊推、推多大力」；把大家推的力平均起來，就是 plug-in 欠的那一塊。
:::

## 01 同一份資料、同一組 nuisance | 投影片 60、77
和 Lab 2 一模一樣：1,500 隻寶可夢，Q̂ 用 kNN（k = 10），ĝ 用 logistic regression。plug-in 是 84.22，真值 80.82。

```r
truth <- candy_truth()
d  <- simulate_candy(1500, seed = 2026)
nf <- nuisance_full(d, k = 10)
c(truth = truth, plugin = est_plugin(nf))
```

## 02 積木 1：多一個人，平均動多少？ | 投影片 68–69
最簡單的參數：平均數 μ = E(Y)。現在資料裡多一隻 Y 很高的寶可夢，平均會被往上推。推多少？大約是 (Y<sub>new</sub> − 平均)/n——離平均越遠，推越大力。這個「每個人的推力」`Y − E(Y)`，就是平均數的 influence function。

```r
y <- d$Y; n <- length(y)
for (y_new in c(300, 800, 1500)) {
  actual <- mean(c(y, y_new)) - mean(y)                       # how much the mean really moves
  approx <- (y_new - mean(y)) / (n + 1)                       # influence function / (n + 1)
  cat(sprintf("new Y = %4d   mean moves %+.3f   (Y_new - mean) / (n + 1) = %+.3f\n", y_new, actual, approx))
}
```

::: look
三個數字完全對上。Y = 300 的那隻比平均低，把平均往下拉；Y = 1500 的那隻往上推很多。**influence function 其實就是「多一個人，答案動多少」，乘上 n 而已。**
:::

## 03 積木 2：分組平均——只有那一組的人會推動它 | 投影片 69–70
分組平均 E(Y | X = x)，比如「伊布的平均 CP」。多一隻皮皮（Pidgey），伊布的平均不會動；多一隻伊布，才會動。而且伊布越少，每一隻推得越大力：推力是 `I(X = x) / P(X = x) · {Y − E(Y | X = x)}`。

```r
is_ev <- d$species == "Eevee"; p_ev <- mean(is_ev); m_ev <- mean(d$Y[is_ev])
new_ev <- data.frame(species = "Eevee", Y = 1500); new_pg <- data.frame(species = "Pidgey", Y = 1500)
move <- function(new) { s <- c(as.character(d$species), new$species); yy <- c(d$Y, new$Y); mean(yy[s == "Eevee"]) - m_ev }
rbind(add_an_Eevee  = c(actual = move(new_ev), formula = 1 / p_ev * (1500 - m_ev) / (n + 1)),
      add_a_Pidgey  = c(actual = move(new_pg), formula = 0))
c(share_of_Eevee = p_ev, push_multiplier = 1 / p_ev)
```

::: look
多一隻伊布，伊布平均動了；多一隻皮皮，完全不動（第一項 I(X = x) = 0）。伊布只占約四分之一，所以每一隻伊布的推力被放大約 4 倍——**那一組越稀有，每個人推得越大力。**等一下你會在 D(O) 裡看到同一個東西：1/g。
:::

## 04 拼出 D(O)：先在四個人身上，再用真的 g、Q 檢查 | 投影片 71–75
用加法規則和乘法規則把兩塊積木拼起來，ATE 的 EIF 是 `D(O; g, Q, ψ) = H(A, X){Y − Q_A(X)} + Q₁(X) − Q₀(X) − ψ`，H = A/g − (1−A)/(1−g)。先用人造的四個人只看 ψ₁ 那一半：預測是 0.4，修正之後是多少？

```r
toy <- data.frame(A = c(1, 0, 1, 0), Y = c(1, 0, 0, 1), Q1hat = c(.2, .2, .6, .6), ghat = c(.5, .5, .25, .25))
toy$contribution <- with(toy, Q1hat + A * (Y - Q1hat) / ghat)    # one-step contribution for psi_1
toy
c(plugin = mean(toy$Q1hat), one_step = mean(toy$contribution))    # 0.4 -> 0.2
true_nu <- data.frame(Q1 = d$Q1, Q0 = d$Q0, g = d$g0)             # the true nuisances: only a simulation has them
D <- eif(d, true_nu, truth)
pop <- candy_population(20000)
c(mean_D = mean(D), sd_D = sd(D), SE_from_D = sd(D) / sqrt(n), efficiency_bound_SE = candy_se_bound(pop, n))
```

::: look
- 第 3 個人預測 0.6、實際 0，殘差 −0.6，ĝ = 0.25 → 放大 4 倍，把 plug-in 從 0.4 拉到 0.2。個人貢獻可以小於 0，它不是機率。
- 代真的 g、Q：1,500 隻的 D 平均約 0（只剩抽樣波動）——**平均為 0。**
- D 的標準差除以 √n 約 1.91，和理論上任何估計器都打不破的下界 `√(V_eff / n)`（約 1.89）幾乎一樣——**變異數就是效率界。**投影片 75 的兩句話，在這裡變成兩個數字。
:::

## 05 修法一：把一階項估出來，加回去（AIPW） | 投影片 76–79
泰勒展開告訴我們：`ψ̂_plug − ψ ≈ −P D(O; ĝ, Q̂, ψ̂_plug)`。真實分布 P 我們拿不到，就用樣本平均 ℙn 代替，把它加回去：`ψ̂ = ψ̂_plug + ℙn D(O; ĝ, Q̂, ψ̂_plug)`。代入 D 的定義，ψ̂_plug 自己消掉，就是 AIPW。

```r
plug <- est_plugin(nf)
one_step <- plug + mean(eif(d, nf, plug))        # psi_plug + P_n D-hat
H  <- d$A / nf$g - (1 - d$A) / (1 - nf$g); QA <- ifelse(d$A == 1, nf$Q1, nf$Q0)
c(plugin = plug, correction = mean(H * (d$Y - QA)), one_step = one_step, truth = truth)
est_aipw(d, nf)                                   # same number, plus an EIF-based SE and 95% CI
mean(eif(d, nf, one_step))                        # P_n D(O; g-hat, Q-hat, psi_AIPW) = 0
```

::: look
plug-in 84.22 + 修正項 −3.88 = 80.34，真值 80.82——投影片 79 的那條算式。最後一行驗證投影片 77 那句話：AIPW 剛好讓 ℙ<sub>n</sub>D̂ = 0。修正項是什麼？就是第 03 格的推力：每一隻的殘差 Y − Q̂<sub>A</sub>，乘上 1/ĝ（越少見的那一臂，推得越大力），平均起來。
:::

## 06 修法二：不改答案，改 Q̂（TMLE） | 投影片 80–81
AIPW 是答案算完再加一個修正項。TMLE 反過來：先把 Q̂ 沿著 H 的方向推一點點，推到 ℙ<sub>n</sub>D = 0 為止，再 plug in。`logit Q̂ε(a, X) = logit Q̂(a, X) + ε·H(a, X)`。Y 先縮到 [0, 1]，對 H 做沒有截距、以 logit Q̂ 為 offset 的 logistic regression，估出 ε̂。

```r
tm <- est_tmle(d, nf); tm
lo <- min(d$Y); hi <- max(d$Y); sc <- function(y) pmin(pmax((y - lo) / (hi - lo), 1e-6), 1 - 1e-6)
QAs <- sc(QA); QAstar <- expit(qlogis(QAs) + tm[["eps"]] * H)        # updated prediction for the observed arm
c(score_before = mean(H * ((d$Y - lo) / (hi - lo) - QAs)), score_after = mean(H * ((d$Y - lo) / (hi - lo) - QAstar)))
```

::: look
ε̂ 很小（約 −0.003），因為 kNN 的 Q̂ 本來就不差。score 從 before 變成 after ≈ 0：logistic fit 的一階條件就是 ℙ<sub>n</sub>[Ĥ(Y − Q̂*<sub>A</sub>)] = 0。答案 80.37，和 AIPW 只差 0.03。
:::

## 07 同一個方程式，兩種解法 | 投影片 82
把 Lab 2、Lab 3 所有的答案放在一起，對著真值看。

```r
tab <- rbind(naive = c(est = mean(d$Y[d$A == 1]) - mean(d$Y[d$A == 0]), lo = NA, hi = NA),
             linear = c(unname(coef(lm(Y ~ A + species + cp, data = d))["A"]), NA, NA),
             plugin_kNN = c(plug, NA, NA), IPTW = c(est_ipw(d, nf), NA, NA),
             AIPW = est_aipw(d, nf)[c("est", "lo", "hi")], TMLE = tm[c("est", "lo", "hi")])
round(tab, 2)
keep <- rownames(tab) != "naive"                                  # naive (229) would squash the x axis
forest_plot(rownames(tab)[keep], tab[keep, 1], tab[keep, 2], tab[keep, 3], ref = truth, ref_lab = "truth 80.8",
            col = c(COL$muted, COL$red, COL$gold, COL$teal, COL$teal), xlab = "ATE estimate (CP)")
```

::: look
| | AIPW（one-step） | TMLE（targeting） |
|---|---|---|
| 怎麼讓 ℙ<sub>n</sub>D = 0 | 直接把殘差修正的平均加到答案上 | 先推 Q̂，讓殘差修正平均變 0 |
| 最後的答案 | ψ̂_plug + ℙ<sub>n</sub>D̂ | ℙ<sub>n</sub>(Q̂*₁ − Q̂*₀) |
| 結果範圍 | 沒有保證（加法修正可能出界） | 尊重 Y 的範圍 |
| 漸近性質 | 一樣：同一個 D、同一個效率界 | 一樣 |

圖上：直線、plug-in、IPTW 各自往一邊偏；AIPW、TMLE 的 CI 都蓋到真值。兩個答案幾乎重疊——因為它們解的是同一個方程式。結果範圍那一列，Lab 6（RHC，二元結果）會看到差別。
:::

## 08 帶走一句話
::: punch
EIF 就是「每個人把答案推多少」。把推力平均起來加回去是 AIPW；把 Q̂ 推到推力平均為 0 是 TMLE。
:::

你可能會想說：修完就沒有誤差了嗎？沒有。泰勒展開還有一個二階項 R₂，還有「自己改考卷」那一塊。Lab 4 把剩下的東西一塊一塊拆開來看。
