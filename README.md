# Pokémon-CP Lab：預測很準，就能回答因果嗎？

搭配簡報《Machine learning in causal inference for epidemiology》（1106 最終版，100 頁，其中 2 頁隱藏）的八個 R Lab。順序跟簡報一樣，每一頁都標了對應的投影片頁碼。

| Lab | 簡報章節 | 內容 |
|---|---|---|
| 1 找函式 | 1–54 | 寶可夢 CP：三步驟、gradient descent、overfitting、bias／variance、ridge、CV、SuperLearner |
| 2 plug-in | 55–68 | 神奇糖果：同一隻預測兩次；**預測最準 ≠ ATE 最準**；plug-in 誤差拆三塊；IPTW |
| 3 EIF 修正 | 70–84 | 多一個人答案動多少（EIF 積木）；D(O) 平均為 0、變異數 = 效率界；AIPW、TMLE |
| 4 剩下什麼 | 85–94 | R₂ 拆解、每隻波波的誤差、**double robustness 2×2**、orthogonality、cross-fitting、positivity、重複模擬 |
| 5 NHEFS | 56–57、77、81–84、87、93–95 | Karim (2026) 主線：診斷、AIPW／TMLE／DML／CV-TMLE、敏感度 |
| 6 RHC | 63、84–87、93 | random forest 自己改考卷；二元結果時 TMLE 的範圍優勢 |
| 7 IHDP | 81–84、87、92–93 | 知道真值：bias、RMSE、coverage |
| 8 LaLonde | 56、58、67–68、77、84、93 | overlap 不存在、權重集中、trimming 改變 estimand |

## 兩種開法

- **網頁（webR）**：雙擊 `start_lab.bat`（需要 Python 3），會開 http://localhost:8767 。或放到任何靜態網站（GitHub Pages 也可以）。計算全部在瀏覽器裡用真正的 R 完成。
- **Colab**：`colab/` 裡每個 Lab 一本 `.ipynb`（R runtime）。到 colab.research.google.com → 上傳筆記本；第一格會從網路下載這個 Lab 需要的資料。網頁版每一頁也有「下載 Colab 版 .ipynb」的連結。

## 跟舊版 `lab/` 有什麼不一樣：程式都在頁面裡

舊版的頁面要 `source("R/helpers.R")`，所以一定要連同 `R/` 資料夾一起放。這帶來兩個問題：學生看不到工具函式寫了什麼，而且瀏覽器會快取舊的 `.R` 檔（上次遇到的 `dec$units` 錯誤就是這樣來的）。

新版的做法：

- **網頁**：每一頁最上面有一格摺起來的「工具箱」，裡面就是這一頁用到的全部 R 函式，會自動先執行，點開就看得到每一行。頁面只需要 `data/` 和 `web/`，不需要 `R/`。
- **Colab**：同一份工具箱是筆記本的第二格。
- **維護**：工具箱的原始碼放在 `src/toolbox/*.R`（估計器、糖果模擬、真實資料、畫圖……各一個檔），Lab 內容放在 `src/labs/*.md`。改完執行一次：

```
python src/build.py
```

就會重新產生 8 個 HTML 和 8 本 notebook。**不要直接改 HTML 或 .ipynb**，下次 build 會被覆蓋。

為什麼不乾脆把每個 Lab 的工具函式手寫在各自的頁面裡？因為同一個 `est_aipw()` 會出現在 7 個 Lab：拆成共用的原始檔、build 時嵌進去，學生看到的是完整的程式，改一個地方就全部更新。

### `src/labs/*.md` 的寫法

```
---
id: 02_plugin                     檔名（輸出 02_plugin.html、colab/02_plugin.ipynb）
nav: 2 plug-in                    上方導覽列的字
title: …                          頁面標題
lead: …                           標題下面那段話
toolbox: estimators, candy, plot  要嵌進來的 src/toolbox/*.R
files: data/rhc.csv               要掛載／下載的資料檔
packages: randomForest            需要安裝的套件
time: 全部約 1 分鐘
---
## 01 標題 | 投影片 43–44        小節（| 後面是投影片頁碼）
一般段落、- 條列、| 表格 |，**粗體**、`程式碼`
```r                              R 程式格
...
```
::: look / ::: think 問題 / ::: note / ::: warn / ::: punch
…
:::
```

## Colab 的資料從哪裡下載

`src/build.py` 的 `DATA_BASE` 設定成 `https://raw.githubusercontent.com/kiwi710680/Pokemon-ML/main/`（已經公開、裡面有全部資料）。如果之後把這個資料夾放到新的 GitHub repo，改 `DATA_BASE` 再 build 一次即可。設定環境變數 `POKEMON_CP_REPO=owner/repo` 再 build，每一頁還會多一個「在 Colab 開啟」的直接連結。

## 資料夾

```
index.html, 01_ml.html … 08_lalonde.html   網頁版（build 產生）
colab/*.ipynb                               Colab 版（build 產生）
data/                                       資料
web/runner.js, web/style.css                webR 執行器與樣式
src/labs/*.md, src/toolbox/*.R, src/build.py   原始檔與產生器
start_lab.bat                               本機開啟
```

## 資料來源

寶可夢：Pokémon GO 紀錄（OpenIntro），課堂切分依投影片重建。神奇糖果：虛構，`simulate_candy()` 模擬。NHEFS：CRAN causaldata；Hernán & Robins。RHC：Connors et al. (1996)；hbiostat.org/data。IHDP：Hill (2011)；CEVAE 版 ihdp_npci_1–10。LaLonde：LaLonde (1986)；Dehejia & Wahba 整理版。

分析方法參考 Karim (2026) *Three Routes to One Answer* 與 github.com/ehsanx/dml-alternatives；程式碼為獨立撰寫。教學敘事受李宏毅教學風格啟發，非本人授課或背書。
