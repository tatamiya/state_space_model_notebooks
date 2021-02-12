# KFASによる状態空間モデル
## ローカルレベルモデル
## 参考：https://sites.google.com/site/iwanamidatascience/vol6/time-series

library(KFAS)
library(ggplot2)
library(gridExtra)

font <- "HiraKakuProN-W3"
options(digits = 3)

# 1. データの読み込み
## 馬場(2019)のデータを使用
## https://github.com/logics-of-blue/book-r-stan-bayesian-model-intro/blob/master/book-data/5-5-1-sales-ts-3.csv
datafile <- "../data/5-5-1-sales-ts-3.csv"
data <- read.csv(datafile)

Y <- data$sales
data$date <- as.POSIXct(data$date)

# 2. モデル構築
## 一次のトレンドモデル（ローカルレベルモデル）
## Q, Hを推定パラメータとしている
model <- SSModel(Y ~ SSMtrend(degree = 1, Q = NA), H=NA)

## パラメータ推定
## 2番目の引数は、パラメータ初期値(numeric(2) = (0, 0))
## methodは最適化手法。今回は準ニュートン法
fit <- fitSSM(model, numeric(2), method="BFGS")

## kalman filter smoothing
## 平滑化で推定した全パラメータを取得する場合は、以下を用いる
## kfs = KFS(fit$model)

## 係数行列
model$Z
model$T
model$R

## 観測ノイズの分散
fit$model$H

## システムノイズの分散
fit$model$Q

## 尤度
logLik(fit$model)

# 3. 結果の出力
## 結果をデータフレームにまとめる関数
makeDataFrameQuantile <- function(data, fitted_model, states="all", interval="confidence", n_ahead=8){
  
  alphahat80 <- predict(fitted_model, interval=interval, states=states, level=0.80)
  alphahat50 <- predict(fitted_model, interval=interval, states=states, level=0.50)
  
  pred80 <- predict(fitted_model, interval=interval, states=states, n.ahead = n_ahead, level=0.80)
  pred50 <- predict(fitted_model, interval=interval, states=states, n.ahead = n_ahead, level=0.50)
  
  date_estimated <- seq(
    from=data$date[1],
    by="days",
    len=length(data$date)+n_ahead)
  
  estimated <- data.frame(date = date_estimated,
                          Value = c(alphahat80[, 1], pred80[, 1]),
                          Lower10 = c(alphahat80[, 2], pred80[, 2]),
                          Lower25 = c(alphahat50[, 2], pred50[, 2]),
                          Upper25 = c(alphahat50[, 3], pred50[, 3]),
                          Upper10 = c(alphahat80[, 3], pred80[, 3])
  )
  
  return(estimated)
}


## 描画用の関数
plotResults <- function(estimated, observed, font){
  
  p <- ggplot(estimated) +
    geom_line(aes(x = date, y = Value), color='gray50') +
    geom_vline(xintercept=tail(observed$date, n=1), linetype='dashed') +
    geom_ribbon(aes(x = date, ymin = Lower10, ymax = Upper10),
                fill = "black", alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = Lower25, ymax = Upper25),
                fill = "black", alpha = 0.25) +
    labs(x = "date", y = "sales") +
    theme_classic(base_family = font) +
    theme(legend.title = element_blank(),
          legend.justification = c(0, 1),
          legend.position = c(0.1, 1)) +
    scale_x_datetime(date_labels = "%Y年%m月")
  if (!is.null(observed)){
    p <- p + geom_line(data=observed, aes(x=date, y=sales), color='black', size=1, alpha=0.9)
  }
  
  return(p)
}


## 観測値（レベル成分+観測ノイズ）
predicted <- makeDataFrameQuantile(data, fit$model, interval="prediction", states="all", n_ahead = 10)
p1 <- plotResults(predicted, data, font)
p1 <- p1 + labs(title="Local Level Model (Level + Noise)")
print(p1)

## レベル成分
estimated <- makeDataFrameQuantile(data, fit$model, states="all")
p2 <- plotResults(estimated, data, font)
p2 <- p2 + labs(title="Local Level Model (Level)")
print(p2)

# おまけ：二次のトレンドモデル


## 二次のトレンドモデル（ローカルレベルモデル）
## Hを推定パラメータとしている
## 状態ノイズQは、第一成分は0で固定、第二成分のみ推定対象
model_2nd_order <- SSModel(Y ~ SSMtrend(degree = 2, Q = c(list(0), list(NA))), H=NA)

## ハイパーパラメータ推定
## 2番目の引数は、パラメータ初期値(numeric(2) = (0, 0))
## methodは最適化手法。今回は準ニュートン法
fit_2nd_order <- fitSSM(model_2nd_order, numeric(2), method="BFGS")


## 観測値（レベル成分+観測ノイズ）
obs_2nd_order <- makeDataFrameQuantile(data, fit_2nd_order$model, interval="prediction", states="all", n_ahead = 10)
p3 <- plotResults(obs_2nd_order, data, font)
p3 <- p3 + labs(title="2nd Order Trend Model (Level + Noise)")
print(p3)

## レベル成分
level_2nd_order <- makeDataFrameQuantile(data, fit_2nd_order$model, states="all")
p4 <- plotResults(level_2nd_order, data, font)
p4 <- p4 + labs(title="2nd Order Trend Model (Level)")
print(p4)
