# KFASによる状態空間モデル
## ローカルレベル+季節成分モデル
## 参考：https://sites.google.com/site/iwanamidatascience/vol6/time-series

library(KFAS)
library(ggplot2)
library(gridExtra)

font <- "HiraKakuProN-W3"
options(digits = 3)

# 1. データの読み込み
## 岩波データサイエンスVol.1 松浦(2015)より
## https://github.com/iwanami-datascience/vol1/matsuura/example2/input/data-season.txt
datafile <- "../data/data-season.txt"
data <- read.csv(datafile)

Y <- data$Y

p <- ggplot(data) +
  geom_line(aes(x=seq_along(Y), y = Y), color='black', size=1) +
  labs(x = "Time [四半期]", y = "販売個数 [千個]") +
  theme_classic(base_family = font) +
  theme(legend.title = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0.1, 1))
print(p)

# 2. モデル構築
## ローカルレベル+季節成分モデル 
## Q, Hを推定パラメータとしている
model <- SSModel(Y ~ SSMtrend(degree = 1, Q=NA) 
                 + SSMseasonal(4, sea.type = "dummy", Q=NA),
                 H=NA)

## パラメータ推定
## 2番目の引数は、パラメータ初期値(numeric(3) = (0, 0, 0))
## methodは最適化手法。今回は準ニュートン法
fit <- fitSSM(model, numeric(3), method="BFGS")

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
makeDataFrameQuantile <- function(Y, fitted_model, states="all", interval="confidence", n_ahead=8){
  
  alphahat80 <- predict(fitted_model, interval=interval, states=states, level=0.80)
  alphahat50 <- predict(fitted_model, interval=interval, states=states, level=0.50)
  
  pred80 <- predict(fitted_model, interval=interval, states=states, n.ahead = n_ahead, level=0.80)
  pred50 <- predict(fitted_model, interval=interval, states=states, n.ahead = n_ahead, level=0.50)
  
  estimated <- data.frame(Time = c(seq_along(Y), length(Y) + seq_along(pred80[,1])),
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
    geom_line(aes(x = Time, y = Value), color='gray50') +
    geom_vline(xintercept=length(Y), linetype='dashed') +
    geom_ribbon(aes(x = Time, ymin = Lower10, ymax = Upper10),
                fill = "black", alpha = 0.5) +
    geom_ribbon(aes(x = Time, ymin = Lower25, ymax = Upper25),
                fill = "black", alpha = 0.25) +
    labs(x = "Time [四半期]", y = "販売個数 [千個]") +
    theme_classic(base_family = font) +
    theme(legend.title = element_blank(),
          legend.justification = c(0, 1),
          legend.position = c(0.1, 1))
  if (!is.null(observed)){
    p <- p + geom_line(data=observed, aes(x=seq_along(Y), y=Y), color='black', size=1, alpha=0.9)
  }
  
  return(p)
}

## レベル成分+季節成分
estimated <- makeDataFrameQuantile(Y, fit$model, states=c("level", "seasonal"))
p1 <- plotResults(estimated, data, font)
p1 <- p1 + labs(title="Level + Seasonal")
print(p1)

## レベル成分
levels <- makeDataFrameQuantile(Y, fit$model, states="level")
p2 <- plotResults(levels, data, font)
p2 <- p2 + labs(title="Level")
print(p2)

## 季節成分
seasonals <- makeDataFrameQuantile(Y, fit$model, states="seasonal")
p3 <- plotResults(seasonals, observed=NULL, font)
p3 <- p3 + labs(title="Seasonal")
print(p3)

## 観測値（レベル成分+季節成分+観測ノイズ）
estimated_obs <- makeDataFrameQuantile(Y, fit$model, states="all", interval='prediction')
p4 <- plotResults(estimated_obs, data, font)
p4 <- p4 + labs(title="Level + Seasonal + Noise")
print(p4)
