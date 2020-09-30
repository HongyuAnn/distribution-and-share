library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
setwd("C:/Users/yl/Desktop/数据/")
dfs <- fread ("data_category.csv", header = T, sep=",")
df <- subset(dfs, price<quantile(dfs$price,0.8))##取样
pinlei <- unique(df$retailtypename)
##建模年度基于品类分品牌二次项回归
result <- c()
for (i in 1:24){
  df2 <- subset (df, retailtypename == pinlei[24])
  
  ##计算每个品牌全年销售额
  sp <- aggregate (df2$amt, by = list (df2$brandcode), sum)
  names(sp)[1:2] <- c("brandcode","amt")
  sp$sk <- sp$amt / sum (sp$amt) * 100
  s1 <- sp[order(-sp$sk),]

  s1$rank<-1:length(s1$sk)

  df2<- left_join(df2,s1,by="brandcode")
  k <- length(unique(df2$brandcode))
  pinpai <- unique(df2$brandcode)
  result1 <- c()
  m <- min(3,k)
  for (n in 1:m){
  ##计算每个品牌sku全年销售额
    df1 <- subset (df2, rank == n)
    s <- aggregate (df1$amt.x, by = list (df1$barcode), sum)
    names(s)[1:2] <- c("barcode","amt")
  
  ##每个品牌sku的市场份额
  
    s$sk <- s$amt / sum (s$amt) * 100
    summary (s$sk)
  
  ##计算每个sku的铺货（加权）
  ###每个商店销售每个sku的销售额
    acv <- aggregate (df1$amt.x, by = list (df1$storecode, df1$barcode), sum)
    names(acv)[1:3] <- c ("storecode", "barcode", "gamt")
  
  ###每个商店的销售总额
    store<-aggregate (df$amt, by = list(df$storecode), sum)
    names(store)[1:2] <- c("storecode", "samt")
    acv1<- left_join (acv, store, by = "storecode")
  
  ###每个sku所在商店的销售额
    acv2 <- aggregate (acv1$samt, by = list (acv1$barcode), sum)
  
  ##每个sku的%acv
    acv2$acv <- acv2$x / sum (store$samt) * 100
    summary (acv2$acv)
    names (acv2) [1:3] <- c("barcode", "x2", "acvk")
    acv4 <- left_join (s, acv2, by = "barcode")
  ##二次项
    acv4$facvk <- acv4$acvk^2
    fit <- lm (acv4$sk ~ acv4$acvk + acv4$facvk)
    result1 <- rbind (result1, coef(summary(fit))[, c(1, 2, 4)])
  }
  result1
  result <- c(result, list(result1))
}

write.csv(result, "result.csv")
