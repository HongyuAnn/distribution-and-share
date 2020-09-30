library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
setwd("C:/Users/yl/Desktop/数据/")
##sku级别分销-份额关系（年度）
dfs <- fread ("data_category.csv", header = T, sep=",")

df <- subset(dfs, price<quantile(dfs$price,0.8))##取样
##计算每个sku全年销售额
s <- aggregate (df$amt, by = list (df$barcode), sum)
names (s) [1:2] <- c ("barcode", "amt")

##每个sku的市场份额
s$sk <- s$amt / sum (s$amt) * 100
summary (s$sk)

##计算每个sku的铺货（加权）
###每个商店销售每个sku的销售额
acv <- aggregate (df$amt, by = list (df$storecode, df$barcode), sum)
names (acv)[1:3] <- c("storecode", "barcode", "gamt")

###每个商店的销售总额
store <- aggregate (df$amt, by = list (df$storecode), sum)
names (store) [1:2] <- c("storecode", "samt")
acv1 <- left_join (acv, store, by = "storecode")

###每个sku所在商店的销售额
acv2 <- aggregate (acv1$samt, by = list (acv1$barcode), sum)
##每个sku的%acv
acv2$acv <- acv2$x / sum (store$samt) * 100
summary (acv2$acv)
names (acv2) [1:3] <- c("barcode", "x2", "acvk")
acv4 <- left_join (s, acv2, by = "barcode")
##建模一般公共参数模型
acv4$facvk<-acv4$acvk^2
lm1<-lm(acv4$sk~acv4$acvk+acv4$facv)
summary(lm1)
##可视化
dev.new()
ggplot (acv4, aes (x = acvk, y = sk)) + geom_point (size = 3, shape = 21)

##品类级别分销-份额关系（年度）
pinlei<-unique(df$retailtypename)
df1 <- subset (df, retailtypename == pinlei[1])

##计算每个sku全年销售额
s <- aggregate (df1$amt, by = list (df1$barcode), sum)
names(s)[1:2] <- c("barcode","amt")

##每个sku的市场份额
s$sk <- s$amt / sum (s$amt) * 100
summary (s$sk)

##计算每个sku的铺货（加权）
###每个商店销售每个sku的销售额
acv <- aggregate (df1$amt, by = list (df1$storecode, df1$barcode), sum)
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
##建模
acv4$facvk<-acv4$acvk^2
lmp1<-lm(acv4$sk~acv4$acvk+acv4$facv)
summary(lmp1)
##建模
result1 <- c()
for (i in 1:24){
  df1 <- subset (df, retailtypename == pinlei[i])
  
  ##计算每个sku全年销售额
  s <- aggregate (df1$amt, by = list (df1$barcode), sum)
  names(s)[1:2] <- c("barcode","amt")
  
  ##每个sku的市场份额
  s$sk <- s$amt / sum (s$amt) * 100
  summary (s$sk)
  
  ##计算每个sku的铺货（加权）
  ###每个商店销售每个sku的销售额
  acv <- aggregate (df1$amt, by = list (df1$storecode, df1$barcode), sum)
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
write.csv(result1, "result.csv")

coef(summary(fit))[,c(1,2,4)]
dev.new()
ggplot (acv4, aes(x = acvk, y = sk)) + geom_point (size = 3,shape = 21)
