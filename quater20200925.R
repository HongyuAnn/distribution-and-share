library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
setwd("C:/Users/yl/Desktop/数据/")
##sku级别分销-份额关系（季度）
dfs <- fread ("data_category.csv", header = T, sep=",")
dfs$quater <- quarter(dfs$saledate)
df <- subset(dfs, price<quantile(dfs$price,0.8))##取样


##计算每个sku每季度销售额
s <- aggregate (df$amt, by = list (df$barcode,df$quater), sum)
names (s) [1:3] <- c ("barcode","quater" ,"amt")
s1 <- aggregate (df$amt, by = list(df$quater), sum )
names(s1) [1:2] <- c ("quater", "amts")
sk <- left_join(s, s1, by = "quater")
##每个sku的市场份额
sk$sk <- sk$amt /sk$amts * 100
summary (sk$sk)

##计算每个sku的铺货（加权）
###每个商店销售每个sku的销售额
acv <- aggregate (df$amt, by = list (df$storecode, df$barcode,df$quater), sum)
names (acv)[1:4] <- c("storecode", "barcode", "quater", "gamt")

###每个商店的销售总额
store <- aggregate (df$amt, by = list (df$storecode, df$quater), sum)
names (store) [1:3] <- c("storecode", "quater", "samt")
acv1 <- left_join (acv, store, by = c("storecode", "quater"))

###每个sku所在商店的销售额
acv2 <- aggregate (acv1$samt, by = list (acv1$barcode, acv1$quater), sum)
names (acv2)[1:3] <- c("barcode", "quater", "x")
store1 <- aggregate(store$samt,by = list(store$quater), sum)
names (store1)[1:2] <- c("quater", "sale")
acv3 <- left_join(acv2, store1,by = "quater")
##每个sku的%acv
acv3$acvk <- acv3$x / acv3$sale * 100
summary (acv3$acv)
acv4 <- left_join (sk, acv3, by = c ("barcode", "quater"))

##可视化
q4 <- subset.data.frame(acv4, quater==4)
dev.new()
ggplot (q4, aes (x = acvk, y = sk)) + geom_point (size = 3, shape = 21)

##品类级别分销-份额关系（季度）
pinlei<-unique(df$retailtypename)
###1季度

df1 <- subset (df, retailtypename == pinlei[24]&quater==4)

##计算每个sku整季度销售额
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

##可视化
dev.new()
ggplot (acv4, aes(x = acvk, y = sk)) + geom_point (size = 3,shape = 21)
