library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
setwd("C:/Users/yl/Desktop/����/")
##sku�������-�ݶ��ϵ�����ȣ�
dfs <- fread ("data_category.csv", header = T, sep=",")
dfs$quater <- quarter(dfs$saledate)
df <- subset(dfs, price<quantile(dfs$price,0.8))##ȡ��


##����ÿ��skuÿ�������۶�
s <- aggregate (df$amt, by = list (df$barcode,df$quater), sum)
names (s) [1:3] <- c ("barcode","quater" ,"amt")
s1 <- aggregate (df$amt, by = list(df$quater), sum )
names(s1) [1:2] <- c ("quater", "amts")
sk <- left_join(s, s1, by = "quater")
##ÿ��sku���г��ݶ�
sk$sk <- sk$amt /sk$amts * 100
summary (sk$sk)

##����ÿ��sku���̻�����Ȩ��
###ÿ���̵�����ÿ��sku�����۶�
acv <- aggregate (df$amt, by = list (df$storecode, df$barcode,df$quater), sum)
names (acv)[1:4] <- c("storecode", "barcode", "quater", "gamt")

###ÿ���̵�������ܶ�
store <- aggregate (df$amt, by = list (df$storecode, df$quater), sum)
names (store) [1:3] <- c("storecode", "quater", "samt")
acv1 <- left_join (acv, store, by = c("storecode", "quater"))

###ÿ��sku�����̵�����۶�
acv2 <- aggregate (acv1$samt, by = list (acv1$barcode, acv1$quater), sum)
names (acv2)[1:3] <- c("barcode", "quater", "x")
store1 <- aggregate(store$samt,by = list(store$quater), sum)
names (store1)[1:2] <- c("quater", "sale")
acv3 <- left_join(acv2, store1,by = "quater")
##ÿ��sku��%acv
acv3$acvk <- acv3$x / acv3$sale * 100
summary (acv3$acv)
acv4 <- left_join (sk, acv3, by = c ("barcode", "quater"))

##���ӻ�
q4 <- subset.data.frame(acv4, quater==4)
dev.new()
ggplot (q4, aes (x = acvk, y = sk)) + geom_point (size = 3, shape = 21)

##Ʒ�༶�����-�ݶ��ϵ�����ȣ�
pinlei<-unique(df$retailtypename)
###1����

df1 <- subset (df, retailtypename == pinlei[24]&quater==4)

##����ÿ��sku���������۶�
s <- aggregate (df1$amt, by = list (df1$barcode), sum)
names(s)[1:2] <- c("barcode","amt")

##ÿ��sku���г��ݶ�
s$sk <- s$amt / sum (s$amt) * 100
summary (s$sk)

##����ÿ��sku���̻�����Ȩ��
###ÿ���̵�����ÿ��sku�����۶�
acv <- aggregate (df1$amt, by = list (df1$storecode, df1$barcode), sum)
names(acv)[1:3] <- c ("storecode", "barcode", "gamt")

###ÿ���̵�������ܶ�
store<-aggregate (df$amt, by = list(df$storecode), sum)
names(store)[1:2] <- c("storecode", "samt")
acv1<- left_join (acv, store, by = "storecode")

###ÿ��sku�����̵�����۶�
acv2 <- aggregate (acv1$samt, by = list (acv1$barcode), sum)

##ÿ��sku��%acv
acv2$acv <- acv2$x / sum (store$samt) * 100
summary (acv2$acv)
names (acv2) [1:3] <- c("barcode", "x2", "acvk")
acv4 <- left_join (s, acv2, by = "barcode")

##���ӻ�
dev.new()
ggplot (acv4, aes(x = acvk, y = sk)) + geom_point (size = 3,shape = 21)