library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
setwd("C:/Users/yl/Desktop/����/")
##sku�������-�ݶ��ϵ����ȣ�
dfs <- fread ("data_category.csv", header = T, sep=",")

df <- subset(dfs, price<quantile(dfs$price,0.8))##ȡ��
##����ÿ��skuȫ�����۶�
s <- aggregate (df$amt, by = list (df$barcode), sum)
names (s) [1:2] <- c ("barcode", "amt")

##ÿ��sku���г��ݶ�
s$sk <- s$amt / sum (s$amt) * 100
summary (s$sk)

##����ÿ��sku���̻�����Ȩ��
###ÿ���̵�����ÿ��sku�����۶�
acv <- aggregate (df$amt, by = list (df$storecode, df$barcode), sum)
names (acv)[1:3] <- c("storecode", "barcode", "gamt")

###ÿ���̵�������ܶ�
store <- aggregate (df$amt, by = list (df$storecode), sum)
names (store) [1:2] <- c("storecode", "samt")
acv1 <- left_join (acv, store, by = "storecode")

###ÿ��sku�����̵�����۶�
acv2 <- aggregate (acv1$samt, by = list (acv1$barcode), sum)
##ÿ��sku��%acv
acv2$acv <- acv2$x / sum (store$samt) * 100
summary (acv2$acv)
names (acv2) [1:3] <- c("barcode", "x2", "acvk")
acv4 <- left_join (s, acv2, by = "barcode")
##��ģһ�㹫������ģ��
acv4$facvk<-acv4$acvk^2
lm1<-lm(acv4$sk~acv4$acvk+acv4$facv)
summary(lm1)
##���ӻ�
dev.new()
ggplot (acv4, aes (x = acvk, y = sk)) + geom_point (size = 3, shape = 21)

##Ʒ�༶�����-�ݶ��ϵ����ȣ�
pinlei<-unique(df$retailtypename)
df1 <- subset (df, retailtypename == pinlei[1])

##����ÿ��skuȫ�����۶�
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
##��ģ
acv4$facvk<-acv4$acvk^2
lmp1<-lm(acv4$sk~acv4$acvk+acv4$facv)
summary(lmp1)
##��ģ
result1 <- c()
for (i in 1:24){
  df1 <- subset (df, retailtypename == pinlei[i])
  
  ##����ÿ��skuȫ�����۶�
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
  ##������
  acv4$facvk <- acv4$acvk^2
  fit <- lm (acv4$sk ~ acv4$acvk + acv4$facvk)
  result1 <- rbind (result1, coef(summary(fit))[, c(1, 2, 4)])
}
write.csv(result1, "result.csv")

coef(summary(fit))[,c(1,2,4)]
dev.new()
ggplot (acv4, aes(x = acvk, y = sk)) + geom_point (size = 3,shape = 21)