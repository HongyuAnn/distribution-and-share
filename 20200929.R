library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
setwd("C:/Users/yl/Desktop/����/")
dfs <- fread ("data_category.csv", header = T, sep=",")
df <- subset(dfs, price<quantile(dfs$price,0.8))##ȡ��
pinlei <- unique(df$retailtypename)
##��ģ��Ȼ���Ʒ���Ʒ�ƶ�����ع�
result <- c()
for (i in 1:24){
  df2 <- subset (df, retailtypename == pinlei[24])
  
  ##����ÿ��Ʒ��ȫ�����۶�
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
  ##����ÿ��Ʒ��skuȫ�����۶�
    df1 <- subset (df2, rank == n)
    s <- aggregate (df1$amt.x, by = list (df1$barcode), sum)
    names(s)[1:2] <- c("barcode","amt")
  
  ##ÿ��Ʒ��sku���г��ݶ�
  
    s$sk <- s$amt / sum (s$amt) * 100
    summary (s$sk)
  
  ##����ÿ��sku���̻�����Ȩ��
  ###ÿ���̵�����ÿ��sku�����۶�
    acv <- aggregate (df1$amt.x, by = list (df1$storecode, df1$barcode), sum)
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
  result1
  result <- c(result, list(result1))
}

write.csv(result, "result.csv")