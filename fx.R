##sku级别分销-份额关系（年度）
df <- fread ("data_category.csv", header = T, sep=",")

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

##可视化
dev.new()
ggplot (acv4, aes (x = acvk, y = sk)) + geom_point (size = 3, shape = 21)

##品类级别分销-份额关系（年度）以冷冻食品为例
df1 <- subset (df, retailtypename == "冷冻食品")

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

##可视化
dev.new()
ggplot (acv4, aes(x = acvk, y = sk)) + geom_point (size = 3,shape = 21)
