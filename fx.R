##sku�������-�ݶ��ϵ����ȣ�
df <- fread ("data_category.csv", header = T, sep=",")

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

##���ӻ�
dev.new()
ggplot (acv4, aes (x = acvk, y = sk)) + geom_point (size = 3, shape = 21)

##Ʒ�༶�����-�ݶ��ϵ����ȣ����䶳ʳƷΪ��
df1 <- subset (df, retailtypename == "�䶳ʳƷ")

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

##���ӻ�
dev.new()
ggplot (acv4, aes(x = acvk, y = sk)) + geom_point (size = 3,shape = 21)