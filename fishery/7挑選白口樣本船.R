library(ggplot2)
data3 <- read.csv('修正2010年8月之前的卸魚資料_修改狗母為合齒魚科.csv', header=T)

head(data3,3)
str(data3)

tt <- data3[data3$fish %in% '白口' & data3$Harbour %in% '蚵仔寮',]

test <- aggregate(weight~CTNO,data=tt,FUN=sum)
test <- test[order(-test$weight),]
# write.csv(test,'2010-2019白口漁船卸魚重排序.csv', row.names=F)

od <- test$CTNO

ggplot(test[1:30,], aes(x = factor(CTNO,levels=od), y = weight/1000,fill=factor(CTNO,levels=od))) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "black", face = "bold", size = 30),
        axis.text.x = element_text( size = 20 , face = "bold",angle = 90),#angle = 90
        axis.text.y = element_text( size = 20 , face = "bold" ),
        strip.text = element_text(size = 20, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = 'none',
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill=NA)) +# 透明legend背景
  guides(fill=guide_legend(ncol=10,byrow=FALSE))   +
  geom_bar(stat="identity",position='stack') + ggtitle('蚵仔寮2010-2019年白口漁船卸魚重排名')+
  xlab('')+ylab('卸魚重量(噸)')+ylim(0,30)


#-------------------------2015-2019年--------------------------------
tt <- data3[data3$fish %in% '白口' & data3$Harbour %in% '蚵仔寮' & data3$Year %in% c(2015:2019),]
tt <- tt[!(tt$Year %in% 2015 & tt$Month<6),]
table(tt$Year,tt$Month)
test <- aggregate(weight~CTNO,data=tt,FUN=sum)
test <- test[order(-test$weight),]
test$黑口船 <- ifelse(test$CTNO %in% c('CT1007794','CT2004384','CT2004519','CT2005314','CT2005605',
                                'CT3002356','CT3002590','CT3003386','CT3003787','CT3004142',
                                'CT3004183','CT3004662','CT3004841','CT3004957','CT3005080','CT3005515',
                                'CT3005578','CT3005605','CT3005653','CT3005686','CT3005796','CT3006046',
                                'CT3006152','CT4002155','CT4002174','CT4002229','CT4002235','CT4002263'),
                 1,0)
head(test,2)
write.csv(test,'2015年6月-2019白口漁船卸魚重排序.csv', row.names=F)


tt <- data3[data3$fish %in% '黑口' & data3$Harbour %in% '蚵仔寮' & data3$Year %in% c(2015:2019),]
tt <- tt[!(tt$Year %in% 2015 & tt$Month<6),]
table(tt$Year,tt$Month)
test <- aggregate(weight~CTNO,data=tt,FUN=sum)
test <- test[order(-test$weight),]
test$黑口船 <- ifelse(test$CTNO %in% c('CT1007794','CT2004384','CT2004519','CT2005314','CT2005605',
                                    'CT3002356','CT3002590','CT3003386','CT3003787','CT3004142',
                                    'CT3004183','CT3004662','CT3004841','CT3004957','CT3005080','CT3005515',
                                    'CT3005578','CT3005605','CT3005653','CT3005686','CT3005796','CT3006046',
                                    'CT3006152','CT4002155','CT4002174','CT4002229','CT4002235','CT4002263'),
                   1,0)
head(test,2)
write.csv(test,'2015年6月-2019黑口漁船卸魚重排序.csv', row.names=F)



#--------------5噸以上----------------
tt <- data3[data3$fish %in% '白口' & data3$Harbour %in% '蚵仔寮',]

test <- aggregate(weight~Year+Month+Day+CTNO,data=tt,FUN=sum)
test <- test[test$CTNO %in% od[1:30] & test$weight > 0,]
table(test$CTNO,test$Year)
test$count <- 1
aggregate(count~CTNO,data=test,FUN=sum)

#--------------4.5噸以上----------------
test <- aggregate(weight~Year+Month+Day+CTNO,data=tt,FUN=sum)
test <- test[test$CTNO %in% od[1:34] & test$weight > 0,]
table(test$CTNO,test$Year)
test$count <- 1
aggregate(count~CTNO,data=test,FUN=sum)

#--------------4噸以上----------------
test <- aggregate(weight~Year+Month+Day+CTNO,data=tt,FUN=sum)
test <- test[test$CTNO %in% od[1:41] & test$weight > 0,]
table(test$CTNO,test$Year)
test$count <- 1

#--------------3.5噸以上----------------
test <- aggregate(weight~Year+Month+Day+CTNO,data=tt,FUN=sum)
test <- test[test$CTNO %in% od[1:45] & test$weight > 0,]
table(test$CTNO,test$Year)
test$count <- 1

#--------------3噸以上----------------
test <- aggregate(weight~Year+Month+Day+CTNO,data=tt,FUN=sum)
test <- test[test$CTNO %in% od[1:49] & test$weight > 0,]
table(test$CTNO,test$Year)
test$count <- 1


#---------------黑口的資料筆數----------------------
tt2 <- data3[data3$fish %in% '黑口' & data3$Harbour %in% '蚵仔寮',]
tt2 <- aggregate(weight~Year+Month+Day+CTNO,data=tt2,FUN=sum)
tt2 <- tt2[tt2$weight > 0 & tt2$CTNO %in% c('CT1007794','CT2004384','CT2004519','CT2005314','CT2005605',
                                            'CT3002356','CT3002590','CT3003386','CT3003787','CT3004142',
                                            'CT3004183','CT3004662','CT3004841','CT3004957','CT3005080','CT3005515',
                                            'CT3005578','CT3005605','CT3005653','CT3005686','CT3005796','CT3006046',
                                            'CT3006152','CT4002155','CT4002174','CT4002229','CT4002235','CT4002263'),]

#---------------------2010-2019年黑口與白口卸魚重--------------------------
tt <- data3[data3$fish %in% c('白口','黑口') & data3$Harbour %in% '蚵仔寮',]
test <- aggregate(weight~Year+fish,data=tt,FUN=sum)

ggplot(test, aes(x = as.factor(Year), y = weight/1000,fill=factor(fish))) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "black", face = "bold", size = 30),
        axis.text.x = element_text( size = 20 , face = "bold",angle = 90),#angle = 90
        axis.text.y = element_text( size = 20 , face = "bold" ),
        strip.text = element_text(size = 20, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = 'right',
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill=NA)) +# 透明legend背景
  guides(fill=guide_legend(ncol=1,byrow=FALSE))   +
  geom_bar(stat="identity",position='dodge') + ggtitle('蚵仔寮白口與黑口卸魚重')+
  xlab('')+ylab('卸魚重量(噸)')+ylim(0,100)

# 去除CT3005605
tt <- data3[data3$fish %in% c('白口','黑口') & data3$Harbour %in% '蚵仔寮',]
tt <- tt[!(tt$CTNO %in% 'CT3005605'),]
test <- aggregate(weight~Year+fish,data=tt,FUN=sum)

ggplot(test, aes(x = as.factor(Year), y = weight/1000,fill=factor(fish))) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "black", face = "bold", size = 30),
        axis.text.x = element_text( size = 20 , face = "bold",angle = 90),#angle = 90
        axis.text.y = element_text( size = 20 , face = "bold" ),
        strip.text = element_text(size = 20, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = 'right',
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill=NA)) +# 透明legend背景
  guides(fill=guide_legend(ncol=1,byrow=FALSE))   +
  geom_bar(stat="identity",position='dodge') + ggtitle('蚵仔寮白口與黑口卸魚重(去掉CT3005605)')+
  xlab('')+ylab('卸魚重量(噸)')+ylim(0,100)



#----------------------白口2015/6-2019年卸魚重排名---------------------------
remove(list=ls())
data3 <- read.csv('修正2010年8月之前的卸魚資料_修改狗母為合齒魚科.csv', header=T)

head(data3,3)
str(data3)

tt <- data3[data3$fish %in% '白口' & data3$Harbour %in% '蚵仔寮',]
tt <- tt[tt$Year >= 2015,]
tt <- tt[!(tt$Year %in% 2015 & tt$Month < 6),]
table(tt$Year,tt$Month)
test <- aggregate(weight~CTNO,data=tt,FUN=sum)
test <- test[order(-test$weight),]

od <- test$CTNO

tt <- data3[data3$fish %in% '白口' & data3$Harbour %in% '蚵仔寮' & data3$CTNO %in% od[1:45],]
test <- aggregate(weight~Year+CTNO,data=tt,FUN=sum)

ggplot(test, aes(x = as.factor(Year), y = weight)) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "black", face = "bold", size = 30),
        axis.text.x = element_text( size = 20 , face = "bold",angle = 90),#angle = 90
        axis.text.y = element_text( size = 20 , face = "bold" ),
        strip.text = element_text(size = 20, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = 'right',
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill=NA)) +# 透明legend背景
  guides(fill=guide_legend(ncol=1,byrow=FALSE))   +
  geom_point() + ggtitle('蚵仔寮白口樣本船')+
  xlab('')+ylab('卸魚重量(公斤)')+ylim(0,8500)+facet_wrap(~as.factor(CTNO))


# 匯出樣本船列表
od <- od[1:45]
od <- as.data.frame(od)
names(od) <- 'CTNO'
write.csv(od,'白口樣本船列表.csv')
