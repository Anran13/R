library(ggplot2)

fish0915 <- read.csv('漁市場資料2009~2015.csv', header=T)
fish1519 <- read.csv('梓官漁會拍賣資料.csv', header=T)
fish_code <- read.csv('魚品種代碼.csv',header=T)

#---------------------整理2010到2019年的漁市場資料-------------------------------

# 2009-2015年的資料 
fish0915$港口 <- gsub(" ", "", fish0915$港口)
fish0915$港口[fish0915$港口=='蚵子寮'] <- '蚵仔寮'
table(fish0915$港口)
table(fish0915$來源)

fish0915 <- fish0915[fish0915$港口 %in% c('蚵仔寮'),]

# 選出2010-2014年的資料 & 只取到2015年的6月以前(之後的資料用新的)
fish1014 <- fish0915[fish0915$年 != 2009,]
fish1014 <- fish1014[!(fish1014$年 == 2015 & fish1014$月>=6),]
fish1014$CTNO <- gsub(" ", "", fish1014$CTNO)
table(fish1014$港口)

fish1014$Year <- fish1014$年
fish1014$Month <- fish1014$月
fish1014$Day <- fish1014$日
#------------------------------------------------------------------------
# 2015-2019年的資料

fish1519$ctno <- gsub(" ", "", fish1519$ctno)
fish1519$ctno <- gsub("-", "", fish1519$ctno)
fish1519$ctno <- gsub("/", "", fish1519$ctno)


fish1519$ctno[fish1519$ctno=='CT41378CT41379'] <- 'CT41379'
fish1519$ctno[fish1519$ctno=='CT33928CT33929'] <- 'CT33929'
table(fish1519$ctno)

fish1519$CTNO <- ifelse( substring(fish1519$ctno,4,5)=='00', fish1519$ctno, 
        
        ifelse( substring(fish1519$ctno,1,3)=='000', fish1519$ctno,
                ifelse( nchar(fish1519$ctno)<5, fish1519$ctno,
                        paste0( substring(fish1519$ctno,1,3), '00',  substring(fish1519$ctno,4,7) ))
                )
        
        )

table(nchar(fish1519$日期))
fish1519$Year <- as.numeric(substring(fish1519$日期,1,4))
fish1519$Month <- as.numeric(substring(fish1519$日期,6,7))
fish1519$Day <- as.numeric(substring(fish1519$日期,9,10))

# 去除年報分類代號100-1200
str(fish1519)
table(nchar(as.character(fish1519$魚號)))
fish1519$品種代號 <- substring(fish1519$魚號,2,5)
# 對照年報分類代號
str(fish_code)
fish1519_code <- merge(fish1519,fish_code,by=c('品種代號'), all.x=T)

# 去除年報分類100-1200的資料
fish1519_code <- fish1519_code[!(fish1519_code$年報分類代號 %in% 100:1200),]
fish1519_code <- fish1519_code[!(fish1519_code$魚名 %in% c('213','吳郭魚')),]


fish1519_code$Harbour <- fish1519_code$港口
fish1519_code$weight <- fish1519_code$重量

# 刪除 < 3年資料的船
tt <- rbind(fish1519_code[c('Year','CTNO')],fish1014[c('Year','CTNO')])
length(unique(tt$CTNO))
tt$count <- 1
tt <- aggregate(count~Year+CTNO,data=tt,FUN=sum)
tt$count <- 1
tt <- aggregate(count~CTNO,data=tt,FUN=sum)
ct_list <- tt$CTNO[tt$count < 3]

fish1519_code <- fish1519_code[!(fish1519_code$CTNO %in% ct_list),]
fish1014 <- fish1014[!(fish1014$CTNO %in% ct_list),]

#
table(nchar(fish1519_code$CTNO))
table(nchar(fish1014$CTNO))


fish1014$weight <- fish1014$卸售重量
fish1014$Harbour <- fish1014$港口



#-------------------奇怪的CT、CTS開頭的與GTS、GTR的黑口漁獲總量占全部漁獲的多少--------------

# 2015-2019
strange <- c('0008847','0009359','0009402','0009492','4791','9317','CY05757',
             'GTRCI0561','GTS6913','GTS8139','GTS8317','RCI0572','RNC0228',
             'S004860','S005634','S008316','高縣筏執08783','無','RCI000572','S00004860')

strange_data <- fish1519_code[substring(fish1519_code$CTNO,1,3)=='CTS',]
strange_data <- rbind(strange_data,fish1519_code[substring(fish1519_code$CTNO,1,3)=='CTR',])
strange_data <- rbind(strange_data, fish1519_code[fish1519_code$ctno %in% strange,] )

strange_target_data1519 <- strange_data[strange_data$魚名 %in% c('白口'),]
ct_list <- unique(strange_data$CTNO)

# 2010-2014
strange <- c(NA,strange)
strange_data <- fish1014[substring(fish1014$CTNO,1,3)=='CTS',]
strange_data <- rbind(strange_data,fish1014[substring(fish1014$CTNO,1,3)=='CTR',])
strange_data <- rbind(strange_data, fish1014[fish1014$ctno %in% strange,] )

strange_target_data1014 <- strange_data[strange_data$年報分類代號 %in% c('白口'),]

ct_list <- c(ct_list,unique(strange_data$CTNO))
ct_list <- unique(ct_list) # 2010-2019年剔除的CT

strange_target_data <- rbind(strange_target_data1519[c('Year','weight','Harbour')],strange_target_data1014[c('Year','weight','Harbour')])

# 算奇怪船之白口卸魚重
strange_target_data_weight <- aggregate(weight~Year+Harbour, data=strange_target_data, FUN=sum)
strange_target_data_weight <- strange_target_data_weight[strange_target_data_weight$Year %in% 2015:2019,]

fish1519_code_weight <- aggregate(weight~Year+Harbour, data=fish1519_code[fish1519_code$魚名 %in% c('白口'),], FUN=sum)
fish1014_code_weight <- aggregate(weight~Year+Harbour, data=fish1014[fish1014$年報分類代號 %in% c(15004),], FUN=sum)

fish1519_code_weight$weight[fish1519_code_weight$Year %in% 2015] <- 
  fish1519_code_weight$weight[fish1519_code_weight$Year %in% 2015] +
  fish1014_code_weight$weight[fish1014_code_weight$Year %in% 2015]

fish1014_code_weight <- fish1014_code_weight[!(fish1014_code_weight$Year %in% 2015),]
fish1019_code_weight <- rbind(fish1014_code_weight,fish1519_code_weight)

fish1019_code_weight <- fish1019_code_weight[fish1019_code_weight$Year %in% 2015:2019,]


strange_p <- merge(strange_target_data_weight,fish1019_code_weight,by=c('Year','Harbour'))
strange_p$p <- strange_p$weight.x/strange_p$weight.y

plot(strange_p$Year,strange_p$p,
     type='b', pch=19,xaxt='n',ylim=c(0,1),xlab='',ylab='',cex.main=1.7,main='刪除之漁船白口卸魚重量占比')
axis(1,at=sort(unique(strange_p$Year)),labels=sort(unique(strange_p$Year)),cex=1,cex.axis=1.2,cex.lab=1.2)
mtext('Year',side=1,line=3,las=0,cex=2,cex.axis=2,cex.lab=3)
mtext('比例',side=2,line=2,las=0,cex=2,cex.axis=2,cex.lab=3)


# 刪除CTS開頭的與GTS、GTR、奇怪的船
fish1519_delete <- fish1519_code[!(fish1519_code$CTNO %in% ct_list  ), ]
fish1014_delete <- fish1014[!(fish1014$CTNO %in% ct_list),]


#-------------------------------合併2010-2019資料------------------------------------
data1014 <- fish1014_delete[c('CTNO','Year','Month','Day','品種名稱','weight','Harbour','年報分類代號')]
data1519 <- fish1519_delete[c('CTNO','Year','Month','Day','魚名','重量','Harbour','魚號','單價')]
names(data1519)[5] <- c('品種名稱')
names(data1519)[6] <- c('weight')
names(data1519)[8] <- names(data1014)[8] <- c('fish_code')
names(data1519)[9] <- 'price'
data1014$price <- NA

data <- rbind(data1014,data1519)


data$count <- 1
a1019 <- aggregate(count~Year+Month+Day+CTNO,data=data,FUN=sum)
a1019 $count <- 1
a1019 <- aggregate(count~Year+CTNO,data=a1019,FUN=sum)
a1019$count <- 1
a1019 <- a1019[a1019$CTNO != '',]
a1019 <- aggregate(count~Year,data=a1019,FUN=sum)


#--------------------------白口------------------------------

data_target <- data[data$fish_code %in% c(12064,15004),]

data_target$count <- 1
b1019 <- aggregate(count~Year+Month+Day+CTNO,data=data_target,FUN=sum)
b1019 $count <- 1
b1019 <- aggregate(count~Year+CTNO,data=b1019,FUN=sum)
b1019$count <- 1
b1019 <- b1019[b1019$CTNO != '',]
b1019 <- aggregate(count~Year,data=b1019,FUN=sum)


#-----------------------------------畫在一起-----------------------------------------
a1019$label <- '總船數'
b1019$label <- '捕白口船數'
ab1019 <- rbind(a1019,b1019)

plot(ab1019$Year[ab1019$label=='總船數'],ab1019$count[ab1019$label=='總船數'],
     type='b', pch=19,xaxt='n',ylim=c(0,150),xlab='',ylab='',cex.main=1.7,main='蚵仔寮')
axis(1,at=sort(unique(ab1019$Year)),labels=sort(unique(ab1019$Year)),cex=1,cex.axis=1.2,cex.lab=1.2)
mtext('Year',side=1,line=3,las=0,cex=2,cex.axis=2,cex.lab=3)
mtext('船數',side=2,line=2,las=0,cex=2,cex.axis=2,cex.lab=3)
lines(ab1019$Year[ab1019$label=='捕白口船數'],ab1019$count[ab1019$label=='捕白口船數'],
      col='red',type='b',pch=19)# 彌陀
legend("bottomright", legend = c("總船數", "捕白口船數"),
       pch = 19, col = c('black','red'),lty=1,cex=1.2,bty="n")


#---------------------------------1.統整魚名--------------------------------
data1 <- data
data1$fish <- gsub(" ", "", data1$品種名稱)
data1$fish <- as.character(data1$fish)
data1 <- data1[data1$fish != '?',]
data1 <- data1[data1$fish != '',]

data1$fish <- gsub("\n", "", data1$fish)
data1$fish[data1$fish %in% '肉魚'] <-  '刺鯧'
data1$fish[data1$fish %in% '?鯧'] <-  '刺鯧'
data1$fish[data1$fish %in% '花身?'] <-  '花身'
data1$fish[data1$fish %in% '?錢魚'] <-  '金錢魚'
data1$fish[data1$fish %in% '?線魚'] <-  '金線魚'
data1$fish[data1$fish %in% '土拖鰆'] <-  '土魠'
data1$fish[data1$fish %in% '土魠鰆'] <-  '土魠'
data1$fish[data1$fish %in% '鰆'] <-  '土魠'

data1$fish[data1$fish %in% '火口'] <-  '大黃魚'
data1$fish[data1$fish %in% '黃花'] <-  '大黃魚'
data1$fish[data1$fish %in% '?仔'] <-  '勿仔'
data1$fish[data1$fish %in% '甘仔?'] <-  '甘(瓜)仔'
data1$fish[data1$fish %in% '瓜仔魚'] <-  '甘(瓜)仔'
data1$fish[data1$fish %in% '白北'] <-  '臺灣馬加鰆'
data1$fish[data1$fish %in% '白姑魚'] <-  '白口'
data1$fish[data1$fish %in% '帶魚屬'] <-  '白帶魚'
data1$fish[data1$fish %in% '白魚'] <-  '白帶魚'

data1$fish[data1$fish %in% '白鯧'] <-  '銀鯧'
data1$fish[data1$fish %in% '剝皮魚'] <-  '單棘魨科(剝皮魚)'
data1$fish[data1$fish %in% '眼眶魚'] <-  '皮刀'
data1$fish[data1$fish %in% '鬚鯛科'] <-  '秋姑'
data1$fish[data1$fish %in% '大棘大眼鯛'] <-  '紅目鰱'
data1$fish[data1$fish %in% '大眼鯛'] <-  '紅目鰱'
data1$fish[data1$fish %in% '真?'] <-  '巴闌'
data1$fish[data1$fish %in% '透抽'] <-  '鎖管'
data1$fish[data1$fish %in% '小卷'] <-  '鎖管'
data1$fish[data1$fish %in% '雜魚'] <-  '什魚'
data1$fish[data1$fish %in% '嘉臘'] <-  '加臘'
data1$fish[data1$fish %in% '嘉?魚'] <-  '加臘'
data1$fish[data1$fish %in% '布氏鯧?'] <-  '黃臘魚參(紅杉)'
data1$fish[data1$fish %in% '鯖屬'] <-  '鯖魚'
data1$fish[data1$fish %in% '鯖'] <-  '鯖魚'
data1$fish[data1$fish %in% '鸚哥'] <-  '鸚哥魚科'
data1$fish[data1$fish %in% '鸚哥魚'] <-  '鸚哥魚科'
data1$fish[data1$fish %in% '鸚哥魚科'] <-  '英哥'

data1$fish[data1$fish %in% '?科'] <-  '赤尾冬'
data1$fish[data1$fish %in% '紅尾冬'] <-  '赤尾冬'
data1$fish[data1$fish %in% '大甲?'] <-  '鐵甲'
data1$fish[data1$fish %in% '?梭魚科'] <-  '金梭魚科'
data1$fish[data1$fish %in% '狗母梭'] <-  '狗母'
data1$fish[data1$fish %in% '狗母'] <-  '合齒魚科'


data1$fish[data1$fish %in% '其他蝦?'] <-  '其他蝦類'
data1$fish[data1$fish %in% '其他貝介?'] <-  '其他貝類'
data1$fish[data1$fish %in% '其他?'] <-  '其他魚參'
data1$fish[data1$fish %in% '其他?類'] <-  '其他魚參類'
data1$fish[data1$fish %in% '其他海水魚?'] <-  '其他海水魚類'
data1$fish[data1$fish %in% '其他淡水魚?'] <-  '其他淡水魚類'
data1$fish[data1$fish %in% '其他鮪?'] <-  '其他鮪類'
data1$fish[data1$fish %in% '其他蟳蟹?'] <-  '其他蟳蟹類'
data1$fish[data1$fish %in% '其他鯔科魚?'] <-  '其他鯔科魚類'
data1$fish[data1$fish %in% '其他頭足?'] <-  '其他頭足類'
data1$fish[data1$fish %in% '其他鰆?'] <-  '其他鰆類'
data1$fish[data1$fish %in% '黑魚或'] <- '黑口'

# 去除淡水魚類
data1 <- data1[!(data1$fish %in% '其他淡水魚類'),]

#---------------------------2.去除無CT碼以及卸魚重量超過10000的----------------------
data2 <- data1
# 去除無CT碼的
data2 <- data2[!(data2$CTNO %in% ''),]
# 去除負的卸魚重量
tt <- data2[data2$weight < 0,]
table(tt$Year)
data2 <- data2[data2$weight >= 0,]
 
hist(data2$weight,ylim=c(0,200))
# 去除卸魚重量超過10000的資料
data2 <- data2[data2$weight < 10000,]

data2$quarter <- (data2$Month>0)+(data2$Month>3)+(data2$Month>6)+(data2$Month>9)
table(data2$Month,data2$quarter)

table(data2$Month,data2$quarter,data2$Year)



data2$ctlevel <- substring(data2$CTNO,3,3)
str(data2)

table(data2$ctlevel) 
data2 <- data2[!(data2$ctlevel %in% 'D'),]

w1019 <- aggregate(weight~Year+Month+Day+quarter+fish+CTNO+ctlevel, data=data2, FUN=sum)
w1019 <- w1019[order(w1019$Year,w1019$Month,w1019$Day,w1019$CTNO,-w1019$weight),]
w1019$Weight <- w1019$weight/1000 # 變成噸單位

  
#-----------------------2.1 修正2010年怪異卸魚重資料---------------------------------------
tt <- data2
test <- aggregate(weight~Year+Harbour,data=tt,FUN=sum)

ggplot(test, aes(x = as.factor(Year), y = weight/1000,fill=as.factor(Harbour))) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "black", face = "bold", size = 30),
        axis.text.x.bottom = element_text(size=17),
        axis.text.x = element_text( size = 20 , face = "bold"),#angle = 90
        axis.text.y = element_text( size = 20 , face = "bold" ),
        strip.text = element_text(size = 20, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = 'none',
        legend.title=element_blank(),
        legend.text=element_text(size=20),
        legend.background = element_rect(fill=NA)) +# 透明legend背景
  guides(fill=guide_legend(ncol=1,byrow=FALSE))   +
  geom_bar(stat="identity",position='dodge')+
  xlab('')+ylab('蚵仔寮卸魚重(噸)')+ylim(0,27000)


tt <- data2[data2$Year %in% 2010,]
tt$group <- ifelse(tt$Month < 8, '1-7月', '8-12月')
tt$fish <- factor(tt$fish,levels=unique(tt$fish))
ggplot(tt, aes(x = as.factor(group), y = weight,fill=as.factor(group))) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "black", face = "bold", size = 30),
        axis.text.x = element_text( size = 20 , face = "bold"),#angle = 90
        axis.text.y = element_text( size = 20 , face = "bold" ),
        strip.text = element_text(size = 20, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = 'none',
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill=NA)) +# 透明legend背景
  guides(fill=guide_legend(ncol=10,byrow=FALSE))   +
  geom_boxplot() + ggtitle('蚵仔寮')+ facet_wrap(~fish)+
  xlab('')+ylab('卸魚重量(噸)')+ylim(0,300)

# 2010年的卸魚重量怪怪的8月之前 8月以前和以後的資料比較
tt <- data2[data2$Year %in% 2010,]
tt <- tt[tt$Harbour %in% '蚵仔寮',] # 只有蚵仔寮的資料
tt$group <- ifelse(tt$Month %in% 1:7, '1-7月', '8-12月')
test <- aggregate(weight~CTNO+fish+group,data=tt,FUN=median)
test1 <- test[test$group %in% '1-7月',]
test2 <- test[test$group %in% '8-12月',]
test <- merge(test1,test2,by=c('CTNO','fish'))

test <- na.omit(test)
test <- as.data.frame(test)
test$group <- ifelse(abs(test$weight.x-test$weight.y)>5,1,0) #medium>5
test$no <- paste0(test$CTNO,'/',test$fish)
out <- test$no[test$group %in% 1]
out <- unique(out)

#---------------------更正2010年1-7月部分魚的卸魚重量----------------------------
test <- data2
test$no <- paste0(test$CTNO,'/',test$fish)

test$weight[test$Year %in% 2010 & test$Month %in% 1:7 & test$no %in% out] <- 
  test$weight[test$Year %in% 2010 & test$Month %in% 1:7 & test$no %in% out]/100

data3 <- test

test <- aggregate(weight~Year,data=test,FUN=sum)
ggplot(test, aes(x = as.factor(Year), y = weight/1000,fill='red')) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "black", face = "bold", size = 30),
        axis.text.x.bottom = element_text(size=17),
        axis.text.x = element_text( size = 20 , face = "bold"),#angle = 90
        axis.text.y = element_text( size = 20 , face = "bold" ),
        strip.text = element_text(size = 20, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = 'none',
        legend.title=element_blank(),
        legend.text=element_text(size=20),
        legend.background = element_rect(fill=NA)) +# 透明legend背景
  guides(fill=guide_legend(ncol=1,byrow=FALSE))   +
  geom_bar(stat="identity",position='dodge',width = 0.6)+scale_fill_brewer(type="qual")+
  xlab('')+ylab('卸魚重(噸)')+ylim(0,2000)



# 看白口
# 分船

test <- data3[data3$fish_code %in% c(12064,15004),]
test <- aggregate(weight~CTNO,data=test,FUN=sum)
test <- test[order(-test$weight),]
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


#-------------------------匯出資料--------------------------------------

write.csv(data3,'修正2010年8月之前的蚵仔寮卸魚資料.csv',row.names=F)

