library(ggplot2)
library(dplyr)
library(forcats)


fish1519 <- read.csv('梓官漁會拍賣資料.csv', header=T)
fish_code <- read.csv('魚品種代碼.csv',header=T)


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
# 去除沒有年報分類代號的資料
fish1519_na <- fish1519_code[(is.na(fish1519_code$年報分類代號)),]
unique(fish1519_na$魚名)
ff <- unique(fish1519_na$魚名)
# 去除年報分類100-1200的資料
fish1519_code <- fish1519_code[!(fish1519_code$年報分類代號 %in% 100:1200),]
fish1519_code <- fish1519_code[!(fish1519_code$魚名 %in% c('213','吳郭魚')),]

fish1519_code$Harbour <- fish1519_code$港口
fish1519_code$weight <- fish1519_code$重量


# CTS開頭的與GTS、GTR的黑口漁獲總量占全部漁獲的多少
# 奇怪的CT
strange <- c('0008847','0009359','0009402','0009492','4791','9317','CY05757',
             'GTRCI0561','GTS6913','GTS8139','GTS8317','RCI0572','RNC0228',
             'S004860','S005634','S008316','高縣筏執08783','無')

strange_data <- fish1519_code[substring(fish1519_code$CTNO,1,3)=='CTS',]
strange_data <- rbind(strange_data,fish1519_code[substring(fish1519_code$CTNO,1,3)=='CTR',])
strange_data <- rbind(strange_data, fish1519_code[fish1519_code$ctno %in% strange,] )

# 刪除CTS開頭的與GTS、GTR、奇怪的船
fish1519_delete <- fish1519_code[!(fish1519_code$CTNO %in% unique(strange_data$CTNO)), ]

# 整理魚名
fish1519_delete$fish <- fish1519_delete$魚名
d1519 <- fish1519_delete

d1519$fish <- gsub("\n", "", d1519$fish)
d1519$fish[d1519$fish %in% '肉魚'] <-  '刺鯧'
d1519$fish[d1519$fish %in% '?鯧'] <-  '刺鯧'
d1519$fish[d1519$fish %in% '花身?'] <-  '花身'
d1519$fish[d1519$fish %in% '?錢魚'] <-  '金錢魚'
d1519$fish[d1519$fish %in% '?線魚'] <-  '金線魚'
d1519$fish[d1519$fish %in% '土拖鰆'] <-  '土魠'
d1519$fish[d1519$fish %in% '土魠鰆'] <-  '土魠'
d1519$fish[d1519$fish %in% '鰆'] <-  '土魠'

d1519$fish[d1519$fish %in% '火口'] <-  '大黃魚'
d1519$fish[d1519$fish %in% '黃花'] <-  '大黃魚'
d1519$fish[d1519$fish %in% '?仔'] <-  '勿仔'
d1519$fish[d1519$fish %in% '甘仔?'] <-  '甘(瓜)仔'
d1519$fish[d1519$fish %in% '瓜仔魚'] <-  '甘(瓜)仔'
d1519$fish[d1519$fish %in% '白北'] <-  '臺灣馬加鰆'
d1519$fish[d1519$fish %in% '白姑魚'] <-  '白口'
d1519$fish[d1519$fish %in% '帶魚屬'] <-  '白帶魚'
d1519$fish[d1519$fish %in% '白魚'] <-  '白帶魚'

d1519$fish[d1519$fish %in% '白鯧'] <-  '銀鯧'
d1519$fish[d1519$fish %in% '剝皮魚'] <-  '單棘魨科(剝皮魚)'
d1519$fish[d1519$fish %in% '眼眶魚'] <-  '皮刀'
d1519$fish[d1519$fish %in% '鬚鯛科'] <-  '秋姑'
d1519$fish[d1519$fish %in% '大棘大眼鯛'] <-  '紅目鰱'
d1519$fish[d1519$fish %in% '大眼鯛'] <-  '紅目鰱'
d1519$fish[d1519$fish %in% '真?'] <-  '巴闌'
d1519$fish[d1519$fish %in% '透抽'] <-  '鎖管'
d1519$fish[d1519$fish %in% '小卷'] <-  '鎖管'
d1519$fish[d1519$fish %in% '雜魚'] <-  '什魚'
d1519$fish[d1519$fish %in% '嘉臘'] <-  '加臘'
d1519$fish[d1519$fish %in% '嘉?魚'] <-  '加臘'
d1519$fish[d1519$fish %in% '布氏鯧?'] <-  '黃臘魚參(紅杉)'
d1519$fish[d1519$fish %in% '鯖屬'] <-  '鯖魚'
d1519$fish[d1519$fish %in% '鯖'] <-  '鯖魚'
d1519$fish[d1519$fish %in% '鸚哥'] <-  '鸚哥魚科'
d1519$fish[d1519$fish %in% '鸚哥魚'] <-  '鸚哥魚科'
d1519$fish[d1519$fish %in% '鸚哥魚科'] <-  '英哥'

d1519$fish[d1519$fish %in% '?科'] <-  '赤尾冬'
d1519$fish[d1519$fish %in% '紅尾冬'] <-  '赤尾冬'
d1519$fish[d1519$fish %in% '大甲?'] <-  '鐵甲'
d1519$fish[d1519$fish %in% '?梭魚科'] <-  '金梭魚科'
d1519$fish[d1519$fish %in% '狗母梭'] <-  '狗母'
data1$fish[data1$fish %in% '狗母'] <-  '合齒魚科'


d1519$fish[d1519$fish %in% '其他蝦?'] <-  '其他蝦類'
d1519$fish[d1519$fish %in% '其他貝介?'] <-  '其他貝類'
d1519$fish[d1519$fish %in% '其他?'] <-  '其他魚參'
d1519$fish[d1519$fish %in% '其他?類'] <-  '其他魚參類'
d1519$fish[d1519$fish %in% '其他海水魚?'] <-  '其他海水魚類'
d1519$fish[d1519$fish %in% '其他淡水魚?'] <-  '其他淡水魚類'
d1519$fish[d1519$fish %in% '其他鮪?'] <-  '其他鮪類'
d1519$fish[d1519$fish %in% '其他蟳蟹?'] <-  '其他蟳蟹類'
d1519$fish[d1519$fish %in% '其他鯔科魚?'] <-  '其他鯔科魚類'
d1519$fish[d1519$fish %in% '其他頭足?'] <-  '其他頭足類'
d1519$fish[d1519$fish %in% '其他鰆?'] <-  '其他鰆類'
d1519$fish[d1519$fish %in% '黑魚或'] <- '黑口'
d1519$fish[d1519$fish %in% '馬?科'] <- '午仔魚'# 黃背牙鯛
d1519$fish[d1519$fish %in% '黃背赤?'] <- '赤宗'
d1519$fish[d1519$fish %in% '海鰻'] <- '海鰻科'
d1519$fish[d1519$fish %in% '日本馬加鰆'] <- '馬加'
d1519$fish[d1519$fish %in% '其他魚類'] <- '其他海水魚類'
d1519$fish[d1519$fish %in% '金梭魚'] <- '金梭魚科'
d1519$fish[d1519$fish %in% '其他鮪魚'] <- '其他鮪類'
d1519$fish[d1519$fish %in% '牛尾'] <- '牛尾魚科'
d1519$fish[d1519$fish %in% '盤仔'] <- '魬鯛'

length(unique(d1519$fish))
d1519 <- d1519[!(d1519$fish %in% '其他淡水魚類'),] #去除淡水魚類
# 刪除重量為負的
d1519 <- d1519[d1519$重量 >= 0,]
d1619 <- d1519[d1519$Year %in% 2016:2019,]

# 不包含(凍)的
out1 <- d1619$fish[grep("凍", d1619$fish)]
out2 <- d1619$fish[grep("冷藏", d1619$fish)]
out3 <- d1619$fish[grep("加工", d1619$fish)]
out4 <- d1619$fish[grep("養", d1619$fish)]
out4 <- unique(out4)

out <- c(out1,out2,out3)
d1619 <- d1619[!(d1619$fish %in% out),]


#------------------------------------------------------------------

hist(d1619$單價)
d1619$price <- d1619$單價

tt <- d1619[d1619$重量 %in% 1 & d1619$單價 >1000,]
table(tt$Harbour)
table(tt$Year,tt$Month,tt$Harbour)
head(tt)
ff <- unique(tt$fish)

for(i in ff){
  # i=ff[38]
  f10 <- d1619[d1619$fish %in% i,]
  boxplot(price ~ Year, data =f10, main=paste0(i,'單價'))
  
}


tt <- d1619[d1619$單價 < 2500,]
for(i in ff){
  # i=ff[38]
  f10 <- tt[tt$fish %in% i,]
  boxplot(price ~ Year, data =f10, main=paste0(i,'單價'))
  
}
# write.csv(d1519,'2016年-2019年魚市場資料產值整理更正.csv',row.names=F)


#------------------------------------蚵仔寮----------------------------------------
tt <- d1619[d1619$單價 < 2500 & d1619$Harbour %in% '蚵仔寮',]
tt$price <- tt$單價
tt$output_value <- tt$合計

pc <- aggregate(price~fish,data=tt,FUN=mean)
pc <- pc[order(-pc$price),]
pc$Rank <- 1:length(pc$fish)
pc[pc$fish %in% '白口',]

# png('2016-2019年前30名蚵仔寮魚種平均單價排序.png',width=838,height=641)
p <- ggplot(pc[1:30,], aes(x = reorder(fish,-price,sum), y = price, fill=reorder(fish,-price,sum))) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "black", face = "bold", size = 30),
        axis.text.x.bottom = element_text(size=17),
        axis.text.x = element_text( size = 20 , face = "bold"),#angle = 90 
        axis.text.y = element_text( size = 20 , face = "bold" ),
        strip.text = element_text(size = 20, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = c(0.8,0.7),
        legend.title=element_blank(), 
        legend.text=element_text(size=20),
        legend.background = element_rect(fill=NA)) +# 透明legend背景
  guides(fill=guide_legend(ncol=3,byrow=FALSE))  +
  geom_bar(stat="identity")+xlab('排名')+
  scale_x_discrete(labels=1:30)+ 
  scale_y_continuous(name="平均單價(元)", breaks=seq(0,1000,100))+
  ggtitle('2016-2019年前30名蚵仔寮魚種單價排序')
print(p)
# dev.off()


pc_year <- aggregate(price~Year+fish,data=tt,FUN=mean)
op_year <- aggregate(output_value~Year+fish,data=tt,FUN=sum)
wt_year <- aggregate(weight~Year+fish,data=tt,FUN=sum)

pc_year <- pc_year[order(pc_year$Year,-pc_year$price), ]
op_year <- op_year[order(op_year$Year,-op_year$output_value), ]
wt_year <- wt_year[order(wt_year$Year,-wt_year$weight), ]

n <- table(pc_year$Year)
pc_year$Rank <- c(1:n[1],1:n[2],1:n[3],1:n[4])
pc_year[pc_year$fish %in% '白口',]

# 黑口每年平均單價
pc <- pc_year[pc_year$fish %in% '白口',]
# 黑口卸魚重量
wt <- wt_year[wt_year$fish %in% '白口',]

n <- table(op_year$Year)
op_year$Rank <- c(1:n[1],1:n[2],1:n[3],1:n[4])
op_year[op_year$fish %in% '白口',]

# 總產值
all_year <- aggregate(output_value~Year,data=op_year,FUN=sum)
names(all_year)[2] <- 'All'
op <- merge(op_year[op_year$fish %in% '白口',],all_year,by=c('Year'))
op$p <- (op$output_value/op$All)*100


png('蚵仔寮白口產值.png',width=797,height=630)
par(oma=c(1,1,1,1),cex.lab=1.25,cex.axis=1.25,oma = c(1, 1, 1, 1))
layout(matrix(c(1,1,1,2,2,2,2,2,2),3,3,byrow=TRUE))

par(mar=c(0,5,3,4),las=1)
plot(pc$Year,pc$price,type='n',pch=19, xaxt='n',yaxt='n', ylab='', ylim=c(60,160),
     xlab='',bty="l",cex.main=2, main='蚵仔寮',cex.main=3)
lines(pc$Year+c(0.3,0.1,-0.1,-0.3),pc$price,type='b',pch=19,lwd=2)
text(pc$Year+c(0.3,0.1,-0.1,-0.3)+0.2,pc$price,pc$Rank,col='red',cex=2)
axis(2,at=seq(60,160,20),labels=seq(60,160,20),cex=1.2,cex.axis=1.2,cex.lab=1.2)
legend("topleft", c("白口單價排名"), pch=19, col=c("red"), 
       pt.cex = 3, box.lty=0,text.font=2,cex=2)#,text.col=c('red')
mtext('白口單價(元)',side=2,line=3,las=0,cex=1.5,cex.axis=1.5,cex.lab=1.5, font=2)
box()

par(mar=c(5,5,0,4),las=1)
barplot(op$All/1000000,yaxt='n',ylim=c(0,400),yaxt='n',ylab='', width=1)
ys <- seq(0,400,by=50)
axis(2,at=ys,labels=ys,cex=1.2,cex.axis=1.2,cex.lab=1.2)
mtext('總產值(百萬)',side=2,line=3,las=0,cex=1.5,cex.axis=1.5,cex.lab=1.5, font=2)

par(new=T,mar=c(5,5,0,4),las=1,xpd=T,oma = c(1, 1, 1, 1))
plot(op$Year,op$p,type='n',pch=19, xaxt='n',yaxt='n', ylab='', ylim=c(0,5),xlab='',bty="l",cex.main=2)
lines(op$Year+c(0.3,0.1,-0.1,-0.3),op$p,type='b',pch=19,lwd=2)
text(op$Year+c(0.3,0.1,-0.1,-0.3)+0.2,op$p,op$Rank,col='red',cex=2)
axis(4,at=seq(0,5,0.5),labels=seq(0,5,0.5),cex=1.2,cex.axis=1.2,cex.lab=1.2)
axis(1,at=op$Year+c(0.3,0.1,-0.1,-0.3),labels=2016:2019,cex=2,cex.axis=2,cex.lab=2)
legend("topright", c("白口產值占比排名"), pch=19, col=c("red"), 
       pt.cex = 3, box.lty=0,text.font=2,cex=2)
mtext('白口產值占比(%)',side=4,line=3,las=0,cex=1.5,cex.axis=1.5,cex.lab=1.5, font=2)
box()
dev.off()
