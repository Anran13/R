library(ggplot2)
library(dplyr)
library(forcats)

# fish <- read.csv('修正2010年8月之前的卸魚資料.csv', header=T)
# fish$fish[fish$fish %in% '狗母'] <- '合齒魚科'
# write.csv(fish,'修正2010年8月之前的卸魚資料_修改狗母為合齒魚科.csv',row.names=F)

data <- read.csv('修正2010年8月之前的卸魚資料_修改狗母為合齒魚科.csv', header=T)
data$Weight <- data$weight/1000
str(data)
data$fish <- as.character(data$fish)
data$CTNO <- as.character(data$CTNO)
data$ctlevel <- as.character(data$ctlevel)
data$Harbour <- as.character(data$Harbour)

w1019 <- aggregate(cbind(weight,Weight) ~ Year+fish+Harbour, data=data, FUN=sum)
w1019_1 <- w1019[w1019$Harbour=='蚵仔寮',]
w1019_2 <- w1019[w1019$Harbour=='彌陀',]

a_1 <- w1019_1
a_2 <- w1019_2
a_1 <- aggregate(cbind(weight,Weight)~Year,data=a_1,FUN=sum)
a_2 <- aggregate(cbind(weight,Weight)~Year,data=a_2,FUN=sum)


fish_1 <- unique(w1019_1$fish)
fish_2 <- unique(w1019_2$fish)

fish <- fish_1[fish_1 %in% fish_2]


#-------------------------畫卸魚重量-------------------------------------
# png('卸魚重量.png',width=838,height=641)
# plot(a_1$Year,a_1$weight,
#      type='b', pch=19,xaxt='n',ylim=c(0,2000),xlab='',ylab='',cex.main=1.7)
# axis(1,at=sort(unique(a_1$Year)),labels=sort(unique(a_1$Year)),cex=1,cex.axis=1.2,cex.lab=1.2)
# mtext('Year',side=1,line=3,las=0,cex=2,cex.axis=2,cex.lab=3)
# mtext('卸漁重量(噸)',side=2,line=2,las=0,cex=2,cex.axis=2,cex.lab=3)
# lines(a_2$Year,a_2$Weight,
#       col='red',type='b',pch=19)
# legend("topleft", legend = c("蚵仔寮", "彌陀"),
#        pch = 19, col = c('black','red'),lty=1,cex=1.5,bty="n")
# dev.off()

#------------------------算魚種卸魚重量排名與占比--------------------------
# 蚵仔寮
yr <- sort(unique(w1019_1$Year))
dt <- c()
for(i in yr){
  # i=2011
  dd <- w1019_1[w1019_1$Year==i,]
  dd <- dd[order(dd$Year,-dd$Weight),]
  ind <- length(dd$fish)
  dd$Rank <- 1:ind
  total <- a_1$Weight[a_1$Year==i]
  dd$proportion <- dd$Weight/total
  dt <- rbind(dd[1:ind,],dt)
  
}
dt <- dt[order(dt$Year,-dt$Weight),]
# write.csv(dt,'蚵仔寮魚種年卸魚重量_排名占比.csv',row.names=F)
head(dt)



# 分開畫
tt <- data
tt$fish <- as.factor(tt$fish)
tt <- aggregate(cbind(weight,Weight)~fish+Harbour, data=tt, FUN=sum)

fish <- as.data.frame(fish)
fish$id <- 1:length(fish$fish)
n <- length(fish$fish)
palette(rainbow(n))
fish$col <- palette(rainbow(n))


tt <- merge(tt,fish,by=c('fish'))
tt <- tt[order(-tt$weight),]

#####################
# 先畫合併漁港
# a <- aggregate(cbind(weight,Weight)~fish, data=tt, FUN=sum)
# a <- a[order(-a$Weight),]
# a <- merge(a,fish,by=c('fish'))
# a <- a[order(-a$weight),]
# 
# png('2010-2019年前20名蚵仔寮與彌陀卸魚重量魚種.png',width=838,height=641)
# par(mar=c(12,6,3,0),las=1)
# barplot(a$weight[1:20]/1000,col=a$col[1:20],yaxt='n')
# xs <- seq(0.6,23.5,length=20)
# axis(1,at=xs,labels=a$fish[1:20],cex=1.8,cex.axis=1.8,cex.lab=1.8,font=2,las=2)
# axis(2,at=seq(0,1500,100),labels=seq(0,1500,100),cex=1.2,cex.axis=1.2,cex.lab=1.2,font=2,las=1)
# mtext('2010-2019年卸魚重前20名',side=3,line=1,las=0,cex=1.9,cex.axis=1.9,cex.lab=1.9, font=2)
# mtext('卸魚重(噸)',side=2,line=4,las=0,cex=1.7,cex.axis=1.7,cex.lab=1.7, font=2)
# dev.off()
######################


tt1 <- tt[tt$Harbour %in% '蚵仔寮',]
tt1 <- tt1[1:20,]


png('蚵仔寮前20卸魚重排名.png',width=982,height=566)

par(mar=c(7,5,3,1),las=2,oma=c(1,1,1,1),cex.lab=1.25,cex.axis=1.25)
barplot(tt1$Weight,col=tt1$col,ylim=c(0,1400),xaxt='n',yaxt='n')#horiz=TRUE
xs <- seq(0.6,23.5,length=20)
axis(1,at=xs,labels=tt1$fish,cex=1.2,cex.axis=1.2,cex.lab=1.2,font=2,las=2)
axis(2,at=seq(0,1400,100),labels=seq(0,1400,100),cex=1.2,cex.axis=1.2,cex.lab=1.2,font=2,las=1)
mtext('蚵仔寮',side=3,line=1,las=0,cex=1.5,cex.axis=1.5,cex.lab=1.5, font=2)
mtext('卸魚重(噸)',side=2,line=3,las=0,cex=1.5,cex.axis=1.5,cex.lab=1.5, font=2)
box()

dev.off()


#-----------------------畫卸魚重量排名前五的魚種-------------------------------
dt5 <- dt[dt$Rank < 6,]
write.csv(dt5,'蚵仔寮卸魚重量前5.csv',row.names=F)

# dt5$fish <- as.character(dt5$fish)
# 
# dt.tab <- matrix(dt5$Weight,5,10)
# 
# palette(rainbow(20)[-c(1,7,9,18,19)])
# dt_target <- dt[dt$fish %in% '黑口',]

# png('蚵仔寮前5的卸魚重量與黑口占比.png',width=909,height=656)
# 
# par(mar=c(9,4,2,5),las=1,oma = c(1, 1, 1, 1))
# 
# x <- seq(3.5,120,length=10)
# y <- dt_target$proportion*100
# 
# plot(x,y,type='n',pch=19, xaxt='n',yaxt='n', ylab='', xlim=c(0,120),ylim=c(0,7),xlab='',bty="l",main='蚵仔寮',cex.main=2)
# lines(x,y,type='b',pch=19)
# text(x,y,round(y,2),col='red')
# text(x,y+0.5,dt_target$Rank,col='blue')
# axis(2,at=seq(1,6,1),labels=seq(1,6,1),cex=1.2,cex.axis=1.2,cex.lab=1.2)
# legend("topright", c("Rank", "%"), pch=19, col=c("blue","red"), pt.cex = 1, box.lty=0)
# mtext('黑口卸魚重量占比(%)',side=2,line=3,las=0,cex=2,cex.axis=2,cex.lab=3, font=2)
# box()
# 
# par(new=T,mar=c(9,4,2,5),las=1,xpd=T,oma = c(1, 1, 1, 1))
# barplot(dt.tab,beside=TRUE,col=as.factor(dt5$fish),names.arg=2010:2019,yaxt='n',
#         ylim=c(0,300),xlim=c(0,54),yaxt='n',ylab='',width=0.9)
# ys <- seq(0,250,by=50)
# axis(4,at=ys,labels=ys,cex=1.2,cex.axis=1.2,cex.lab=1.2)
# xy <- par('usr')
# legend(xy[2]-xinch(7),xy[3]-yinch(0.7),legend=unique(dt5$fish), box.lty=0,x.intersp=0.6,
#        fill=unique(as.factor(dt5$fish)),ncol=5)
# mtext('各年卸魚重量(噸)',side=4,line=3.5,las=0,cex=2,cex.axis=2,cex.lab=3, font=2)
# 
# dev.off()

dt5 <- dt[dt$Rank < 6,]
head(dt5)
# 計算有重複排名前5的
tt <- dt5
tt$count <- 1
tt <- aggregate(count~fish,data=tt,FUN=sum)
tt <- merge(tt,fish,by='fish')
tt <- tt[tt$count>3,]

dt5 <- merge(dt5,tt,by='fish',all = T)
dt5$col <- ifelse(is.na(dt5$count),'grey',dt5$col)
dt5 <- dt5[order(dt5$Year,dt5$Rank),]

dt.tab <- matrix(dt5$weight/1000,5,10)
dt_target <- dt[dt$fish %in% '白口',]


#畫圖
par(oma=c(2,1,2,1),cex.lab=1.25,cex.axis=1.25)
layout(matrix(c(1,1,1,2,2,2,2,2,2),3,3,byrow=TRUE))

par(mar=c(0,10,3,4),las=1)

plot(1:10,dt_target$weight/1000,xaxt='n',yaxt='n',type='n',bty="n",xlab='',ylab='',ylim=c(10,110))
lines(1:10,dt_target$weight/1000,type='b',pch=19)
text(1:10,dt_target$weight/1000+8,paste0(round(dt_target$proportion*100,2),'%'),
     col='red',cex=2)
text(1:10,dt_target$weight/1000-8,dt_target$Rank,col='blue',cex=2)
axis(1,1:10,dt_target$Year,font=2,cex.axis=2)
axis(4,at=seq(10,90,20),labels=seq(10,90,20),font=2,cex.axis=1.5,pos=10.2)
legend('topleft',legend=c('白口卸魚重占比','白口卸魚重排名'), box.lty=0,fill=c('red','blue'),
       cex=1.6,text.font=2)
mtext('白口卸魚重(噸)',side=4,line=2.5,las=0,cex=2,cex.axis=2,cex.lab=3, font=2)



par(mar=c(12,3,0,0))
barplot(dt.tab,beside=TRUE,col=dt5$col,yaxt='n',ylim=c(0,300),xlim=c(0,54),yaxt='n',ylab='',width=0.9)
xs <- seq(1.3,54.3,0.9)[-seq(6,54,6)]
axis(1,at=xs,labels=dt5$fish,las=2,font=2,cex=2,cex.axis=2.5,cex.lab=2)
axis(2,at=seq(0,300,50),labels=seq(0,300,50),font=2,cex.axis=1.5,pos=0.1)
mtext('卸魚重(噸)',side=2,line=0.5,las=0,cex=2,cex.axis=2,cex.lab=3, font=2)


