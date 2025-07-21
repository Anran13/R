library(ggplot2)
library(dplyr)
library(forcats)

od <- read.csv('白口樣本船列表.csv',header=T)
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
tt1 <- tt[tt$Harbour %in% '蚵仔寮',]
tt1 <- tt1[1:20,]
#---------------------------------45艘樣本船之卸魚重前20名-----------------------------------

str(data)
tt <- data[data$CTNO %in% od$CTNO & data$Harbour %in% '蚵仔寮',]
tt <- tt[tt$fish %in% fish$fish,]
tt <- aggregate(weight~fish,data=tt,FUN=sum)
tt <- tt[order(-tt$weight),]
tt <- tt[1:20,]
tt <- merge(tt,fish,by=c('fish'),all.x=T)
tt$Weight <- tt$weight/1000
tt <- tt[order(-tt$weight),]

png('蚵仔寮全部船與標本船的前20卸魚重排名.png',width=982,height=566)
par(oma=c(2,1,1,1),cex.lab=1.25,cex.axis=1.25)
layout(matrix(c(2,1,2,1),2,2,byrow=TRUE))

par(mar=c(3,0,2,6.5),las=1)
barplot(tt1$Weight,col=tt1$col,xlim=c(0,1400),horiz=TRUE,xaxt='n')
ys <- seq(0.6,23.5,length=20)
axis(4,at=ys,labels=tt1$fish,cex=1.2,cex.axis=1.2,cex.lab=1.2,font=2,las=1)
axis(1,at=seq(0,1400,100),labels=seq(0,1400,100),cex=1.2,cex.axis=1.2,cex.lab=1.2,font=2,las=1)
mtext('蚵仔寮全部船',side=3,line=1,las=0,cex=1.5,cex.axis=1.5,cex.lab=1.5, font=2)
box()

par(mar=c(3,6.5,2,0),las=1)
barplot(-tt$Weight,col=tt$col,xlim=c(-1400,0),horiz=TRUE,xaxt='n')
axis(2,at=ys,labels=tt$fish,cex=1.2,cex.axis=1.2,cex.lab=1.2,font=2)
axis(1,at=seq(-1400,0,100),labels=seq(1400,0,-100),cex=1.2,cex.axis=1.2,cex.lab=1.2,font=2,las=1)
mtext('蚵仔寮標本船',side=3,line=1,las=0,cex=1.5,cex.axis=1.5,cex.lab=1.5, font=2)
box()
mtext('卸魚重量(噸)',side=1,line=0.5,las=0,cex=1.5,cex.axis=1.5,cex.lab=1.5, font=2, outer = T)
dev.off()