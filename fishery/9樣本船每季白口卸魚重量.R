library(ggplot2)
data3 <- read.csv('修正2010年8月之前的卸魚資料_修改狗母為合齒魚科.csv', header=T)

head(data3,3)
str(data3)
data3$date <- paste0(data3$Year,'/',data3$Month)
data3 <- data3[order(data3$Year,data3$Month,data3$Day),]

test <- data3[data3$fish %in% '白口',]
test$Month <- ifelse(test$Month < 10, paste0(0,test$Month), test$Month)
test$date <- paste0(test$Year,'/',test$Month)

# 樣本船
od <- read.csv('白口樣本船列表.csv',header=T)

#------------------畫每年每月黑口卸魚重量 (全部船與樣本船)----------------------
tt <- data3[data3$CTNO %in% od$CTNO & data3$Harbour %in% '蚵仔寮',]
tt <- tt[tt$fish %in% '白口',]
test <- aggregate(weight~Year+quarter+CTNO,data=tt,FUN=sum)
test$yq <- paste0(test$Year,'/',test$quarter)
str(test)
test$yq <- as.factor(test$yq)
test <- test[order(test$Year,test$quarter),]

a <- aggregate(weight~Year+quarter,data=test,FUN=sum)
a$yq <- paste0(a$Year,'/',a$quarter)
a <- a[order(a$Year,a$quarter),]
# a$label <- '標本船'


tt <- data3[data3$Harbour %in% '蚵仔寮',]
tt <- tt[tt$fish %in% '白口',]
b <- aggregate(weight~Year+quarter,data=tt,FUN=sum)
b <- b[order(b$Year,b$quarter),]
b$yq <- paste0(b$Year,'/',b$quarter)
# b$label <- '全部船'


y_up <- max(na.omit(b$weight))/1000

png(paste0('白口每年季樣本船之卸魚重.png'),width=833,height=566)
par(mar=c(5,4,1.5,5),las=2,oma = c(1, 1, 1, 1))
plot(b$weight/1000,type='p',pch=19,xaxt='n',yaxt='n',xlab='',ylab='',cex=1.2,ylim=c(0,y_up),
     col=rep(1:4,length(b$weight)/4),lty=2)
lines(b$weight/1000,type='l',pch=19,xaxt='n',xlab='',ylab='',cex=1.2,ylim=c(0,y_up),
      col='blue',lty=2)
lines(a$weight/1000,type='p',pch=19,xaxt='n',xlab='',ylab='',cex=1.2,
      col=rep(1:4,length(b$weight)/4),lty=1)
lines(a$weight/1000,type='l',pch=19,xaxt='n',xlab='',ylab='',cex=1.2,ylim=c(0,y_up),
      col='red',lty=1)
axis(1,at=1:length(a$yq),labels = a$yq,cex=1.7,cex.axis=1.7,cex.lab=1.7,font=2)
axis(2,at=seq(0,y_up,0.5),labels = seq(0,y_up,0.5),cex=1.2,cex.axis=1.2,cex.lab=1.2,font=2)
mtext('白口卸魚重量(噸)',side=2,line=3.5,las=0,cex=1.7,cex.axis=1.7,cex.lab=1.7,font=2)
legend("topleft", legend = c("第1季", "第2季", "第3季",'第4季'),
       fill = c(1:4), text.col = c(1:4),bty='n',cex=2)
legend(30,25, legend = c('全部船','標本船'),lty=c(2,1),col=c('blue','red'),bty='n',cex=2)
dev.off()

#------------------標本船與全部船的黑口卸魚重比例-----------------
ab <- merge(a,b,by=c('Year','quarter','yq'))

png('標本船占全部船之白口卸魚重比例.png',width=833,height=566)
ab$p <- ab$weight.x/ab$weight.y
par(mar=c(5,4,1.5,5),las=2,oma = c(1, 1, 1, 1))
plot(ab$p,type='p',pch=19,xaxt='n',yaxt='n',xlab='',ylab='',cex=1.2,ylim=c(0,1),col=rep(1:4,length(ab$yq)/4))
lines(ab$p,type='l',pch=19,xaxt='n',xlab='',ylab='',cex=1.2,ylim=c(0,1))
axis(1,at=1:length(ab$yq),labels = ab$yq,cex=1.7,cex.axis=1.7,cex.lab=1.7,font=2)
axis(2,at=seq(0,1,0.2),labels = seq(0,1,0.2),cex=1.2,cex.axis=1.2,cex.lab=1.2,font=2)
mtext('標本船白口卸魚重占全部船黑口卸魚重之比例',side=2,line=3.5,las=0,cex=1.7,cex.axis=1.7,cex.lab=1.7,font=2)
mtext('蚵仔寮',side=3,line=1,las=0,cex=1.7,cex.axis=1.7,cex.lab=1.7,font=2)
dev.off()