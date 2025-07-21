library(ggplot2)
data3 <- read.csv('修正2010年8月之前的卸魚資料_修改狗母為合齒魚科.csv', header=T)

head(data3,3)
str(data3)
data3$date <- paste0(data3$Year,'/',data3$Month)
data3 <- data3[order(data3$Year,data3$Month,data3$Day),]

tt <- data3[data3$fish %in% '白口' & data3$Harbour %in% '蚵仔寮',]
tt$Month <- ifelse(tt$Month < 10, paste0(0,tt$Month), tt$Month)
tt$date <- paste0(tt$Year,'/',tt$Month)

b <- aggregate(weight~Year+quarter,data=tt,FUN=sum)
b <- b[order(b$Year,b$quarter),]
b$yq <- paste0(b$Year,'/',b$quarter)
# b$label <- '全部船'

y_up <- max(na.omit(b$weight))/1000

png(paste0('白口每年季之卸魚重.png'),width=833,height=566)
par(mar=c(5,4,1.5,5),las=2,oma = c(1, 1, 1, 1))
plot(b$weight/1000,type='p',pch=19,xaxt='n',yaxt='n',xlab='',ylab='',cex=1.7,
     ylim=c(0,y_up+1),col=rep(1:4,length(b$weight)/4))
lines(b$weight/1000,type='l',pch=19,xaxt='n',xlab='',ylab='',cex=1.2,ylim=c(0,y_up))
axis(1,at=1:length(b$yq),labels = b$yq,cex=1.7,cex.axis=1.7,cex.lab=1.7,font=2)
axis(2,at=seq(0,y_up+1,0.5),labels = seq(0,y_up+1,0.5),cex=1.2,cex.axis=1.2,cex.lab=1.2,font=2)
mtext('白口卸魚重量(噸)',side=2,line=3.5,las=0,cex=1.7,cex.axis=1.7,cex.lab=1.7,font=2)
legend("topleft", legend = c("第1季", "第2季", "第3季",'第4季'),
       fill = c(1:4), text.col = c(1:4),bty='n',cex=2)
dev.off()


test <- aggregate(weight~CTNO,data=tt,FUN=sum)
test <- test[order(-test$weight),]
write.csv(test,'2010-2019白口漁船卸魚重排序.csv', row.names=F)


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


table(tt$CTNO[tt$CTNO %in% od[1:30]],tt$Year[tt$CTNO %in% od[1:30]])

# data <- read.csv('修正2010年8月之前的蚵仔寮卸魚資料.csv', header=T)
# head(data,2)
# tt <- data[data$fish %in% '白口',]
# test <- aggregate(weight~CTNO,data=tt,FUN=sum)
# test <- test[order(-test$weight),]
# write.csv(test,'2010-2019白口漁船卸魚重排序_1.csv', row.names=F)


