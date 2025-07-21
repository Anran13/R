library(ggplot2)

data <- read.csv('白口縣市別資料.csv', header=T)

head(data,2)
str(data)
#---------------------整理縣市別資料-------------------------------

# 刪除空格
unique(data$district)
data$district <- gsub(" ", "", data$district)

# 國外補給港令為國外基地
data$district[data$district=='國外補給港'] <- '國外基地'
unique(data$district)

# data <- na.omit(data)

# 1999~2001年沒有資料
# unique(data$year)
# dt <- as.data.frame(rbind(c(NA,NA,NA,NA,1999),c(NA,NA,NA,NA,2000),c(NA,NA,NA,NA,2001)))
# names(dt) <- names(data)
# data <- rbind(dt,data)

#-----------------------------------畫圖---------------------------------------------------
# 看2002年以後的資料
tt <- data[!(data$district %in% '國外基地'),]
tt <- tt[tt$year > 2001,]


tt_wide <- tt[ order(tt$district, tt$year), ]
ref <- data.frame(district=unique(tt$district),id=c(1:length(unique(tt$district))))
tt_wide <- merge(tt_wide,ref,by=c('district'))

library(reshape2)
tt_wide <- dcast(tt_wide, id + district ~ year, value.var="quantity")
write.csv(tt_wide,'縣市別年間白口產量.csv',row.names=F)

library("ggrepel")

ggplot(tt)+
  theme(panel.grid.minor = element_blank(),
        # plot.title = element_text(colour = "black", face = "bold", size = 30),
        axis.text.x = element_text( size = 13 , face = "bold"),#angle = 90
        axis.text.y = element_text( size = 15 , face = "bold" ),
        strip.text = element_text(size = 20, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = 'right',
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill=NA)) +# 透明legend背景
  guides(fill=guide_legend(ncol=1,byrow=FALSE)) +
  geom_bar(aes(x=as.factor(year),y=quantity,fill=as.factor(district)),
           position = "stack",stat="identity") +
  coord_flip() + ylab('產量(公噸)')+ xlab('年')

# 只畫高雄縣市
target <- tt[tt$district %in% c('高雄市','高雄縣'),]

p <- ggplot(target, aes(x = as.factor(year), y = quantity,
               group=as.factor(district),color=as.factor(district))) +  
  geom_line()+ geom_point() +
  theme(panel.grid.minor = element_blank(),
        # plot.title = element_text(colour = "black", face = "bold", size = 30),
        axis.text.x = element_text( size = 13 , face = "bold",angle = 90),#angle = 90
        axis.text.y = element_text( size = 15 , face = "bold" ),
        strip.text = element_text(size = 20, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = c(0.8,0.8),
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill=NA)) +# 透明legend背景
  guides(fill=guide_legend(nrow=3,byrow=FALSE)) +
  xlab('')+ylab('產量(公噸)')+
  scale_y_continuous(breaks = seq(0, 1700,by=200))
print(p)
ggsave('高雄縣市白口產量.png')