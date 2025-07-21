library(ggplot2)

data <- read.csv('白口漁業別資料.csv', header=T)

head(data,2)
str(data)
#---------------------整理漁業別資料-------------------------------

# 刪除空格
unique(data$fishery)
data$fishery <- gsub(" ", "", data$fishery)
data$fishery <- gsub("　", "", data$fishery)



#-----------------------------------畫圖---------------------------------------------------
# 2002年開始
tt <- data[data$year > 2001,]

# 去除合計
tt <- tt[-(grep("合計", tt$fishery)),]
unique(tt$fishery)
tt$fishery[tt$fishery %in% '中小拖網'] <- '中小型拖網'

# 刺網皆合在一起看
# tt$fishery[tt$code %in% c(205,2005)] <- '近海刺網'
# tt$fishery[tt$code %in% c(304,3004)] <- '沿岸刺網'


tt_wide <- tt[ order(tt$fishery, tt$year), ]
ref <- data.frame(fishery=unique(tt$fishery),id=c(1:length(unique(tt$fishery))))
tt_wide <- merge(tt_wide,ref,by=c('fishery'))
tt_wide <- na.omit(tt_wide)

library(reshape2)
tt_wide <- dcast(tt_wide, fishery ~ year, value.var="quantity",fun.aggregate = sum)
write.csv(tt_wide,'漁業別年間白口產量_刺網合在一起看.csv',row.names=F)


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
  geom_bar(aes(x=as.factor(year),y=quantity,fill=as.factor(fishery)),
           position = "stack",stat="identity") +
  coord_flip() + ylab('產量(千噸)')+ xlab('年')


# target <- tt[tt$fishery %in% c('單船拖網','雙船拖網','中小型拖網','近海刺網','沿岸刺網'),]

# 刺網合在一起看
target <- tt[tt$fishery %in% c('中小型拖網','刺網','一支釣'),]
target <- aggregate(quantity~year+fishery,data=target,FUN=sum)
p <- ggplot(target, aes(x = as.factor(year), y = quantity,
               group=as.factor(fishery),color=as.factor(fishery))) +  
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
ggsave('漁業白口產量_刺網合在一起看.png')
