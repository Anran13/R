#================import package================
library (readr)
library(plyr)
library(reshape2)

rm(list=ls())
#================import data================
# from github
# data from 20200122
# we use only one year data

urlfile <- "https://raw.githubusercontent.com/Anran13/COVID-19/refs/heads/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
mydata <- readr::read_csv(url(urlfile))
readr::cols_condense(mydata)
readr::spec(mydata)
head(mydata,4); dim(mydata)
names(mydata)[2] <- c("region")

# aggregate countries' name
mydata$region[mydata$region %in% "Congo (Brazzaville)"] <- "Congo"
mydata$region[mydata$region %in% "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
mydata$region[mydata$region %in% "Taiwan*"] <- "Taiwan"
mydata$region[mydata$region %in% "Vietnam"] <-"Viet Nam"
mydata$region[mydata$region %in% "Korea, South"] <- "Democratic People's Republic of Korea"
mydata$region[mydata$region %in% "Tanzania"] <- "United Republic of Tanzania"
mydata$region[mydata$region %in% "Syria"] <- "Syrian Arab Republic"
mydata$region[mydata$region %in% "Brunei"] <- "Brunei Darussalam"
mydata$region[mydata$region %in% "Burma"] <- "Myanmar"
mydata$region[mydata$region %in% "Moldova"] <- "Republic of Moldova"
mydata$region[mydata$region %in% "Russia"] <- "Russian Federation"
mydata$region[mydata$region %in% "West Bank and Gaza"] <- "Palestine"
mydata$region[mydata$region %in% "Laos"] <- "Lao People's Democratic Republic"

# 20200122 ~ 20210122 covid-19 cases
updated.data <- mydata[,c(2,371)]
names(updated.data)[2] <- c("cases")
updated.data <- plyr::ddply(updated.data, "region", numcolwise(sum))


# country population (unit: thousands)
country <- read.csv("country_population.csv",header=T)
colnames(country)[1] <- "region"
country$region <- as.character(country$region)

# country$region[country$region %in% "C繫te d'Ivoire"] <- "Cote d'Ivoire"
delete <- grep("\\(", country$region) # remove "()"
for(i in delete){
    country$region[i] <- substring(country$region[i], 1, regexpr("\\(", country$region[i])[1]-2)
}

country$region[country$region %in% "China, Taiwan Province of China"] <- "Taiwan"
country$region[country$region %in% "Dem. People's Republic of Korea"] <- "Democratic People's Republic of Korea"
country$region[country$region %in% "Northern America"] <- "US"
country$region[country$region %in% "State of Palestine"] <- "Palestine"


#================merge data================
data <- merge(updated.data, country[,c(1,10)],by=c("region"),all = T)
data <- na.omit(data)
colnames(data)[3] <- "population"
data$rate <- data$cases / data$population
data <- data[order(-data$rate),]

# plot(data$rate[order(-data$rate)])
# ggplot(data=data, aes(x=region, y=rate)) + geom_bar(stat="identity")

former10 <- 1:10
dat <- t(data$rate[former10])
colnames(dat) <- data$region[former10]
barplot(dat, density = c(25), angle = c(45), ylim = c(0,150), las=2, ylab = "Covid-19 case rate")
box()


#=========================================================================================================

tw_data <- mydata[mydata$region %in% "Taiwan", -c(1,3,4)]
colnames(tw_data)

tw_long <- reshape2::melt(tw_data, id.vars = "region")
colnames(tw_long)[2:3] <- c("date", "case")
tw_long$date <- as.character(tw_long$date)

tw_long$date <- as.Date(tw_long$date, format="%m/%d/%y")
tw_long <- tw_long[order(tw_long$date),]

dat2 <- t(tw_long$case)
colnames(dat2) <- as.character(tw_long$date)
barplot(dat2, density = c(25), angle = c(45), ylim = c(0,max(tw_long$case)+100), las=2, main="Cumulative Confirmed Covid-19 cases")
box()