# Table of Contents
# 1. load data for the first time
# 2. load prepared data
# 3. exploratory analysis
# 3.1 eda on one shop
# 3.2 eda on shop id 1 to 100
# 3.3 eda on shop id 1 to 300
# 3.4 eda on shop id 1 to 300
# 4. try auto arima
# 5. try forecastxgb
# 6. try forecastxgb with external regressors (weekday number)
# 6.2 try forecastxgb with external regressors (weekday number+if is holiday)
# 6.3 try forecastxgb with external regressors (province id)

#########################################################################################################################################

# 1. load data for the first time
setwd("C:/Users/steven-j.yu/Downloads/Steven's/4. 项目/4.5 其他项目/6.5 数据分析/20170123 IJCAI-17口碑/dataset")
shop_info <- read.table("shop_info.txt",sep=",",fileEncoding="UTF-8",header=FALSE)
colnames(shop_info) = c("shop_id","city_name","location_id","per_pay","score",
                        "comment_cnt","shop_level",
                        "cate_1_name","cate_2_name","cate_3_name");
head(shop_info)

user_pay <- read.table("user_pay.txt",sep=",",fileEncoding="UTF-8",header=FALSE)
colnames(user_pay)=c("user_id","shop_id","time_stamp");
head(user_pay)

user_view <- read.table("user_view.txt",sep=",",fileEncoding="UTF-8",header=FALSE)
colnames(user_view)=c("user_id","shop_id","time_stamp");
head(user_view)

# save rdata file to share
save.image("tc_st_0210_prep.RData")


#########################################################################################################################################
# 2. load prepared data
setwd("C:/Users/steven-j.yu/Downloads/Steven's/4. 项目/4.5 其他项目/6.5 数据分析/20170123 IJCAI-17口碑/dataset")
load(file = "tc_st_0208_prep.RData")


#########################################################################################################################################
# 3. exploratory analysis
# 3.1 eda on one shop
# plot shop_id = 1862 first
up1862 <- user_pay[user_pay$shop_id==1862,]
head(up1862)
str(up1862)

library(sqldf)

# convert date
up1862$date <- as.Date(up1862$time_stamp)
head(up1862)

up1862g <- sqldf("select date,count(*) as cnt from up1862 group by date order by date")
head(up1862g)

library(ggplot2)
ggplot(up1862g, aes(x = date, y = cnt)) + geom_line()

# 3.2 eda on shop id 1 to 100
up1_100 <- user_pay[user_pay$shop_id>=1 & user_pay$shop_id<=100,]
head(up1_100)
str(up1_100)

# convert date
up1_100$date <- as.Date(up1_100$time_stamp)
# warning: as.Date seems not work well on large dataset, need find other ways to transfer the date column
head(up1_100)

up1_100g <- sqldf("select shop_id,date,count(*) as cnt from up1_100 group by date order by shop_id,date")
head(up1_100g)

library(ggplot2)
ggplot(up1_100g, aes(x = date, y = cnt,color = factor(shop_id))) + theme_hc(bgcolor = "darkunica") + scale_colour_hc("darkunica") + geom_line() + geom_point(size = 1) + ggtitle("Payment Count Trend of Shop ID 1-100") + labs(x="Date", y="User Pay Count")
# found many shops have been out of receiving payments for a long time

# 3.3 eda on shop id 1 to 300
up1_300 <- user_pay[user_pay$shop_id>=1 & user_pay$shop_id<=300,]
head(up1_300)
str(up1_300)

# convert date
up1_300$date <- as.Date(up1_300$time_stamp)
# warning: as.Date seems not work well on large dataset, need find other ways to transfer the date column
head(up3_100)

up1_300g <- sqldf("select shop_id,date,count(*) as cnt from up1_300 group by date order by shop_id,date")
head(up1_300g)

library(ggplot2)
ggplot(up1_300g, aes(x = date, y = cnt,color = factor(shop_id))) + theme_hc(bgcolor = "darkunica") + scale_colour_hc("darkunica") + geom_line() + geom_point(size = 1) + ggtitle("Payment Count Trend of Shop ID 1-300") + labs(x="Date", y="User Pay Count")
# found many shops have been out of receiving payments for a long time


#########################################################################################################################################
# 4. try auto arima

# load aggregated data
setwd("C:/Users/steven-j.yu/Downloads/Steven's/4. 项目/4.5 其他项目/6.5 数据分析/20170123 IJCAI-17口碑/dataset")
user_pay_gp <- read.csv("tc_user_pay_gp_by_IDDateCnt.csv",sep=",",fileEncoding="UTF-8",header=TRUE)
head(user_pay_gp)

# try auto arima
library(forecast)
shop1 <- sqldf("select Date,CNT from user_pay_gp where shop_id = 1 order by Date")
head(shop1)
shop1_ar <- auto.arima(shop1$CNT)
#plot(forecast(shop1_ar,h=14))

shop1_ar_fc <- forecast(shop1_ar,h=14)
head(shop1_ar_fc)
pre_res <- data.frame(t(c(as.integer(1),as.integer(trunc(shop1_ar_fc$mean)))),stringsAsFactors = FALSE)
head(pre_res)

colnames(pre_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'	,'day_5'	,'day_6'	,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
head(pre_res)


# create function for arima result
library(sqldf)
shop_list <- sqldf("select shop_id from shop_info group by shop_id")
str(shop_list)

for (i in 2:2000){
  
  print(paste("Start processing shop ",i,"."))
  
  # 1. select data by id
  current_shop_id <- as.character(shop_list[[1]][i])
  sqltorun <- paste("select Date,CNT from user_pay_gp where shop_id = ", i, " order by Date")
  current_shop <- sqldf(sqltorun)
  print("Select data by id done.")
  
  # 2. apply auto.arima
  current_shop_aa <- auto.arima(current_shop$CNT)
  current_shop_aa_pr <- forecast(current_shop_aa,h=14)
  current_shop_res <- data.frame(t(c(as.integer(i),as.integer(trunc(current_shop_aa_pr$mean)))),stringsAsFactors = FALSE)
  colnames(current_shop_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'	,'day_6'	,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
  print("Apply auto arima done.")
  
  # 3. combine data into one frame
  pre_res <- rbind(pre_res,current_shop_res,stringsAsFactors = F)
  print("Combine data done.")
}

print("Full process done.")

write.table(pre_res,file="prediction3.csv",quote = FALSE,sep=",",col.names=FALSE,fileEncoding="UTF-8")


#########################################################################################################################################
# 5. try forecastxgb

# load aggregated data
setwd("C:/Users/steven-j.yu/Downloads/Steven's/4. 项目/4.5 其他项目/6.5 数据分析/20170123 IJCAI-17口碑/dataset")
user_pay_gp <- read.csv("tc_user_pay_gp_by_IDDateCnt.csv",sep=",",fileEncoding="UTF-8",header=TRUE)
head(user_pay_gp)

# try forecastxgb
library(forecastxgb)

shop1_fxgb <- xgbar(ts(shop1$CNT))
shop1_fxgb_fc <- forecast(shop1_fxgb,h=14)

plot(shop1_fxgb_fc)

pre_res <- data.frame(t(c(as.integer(1),as.integer(trunc(shop1_fxgb_fc$mean)))),stringsAsFactors = FALSE)
head(pre_res)

colnames(pre_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'	,'day_6'	,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
head(pre_res)


# create function for forecastxgb result
library(sqldf)
shop_list <- sqldf("select shop_id from shop_info group by shop_id")
str(shop_list)

for (i in 2:2000){
  
  print(paste("Start processing shop ",i,". ", percentage((i-1)/2000), " processed.")
  
  # 1. select data by id
  current_shop_id <- as.character(shop_list[[1]][i])
  sqltorun <- paste("select Date,CNT from user_pay_gp where shop_id = ", i, " order by Date")
  current_shop <- sqldf(sqltorun)
  print("Select data by id done.")
  
  # 2. apply forecastxgb
  current_shop_aa <- xgbar(ts(current_shop$CNT))
  current_shop_aa_pr <- forecast(current_shop_aa,h=14)
  current_shop_res <- data.frame(t(c(as.integer(i),as.integer(trunc(current_shop_aa_pr$mean)))),stringsAsFactors = FALSE)
  colnames(current_shop_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'	,'day_6'	,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
  print("Apply auto arima done.")
  
  # 3. combine data into one frame
  pre_res <- rbind(pre_res,current_shop_res,stringsAsFactors = F)
  print("Combine data done.")
}

print("Full process done.")

write.table(pre_res,file="prediction4.csv",quote = FALSE,sep=",",row.names=FALSE,col.names=FALSE,fileEncoding="UTF-8")
# loss 0.115


#########################################################################################################################################
# 6. try forecastxgb with external regressors

# load aggregated data
setwd("C:/Users/steven-j.yu/Downloads/Steven's/4. 项目/4.5 其他项目/6.5 数据分析/20170123 IJCAI-17口碑/dataset")
load(file = "tc_st_0210_prep.RData")

user_pay_gp_hldprv <- read.csv("tc_user_pay_gp_by_IDDateCnt_hp_added.csv",sep=",",fileEncoding="UTF-8",header=TRUE)
head(user_pay_gp_hldprv)

# try forecastxgb using external regressors - one x
library(forecastxgb)
shop1 <- sqldf("select Date,CNT,HolidayTypeNum,WeekdayNum from user_pay_gp_hldprv where shop_id = 1 order by Date")
head(shop1)

shop1CNT <- ts(shop1$CNT)
shop1WeekdayNum <- matrix(shop1$WeekdayNum, dimnames = list(NULL, "WeekdayNum"))
head(shop1WeekdayNum)

shop1_fxgb_wkdn <- xgbar(y=shop1CNT,x=shop1WeekdayNum)
shop1_fxgb_wkdn_future <- matrix(forecast(xgbar(ts(shop1$WeekdayNum)), h = 14)$mean, 
                             dimnames = list(NULL, "WeekdayNum")) # we need forecast the x feature first
head(shop1_fxgb_wkdn_future)
temp <- data.frame(t(c(as.integer(1),as.integer(trunc(shop1_fxgb_wkdn_future)))),stringsAsFactors = FALSE)

plot(forecast(shop1_fxgb_wkdn, xreg = shop1_fxgb_wkdn_future))

str(forecast(shop1_fxgb_wkdn, xreg = shop1_fxgb_wkdn_future))
shop1_fc <- forecast(shop1_fxgb_wkdn, xreg = shop1_fxgb_wkdn_future)

pre_res <- data.frame(t(c(as.integer(1),as.integer(trunc(shop1_fc$mean)))),stringsAsFactors = FALSE)

colnames(pre_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'  ,'day_6'	,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
head(pre_res)


# create function for forecastxgb result
library(sqldf)
shop_list <- sqldf("select shop_id from shop_info group by shop_id")
str(shop_list)

for (i in 2:2000){
  
  print(paste("Start processing shop ",i,". ", (i-1)/2000, " processed."))
        
        # 1. select data by id
        current_shop_id <- as.character(shop_list[[1]][i])
        sqltorun <- paste("select Date,CNT,WeekdayNum from user_pay_gp_hldprv where shop_id = ", i, " order by Date")
        current_shop <- sqldf(sqltorun)
        print("Select data by id done.")
        
        # 2. apply forecastxgb with one variable
        current_shop_CNT <- ts(current_shop$CNT)
        current_shop_WeekdayNum <- matrix(current_shop$WeekdayNum, dimnames = list(NULL, "WeekdayNum"))
        
        current_shop_wkdn <- xgbar(y=current_shop_CNT,x=current_shop_WeekdayNum)
        current_shop_wkdn_future <- matrix(forecast(xgbar(ts(current_shop$WeekdayNum)), h = 14)$mean, 
                                           dimnames = list(NULL, "WeekdayNum")) # forecast weekday num first
        current_shop_pr <- forecast(current_shop_wkdn, xreg = current_shop_wkdn_future)
        current_shop_res <- data.frame(t(c(as.integer(1),as.integer(trunc(current_shop_pr$mean)))),stringsAsFactors = FALSE)
        
        colnames(current_shop_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'	,'day_6'	,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
        print("Apply forecastxgb using one variable done.")
        
        # 3. combine data into one frame
        pre_res <- rbind(pre_res,current_shop_res,stringsAsFactors = F)
        print("Combine data done.")
}

print("Full process done.")

write.table(pre_res,file="prediction5.csv",quote = FALSE,sep=",",row.names=FALSE,col.names=FALSE,fileEncoding="UTF-8")
# loss 0.105, great

#########################################################################################################################################
# 6.2 try forecastxgb with external regressors (weekday number+if is holiday)

# load aggregated data
setwd("C:/Users/steven-j.yu/Downloads/Steven's/4. 项目/4.5 其他项目/6.5 数据分析/20170123 IJCAI-17口碑/dataset")
load(file = "tc_st_0210_prep.RData")

user_pay_gp_hldprv2 <- read.csv("tc_user_pay_gp_by_IDDateCnt_hp_added2.csv",sep=",",fileEncoding="UTF-8",header=TRUE)
head(user_pay_gp_hldprv2)

# try forecastxgb using external regressors - one x
library(sqldf)
library(forecastxgb)
shop1 <- sqldf("select Date,CNT,WeekdayNHoliday from user_pay_gp_hldprv2 where shop_id = 1 order by Date")
head(shop1)

shop1CNT <- ts(shop1$CNT)
shop1WeekdayNHoliday <- matrix(shop1$WeekdayNHoliday, dimnames = list(NULL, "WeekdayNHoliday"))
head(shop1WeekdayNHoliday)

shop1_fxgb_wkdn <- xgbar(y=shop1CNT,x=shop1WeekdayNHoliday)
shop1_fxgb_wkdn_future <- matrix(forecast(xgbar(ts(shop1$WeekdayNHoliday)), h = 14)$mean, 
                                 dimnames = list(NULL, "WeekdayNHoliday")) # we need forecast the x feature first
head(shop1_fxgb_wkdn_future)
temp <- data.frame(t(c(as.integer(1),as.integer(trunc(shop1_fxgb_wkdn_future)))),stringsAsFactors = FALSE)

plot(forecast(shop1_fxgb_wkdn, xreg = shop1_fxgb_wkdn_future))

str(forecast(shop1_fxgb_wkdn, xreg = shop1_fxgb_wkdn_future))
shop1_fc <- forecast(shop1_fxgb_wkdn, xreg = shop1_fxgb_wkdn_future)

pre_res <- data.frame(t(c(as.integer(1),as.integer(trunc(shop1_fc$mean)))),stringsAsFactors = FALSE)

colnames(pre_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'  ,'day_6'  ,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
head(pre_res)


# create function for forecastxgb result
shop_list <- sqldf("select shop_id from shop_info group by shop_id")
str(shop_list)

for (i in 2:2000){
  
  print(paste("Start processing shop ",i,". ", (i-1)/2000, " processed."))
  
  # 1. select data by id
  current_shop_id <- as.character(shop_list[[1]][i])
  sqltorun <- paste("select Date,CNT,WeekdayNHoliday from user_pay_gp_hldprv2 where shop_id = ", i, " order by Date")
  current_shop <- sqldf(sqltorun)
  print("Select data by id done.")
  
  # 2. apply forecastxgb with one variable
  current_shop_CNT <- ts(current_shop$CNT)
  current_shop_WeekdayNum <- matrix(current_shop$WeekdayNHoliday, dimnames = list(NULL, "WeekdayNHoliday"))
  
  current_shop_wkdn <- xgbar(y=current_shop_CNT,x=current_shop_WeekdayNum)
  current_shop_wkdn_future <- matrix(forecast(xgbar(ts(current_shop$WeekdayNHoliday)), h = 14)$mean, 
                                     dimnames = list(NULL, "WeekdayNHoliday")) # forecast weekday num first
  current_shop_pr <- forecast(current_shop_wkdn, xreg = current_shop_wkdn_future)
  current_shop_res <- data.frame(t(c(as.integer(1),as.integer(trunc(current_shop_pr$mean)))),stringsAsFactors = FALSE)
  
  colnames(current_shop_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'	,'day_6'	,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
  print("Apply forecastxgb using one variable done.")
  
  # 3. combine data into one frame
  pre_res <- rbind(pre_res,current_shop_res,stringsAsFactors = F)
  print("Combine data done.")
}

print("Full process done.")

write.table(pre_res,file="prediction6.csv",quote = FALSE,sep=",",row.names=FALSE,col.names=FALSE,fileEncoding="UTF-8")
# loss

#########################################################################################################################################
# 6.3 try forecastxgb with external regressors (province id)

# load aggregated data
setwd("C:/Users/steven-j.yu/Downloads/Steven's/4. 项目/4.5 其他项目/6.5 数据分析/20170123 IJCAI-17口碑/dataset")
load(file = "tc_st_0216_prep.RData")

user_pay_gp_hldprv2 <- read.csv("tc_user_pay_gp_by_IDDateCnt_hp_added_provid.csv",sep=",",fileEncoding="UTF-8",header=TRUE)
head(user_pay_gp_hldprv2)

# try forecastxgb using external regressors - one x
library(sqldf)
library(forecastxgb)
shop1 <- sqldf("select Date,CNT,ProvID as WeekdayNHoliday from user_pay_gp_hldprv2 where shop_id = 1 order by Date")
head(shop1)

shop1CNT <- ts(shop1$CNT)
shop1WeekdayNHoliday <- matrix(shop1$WeekdayNHoliday, dimnames = list(NULL, "WeekdayNHoliday"))
head(shop1WeekdayNHoliday)

shop1_fxgb_wkdn <- xgbar(y=shop1CNT,x=shop1WeekdayNHoliday)
shop1_fxgb_wkdn_future <- matrix(forecast(xgbar(ts(shop1$WeekdayNHoliday)), h = 14)$mean, 
                                 dimnames = list(NULL, "WeekdayNHoliday")) # we need forecast the x feature first
head(shop1_fxgb_wkdn_future)
temp <- data.frame(t(c(as.integer(1),as.integer(trunc(shop1_fxgb_wkdn_future)))),stringsAsFactors = FALSE)

plot(forecast(shop1_fxgb_wkdn, xreg = shop1_fxgb_wkdn_future))

str(forecast(shop1_fxgb_wkdn, xreg = shop1_fxgb_wkdn_future))
shop1_fc <- forecast(shop1_fxgb_wkdn, xreg = shop1_fxgb_wkdn_future)

pre_res <- data.frame(t(c(as.integer(1),as.integer(trunc(shop1_fc$mean)))),stringsAsFactors = FALSE)

colnames(pre_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'  ,'day_6'  ,'day_7'  ,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
head(pre_res)


# create function for forecastxgb result
shop_list <- sqldf("select shop_id from shop_info group by shop_id")
str(shop_list)
i=2
for (i in 2:2000){
  
  print(paste("Start processing shop ",i,". ", (i-1)/2000, " processed."))
  
  # 1. select data by id
  current_shop_id <- as.character(shop_list[[1]][i])
  sqltorun <- paste("select Date,CNT,ProvID as WeekdayNHoliday from user_pay_gp_hldprv2 where shop_id = ", i, " order by Date")
  current_shop <- sqldf(sqltorun)
  print("Select data by id done.")
  
  # 2. apply forecastxgb with one variable
  current_shop_CNT <- ts(current_shop$CNT)
  current_shop_WeekdayNum <- matrix(current_shop$WeekdayNHoliday, dimnames = list(NULL, "WeekdayNHoliday"))
  
  current_shop_wkdn <- xgbar(y=current_shop_CNT,x=current_shop_WeekdayNum)
  current_shop_wkdn_future <- matrix(forecast(xgbar(ts(current_shop$WeekdayNHoliday)), h = 14)$mean, 
                                     dimnames = list(NULL, "WeekdayNHoliday")) # forecast weekday num first
  current_shop_pr <- forecast(current_shop_wkdn, xreg = current_shop_wkdn_future)
  current_shop_res <- data.frame(t(c(as.integer(i),as.integer(trunc(current_shop_pr$mean)))),stringsAsFactors = FALSE)
  
  colnames(current_shop_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'	,'day_6'	,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
  print("Apply forecastxgb using one variable done.")
  
  # 3. combine data into one frame
  pre_res <- rbind(pre_res,current_shop_res,stringsAsFactors = F)
  print("Combine data done.")
}

print("Full process done.")

write.table(pre_res,file="prediction7.csv",quote = FALSE,sep=",",row.names=FALSE,col.names=FALSE,fileEncoding="UTF-8")
# loss

save.image("tc_st_0216_prep.RData")









# try forecastxgb using external regressors - two x - not yet done
head(shop1)

shop1CNT <- ts(shop1$CNT)
shop1HTNWN <- matrix(shop1$HolidayTypeNum, shop1$WeekdayNum, dimnames = list("HolidayTypeNum","WeekdayNum"))
head(shop1HTNWN)

shop1_fxgb_wkdn <- xgbar(y=shop1CNT,x=shop1WeekdayNum)
shop1_fxgb_wkdn_future <- matrix(forecast(xgbar(ts(shop1$WeekdayNum)), h = 14)$mean, 
                                 dimnames = list(NULL, "WeekdayNum")) # we need forecast the x feature first

plot(forecast(shop1_fxgb_wkdn, xreg = shop1_fxgb_wkdn_future))

# recap results into desired frame



