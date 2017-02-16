
#### 0. supposed input variables
#### a. source data table, below is: user_pay_gp_hldprv2
#### b. time series column name to be predicted and date label for order, below is: CNT, Date
#### c. regressors, below is: WeekdayNHoliday,ProvID (need research how to include multiple input here)
#### d. periods need to predict, below is: 14

#### 1. load packages
library(sqldf)
library(forecastxgb)

#### 2. create the first row of the final framework (could be combined into step3)
###  2.1 select data for the first shop id, need confirm to have specified columns
shop1 <- sqldf("select Date,CNT,WeekdayNHoliday,ProvID from user_pay_gp_hldprv2 where shop_id = 1 order by Date") # prefer to set as select *, need confirm if possible

###  2.2 transfer columns to data matrix
shop1CNT <- ts(shop1$CNT) # no need change 
shop1WNHPI <- data.frame(shop1$WeekdayNHoliday, shop1$ProvID)) #!! here need update to certain circumstances
colnames(shop1WNHPI) <- c("WeekdayNHoliday","ProvID") #!! here also
shop1WNHPIm <- data.matrix(shop1WNHPI) # no need change 

###  2.3 run the model
shop1WNHPIm_mdl <- xgbar(y=shop1CNT,x=shop1WNHPIm) # no need change 
shop1WNHPIm_mdl # no need change 

###  2.4 prepare predcition for each column
shop1WNHPIm_x_df <- data.frame(
  forecast(xgbar(ts(shop1$WeekdayNHoliday)), h=14)$mean, 
  forecast(xgbar(ts(shop1$ProvID)), h=14)$mean) #!!need update

colnames(shop1WNHPIm_x_df) <- c("WeekdayNHoliday","ProvID")  #!!need update
shop1WNHPIm_x_dm <- data.matrix(shop1WNHPIm_x_df) # no need change

###  2.5 predict the final y
shop1_fc <- forecast(shop1WNHPIm_mdl, xreg = shop1WNHPIm_x_dm) # no need change

###  2.6 transfer to desired format
pre_res <- data.frame(t(c(as.integer(1),as.integer(trunc(shop1_fc$mean)))),stringsAsFactors = FALSE) # no need change
colnames(pre_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'  ,'day_6'  ,'day_7'  ,'day_8'  ,'day_9'  ,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14') # no need change

#### 3. create loop to left shops
###  3.1 get shop list
shop_list <- sqldf("select shop_id from shop_info group by shop_id") # no need change

for (i in 2:2000){
  
  print(paste("Start processing shop ",i,". ", (i-1)/2000, " processed.")) # no need change
  
###  3.2 select data by id
  current_shop_id <- as.character(shop_list[[1]][i]) # no need change
  sqltorun <- paste("select Date,CNT,WeekdayNHoliday,ProvID from user_pay_gp_hldprv2 where shop_id = ", i, " order by Date") # prefer to set as select *, need confirm if possible
  current_shop <- sqldf(sqltorun) # no need change
  print("Select data by id done.")
  
###  3.3 apply forecastxgb with one variable
  current_shop_CNT <- ts(current_shop$CNT) # no need change
  current_shop_WeekdayNum <- data.frame(current_shop$WeekdayNHoliday, current_shop$ProvID) #!!need update
  colnames(current_shop_WeekdayNum) <- c("WeekdayNHoliday","ProvID") #!!need update
  current_shop_WeekdayNum <- data.matrix(current_shop_WeekdayNum) # no need change
  
  current_shop_wkdn <- xgbar(y=current_shop_CNT,x=current_shop_WeekdayNum) # no need change
  
  current_shop_wkdn_future <- data.frame(
    forecast(xgbar(ts(current_shop$WeekdayNHoliday)), h=14)$mean,
    forecast(xgbar(ts(current_shop$ProvID)), h=14)$mean) #!!need update
  
  colnames(current_shop_wkdn_future) <- c("WeekdayNHoliday","ProvID") #!!need update
  current_shop_wkdn_future <- data.matrix(current_shop_wkdn_future) # no need change
  
  current_shop_pr <- forecast(current_shop_wkdn, xreg = current_shop_wkdn_future) # no need change
  current_shop_res <- data.frame(t(c(as.integer(i),as.integer(trunc(current_shop_pr$mean)))),stringsAsFactors = FALSE) # no need change
  
  colnames(current_shop_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'  ,'day_6'	,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14') # no need change
  print("Apply forecastxgb using one variable done.")  # no need change
  
###  3.4 combine data into one frame
  pre_res <- rbind(pre_res,current_shop_res,stringsAsFactors = F) # no need change
  print("Combine data done.")
}

#### 4. output result, the result matrix is pre_res
print("Full process done.")
