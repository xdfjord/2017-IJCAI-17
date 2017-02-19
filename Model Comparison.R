library(data.table)
library(sqldf)
library(forecastxgb)

## prepare score function
score_fun = function (pre,real){ 
  if (nrow(pre)!=nrow(real) | ncol(pre)!=ncol(real)) 
    print ("Row number or Column number not match.") 
  else { 
    a=0 
    for (i in 1:nrow(pre)) { 
      for (j in 2:ncol(pre)){ 
        a=a+abs((pre[i,j]-real[i,j])/(pre[i,j]+real[i,j])) 
      } 
    } 
    score=a/(nrow(pre)*(ncol(pre)-1)) 
    return(score) 
  } 
} 



## prepare real data for 14 days

ref_set = subset(user_pay,as.Date(user_pay$time_stamp)>"2016-10-17") #change the date when necessary
head(ref_set)
ref_set$Date = as.Date(ref_set$time_stamp)
ref_set = subset(ref_set,select = -c(time_stamp))
real = table(ref_set$shop_id,ref_set$Date)
real = as.data.frame.matrix(real)
setDT(real,keep.rownames = TRUE)
colnames(real)=c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'  ,'day_6'  ,'day_7'  ,'day_8'  ,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
real = data.frame(real)

# only 1999 shops in above data, put the missing one back
for (i in 1:2000){
  if (!(i %in% real$shop_id))
  {print(i)
    real = rbind(real,c(i,rep(0,14)))}
}
real_res = real[order(as.numeric(real$shop_id)),]


## prepare result list for (model, score)
model_score = data.frame(mod_name=character(),mod_score=numeric())


## prepare training and testing data set
train_set = subset(user_pay_gp_hldprv2,as.Date(user_pay_gp_hldprv2$Date)<="2016-10-17")



## START MODEL SELECTION
# Generate a list for indicating which columns are used in current model
col_list = c("HolidayType","WeekdayNHoliday","ProvID") #list all available column names
n = length(col_list)
col_use = data.frame(col_name = col_list, col_id = rep(0,n),col_ind = rep(0,n))

for (i in 1:n){
  col_use$col_id[i]=i
}

for (k in 1:2^n-1){
  # indicate the columns used in current model
  x = k
  for (i in 1:n){
    j = x %/% 2^(n-i)
    if (j==1) {col_use$col_ind[i]=1}
    else {col_use$col_ind[i]=0}
    x = x-j*2^(n-i)
  }    
  ##print(col_use$col_ind)}
  
  ## create command lines for current model as strings
  sel_str = character(0)
  df_str = character(0)
  df_str_shop1 = character(0)
  com_str = character(0)
  for_str = character(0)
  for_str_shop1 = character(0)
  for (i in 1:n){
    if (col_use$col_ind[i]==1){
      sel_str = paste(sel_str,col_use$col_name[i],sep = ",")
      df_str = paste(df_str,paste("current_shop$",col_use$col_name[i],sep=""),sep = ",")
      df_str_shop1 = paste(df_str_shop1,paste("shop1$",col_use$col_name[i],sep=""),sep = ",")
      com_str = paste(com_str,paste("\"",col_use$col_name[i],"\"",sep=""),sep=",")
      for_str = paste(for_str,paste("forecast(xgbar(ts(current_shop$",col_use$col_name[i],")), h=14)$mean",sep=""),sep=",")
      for_str_shop1 = paste(for_str_shop1,paste("forecast(xgbar(ts(shop1$",col_use$col_name[i],")), h=14)$mean",sep=""),sep=",")
    }
  }
  sel_str = substr(sel_str,2,nchar(sel_str))
  df_str = paste("data.frame(",substr(df_str,2,nchar(df_str)),")",sep = "")
  df_str_shop1 = paste("data.frame(",substr(df_str_shop1,2,nchar(df_str_shop1)),")",sep = "")
  com_str = paste("c(",substr(com_str,2,nchar(com_str)),")",sep="")
  for_str = paste("data.frame(",substr(for_str,2,nchar(for_str)),")",sep="")
  for_str_shop1 = paste("data.frame(",substr(for_str_shop1,2,nchar(for_str_shop1)),")",sep="")
  
  
  shop1 <- sqldf("select Date,CNT,",sel_str," from train_set where shop_id = 1 order by Date")
  head(shop1)
  
  shop1CNT <- ts(shop1$CNT)
  shop1WNHPI <- eval(parse(text=df_str_shop1))
  colnames(shop1WNHPI) <- eval(parse(text=com_str))
  head(shop1WNHPI)
  shop1WNHPIm <- data.matrix(shop1WNHPI)
  head(shop1WNHPIm)
  
  # run the model
  shop1WNHPIm_mdl <- xgbar(y=shop1CNT,x=shop1WNHPIm)
  shop1WNHPIm_mdl
  
  # prepare predcition for each column
  shop1WNHPIm_x_df <- eval(parse(text=for_str_shop1))
  
  colnames(shop1WNHPIm_x_df) <- eval(parse(text=com_str))
  shop1WNHPIm_x_dm <- data.matrix(shop1WNHPIm_x_df)
  head(shop1WNHPIm_x_dm)
  
  plot(forecast(shop1WNHPIm_mdl, xreg = shop1WNHPIm_x_dm))
  
  shop1_fc <- forecast(shop1WNHPIm_mdl, xreg = shop1WNHPIm_x_dm)
  
  pre_res <- data.frame(t(c(as.integer(1),as.integer(trunc(shop1_fc$mean)))),stringsAsFactors = FALSE)
  colnames(pre_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'  ,'day_6'  ,'day_7'  ,'day_8'  ,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
  

  # for remaining 1999 shops  
  for (i in 2:2000){
    
    print(paste("Start processing shop ",i,". ", (i-1)/2000, " processed."))
    
    # 1. select data by id
    current_shop_id <- as.character(shop_list[[1]][i])
    sqltorun <- paste("select Date,CNT,",sel_str," from train_set where shop_id = ", i, " order by Date")
    current_shop <- sqldf(sqltorun)
    print("Select data by id done.")
    
    # 2. apply forecastxgb with one variable
    current_shop_CNT <- ts(current_shop$CNT)
    current_shop_WeekdayNum <- eval(parse(text = df_str))
    colnames(current_shop_WeekdayNum) <- eval(parse(text=com_str))
    current_shop_WeekdayNum <- data.matrix(current_shop_WeekdayNum)
    
    current_shop_wkdn <- xgbar(y=current_shop_CNT,x=current_shop_WeekdayNum)
    
    current_shop_wkdn_future <- eval(parse(text=for_str))
    
    colnames(current_shop_wkdn_future) <- eval(parse(text=com_str))
    current_shop_wkdn_future <- data.matrix(current_shop_wkdn_future)
    
    current_shop_pr <- forecast(current_shop_wkdn, xreg = current_shop_wkdn_future)
    current_shop_res <- data.frame(t(c(as.integer(i),as.integer(trunc(current_shop_pr$mean)))),stringsAsFactors = FALSE)
    
    colnames(current_shop_res) <- c('shop_id','day_1','day_2'  ,'day_3'  ,'day_4'  ,'day_5'  ,'day_6'	,'day_7'	,'day_8'	,'day_9'	,'day_10'	,'day_11'	,'day_12'	,'day_13'	,'day_14')
    print("Apply forecastxgb using one variable done.")
    
    # 3. combine data into one frame
    pre_res <- rbind(pre_res,current_shop_res,stringsAsFactors = F)
    print("Combine data done.")
  }
  
model_score=rbind(model_score,c(sel_str,score_fun(pre_res,real_res)))
}
colnames(model_score) = c("mod_name","mod_score")
print("Model coparison done.")
