install.packages("DBI")
install.packages("RSQLite")
install.packages("rpart.plot")
library(DBI)
library(RSQLite)
library(rpart)
library(rpart.plot)
getwd()


#connecting the data
Data <- dbConnect(RSQLite::SQLite(), "project1_raw_data.db")
dbListTables(Data)

#Loading the tables in the data
train   <- dbGetQuery(Data, "SELECT * FROM train")
weather <- dbGetQuery(Data, "SELECT * FROM weather")
key_tbl <- dbGetQuery(Data, "SELECT * FROM key")
station <- dbGetQuery(Data, "SELECT * FROM station")

#checking one of the tables to see the structure
head(train)

#cleaning the data
#converting incorrect data type
#char to date
train$date   <- as.Date(train$date)
weather$date <- as.Date(weather$date)

#Section A: SQL Queries
#1. Write a query to find the top 3 products with the highest total sales (based on units 
# sold) from the train table. 

#Top 3 selling product from train table
top3 <- dbGetQuery(Data, "
  SELECT item_nbr, SUM(units) AS total_units
  FROM train
  GROUP BY item_nbr
  ORDER BY total_units DESC
  LIMIT 3;
")
top3

#2. Using the key table, write a query to join the sales data (train) with the correct 
#weather station using store_nbr.
#Joining table,train,key and weather. Correction: Selecting all from sales, key and weather to get a full join.
joined_data <- dbGetQuery(Data, "
  SELECT 
    t.*,
    k.*,
    w.*
  FROM train t
  JOIN key k
    ON t.store_nbr = k.store_nbr
  JOIN weather w
    ON k.station_nbr = w.station_nbr
   AND t.date = w.date
")

# To confirm the join was successful
head(joined_data)

#3. Write a query to return daily sales and average temperature (tavg) for one of the top 3 
#products. 
#daily sales and average temperature (tavg) for one of the top 3 products.
product_id <- top3$item_nbr[1]

daily_sales_weather <- dbGetQuery(Data, paste0("
  SELECT 
    t.date,
    t.units,
    w.tavg
  FROM train t
  JOIN key k
    ON t.store_nbr = k.store_nbr
  JOIN weather w
    ON k.station_nbr = w.station_nbr
   AND t.date = w.date
  WHERE t.item_nbr = ", product_id, "
"))


#Section B: R Programming
#1. Load the joined dataset into R using DBI and RSQLite; and convert date columns to Date 
#format
#Changing the date data type for the joined data
joined_data$date <- as.Date(joined_data$date)

#cleaning the joined data
#checking for missing values
colSums(is.na(joined_data))
summary(is.na(joined_data$tavg))
table(joined_data$station_nbr, is.na(joined_data$tavg))


#tmax,tmin,tavg,depart,dewpoint,wetbulb,heat,cool,sunrise,sunset,snowfall,preciptotal,stnpressure,sealevel,resultspeed,resultdir and avgspeed all have missing values
#Handling missing weather values (e.g., preciptotal, tavg).
#The joined data has two store_nbr, station_nbr and date column from sales and weather table. they both show two names so i am checking to see the correct one to drop
names(joined_data)

# Removing the second date, second store_nbr, and second station_nbr
joined_data <- joined_data[ , !duplicated(names(joined_data))]

#Checking the distinct value of station_nbr to be certain when imputing missing values for weather
unique(joined_data$station_nbr)

#Reconfirming if there is any weather column that needs to be changed to number before imputation of missing values
str(joined_data)
#tavg tmax, tmin are integer which means their median can be calculated.

#Fixing missing values
joined_data$tavg <- ifelse(
  is.na(joined_data$tavg),
  ave(joined_data$tavg, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$tavg
)

joined_data$tmax <- ifelse(
  is.na(joined_data$tmax),
  ave(joined_data$tmax, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$tmax
)

joined_data$tmin <- ifelse(
  is.na(joined_data$tmin),
  ave(joined_data$tmin, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$tmin
)

joined_data$depart <- ifelse(
  is.na(joined_data$depart),
  ave(joined_data$depart, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$depart
)

joined_data$dewpoint <- ifelse(
  is.na(joined_data$dewpoint),
  ave(joined_data$dewpoint, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$dewpoint
)

joined_data$wetbulb <- ifelse(
  is.na(joined_data$wetbulb),
  ave(joined_data$wetbulb, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$wetbulb
)

joined_data$heat <- ifelse(
  is.na(joined_data$heat),
  ave(joined_data$heat, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$heat
)

joined_data$cool <- ifelse(
  is.na(joined_data$cool),
  ave(joined_data$cool, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$cool
)
#Fixing T trace values before numeric conversion
joined_data$snowfall[joined_data$snowfall == "T"] <- 0.01
joined_data$preciptotal[joined_data$preciptotal == "T"] <- 0.01


#Converting snowfalland preciptotal datatype to number before imputting missing values
joined_data$snowfall <- as.numeric(joined_data$snowfall)
joined_data$preciptotal <- as.numeric(joined_data$preciptotal)

# snowfall missing value will be replaced with 0
joined_data$snowfall <- ifelse(is.na(joined_data$snowfall), 0, joined_data$snowfall)

joined_data$preciptotal <- ifelse(
  is.na(joined_data$preciptotal),
  ave(joined_data$preciptotal, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$preciptotal
)

joined_data$stnpressure <- ifelse(
  is.na(joined_data$stnpressure),
  ave(joined_data$stnpressure, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$stnpressure
)

joined_data$sealevel <- ifelse(
  is.na(joined_data$sealevel),
  ave(joined_data$sealevel, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$sealevel
)

joined_data$resultspeed <- ifelse(
  is.na(joined_data$resultspeed),
  ave(joined_data$resultspeed, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$resultspeed
)

joined_data$resultdir <- ifelse(
  is.na(joined_data$resultdir),
  ave(joined_data$resultdir, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$resultdir
)

joined_data$avgspeed <- ifelse(
  is.na(joined_data$avgspeed),
  ave(joined_data$avgspeed, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$avgspeed
)
joined_data$depart <- ifelse(
  is.na(joined_data$depart),
  ave(joined_data$depart, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$depart
)

joined_data$wetbulb <- ifelse(
  is.na(joined_data$wetbulb),
  ave(joined_data$wetbulb, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$wetbulb
)

joined_data$stnpressure <- ifelse(
  is.na(joined_data$stnpressure),
  ave(joined_data$stnpressure, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$stnpressure
)

joined_data$sealevel <- ifelse(
  is.na(joined_data$sealevel),
  ave(joined_data$sealevel, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$sealevel
)


## Normal median was used to fix missing value for sealevel,wetbulb and stnpressure becauses some station such as station 5 are all missing values so the median can't be used.
joined_data$sealevel <- ifelse(
  is.na(joined_data$sealevel),
  ave(joined_data$sealevel, joined_data$station_nbr, FUN = function(x) median(x, na.rm = TRUE)),
  joined_data$sealevel
)
joined_data$wetbulb[is.na(joined_data$wetbulb)] <- median(joined_data$wetbulb, na.rm = TRUE)
joined_data$stnpressure[is.na(joined_data$stnpressure)] <- median(joined_data$stnpressure, na.rm = TRUE)
joined_data$sealevel[is.na(joined_data$sealevel)] <- median(joined_data$sealevel, na.rm = TRUE)


#depart,sunrise and sunset missing values are extremely high so their columns are dropped
joined_data <- subset(joined_data, select = -c(sunrise, sunset, depart))


#Checking if missing values still exist
colSums(is.na(joined_data))



#Handling the outliers
##checking the distribution
summary(joined_data$units)
quantile(joined_data$units, c(0.99, 0.995, 0.999))

# The units variable contains extreme outliers (max = 5568), which can distort the model.
# To reduce their impact while keeping the overall distribution, I capped values above the
# 99.5th percentile (winsorizing). This preserves most data while preventing extreme spikes
# from dominating the regression.

upper_limit <- quantile(joined_data$units, 0.995)
joined_data$units[joined_data$units > upper_limit] <- upper_limit



#### 2. Task 4: Predictive Modelling ####
dir.create("plots", showWarnings = FALSE)

run_product_model <- function(product_id, product_name) {
  
  message(paste("Running model for", product_name))
  
  # Filter dataset
  df <- subset(joined_data, item_nbr == product_id)
  
  # Feature engineering
  df$is_weekend <- ifelse(weekdays(df$date) %in% c("Saturday", "Sunday"), 1, 0)
  df$is_rainy <- ifelse(df$preciptotal > 0, 1, 0)
  
  # Train-test-validation split
  set.seed(123)
  n <- nrow(df)
  train_idx <- sample(1:n, 0.6*n)
  remaining <- setdiff(1:n, train_idx)
  test_idx <- sample(remaining, 0.5*length(remaining))
  val_idx <- setdiff(remaining, test_idx)
  
  train <- df[train_idx, ]
  test  <- df[test_idx, ]
  val   <- df[val_idx, ]
  
  # Models
  model_lm <- lm(units ~ tavg + preciptotal + is_weekend + is_rainy, data = train)
  model_tree <- rpart(units ~ tavg + preciptotal + is_weekend + is_rainy, data = train)
  
  # Predictions
  test$pred_lm <- predict(model_lm, test)
  test$pred_tree <- predict(model_tree, test)
  
  val$pred_lm <- predict(model_lm, val)
  val$pred_tree <- predict(model_tree, val)
  
  # RMSE
  rmse_lm <- sqrt(mean((test$units - test$pred_lm)^2))
  rmse_tree <- sqrt(mean((test$units - test$pred_tree)^2))
  
  rmse_lm_val <- sqrt(mean((val$units - val$pred_lm)^2))
  rmse_tree_val <- sqrt(mean((val$units - val$pred_tree)^2))
  
  # R²
  r2 <- summary(model_lm)$r.squared
  
  # Save plots
  dir.create("plots", showWarnings = FALSE)
  
  png(paste0("plots/", product_name, "_temp_vs_sales.png"))
  plot(df$tavg, df$units, main = paste("Temperature vs Sales (", product_name, ")"),
       xlab = "Temperature", ylab = "Units Sold", pch = 16, col = "steelblue")
  dev.off()
  
  png(paste0("plots/", product_name, "_rain_boxplot.png"))
  boxplot(units ~ is_rainy, data = df, names = c("No Rain", "Rain"),
          main = paste("Rain vs No Rain (", product_name, ")"),
          col = c("lightgreen", "lightblue"))
  dev.off()
  
  png(paste0("plots/", product_name, "_weekend_boxplot.png"))
  boxplot(units ~ is_weekend, data = df, names = c("Weekday", "Weekend"),
          main = paste("Weekend vs Weekday (", product_name, ")"),
          col = c("lightblue", "darkblue"))
  dev.off()
  
  png(paste0("plots/", product_name, "_sales_over_time.png"))
  plot(df$date, df$units, type = "l",
       main = paste("Sales Over Time (", product_name, ")"),
       xlab = "Date", ylab = "Units Sold", col = "darkblue")
  dev.off()
  
  png(paste0("plots/", product_name, "_tree.png"))
  rpart.plot(model_tree, main = paste("Decision Tree for", product_name))
  dev.off()
  
  # Return results
  return(list(
    product = product_name,
    r2 = r2,
    rmse_lm = rmse_lm,
    rmse_tree = rmse_tree,
    rmse_lm_val = rmse_lm_val,
    rmse_tree_val = rmse_tree_val,
    model_lm = model_lm,
    model_tree = model_tree
  ))
  
}

result1 <- run_product_model(top3$item_nbr[1], "Product1")
result2 <- run_product_model(top3$item_nbr[2], "Product2")
result3 <- run_product_model(top3$item_nbr[3], "Product3")

print(result1)
print(result2)
print(result3)

print(result1$model_tree)
print(result2$model_tree)
print(result3$model_tree)


dbDisconnect(Data)
