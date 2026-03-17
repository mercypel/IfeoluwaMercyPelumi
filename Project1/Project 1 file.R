install.packages("DBI")
install.packages("RSQLite")
library(DBI)
library(RSQLite)
getwd()

#connecting the data
Data <- dbConnect(RSQLite::SQLite(), "project1_raw_data.db")
dbListTables(con)

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

#Top 3 selling product from train table
top3 <- dbGetQuery(Data, "
  SELECT item_nbr, SUM(units) AS total_units
  FROM train
  GROUP BY item_nbr
  ORDER BY total_units DESC
  LIMIT 3;
")

top3

#Joining table,train,key and weather
joined_data <- dbGetQuery(Data, "
  SELECT 
    t.date,
    t.store_nbr,
    t.item_nbr,
    t.units,
    w.tavg,
    w.preciptotal,
    w.codesum
  FROM train t
  JOIN key k
    ON t.store_nbr = k.store_nbr
  JOIN weather w
    ON k.station_nbr = w.station_nbr
   AND t.date = w.date
")

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

#Loading the joined dataset
joined_data <- dbGetQuery(Data, "
  SELECT 
    t.date,
    t.store_nbr,
    t.item_nbr,
    t.units,
    w.tavg,
    w.preciptotal,
    w.codesum
  FROM train t
  JOIN key k
    ON t.store_nbr = k.store_nbr
  JOIN weather w
    ON k.station_nbr = w.station_nbr
   AND t.date = w.date
")

#Changing the date data type for the joined data
joined_data$date <- as.Date(joined_data$date)

#cleaning the joined data
#checking for missing values
colSums(is.na(joined_data))
# Converting "T" (trace) to 0.01
joined_data$preciptotal[joined_data$preciptotal == "T"] <- "0.01"

# Converting empty strings to NA
joined_data$preciptotal[joined_data$preciptotal == ""] <- NA

#Using the median of the available values for tavg and preciptotal to fill their missing values
joined_data$preciptotal[is.na(joined_data$preciptotal)] <- 
  median(joined_data$preciptotal, na.rm = TRUE)

joined_data$tavg[is.na(joined_data$tavg)] <- median(joined_data$tavg, na.rm = TRUE)


# Converting preciptotal to numeric to ensure it is still numeric after cleaning
joined_data$preciptotal <- as.numeric(joined_data$preciptotal)

joined_data$tavg <- as.numeric(joined_data$tavg)

#Checking if there is still missing value
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

#filtering the dataset to one of the top 3 product since i want to model just one product
#This is product 1
product_id <- top3$item_nbr[1]   
df <- subset(joined_data, item_nbr == product_id)


#1. I creating two features to capture how temperature, rainfall, and weekend shopping behaviour may influence daily product sales.
# create is_weekend
joined_data$is_weekend <- ifelse(weekdays(joined_data$date) %in% c("Saturday", "Sunday"), 1, 0)
# create is_raining
joined_data$is_rainy <- ifelse(joined_data$preciptotal > 0, 1, 0)

# 2. Recreating df AFTER creating features
product_id <- top3$item_nbr[1]
df <- subset(joined_data, item_nbr == product_id)

#3. Build Model
#Building Linear Regression Model
model_lm <- lm(units ~ tavg + preciptotal + is_weekend + is_rainy, data = df)
summary(model_lm)


#Building the decision tree model
library(rpart)

model_tree <- rpart(units ~ tavg + preciptotal + is_weekend + is_rainy, data = df)
model_tree

#Ploting the tree
plot(model_tree)
text(model_tree)

#COMPARING THE TWO MODEL
df$pred_lm <- predict(model_lm, df)
df$pred_tree <- predict(model_tree, df)

lm_rmse <- sqrt(mean((df$units - df$pred_lm)^2))
tree_rmse <- sqrt(mean((df$units - df$pred_tree)^2))

lm_rmse
tree_rmse

#Scatter plot: Temperature Vs Sales
plot(df$tavg, df$units,
     main = "Temperature vs Sales",
     xlab = "Average Temperature (°F)",
     ylab = "Units Sold",
     pch = 16, col = "steelblue")

#Boxplot: Sales on Rainy vs Non‑Rainy Days
boxplot(units ~ is_rainy, data = df,
        names = c("No Rain", "Rain"),
        main = "Sales Distribution: Rainy vs Non‑Rainy Days",
        ylab = "Units Sold",
        col = c("lightgreen", "lightblue"))

#Boxplot: Weekend vs Weekday Sales
boxplot(units ~ is_weekend, data = df,
        names = c("Weekday", "Weekend"),
        main = "Sales on Weekdays vs Weekends",
        ylab = "Units Sold",
        col = c("lightblue", "darkblue"))

# Line Plot Over Time
plot(df$date, df$units, type = "l",
     main = "Sales Over Time",
     xlab = "Date",
     ylab = "Units Sold",
     col = "darkblue")


#For product 2
product_id2 <- top3$item_nbr[2]
df2 <- subset(joined_data, item_nbr == product_id2)

model_lm2 <- lm(units ~ tavg + preciptotal + is_weekend + is_rainy, data = df2)
summary(model_lm2)

library(rpart)
model_tree2 <- rpart(units ~ tavg + preciptotal + is_weekend + is_rainy, data = df2)
model_tree2

#Computing RMSE FOR PRODUCT 2
df2$pred_lm <- predict(model_lm2, df2)
df2$pred_tree <- predict(model_tree2, df2)

lm_rmse2 <- sqrt(mean((df2$units - df2$pred_lm)^2))
tree_rmse2 <- sqrt(mean((df2$units - df2$pred_tree)^2))

lm_rmse2
tree_rmse2

#FOR PRODUCT 3
product_id3 <- top3$item_nbr[3]
df3 <- subset(joined_data, item_nbr == product_id3)

model_lm3 <- lm(units ~ tavg + preciptotal + is_weekend + is_rainy, data = df3)
summary(model_lm3)

model_tree3 <- rpart(units ~ tavg + preciptotal + is_weekend + is_rainy, data = df3)
model_tree3

#COMPUTING RMSE FOR PRODUCT 3
df3$pred_lm <- predict(model_lm3, df3)
df3$pred_tree <- predict(model_tree3, df3)

lm_rmse3 <- sqrt(mean((df3$units - df3$pred_lm)^2))
tree_rmse3 <- sqrt(mean((df3$units - df3$pred_tree)^2))

lm_rmse3
tree_rmse3
