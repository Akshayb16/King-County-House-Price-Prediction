
# linear regression

house <- read.csv('kc_house_data.csv')
View(house)

install.packages("lubridate")           #to convert char to date 
library("lubridate") 

length(unique(house$id))
set.seed(42)
house$date<-gsub("T000000","",as.character(house$date)) #to remove T000000 from date column
house$date
house$date<- ymd(house$date)
house$date
View(house)

house$id = NULL
house$date = NULL
#run correlation analysis among all variables in the dataset
library(caTools)

split1 = sample.split(house$price, SplitRatio = 0.9)
house_train = subset(house, split1 == TRUE)
house_test = subset(house, split1 == FALSE)
lmregtest = lm(price ~ ., data = house_train)
summary(lmregtest)

cor(house)

newhouse= house
newhouse$grade = NULL
#newhouse$sqft_living15 = NULL
#newhouse$sqft_lot15 = NULL
newhouse$lat = NULL
newhouse$long = NULL
#newhouse$zipcode = NULL
####
#newhouse$sqft_basement = NULL
newhouse$yr_built = NULL
#newhouse$sqft_above = NULL
#newhouse$bedrooms = NULL
#newhouse$bathrooms = NULL
#newhouse$sqft_lot = NULL

split = sample.split(newhouse$price, SplitRatio = 0.7)
newhouse_train = subset(newhouse, split == TRUE)
newhouse_test = subset(newhouse, split == FALSE)
lmreg = lm(price ~ ., data = newhouse_train)
summary(lmreg)

lmpredict = predict(lmreg, newdata = newhouse_test)
head(lmpredict)
SSE_lm = sum((newhouse_test$price - lmpredict)^2)
SST_lm = sum((newhouse_test$price - mean(newhouse_test$price))^2)
R_Sqr_lm = 1-(SSE_lm/SST_lm)
R_Sqr_lm

