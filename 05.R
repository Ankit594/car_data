library(dplyr)
library(ggplot2)
library(UsingR)
library(lubridate)
car_data <- read.csv(file = 'Jaypee/BA/Vehicle dataset from cardekho/car_data.csv')
head(car_data)
View(car_data)
dim(car_data)
sum(is.na(car_data))
car_data$Fuel_Type
table(car_data$Fuel_Type)
car_data$Seller_Type
table(car_data$Seller_Type)
car_data$Transmission
table(car_data$Transmission)
ggplot(data = car_data, aes(x=Selling_Price)) + geom_histogram(fill="green", col = "black")
ggplot(data = car_data , aes(x=Present_Price)) + geom_bar(fill = "orange")
ggplot(data = car_data, aes(x=Present_Price, na.rm = TRUE)) + geom_histogram(fill="red", col = "black")
plot(car_data$Present_Price, car_data$Selling_Price, xlab = "Kms_Driven", ylab = "Selling_Price")
df = subset(car_data,select = -C(Car_Name))
car_data$age = year(today())-car_data$Year
car_data$age
View(car_data)
data1 = sort(sample(nrow(car_data), nrow(car_data)*.75))
head(data1)

train<-car_data[data1,]
head(train)

test<-car_data[-data1,]
head(test)

car_data_lm <- lm(Selling_Price ~ Transmission + Present_Price + Kms_Driven + age + Fuel_Type + Owner, data = car_data)
car_data_lm
summary(car_data_lm)
Sprice = 4.461e+00 + -1.259e+00 * TransmissionManual +  4.680e-01 * Present_Price + -4.039e-01 * age

Sprice = 4.461e+00 + -1.259e+00 * 1 +  4.680e-01 * 6.87 + -4.039e-01 * 6
Sprice
