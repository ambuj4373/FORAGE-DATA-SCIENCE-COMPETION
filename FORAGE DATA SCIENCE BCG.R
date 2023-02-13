str(client_data)
str(price_data)
summary(client_data)
summary(price_data)

#converting columns V3 to V26 into numeric datatype

client_data[, 3:26] <- apply(client_data[, 3:26], 2, as.numeric)
summary(client_data)

price_data$price_date<- as.Date(price_data$price_date)
summary(price_data)

# Create new data frame that only contains the necessary variables
price_sensitivity_data <- price_data[,c("id", "price_date", "price_off_peak_var", "price_peak_var")]

# Create a variable that represents the total monthly energy consumption by multiplying the energy consumed during each hour by its corresponding price
price_sensitivity_data$total_monthly_consumption_off_peak <- price_sensitivity_data$price_off_peak_var * price_sensitivity_data$price_off_peak_var
price_sensitivity_data$total_monthly_consumption_peak <- price_sensitivity_data$price_peak_var * price_sensitivity_data$price_peak_var

# Group the data by customer id and calculate the average monthly energy consumption during peak and off-peak hours
price_sensitivity_data <- aggregate(cbind(total_monthly_consumption_off_peak, total_monthly_consumption_peak) ~ id, data = price_sensitivity_data, FUN = mean)

# Calculate price sensitivity by dividing the average monthly energy consumption during peak hours by the average monthly energy consumption during off-peak hours
price_sensitivity_data$price_sensitivity <- price_sensitivity_data$total_monthly_consumption_peak / price_sensitivity_data$total_monthly_consumption_off_peak

# Merge the price sensitivity data with the client data
client_data_with_price_sensitivity <- merge(client_data, price_sensitivity_data, by = "id", all.x = TRUE)

# Check for any missing values in the price sensitivity variable
sum(is.na(client_data_with_price_sensitivity$price_sensitivity))

# Create a scatter plot to visualize the relationship between price sensitivity and churn
plot(client_data_with_price_sensitivity$price_sensitivity, client_data_with_price_sensitivity$churn)

client_data_with_price_sensitivity_complete <- na.omit(client_data_with_price_sensitivity) #removing 20 na (missing values)

# Calculate the Pearson's correlation coefficient between price sensitivity and churn
cor(client_data_with_price_sensitivity_complete$price_sensitivity, client_data_with_price_sensitivity_complete$churn)


#The correlation coefficient between price sensitivity and churn is 0.04038787. This value is close to 0, which means that there is little to no linear relationship between the two variables. 
