# This script has examples from Week 7

data()

indata <- airquality

dim(indata)

head(indata)

tail(indata)

is.data.frame(indata)

plot(indata$Ozone, indata$Solar.R)

# Data Quality checks

cleaned_data <- na.omit(indata)

dim(cleaned_data)

print(paste("Number of rows dropped due to missing values: ", dim(indata)[1] - dim(cleaned_data)[1]))

# Check for duplicates
sum(duplicated(cleaned_data))

# Descriptive Statistics

summary(cleaned_data)

cleaned_data$cMonth = as.factor(cleaned_data$Month)


