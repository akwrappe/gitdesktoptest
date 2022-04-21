library(tidyverse)

indata <- airquality
cleaned_data <- na.omit(indata)
cleaned_data$cMonth = as.factor(cleaned_data$Month)

cleaned_data <- as_tibble(cleaned_data)

is_tibble(indata)

# use ggplot
ggplot(data = cleaned_data, mapping = aes(x= Ozone, y = Solar.R)) + geom_point() + geom_smooth()

ggplot(data = cleaned_data) + geom_histogram(mapping = aes(x= Solar.R), binwidth = 35) 

ggplot(data = cleaned_data, aes(Solar.R)) + facet_wrap(~cMonth, scales = 'free_x') + geom_histogram(binwidth = function(x) 2*IQR(x)/ length(x)^(1/3))

# filter

cleaned_data %>% filter(Month %in% c(5, 7)) %>% ggplot() +geom_histogram(mapping = aes(Solar.R), binwidth = function(x) 2*IQR(x)/ length(x)^(1/3))

cleaned_data %>% head()

# arrange

cleaned_data %>% arrange(Month, desc(Day)) %>% head()

# select 

cleaned_data %>% select(Solar.R, Temp, Month, Day) %>% head()

# group_by

cleaned_data %>% group_by(cMonth) %>% summarise( count=n(), avgTemp = mean(Temp), avgWind = mean(Wind), avgSolarR = mean(Solar.R))