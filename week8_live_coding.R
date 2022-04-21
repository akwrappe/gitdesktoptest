# Include Library
library(tidyverse)

# Make sure the directory path is set to working directory

# Import data
indata <- read.csv("FertilityRates.csv")

# Basic checks
dim(indata)
head(indata)

# Is it a data frame or a tibble?
is.data.frame(indata)
is_tibble(indata)
# If not tibble, convert into tibble
indata_tibble <- as_tibble(indata)

is.tibble(indata_tibble)
# Summary of the data

indata_tibble
summary(indata_tibble)

# Make Factors

indata_tibble$Country.Name = factor(indata_tibble$Country.Name)
# Clean the data by removing redundant columns

indata_cleaned <- select(indata_tibble, -(Country.Code:Indicator.Code))
# Checks
summary(indata_cleaned)


# Pivot to long dataset
indata_pivoted <- pivot_longer(indata_cleaned, c(str_c("X", c(1960:2011))), names_to = "Year", values_to = "Fertility.Rates")


indata_pivoted$Year <- as.integer(str_sub(indata_pivoted$Year, 2, 5))


# Missing Values
sum(is.na(indata_pivoted$Fertility.Rates))

# Check missing values by country
## Step 1: Filter by missing values
## Step 2: Group by country
## Step 3: Count number of values
indata_pivoted %>% filter(is.na(Fertility.Rates)) %>% group_by(Country.Name) %>% summarise(count = n())

# Fill the missing values within countries
indata_filled <- indata_pivoted %>% group_by(Country.Name) %>% fill(Fertility.Rates, .direction="downup") %>% ungroup()

#Check if still missing
indata_filled %>% filter(is.na(Fertility.Rates)) %>% group_by(Country.Name) %>% summarise(count = n())
# drop the missing values 

indata_filled <- indata_filled %>% filter(!is.na(Fertility.Rates))

##### Dropped 9 countries

ggplot(data = indata_filled) + geom_point(mapping = aes(x= Year, y = Fertility.Rates), position = "jitter", alpha = 0.1) + geom_smooth(mapping = aes(x= Year, y = Fertility.Rates)) + labs(x = "Year", y = "Fertility rates", title = "Fertility rates over the years" , subtitle = "Global fertility rates have decreased since 1960") 

# Need a way to categorize 210 countries

## Option 1: Subset the data
#indata_subset <- filter(indata_tibble, Country.Name %in% c("United States", "Mexico", "Canada" ))

## Option 2: Select the top or bottom countries by a statistic
# Top 10
#indata_pivoted %>% group_by(Country.Name) %>% summarise(avg = mean(Fertility.Rates)) %>% arrange(desc(avg)) %>% print(n= 10)

# Bottom 10
#indata_pivoted %>% group_by(Country.Name) %>% summarise(avg = mean(Fertility.Rates))  %>% arrange(desc(avg))  %>% top_n(-10)

# can put that subset data into a tibble and plot as shown.

## Option 3: Get Continent or region mapping
library(countrycode)

temp <- as.data.frame(indata_filled)
temp$Continent.Name <- factor(countrycode(sourcevar = temp[,"Country.Name"], origin="country.name", destination = "continent"))

temp$Region.Name <- factor(countrycode(sourcevar = temp[,"Country.Name"], origin="country.name", destination = "region"))

indata_tibble <- as.tibble(temp)



# Color by Continent
ggplot(data = indata_tibble) + geom_point(mapping = aes(x= Year, y = Fertility.Rates, color=Continent.Name), position = "jitter", alpha = 0.1) + geom_smooth(mapping = aes(x= Year, y = Fertility.Rates)) + labs(x = "Year", y = "Fertility rates", title = "Fertility rates over the years" , subtitle = "Global fertility rates have decreased since 1960") 


# Facet plotting
ggplot(data = indata_tibble) + geom_point(mapping = aes(x= Year, y = Fertility.Rates, color=Country.Name), position = "jitter", show.legend=FALSE) + facet_wrap(~ Region.Name) 


# Filtering and plotting
indata_tibble %>% filter(Region.Name == "Middle East & North Africa" &  Continent.Name=="Asia") %>% ggplot() + geom_line(mapping = aes(x=Year, y= Fertility.Rates, color = Country.Name), size = 2, linetype = 2) 


# Boxplot
ggplot(data= indata_tibble, mapping=aes(x= Continent.Name, y = Fertility.Rates)) + geom_boxplot() + coord_flip()

# Histogram
ggplot(data= indata_tibble) + geom_histogram(mapping = aes(x= Fertility.Rates), binwidth = 0.5) 
# Bar Chart
indata_tibble %>% group_by(Region.Name) %>% summarise(avg = mean(Fertility.Rates)) %>% ggplot(mapping =  aes(x= reorder(Region.Name, -avg), y= avg, fill = Region.Name)) + geom_bar(stat="identity") + labs(x = "Region" , y= "Average Fertility rate ", fill="Region") +coord_flip()

