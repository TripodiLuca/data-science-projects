### 17 - Final Project - Real Estate Texas ###


# Packages
if (!require(here)) {
  install.packages("here") # to get current path
}
library(here)

if (!require(DescTools)) {
  install.packages("DescTools") # for Mode function
}
library(DescTools)

if (!require(moments)) {
  install.packages("moments") # for shape indexes functions
}
library(moments)

if (!require(ggplot2)) {
  install.packages("ggplot2") # for plots
}
library(ggplot2)

if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)



# Functions
gini.index_norm <- function(x) {
  ni <- table(x)       # absolute frequencies distribution
  fi <- ni/length(x)   # relative frequencies distribution
  fi2 = fi^2
  J = length(table(x)) # number of modalities
  
  gini_index = 1 - sum(fi2)               # Gini index
  gini_index_norm = gini_index/((J-1)/J)  # normalized Gini index
  
  return(gini_index_norm)
}


# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



##### 
# 1 #
#####
# Import the csv file
data_real_estate_texas <- read.csv("real_estate_texas.csv", header = TRUE, sep = ',')

# Inspect the header
head(data_real_estate_texas, 5)

#####
# 2 #
#####

# "city": categorical variable on nominal scale
## city

# "year": categorical variable on ordinal scale
## reference year

# "month": categorical variable on ordinal scale (note: order is circular)
## reference month

# "sales": numeric discrete variable
## total number of sales

# "volume": numeric continuous variable
## total value of sales expressed in millions of dollars

# "median_price": numeric continuous variable
## median price expressed in dollars

# "listings": numeric discrete variable
## total number of active sale advertisements

# "months_inventory": numeric continuous variable
## amount of time expressed in months required to sell all current listings at current sales rate




#####
# 3 #
#####
### Measures of Central Tendency ###
# Mode
# Minimum
# Maximum
# Median
# Mean
# Quartiles

### Measures of Dispersion ###
# Range
# Interquartile range
# Variance
# Standard deviation
# Coefficient of variation
# Gini index normalized


# Number of observations
N = dim(data_real_estate_texas)[1]

# Conversion for categorical variables
data_real_estate_texas$city <- as.factor(data_real_estate_texas$city)
data_real_estate_texas$year <- as.factor(data_real_estate_texas$year)
data_real_estate_texas$month <- as.factor(data_real_estate_texas$month)

# Extract variables names
attach(data_real_estate_texas)

# Summaries, Dispersion Indexes and Shapes
for (variable_name in names(data_real_estate_texas)) {
  variable_real_estate_texas = data_real_estate_texas[[variable_name]]
  cat("Summary for", variable_name,":\n")
  print(summary(variable_real_estate_texas))
  
  if (is.numeric(variable_real_estate_texas)) {
    cat("\nDispersion Indexes:\n")
    cat("- Variance:", round(var(variable_real_estate_texas), 2), "\n")
    cat("- Standard Deviation:", round(sd(variable_real_estate_texas), 2), "\n")
    cat("- Coefficient of Variation:", round(sd(variable_real_estate_texas)/mean(variable_real_estate_texas)*100, 2), "\n")
    cat("- Normalized Gini Index:", round(gini.index_norm(variable_real_estate_texas), 2), "\n")
    cat("- Fisher Index:", round(skewness(variable_real_estate_texas), 2), "\n")
    cat("- Kurtosis Index:", round(kurtosis(variable_real_estate_texas), 2), "\n")
  }
  cat("\n************************************************\n")
}




#####
# 4 #
#####
### Evaluate the variability ##

# Show standard deviation for numerical variables
variable_SD <- c("sales_SD", "volume_SD", "median_price_SD", "listings_SD", "months_inventory_SD")

for (variable_name in variable_SD) {
  variable_value <- round(eval(parse(text = variable_name)), 2)
  cat(variable_name, " = ", variable_value, "\n", sep = "")
}

# Show Coefficients of variation for numerical variables
variable_CV <- c("sales_CV", "volume_CV", "median_price_CV", "listings_CV", "months_inventory_CV")

for (variable_name in variable_CV) {
  variable_value <- round(eval(parse(text = variable_name)), 2)
  cat(variable_name, " = ", variable_value, "\n", sep = "")
}

# Note: since the domains of numeric variables consist only of positive
# values, it is meaningful to assess the coefficients of variation.
# In absolute terms, the "median_price" variable shows highest deviation,
# but when considering the coefficient of variation, the "volume" variable is
# the one with the highest relative variability.



## Evaluate the shape of numerical variables ##

# Fisher indexes
sales_skew = skewness(sales)  # 0.718
volume_skew = skewness(volume)  # 0.885
median_price_skew = skewness(median_price)  # -0.364
listings_skew = skewness(listings)  # 0.649
months_inventory_skew = skewness(months_inventory)  # 0.041

# Summary
summary(sales)
summary(volume)
summary(median_price)
summary(listings)
summary(months_inventory)

# Note: evaluating the Fisher indexes for all variables, it is observed that
# the "volume" variable exhibits the highest absolute value, indicating the
# greatest asymmetry among the variables. The signs of Fisher indexes were
# predicted by comparing the mean and median values: variables with means
# higher than their medians show positive asymmetry, so with distributions
# skewed to the left; "median_price" variable is the only one characterized by
# a negative index and so skewness to the right


# Z score standardization
sales_zscore <- (sales - mean(sales))/sd(sales)
volume_zscore <- (volume - mean(volume))/sd(volume)
median_price_zscore <- (median_price - mean(median_price))/sd(median_price)
listings_zscore <- (listings - mean(listings))/sd(listings)
months_inventory_zscore <- (months_inventory - mean(months_inventory))/sd(months_inventory)

# Density plots
ggplot(data_real_estate_texas) +
  geom_density(aes(x = sales_zscore, color = "Sales"), fill = "lightblue", alpha = 0.5) +
  geom_density(aes(x = volume_zscore, color = "Volume"), fill = "lightgreen", alpha = 0.5) +
  geom_density(aes(x = median_price_zscore, color = "Median Price"), fill = "pink", alpha = 0.5) +
  geom_density(aes(x = listings_zscore, color = "Listings"), fill = "khaki", alpha = 0.5) +
  geom_density(aes(x = months_inventory_zscore, color = "Months Inventory"), fill = "slategray4", alpha = 0.5) +
  scale_color_manual(name = NULL, values = c("Sales" = "blue", "Volume" = "green", "Median Price" = "red", "Listings" = "yellow", "Months Inventory" = "gray")) +
  labs(
    title = "Shapes Comparison of Normalized Distributions",
    x = "Normalized Variability",
    y = "Normalized Frequency")



### Kurtosis ###
sales_kurtosis = kurtosis(sales) - 3  # -0.313
volume_kurtosis = kurtosis(volume) - 3  # 0.177
median_price_kurtosis = kurtosis(median_price) - 3  # -0.623
listings_kurtosis = kurtosis(listings) - 3  # -0.792
months_inventory_kurtosis = kurtosis(months_inventory) - 3  # -0.174

#####
# 5 #
#####
### Classes Division ###
summary(sales)

# Absolute frequency distribution
class_step = 100
class_max_val = ceiling(max(sales)/class_step)*class_step
sales_class <- cut(sales, breaks = seq(0, class_max_val, class_step)) # class is rounded to next class step so not to exclude any values
sales_class_max_val = max(as.vector(table(sales_class)))

ggplot(data = data_real_estate_texas, aes(x = sales_class)) +
  geom_bar(fill = "lightgreen", color = "green4")  +
  geom_text(stat = 'count', aes(label = after_stat(..count..)),
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "Absolute Frequency Distribution of Sales Classes",
       x = "Sales Classes",
       y = "Absolute Frequency") +
  scale_y_continuous(limits = c(0, 1.05*sales_class_max_val)) +
  theme_minimal()

# Relative frequency distribution
sales_class_fi <- as.vector(table(sales_class)/N*100)
sales_class_level = levels(sales_class)
sales_df_class_fi <- data.frame(cbind(sales_class_level, sales_class_fi))

ggplot(data = sales_df_class_fi, aes(x = sales_class_level,
                                     y = as.numeric(sales_class_fi))) +
  labs(
    title = "Relative Frequency Distribution of Sales Classes",
    x = "Sales Classes",
    y = "Relative Frequency [%]") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "green4") +
  geom_text(aes(label = paste(round(as.numeric(sales_class_fi)), "%", sep = "")),
            vjust = -0.5, size = 3, color = "black") +
  theme_minimal()
  
# Cumulative frequency distribution
sales_class_Ni <- cumsum(as.vector(table(sales_class)))
sales_df_class_Ni <- data.frame(cbind(as.factor(sales_class_level), sales_class_Ni))

ggplot(data = sales_df_class_Ni, aes(x = sales_class_level, y = sales_class_Ni)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "green4") +
  geom_text(aes(label = sales_class_Ni), vjust = -0.5, size = 3, color = "black") +
  labs(
    title = "Cumulative Frequency Distribution of Sales Classes",
    x = "Sales Classes",
    y = "Cumulative Frequency"
  ) +
  scale_y_continuous(limits = c(0, 1.05*N)) +
  theme_minimal()

# Cumulative relative frequency distribution
sales_class_Fi <- as.vector(sales_class_Ni/N*100)
sales_class_level = levels(sales_class)
sales_df_class_Fi <- data.frame(cbind(sales_class_level, sales_class_Fi))

ggplot(data = sales_df_class_Fi, aes(x = sales_class_level,
                                     y = as.numeric(sales_class_Fi))) +
  labs(
    title = "Cumulative Relative Frequency Distribution of Sales Classes",
    x = "Sales Classes",
    y = "Cumulative Relative Frequency [%]") +
  scale_y_continuous(limits = c(0, 105)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "green4") +
  geom_text(aes(label = paste(round(as.numeric(sales_class_Fi)), "%", sep = "")),
            vjust = -0.5, size = 3, color = "black") +
  theme_minimal()




#####
# 7 #
#####
### Probability ###

# P(city == Beaumont)
sum(city == "Beaumont")/N*100 # 25%

# P(month == 7)
sum(month == 7)/N*100 # 8.33%

# P(month == 12 & year == 2012)
sum(year == 2012 & month == 12)/N*100 # 1,67




#####
# 8 #
#####
### Mean price ###
mean_price <- round((volume*10e6)/sales)  # multiplied to 10e6 to obtain the price in $ as the same order of median price
data_real_estate_texas$mean_price <- mean_price




#####
# 9 #
#####
### Sales efficiency ###

# The efficiency of the listings could be measured by the ratio
# of the number of sales to the number of listings
listings_efficiency <- round(sales/listings*100)
data_real_estate_texas$listings_efficiency <- listings_efficiency




######
# 10 #
######
### Summaries - Mean and standard deviation ###

# Mean and standard deviation for sales, volume and listings grouping by city
data_summary_city <- data_real_estate_texas %>%
  group_by(city) %>%
  summarise(
    sales_mean = round(mean(sales)),
    volume_mean = round(mean(volume), 3),
    listings_mean = round(mean(listings)),
    sales_SD = round(sd(sales)),
    volume_SD = round(sd(volume), 3),
    listings_SD = round(sd(listings))
  )


# Mean and standard deviation for sales, volume and listings grouping by year
data_summary_year <- data_real_estate_texas %>%
  group_by(year) %>%
  summarise(
    sales_mean = round(mean(sales)),
    volume_mean = round(mean(volume), 3),
    listings_mean = round(mean(listings)),
    sales_SD = round(sd(sales)),
    volume_SD = round(sd(volume), 3),
    listings_SD = round(sd(listings))
  )

# Mean and standard deviation for sales, volume and listings grouping by month
data_summary_month <- data_real_estate_texas %>%
  group_by(month) %>%
  summarise(
    sales_mean = round(mean(sales)),
    volume_mean = round(mean(volume), 3),
    listings_mean = round(mean(listings)),
    sales_SD = round(sd(sales)),
    volume_SD = round(sd(volume), 3),
    listings_SD = round(sd(listings))
  )


########
# 10.1 #
########
### Boxplots ###

# Median prices by cities
ggplot(data = data_real_estate_texas) +
  geom_boxplot(aes(x = city, y = median_price), fill = "lightblue", color = "lightblue3", alpha = 0.7, outlier.shape = 7) +
  geom_jitter(aes(x = city, y = median_price), color = "red", alpha = 0.5, width = 0.1, size = 1) +
  labs(
    title = "Distribution of Median Prices by Cities",
    x = "Cities",
    y = "Price [$]"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
# Note: cities show varying median prices, while the distributions display
# relatively similar dispersion (a bit higher for Wichita Falls)

# Median prices by years
ggplot(data = data_real_estate_texas) +
  geom_boxplot(aes(x = as.factor(year), y = median_price), fill = "lightblue", color = "lightblue3", alpha = 0.7, outlier.shape = 7) +
  geom_jitter(aes(x = as.factor(year), y = median_price), color = "red", alpha = 0.5, width = 0.1, size = 1) +
  labs(
    title = "Distribution of Median Prices by Years",
    x = "Years",
    y = "Price [$]"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
# Note: comparing by years, the median prices appear more scattered, 
# reflecting the different position indices of prices across cities

# Median prices by months
ggplot(data = data_real_estate_texas) +
  geom_boxplot(aes(x = as.factor(month), y = median_price), fill = "lightblue", color = "lightblue3", alpha = 0.7, outlier.shape = 7) +
  geom_jitter(aes(x = as.factor(month), y = median_price), color = "red", alpha = 0.5, width = 0.1, size = 1) +
  labs(
    title = "Distribution of Median Prices by Months",
    x = "Months",
    y = "Price [$]"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
# Note: with finer division into months, the median prices appear to have
# a slightly increased variability, probably due to the reduced sample
# sizes for each month




########
# 10.2 #
########
# Total volume by cities
ggplot(data = data_real_estate_texas) +
  geom_boxplot(aes(x = city, y = volume), fill = "lightblue", color = "lightblue3", alpha = 0.7, outlier.shape = 7) +
  geom_jitter(aes(x = city, y = volume), color = "red", alpha = 0.5, width = 0.1, size = 1) +
  labs(
    title = "Distribution of Volumes by Cities",
    x = "Cities",
    y = "Price [M$]"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
# Note: comparing volume distributions across cities, it is observed a
# reduced variability in Beaumont and even narrower variability in Wichita Falls.

# Total volume by years
ggplot(data = data_real_estate_texas) +
  geom_boxplot(aes(x = as.factor(year), y = volume), fill = "lightblue", color = "lightblue3", alpha = 0.7, outlier.shape = 7) +
  geom_jitter(aes(x = as.factor(year), y = volume), color = "red", alpha = 0.5, width = 0.1, size = 1) +
  labs(
    title = "Distribution of Volumes by Years",
    x = "Years",
    y = "Price [M$]"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
# Note: volume distributions across the years seem to slightly
# increase in terms of dispersion as the years pass

# Total volume by months
ggplot(data = data_real_estate_texas) +
  geom_boxplot(aes(x = as.factor(month), y = volume), fill = "lightblue", color = "lightblue3", alpha = 0.7, outlier.shape = 7) +
  geom_jitter(aes(x = as.factor(month), y = volume), color = "red", alpha = 0.5, width = 0.1, size = 1) +
  labs(
    title = "Distribution of Volumes by Months",
    x = "Months",
    y = "Price [M$]"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
# Note: volume distributions across the months seem to have an higher
# dispersion when the the median price is higher




########
# 10.3 #
########
### Stack Bar Plots ###

## Sales for each month for all cities ##
data_summary_monthly_sales_for_city <- data_real_estate_texas %>%
  group_by(city, month) %>%
  summarise(sales_sum = sum(sales))

# Total sales for each month
data_monthly_sales_sum <- data_summary_monthly_sales_for_city %>%
  group_by(month) %>%
  summarise(sales_total = sum(sales_sum))

ggplot(data = data_summary_monthly_sales_for_city,
       aes(x = as.factor(month), y = sales_sum, fill = city)) +
  geom_col(position = "stack") +
  geom_text(data = data_monthly_sales_sum,
            aes(x = as.factor(month), y = sales_total, label = sales_total, fill = NULL),
            position = position_stack(vjust = 1.05), size = 4) +
  labs(
    title = "Total Monthly Sales for All Cities",
    x = "Months",
    y = "Sales"
  ) +
  theme_minimal()
# Note: The total monthly sales data shows a clear upward trend
# in the number of sales, starting from the beginning of the year and
# reaching its peak in June. Subsequently, there is a gradual decline
# until November. This pattern is particularly noticeable in 
# Bryan-College Station and Tyler, the two cities with the highest 
# absolute number of sales compared to the others



## Total sales for each month split by year ##
data_monthly_sales_sum_by_year <- data_real_estate_texas %>%
  group_by(year, month) %>%
  summarise(sales_monthly_total = sum(sales))

ggplot(data = data_real_estate_texas,
       aes(x = as.factor(month), y = sales, fill = city)) +
  geom_col(position = "stack") +
  geom_text(data = data_monthly_sales_sum_by_year,
            aes(x = as.factor(month), y = sales_monthly_total, label = sales_monthly_total, fill = NULL),
            position = position_stack(vjust = 1.15), size = 4) +
  labs(
    title = "Total Monthly Sales for All City by Years",
    x = "Months",
    y = "Sales"
  ) +
  theme_minimal() +
  facet_wrap(~ year, ncol = 1)
# Note: splitting by year, the same trend curves are observed, but the maximum
# sales peak seems falling in a different month each year

  
## Normalized total sales for each month ##

# Append total sales for each month and add normalized sum
data_summary_monthly_sales_for_city <- data_summary_monthly_sales_for_city %>%
  left_join(data_monthly_sales_sum, by = "month")

data_summary_monthly_sales_for_city <- data_summary_monthly_sales_for_city %>%
  mutate(sales_sum_normalized = (sales_sum / sales_total)*100)

ggplot(data = data_summary_monthly_sales_for_city,
       aes(x = as.factor(month), y = sales_sum_normalized, fill = city)) +
  geom_col(position = "stack") +
  labs(
    title = "Normalized Total Monthly Sales for All Cities",
    x = "Months",
    y = "Sales [%]"
  ) +
  theme_minimal()




########
# 10.4 #
########
### Line Chart ###

listings_plot <- ggplot(data = data_real_estate_texas,
                        aes(x = as.Date(paste(year, month, "01", sep = "-")), y = listings, group = city, color = city)) +
  geom_line() +
  labs(
    title = "Listings Trends Over Years",
    x = "Year",
    y = "Listings"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal() +
  theme(legend.position = "bottom")

for(year in 2010:2014) {
  listings_plot <- listings_plot +
  geom_vline(xintercept = as.Date(paste(year, "01", "01", sep = "-")), linetype = "solid", color = "lightcyan3")
  for(month in 2:12) {
    listings_plot <- listings_plot +
      geom_vline(xintercept = as.Date(paste(year, month, "01", sep = "-")), linetype = "dotted", color = "grey")
  }
}

listings_plot <- listings_plot +
  scale_color_manual(values = c("Beaumont" = "#F8766D",
                                "Bryan-College Station" = "#7CAE00",
                                "Tyler" = "#00BFC4",
                                "Wichita Falls" = "#C77CFF"))

print(listings_plot)
# Note: similarly to sales, the listings trend exhibits an annual periodicity,
# with a rising trend from the beginning of the year until mid-year, followed by
# a decrease towards year-end. This pattern is even more pronounced in Tyler city,
# which has a significantly higher number of advertisements compared to the others


