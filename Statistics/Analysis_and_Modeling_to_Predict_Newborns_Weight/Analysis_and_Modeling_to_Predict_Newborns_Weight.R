### Final Project - Neonatal Weight Prediction ###


## Packages ##
if (!require(here)) {
  install.packages("here") # to get current path
}
library(here)

if (!require(moments)) {
  install.packages("moments") # for shape indexes functions
}
library(moments)

if (!require(ggplot2)) {
  install.packages("ggplot2") # for plots
}
library(ggplot2)

if (!require(scales)) {
  install.packages("scales") # for percent_format function
}
library(scales)

if (!require(dplyr)) {
  install.packages("dplyr") # for grouping dataset
}
library(dplyr)

if (!require(car)) {
  install.packages("car") # for multicollinearity test
}
library(car)

if (!require(lmtest)) {
  install.packages("lmtest") # for residuals diagnostics
}
library(lmtest)

if (!require(scatterplot3d)) {
  install.packages("scatterplot3d") # for 3D plot
}
library(scatterplot3d)

if (!require(rgl)) {
  install.packages("rgl") # for 3D plot saving
}
library(rgl)



## Functions ##
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
data_newborns <- read.csv("neonati.csv", header = TRUE, sep = ',')

# Inspect the header
head(data_newborns, 5)
tail(data_newborns, 5)




#####
# 2 #
#####
### Dataset Variables and Relative Types ###
names(data_newborns)

"Anni.madre" # age of the mother [year]
# numeric continuous variable

"N.gravidanze" # number of pregnancies
# numeric discrete variable

"Fumatrici" # mother is a smoker (0 = no, 1 = yes)
# categorical variable on nominal scale (control variable)

"Gestazione" # duration of pregnancy [weeks]
# numeric continuous variable

"Peso" # newborn weight [g]
# numeric continuous variable (response variable)

"Lunghezza" # newborn length [mm]
# numeric continuous variable

"Cranio" # newborn cranium diameter [mm]
# numeric continuous variable

"Tipo.parto" # type of delivery (natural or cesarean)
# categorical variable on nominal scale (control variable)

"Ospedale" # hospital of birth (1, 2, 3)
# categorical variable on nominal scale (control variable)

"Sesso" # newborn gender (male or female)
# categorical variable on nominal scale (control variable)

## NOTE ##
# The research objective is the prediction of the newborn weight by assessing the 
# mother background (age, number of previous pregnancies, smoking status, gestational
# period), examining some information about the newborn obtained through echography
# (length, cranial diameter, gender) and taking into account certain circumstantial
# data (birth hospital and delivery method).




#####
# 3 #
#####
### Descriptive Analysis ###

# Number of observations
N = dim(data_newborns)[1]

# Conversion for categorical variables
data_newborns$Fumatrici <- as.factor(data_newborns$Fumatrici)
data_newborns$Tipo.parto <- as.factor(data_newborns$Tipo.parto)
data_newborns$Ospedale <- as.factor(data_newborns$Ospedale)
data_newborns$Sesso <- as.factor(data_newborns$Sesso)

# Extract variables names
attach(data_newborns)

# Summaries, Dispersion Indexes and Shapes
for (variable_name in names(data_newborns)) {
  variable_newborns = data_newborns[[variable_name]]
  cat("Summary for", variable_name,":\n")
  print(summary(variable_newborns))
  
  if (is.numeric(variable_newborns)) {
    cat("\nDispersion Indexes:\n")
    cat("- Variance:", round(var(variable_newborns), 2), "\n")
    cat("- Standard Deviation:", round(sd(variable_newborns), 2), "\n")
    cat("- Coefficient of Variation:", round(sd(variable_newborns)/mean(variable_newborns)*100, 2), "\n")
    cat("- Normalized Gini Index:", round(gini.index_norm(variable_newborns), 2), "\n")
    cat("- Fisher Index:", round(skewness(variable_newborns), 2), "\n")
    cat("- Kurtosis Index:", round(kurtosis(variable_newborns), 2), "\n")
    }
  cat("\n************************************************\n")
}



## Global Plots ##

# Distribution Plots
distribution_plots <- list()
distribution_plots_variables <- c("Anni.madre", "N.gravidanze", "Gestazione")
for (variable_name in distribution_plots_variables) {
  
  # Create a dataframe with relative frequencies
  variable_newborns = data_newborns[[variable_name]]
  variable_frequency_relative <- table(variable_newborns)/N*100
  variable_frequency_relative_df <- data.frame(variable_name = as.numeric(names(variable_frequency_relative)), frequency = as.numeric(variable_frequency_relative))
  
  plot <- ggplot(variable_frequency_relative_df, aes(x = variable_name, y = frequency)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    labs(title = paste("Relative Frequency Distribution for", variable_name), x = variable_name, y = "Relative Frequency [%]")
  
  distribution_plots[[variable_name]] <- plot
}

distribution_plots


# Density Plots
density_plots <- list()
density_plots_variables <- c("Peso", "Lunghezza", "Cranio")
for (variable_name in density_plots_variables) {
  
  density_plots[[variable_name]] <- ggplot(data_newborns, aes(x = .data[[variable_name]])) +
    geom_density(fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = paste("Density Distribution for", variable_name), x = variable_name, y = "Density")
}

density_plots


## Plots - Split by Control Variables ##

control_variables <- c("Fumatrici", "Tipo.parto", "Ospedale", "Sesso")

## NOT USED ##
# Distribution Plots
# distribution_plots <- list()
# distribution_plots_variables <- c("Anni.madre", "N.gravidanze", "Gestazione")
# for (control_variable in control_variables) {
#   
#   distribution_plots[[control_variable]] <- list()
#   for (variable_name in distribution_plots_variables) {
#     
#     data_newborns_by_modality <- list()
#     distribution_plots[[control_variable]][[variable_name]] <- list()
#     modalities <- unique(data_newborns[[control_variable]])
#     for (modality in modalities) {
#       
#       # Split for single modality for current control variable
#       data_newborns_by_modality[[modality]] <-subset(data_newborns, data_newborns[[control_variable]] == modality)
#       
#       # Create a dataframe with relative frequencies
#       variable_newborns = data_newborns_by_modality[[modality]][[variable_name]]
#       variable_frequency_relative <- table(variable_newborns)/N*100
#       variable_frequency_relative_df <- data.frame(variable_name = as.numeric(names(variable_frequency_relative)), frequency = as.numeric(variable_frequency_relative))
#       
#       plot <- ggplot(variable_frequency_relative_df, aes(x = variable_name, y = frequency)) +
#         geom_bar(stat = "identity", fill = "lightblue", color = "black") +
#         labs(title = paste("Relative Frequency Distribution for", variable_name, "\nSplit by", control_variable, "=", modality), x = variable_name, y = "Relative Frequency [%]")
#     
#       distribution_plots[[control_variable]][[variable_name]][[modality]] <- plot
#     }
#   }
# }
# 
# distribution_plots


# Density Plots
density_plots <- list()
density_plots_variables <- c("Peso", "Lunghezza", "Cranio", "Anni.madre", "N.gravidanze", "Gestazione")
for (control_variable in control_variables) {
  
  density_plots[[control_variable]] <- list()
  for (variable_name in density_plots_variables) {
    
    plot <- ggplot(data_newborns, aes(x = .data[[variable_name]], fill = .data[[control_variable]], color = .data[[control_variable]])) +
      geom_density(alpha = 0.3) +
      labs(
        title = paste("Density Distribution for", variable_name, "Split by", control_variable),
        x = variable_name,
        y = "Density"
      )
    
    density_plots[[control_variable]][[variable_name]] <- plot
  }
}

density_plots



#####
# 4 #
#####
### Hypothesis Test - Standard Values for Weight and Length ##

### NOTE ###
# The objective is to assess if the average neonatal weight and length calculated 
# for this dataset sample significantly differ from those measured for the entire 
# population. Since the standard deviation related to average neonatal weight and 
# length in the population is unknown, the Student's t-test can be appropriately 
# applied to check if the mean values in the sample could reasonably be considered
# equal to those of the population.


# Weight 
## Source: https://www.who.int/tools/child-growth-standards/standards/weight-for-age
## Reference mean weight calculated supposing almost equal number of males and females

H0_mean_weight = (3346 + 3232)/2
alpha = 0.05

t.test(Peso,
       mu = H0_mean_weight,
       conf.level = 1 - alpha,
       alternative = "two.sided")

# One Sample t-test
# data:  Peso
# t = -0.46846, df = 2499, p-value = 0.6395
# alternative hypothesis: true mean is not equal to 3289
# 95 percent confidence interval:
#   3263.490 3304.672
# sample estimates:
#   mean of x
# 3284.081
### NOTE ###
# With a 95% confidence interval and a p-value of 0.64, there is inadequate 
# statistical evidence to reject the null hypothesis. The estimated sample mean
# weight of 3284.1 falls within the confidence interval [3263.5, 3305.7], making
# it plausible to consider it consistent with the null hypothesis, which assert
# that sample weight is significantly the same as the population one.



# Length 
## Source: https://www.who.int/tools/child-growth-standards/standards/length-height-for-age
## Reference mean length calculated supposing almost equal number of males and females

H0_mean_length = (499 + 491)/2
alpha = 0.05

t.test(Lunghezza,
       mu = H0_mean_length,
       conf.level = 1 - alpha,
       alternative = "two.sided")

# One Sample t-test
# data:  Lunghezza
# t = -0.58514, df = 2499, p-value = 0.5585
# alternative hypothesis: true mean is not equal to 495
# 95 percent confidence interval:
#   493.6598 495.7242
# sample estimates:
#   mean of x
# 494.692
### NOTE ###
# With a 95% confidence interval and a p-value of 0.56, there is inadequate 
# statistical evidence to reject the null hypothesis. The estimated sample mean 
# length of 494.7 falls within the confidence interval [493.7, 495.7], making it 
# plausible to consider it consistent with the null hypothesis, which asserts that
# the sample length is not significantly different from the population length.




#####
# 5 #
#####
### Hypothesis Test - Gender Differences ###


# Box Plots Comparison
box_plots <- list()
box_plots_variables <- c("Peso", "Lunghezza", "Gestazione", "Cranio")

for (variable_name in box_plots_variables) {
  plot <- ggplot(data = data_newborns) +
    geom_boxplot(aes(x = Sesso, y = .data[[variable_name]]), fill = "lightblue", color = "lightblue3", alpha = 0.7, outlier.shape = 7) +
    geom_jitter(aes(x = Sesso, y = .data[[variable_name]]), color = "red", alpha = 0.5, width = 0.2, size = 0.2) +
    labs(
      title = paste("Distribution of", variable_name, "by Sesso"),
      x = "Sesso",
      y = variable_name
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()
    box_plots[[variable_name]] <- plot
}
box_plots


# Weight
weight_male <- data_newborns$Peso[Sesso == "M"]
weight_female <- data_newborns$Peso[Sesso == "F"]

t.test(weight_male, weight_female,
       conf.level = 1 - alpha,
       alternative = "two.sided")

# Welch Two Sample t-test
# data:  weight_male and weight_female
# t = 12.106, df = 2490.7, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   207.0615 287.1051
# sample estimates:
#   mean of x mean of y 
# 3408.215  3161.132
### NOTE ###
# With a 95% confidence interval and a very low p-value (close to 0), there is
# strong statistical evidence to reject the null hypothesis and accept the
# alternative hypothesis. The estimated difference between the mean weights of
# males and females falls within the confidence interval [207.1, 287.1],
# indicating a significant difference between the two groups."


# Length
length_male <- data_newborns$Lunghezza[Sesso == "M"]
length_female <- data_newborns$Lunghezza[Sesso == "F"]

t.test(length_male, length_female,
       conf.level = 1 - alpha,
       alternative = "two.sided")

# Welch Two Sample t-test
# data:  length_male and length_female
# t = 9.582, df = 2459.3, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   7.876273 11.929470
# sample estimates:
#   mean of x mean of y 
# 499.6672  489.7643 
### NOTE ###
# With a 95% confidence interval and a very low p-value (close to 0), there is
# strong statistical evidence to reject the null hypothesis and accept the
# alternative hypothesis. The estimated difference between the mean weights of
# males and females falls within the confidence interval [7.9, 11.9],
# indicating a significant difference between the two groups."


# Pregnancy Time
pregnancy_time_male <- data_newborns$Gestazione[Sesso == "M"]
pregnancy_time_female <- data_newborns$Gestazione[Sesso == "F"]

t.test(pregnancy_time_male, pregnancy_time_female,
       conf.level = 1 - alpha,
       alternative = "two.sided")

# Welch Two Sample t-test
# data:  pregnancy_time_male and pregnancy_time_female
# t = 6.7072, df = 2446.6, p-value = 2.456e-11
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.3514294 0.6418172
# sample estimates:
#   mean of x mean of y 
# 39.22990  38.73328 
### NOTE ###
# With a 95% confidence interval and a very low p-value, there is strong 
# statistical evidence to reject the null hypothesis and accept the alternative
# hypothesis. The estimated difference between the mean pregnancy time of
# males and females falls within the confidence interval [035, 0.64],
# indicating a significant difference between the two groups
#
# The result is somewhat surprising because a significant difference in
# gestation period based on the baby's gender would not be expected. Since the
# absolute difference is relatively small (a few days), it could be attributed to
# the gestation period being discretized in weeks. Alternatively, this dataset may
# be influenced by specific cases or undisclosed variables and environmental
# conditions, making it challenging to explain the difference. In conclusion, this
# result should be interpreted with caution.


# Cranium Diameter
cranium_male <- data_newborns$Cranio[Sesso == "M"]
cranium_female <- data_newborns$Cranio[Sesso == "F"]

t.test(cranium_male, cranium_female,
       conf.level = 1 - alpha,
       alternative = "two.sided")
# Welch Two Sample t-test
# data:  cranium_male and cranium_female
# t = 7.4102, df = 2491.4, p-value = 1.718e-13
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   3.541270 6.089912
# sample estimates:
#   mean of x mean of y 
# 342.4486  337.6330
### NOTE ###
# With a 95% confidence interval and a very low p-value, there is strong 
# statistical evidence to reject the null hypothesis and accept the alternative
# hypothesis. The estimated difference between the mean cranium diameter of
# males and females falls within the confidence interval [3.5, 6.1],
# indicating a significant difference between the two groups



###########
# 5 Extra #
###########
### Hypothesis Test - Smokers Difference ###


# Box Plots Comparison
box_plots <- list()
box_plots_variables <- c("Peso")

for (variable_name in box_plots_variables) {
  plot <- ggplot(data = data_newborns) +
    geom_boxplot(aes(x = Fumatrici, y = .data[[variable_name]]), fill = "lightblue", color = "lightblue3", alpha = 0.7, outlier.shape = 7) +
    geom_jitter(aes(x = Fumatrici, y = .data[[variable_name]]), color = "red", alpha = 0.5, width = 0.2, size = 0.2) +
    labs(
      title = paste("Distribution of", variable_name, "by Fumatrici"),
      x = "Fumatrici",
      y = variable_name
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()
  box_plots[[variable_name]] <- plot
}
box_plots


# Weight
weight_smoker_no <- data_newborns$Peso[Fumatrici == 0]
weight_smoker_yes <- data_newborns$Peso[Fumatrici == 1]

t.test(weight_smoker_no, weight_smoker_yes,
       conf.level = 1 - alpha,
       alternative = "two.sided")
# Welch Two Sample t-test
# data:  weight_smoker_no and weight_smoker_yes
# t = 1.034, df = 114.1, p-value = 0.3033
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -45.61354 145.22674
# sample estimates:
#   mean of x mean of y
# 3286.153  3236.346
### NOTE ###
# With a 95% confidence interval and a p-value of 0.3, there is not sufficient 
# statistical evidence to reject the null hypothesis. While the plots hint that 
# newborns from smoking mothers may weigh less than others, it is possible that
# with a larger sample size, this difference could become more statistically 
# significant.




#####
# 6 #
#####
### Test on Delivery Type Relation by Hospital ###

### NOTE ###
# To assess whether there is a significant relationship between the delivery type
# and the hospital where the birth took place, a contingency table is constructed
# for these two variables. Subsequently, observed frequencies are compared to 
# expected frequencies using a Chi-squared test. The null hypothesis assumes that
# the variables are independent.

data_delivery_type_by_hospital <- data_newborns %>%
  group_by(Ospedale) %>%
  summarize(
    sum_delivery_natural = sum(Tipo.parto == "Nat"),
    sum_delivery_cesarean = sum(Tipo.parto == "Ces")
  )
data_delivery_type_by_hospital

table_observed <- as.matrix(data_delivery_type_by_hospital[,2:3])
table_expected <- as.matrix(data_delivery_type_by_hospital[,2:3])

for (r in 1:nrow(table_expected)) {
  for (c in 1:ncol(table_expected)) {
    table_expected[r, c] =                   # expected value at cell [r, c]
      (margin.table(table_observed, 1)[r] *  # sum of all values at row r
      margin.table(table_observed, 2)[c]) /  # sum of all values at column c
      N                                      # sum of table = number of observations
  cat(table_expected[r, c], ' ')
  }
  cat("\n")
}

chi_squared = sum((table_observed - table_expected)^2/table_expected)

chisq.test(table_observed)
# Pearson's Chi-squared test
# data:  table_observed
# X-squared = 1.0972, df = 2, p-value = 0.5778
### NOTE ###
# The chi-squared test results in a high p-value, indicating that there is no
# significant statistical evidence to suggest a relationship between delivery
# types and hospitals.


n = 10^6
alpha = 0.05
df = (nrow(table_expected) - 1)*(ncol(table_expected) - 1)
chisq_distribution <- data.frame(x = rchisq(n, df))

ggplot(chisq_distribution, aes(x)) +
  geom_density(color = "black") +
  geom_vline(xintercept = qchisq(1 - alpha, df), color = "red") +
  geom_point(data = data.frame(x = chi_squared), aes(x, 0), size = 5, color = "blue", shape = 20) +
  labs(
    title = "Chi-Squared Distribution",
    x = "Chi-Squared Value",
    y = "Density"
  ) +
  coord_cartesian(xlim = c(0, 10))


#############################
# Multidimensional Analysis #
#############################

#####
# 1 #
#####
### Correlation Analysis ###

data_newborns_numeric <- data_newborns[, sapply(data_newborns, is.numeric)]
round(cor(data_newborns_numeric), 2)

#              Anni.madre  N.gravidanze  Gestazione   Peso  Lunghezza  Cranio
# Anni.madre         1.00          0.38       -0.14  -0.02      -0.06    0.02
# N.gravidanze       0.38          1.00       -0.10   0.00      -0.06    0.04
# Gestazione        -0.14         -0.10        1.00   0.59       0.62    0.46
# Peso              -0.02          0.00        0.59   1.00       0.80    0.70
# Lunghezza         -0.06         -0.06        0.62   0.80       1.00    0.60
# Cranio             0.02          0.04        0.46   0.70       0.60    1.00

### NOTE ###
## Relations with response variable Peso ##
# Based on the results obtained from the correlation matrix, the weight of newborns
# appears to have a moderate positive correlation with gestational period, newborn
# length and cranial dimensions. However, it's important to note that correlation
# does not necessarily imply a direct causal relationship between these variables.
# Regarding the correlation between weight and gestational period, it can be
# hypothesized that a shorter gestational period may lead to lighter newborns, as
# premature births could affect birth weight. On the other hand, it seems plausible
# that newborns with greater length and larger head dimensions have a higher birth
# weight.
## Relations among variables ##
# There is a slight positive correlation between the mother's age and the number 
# of pregnancies, plausibly due to the logical fact that more time (and so at older
# age) is required for additional pregnancies. It can be noticed that gestational 
# period, length, and cranial diameter are moderately positively correlated.
# The correlation coefficients suggest that on average longer gestational periods
# are associated with increased newborn length and cranial diameter. This implies 
# that a shorter gestational period might lead to less developed newborns in terms
# of these measurements.

x11()

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = if (r > 0.2) cex.cor * r else 1)
}
pairs(data_newborns, upper.panel = panel.smooth, lower.panel = panel.cor)




#####
# 2 #
#####
### Multiple Linear Regression Model - All Variables ###

# Preliminary check on response variable
skewness(Peso)      # -0.65 -> negative skewness
kurtosis(Peso) - 3  # 2.03  -> leptokurtic distribution

shapiro.test(Peso)
# Shapiro-Wilk normality test
# data:  Peso
# W = 0.97066, p-value < 2.2e-16

### NOTE ###
# Although weight should have a normal distribution in the population, 
# this sample exhibits a non-normal distribution. This significant 
# deviation from normality can lead to distortion in regression coefficients,
# affect the normal distribution of residuals, and result in less accurate
# predictions.


model_1_all_var <- lm(Peso ~ ., data_newborns)
summary(model_1_all_var)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1124.40  -181.66   -14.42   160.91  2611.89 
# 
# Coefficients:
#                 Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)   -6738.4762    141.3087  -47.686   < 2e-16 ***
# Anni.madre        0.8921      1.1323    0.788    0.4308    
# N.gravidanze     11.2665      4.6608    2.417    0.0157 *  
# Fumatrici1      -30.1631     27.5386   -1.095    0.2735    
# Gestazione       32.5696      3.8187    8.529   < 2e-16 ***
# Lunghezza        10.2945      0.3007   34.236   < 2e-16 ***
# Cranio           10.4707      0.4260   24.578   < 2e-16 ***
# Tipo.partoNat    29.5254     12.0844    2.443    0.0146 *  
# Ospedaleosp2    -11.2095     13.4379   -0.834    0.4043    
# Ospedaleosp3     28.0958     13.4957    2.082    0.0375 *  
# SessoM           77.5409     11.1776    6.937  5.08e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 273.9 on 2489 degrees of freedom
# Multiple R-squared:  0.7289,	Adjusted R-squared:  0.7278 
# F-statistic: 669.2 on 10 and 2489 DF,  p-value: < 2.2e-16

### NOTE ###
## The Most Significant ##
# The gestation period, newborn length and cranium diameter have a
# substantial impact on the model, as indicated by their p-values close to 0.
# These variables show a strong influence and their increases appear
# to significantly contribute to an increase in the newborn's weight.
# Newborn gender also appears to be a significant factor in predicting
# the weight. The coefficient suggests that on average a male newborn
# contributes to a weight increase of 77.5 units compared to a female 
# newborn when all other variables are held constant. This was expected
# based on test result on mean weights comparison by genders.
## The Less Significant ##
# The number of pregnancies appears to have a slight but significant
# positive influence on weight increase. It is also noteworthy that on
# average, a natural birth may positively affect the estimated weight.
# Conversely, it can be hypothesized that a cesarean birth might be 
# performed under critical conditions where gestation does not reach full
# term, which significantly impacts weight.
# Furthermore, it seems that being born in Hospital 2 or Hospital 3 has
# a slightly significant effect on weight, either negative or positive, 
# respectively. Without additional data or analysis, it could be assumed
# that there might be demographic and geographical reasons for the
# difference in contribution.




#####
# 3 #
#####
### Multiple Linear Regression Model - Better Model ###

## Removal of not significant regressors ##
model_2_only_significant <- update(model_1_all_var, ~.
                                   - Anni.madre
                                   - Fumatrici
                                   - Ospedale)
summary(model_2_only_significant)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1129.31  -181.70   -16.31   161.07  2638.85 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -6707.2971   135.9911 -49.322  < 2e-16 ***
#   N.gravidanze     12.7558     4.3366   2.941   0.0033 ** 
#   Gestazione       32.2713     3.7941   8.506  < 2e-16 ***
#   Lunghezza        10.2864     0.3007  34.207  < 2e-16 ***
#   Cranio           10.5057     0.4260  24.659  < 2e-16 ***
#   Tipo.partoNat    30.0342    12.0969   2.483   0.0131 *  
#   SessoM           77.9285    11.1905   6.964 4.22e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 274.3 on 2493 degrees of freedom
# Multiple R-squared:  0.7277,	Adjusted R-squared:  0.727 
# F-statistic:  1110 on 6 and 2493 DF,  p-value: < 2.2e-16

### NOTE ###
# Adjusted R-squared values between model 1 and model 2 are the same,
# so no improvements by simplifying the model so far.


## Retention of only the most significant regressors ##
model_3_best_significant <- update(model_2_only_significant, ~.
                                          - N.gravidanze
                                          - Tipo.parto)
summary(model_3_best_significant)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1138.2  -184.3   -17.6   163.3  2627.3 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -6651.1188   135.5172 -49.080  < 2e-16 ***
#   Gestazione     31.2737     3.7856   8.261 2.31e-16 ***
#   Lunghezza      10.2054     0.3007  33.939  < 2e-16 ***
#   Cranio         10.6704     0.4245  25.139  < 2e-16 ***
#   SessoM         79.1049    11.2117   7.056 2.22e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 275 on 2495 degrees of freedom
# Multiple R-squared:  0.7261,	Adjusted R-squared:  0.7257 
# F-statistic:  1654 on 4 and 2495 DF,  p-value: < 2.2e-16

### NOTE ###
# Adjusted R-squared value is almost the same (slightly worse), it is
# possible that some predictors are multicollinear.


## Multicollinearity test and modeling ##
round(vif(model_3_best_significant), 2)
# Gestazione  Lunghezza     Cranio      Sesso 
#       1.65       2.07       1.61       1.04 

### NOTE ###
# The length appears to have the highest multicollinearity with the other
# variables. Even if a variance inflation factor close to 2 is quite
# moderate. removing this predictor could enhance the model.


model_4_reduce_multicollinearity_Lunghezza <- update(model_3_best_significant, ~.
                                   - Lunghezza)
summary(model_4_reduce_multicollinearity_Lunghezza)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1316.39  -219.02   -10.96   210.38  1532.14 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -6214.5285   163.0677 -38.110   <2e-16 ***
#   Gestazione     92.6067     4.0208  23.032   <2e-16 ***
#   Cranio         17.1449     0.4583  37.408   <2e-16 ***
#   SessoM        118.5296    13.4793   8.793   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 332.4 on 2496 degrees of freedom
# Multiple R-squared:  0.5996,	Adjusted R-squared:  0.5992 
# F-statistic:  1246 on 3 and 2496 DF,  p-value: < 2.2e-16

### NOTE ###
# Removing the lenght, the model results even more inaccurate,
# so this road is not the best one.


model_5_reduce_multicollinearity_Cranio <- update(model_3_best_significant, ~.
                                           - Cranio)
summary(model_5_reduce_multicollinearity_Cranio)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1138.1  -193.0   -22.8   187.0  3618.8 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -5224.0093   137.7319  -37.93  < 2e-16 ***
#   Gestazione     44.4826     4.1961   10.60  < 2e-16 ***
#   Lunghezza      13.6028     0.3007   45.24  < 2e-16 ***
#   SessoM         90.2850    12.5392    7.20 7.92e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 307.8 on 2496 degrees of freedom
# Multiple R-squared:  0.6567,	Adjusted R-squared:  0.6563 
# F-statistic:  1592 on 3 and 2496 DF,  p-value: < 2.2e-16

### NOTE ###
# Removing the cranium diameter, the model results more inaccurate,
# similar by removing the length.



## Automatic stepwise procedure ##

# Akaike Information Criterion (prefer over-parameterized models)
model_6_stepwise_AIC <- MASS::stepAIC(model_1_all_var,
                                       direction = "both",
                                       k = 2)

# Bayesian Information Criterion (penalize over-parameterized models)
model_7_stepwise_BIC <- MASS::stepAIC(model_1_all_var,
                                       direction = "both",
                                       k = log(n))

summary(model_6_stepwise_AIC)
### NOTE ###
# Similar result as "model_2_only_significant", but more complex
# due to added variable Ospedale

summary(model_7_stepwise_BIC)
### NOTE ###
# Obtained the same model as "model_3_best_significant"



### Models Evaluation ###


## Variance analysis ##

# Adjusted coefficient of determination previously evaluated:
# 0.7278 -> model_1_all_var
# 0.727  -> model_2_only_significant
# 0.7257 -> model_3_best_significant
# 0.5992 -> model_4_reduce_multicollinearity_Lunghezza
# 0.6563 -> model_5_reduce_multicollinearity_Cranio
# 0.7278 -> model_6_stepwise_AIC
# 0.7257 -> model_7_stepwise_BIC (same as model 3)

# Variance analysis on best models based on Adjusted R-squared
anova(model_1_all_var,
      model_2_only_significant,
      model_3_best_significant,
      model_6_stepwise_AIC)
#    Res.Df        RSS  Df  Sum of Sq       F     Pr(>F)    
# 1    2489  186762521                                      
# 2    2493  187601677  -4    -839155  2.7959  0.0247927 *  
# 3    2495  188688687  -2   -1087011  7.2433  0.0007301 ***
# 4    2491  186899996   4    1788691  5.9595   9.03e-05 ***
### NOTE ###
# Analyzing the variance, all models exhibit similar sums of squared residuals.
# However, the model 3 is preferred due to its simplicity (fewer predictors).

# AIC 
AIC(model_1_all_var,
      model_2_only_significant,
      model_3_best_significant,
      model_6_stepwise_AIC)
# BIC 
BIC(model_1_all_var,
    model_2_only_significant,
    model_3_best_significant,
    model_6_stepwise_AIC)

### NOTE ###
# Evaluating the goodness of fit of the models using Akaike and Bayesian
# evaluation criteria, the outcomes are once again very similar. Therefore the
# simpler Model 3 is considered the best choice to fitting the data.




#####
# 4 #
#####
### Not Linear Relations ###

model_1.1_not_linear <- update(model_1_all_var, ~.
                                                + I(Cranio^3))
summary(model_1.1_not_linear)
# Adjusted R-squared:  0.7312 

model_1.2_not_linear <- update(model_1.1_not_linear, ~.
                                                + I(Lunghezza^3))
summary(model_1.2_not_linear)
# Adjusted R-squared:  0.7369 

model_1.3_not_linear <- update(model_1.2_not_linear, ~.
                                                + Cranio:Lunghezza)
summary(model_1.3_not_linear)
# Adjusted R-squared:  0.7388 

model_1.4_not_linear <- MASS::stepAIC(model_1.3_not_linear,
                                      direction = "both",
                                      k = log(n))
summary(model_1.4_not_linear)
# Adjusted R-squared:  0.7338


anova(model_3_best_significant,
      model_1.1_not_linear,
      model_1.2_not_linear,
      model_1.3_not_linear,
      model_1.4_not_linear)

AIC(model_3_best_significant,
      model_1.1_not_linear,
      model_1.2_not_linear,
      model_1.3_not_linear,
      model_1.4_not_linear)

BIC(model_3_best_significant,
      model_1.1_not_linear,
      model_1.2_not_linear,
      model_1.3_not_linear,
      model_1.4_not_linear)

### NOTE ###
# Evaluating the ANOVA test, model 1.3 has the lowest residual sum of squares
# and the highest explained sum of squares, but it is also the most complex model.
# To favor simplicity with fewer predictor variables, considering the similar 
# results about Information Criteria and the related adjusted R-squared (0.7338),
# model 1.4 could be a preferred choice for modeling the data.



#####
# 5 #
#####
### Residual Diagnostics ###

x11()
par(mfrow = c(2, 2))
plot(model_3_best_significant)
par(mfrow = c(1, 1))

## Residuals vs Fitted ##
# Relating the estimates obtained from the model to their respective residuals,
# the plot shows data points moderately scattered randomly around the mean of 0.
# Especially for lower estimated values, there is a slight curve, indicating that
# some of the information hasn't been correctly filtered out by the predictors
# and has manifested in the residuals.

## Q-Q Residuals ##
# When relating standardized residuals to theoretical quantiles of a normal
# distribution, the points are distributed around the bisector for a significant
# portion of the central region of the plot, except in the outer regions where
# they diverge. This indicates that the residuals mostly follow a normal
# distribution, except for low or high values of the response variable where the
# model does not fit accurately, and nonlinear relationships may be present.

## Scale-Location ##
# When relating the square root of standardized residuals to the model's
# estimates, the plot displays a random scatter of points around a horizontal
# line for the upper part. This indicates that in that region, the variance among
# residuals is constant, indicating homoscedasticity. However for lower values
# different variances are observed.

## Residuals vs Leverage ##
# By relating the standardized residuals to the leverages, all values fall 
# within the Cook's distance except for observation 1551. Therefore it is
# necessary to assess how this influential observation impacts the regression
# coefficients and consequently the whole model estimation.


## Leverages ##

# Normalized distances between observed and fitted values 
leverages <- hatvalues(model_3_best_significant)
plot(leverages,
     main = "Leverages",
     xlab = "Observations", ylab = "Normalized Distance")

threshold_leverages = 2 * sum(leverages)/N
abline(h = threshold_leverages, col = "red")

length(leverages[leverages > threshold_leverages]) # 135
### NOTE ###
# Based on the calculated threshold, there are 135 observations with high
# leverage values that significantly deviate from the rest of the observations
# in the predictor space.


## Outliers ##

# Standard deviations distances between observed and predicted values
outliers <- rstudent(model_3_best_significant)
plot(outliers,
     main = "Outliers",
     xlab = "Observations", ylab = "Standard Deviation Distance")

threshold_outliers = qt(1 - alpha/2, df = (N - 1))
abline(h = c(-threshold_outliers, threshold_outliers), col = "red")

length(outliers[outliers < -threshold_outliers]) +
  length(outliers[outliers >threshold_outliers]) # 112
### NOTE ###
# Based on the calculated threshold, 112 observations are considered outliers 
# compared to the rest of the observations

# Test by applying Bonferroni correction evaluating p-values
outlierTest(model_3_best_significant)
### NOTE ###
# Applying Bonferroni correction, 3 observations are considered outliers


## Cook's Distance
cook_distances <- cooks.distance(model_3_best_significant)
plot(cook_distances,
     main = "Evaluation of Influential Observations ",
     xlab = "Observations", ylab = "Cook's Distance")

threshold_warning = 0.5
abline(h = threshold_warning, col = "red")
cook_distances[cook_distances > threshold_warning]

influential_points = which(cook_distances > threshold_warning)
text(influential_points, cook_distances[influential_points], labels = influential_points, pos = 1)

### NOTE ###
# Observation 1551 with a Cook's distance value very close to 1 is a strong
# influential value, which could impact the estimation of regressor coefficients


# Check best model removing influential observation
data_newborns_fixed <- data_newborns[-(1551+1), ]
model_3_best_significant_fixed = lm(data = data_newborns_fixed,
   formula = Peso ~ Gestazione + Lunghezza + Cranio + Sesso)
summary(model_3_best_significant_fixed)
### NOTE ###
# By modeling again without the influential observation, the adjusted R-squared
# is quite the same of previous model (0.7257)


## Check Residuals Assumptions ##

# Normality - Shapiro-Wilk test
shapiro.test(residuals(model_3_best_significant))
plot(density(residuals(model_3_best_significant)))
# W = 0.9742, p-value < 2.2e-16
# Residuals are not distributed as a normal distribution

# Mean close to 0
mean(residuals(model_3_best_significant))
# mean = -9.39e-15
     
# Homoscedasticity - Breusch-Pagan test
bptest(model_3_best_significant)
# BP = 89.148, df = 4, p-value < 2.2e-16
# Variance is not constant 

# Independence - Durbin-Watson test
dwtest(model_3_best_significant)
# DW = 1.9557, p-value = 0.1337
# No autocorrelation among residuals




#####
# 6 #
#####
### Model Evaluation ###

# Mean Square Error of best models
model_names = c("model_1_all_var",
               "model_2_only_significant",
               "model_3_best_significant_fixed",
               "model_1.4_not_linear",
               "model_6_stepwise_AIC")

MSE <- numeric(length(model_names))
for (i in 1:length(model_names)) {
  model_name = model_names[i]
  Peso_predicted <- predict(get(model_name), newdata = data_newborns)
  MSE[i] <- round(sum((data_newborns$Peso - Peso_predicted)^2)/N)
}

MSE_results <- data.frame(Model = model_names, MSE = MSE)
print(MSE_results)
#   Model                                    MSE
# 1 model_1_all_var                        74705
# 2 model_2_only_significant               75041
# 3 model_3_best_significant_fixed         75475
# 4 model_1.4_not_linear                   73238
# 5 model_6_stepwise_AIC                   74760

### NOTE ###
# When comparing the Mean Squared Error (MSE) among the best models, it's
# evident that the results are quite similar. However, when considering the MSE
# values in the context of the variance of the response variable (275665), all
# these models appear to explain a significant portion of the response variable.




#####
# 7 #
#####
### Model Prediction ###

data_newborns_3P_39W <- data_newborns[N.gravidanze == 3 & Gestazione == 39, ]
summary(data_newborns_3P_39W$Peso)

model_8_prediction <- lm(Peso ~ N.gravidanze + Gestazione)
summary(model_8_prediction)
predict(model_8_prediction, newdata = data.frame(N.gravidanze = 3, Gestazione = 39)) # 3340
### NOTE ###
# The prediction of 3340 for this specific case seems reasonable as it aligns
# with the central values of the data. However it's important to note that the
# model constructed using only two explanatory variables has a relatively low
# Adjusted R-squared value. This suggests that the model, overall, may have
# limited accuracy in making accurate predictions across a broader range of cases




#####
# 8 #
#####
### Model Visualization ###

# Model simplification for 3D representation
model_9_plot <- update(model_3_best_significant_fixed, ~.
                       - Cranio
                       - Sesso)
summary(model_9_plot)
### NOTE ###
# Simplifying further the model, the Adjusted R-squared gets worse (0.6492)

color_mapping <- c("pink3", "steelblue2")

## NOT USED ##
# scatter.3d <- with(data_newborns,
#                    scatterplot3d(Gestazione,
#                                  Lunghezza,
#                                  Peso,
#                                  color = color_mapping[Sesso],
#                                  pch = 1,
#                                  angle = 360)
# )
# scatter.3d$plane3d(model_9_plot, col = "blue", draw_polygon = TRUE)
# legend3d("topright", legend = unique(data_newborns$Sesso), pch = 15, col = unique(color_mapping$Sesso))

open3d()
plot3d(model_9_plot,
       plane.col = 'white', col = color_mapping[Sesso],
       size = 3)
legend3d("topright", legend = unique(Sesso), pch = 16,
         col = color_mapping[Sesso], cex = 1, inset = c(0.05))
title3d("Model Visualization for Weight Prediction")
