library(MASS)
library(rpart)
library(rpart.plot)
library(readr)
library(ggplot2)
library(caret)
library(dplyr)
library(patchwork)
library(mlbench)
data("BostonHousing2", package = "mlbench")
head(BostonHousing2)
Boston <- BostonHousing2 %>% select(-medv, -lon, -lat)

# Annotation of variables
# • CRIM - per capita crime rate by town
# • ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# • INDUS - proportion of non-retail business acres per town.
# • CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# • NOX - nitric oxides concentration (parts per 10 million)
# • RM - average number of rooms per dwelling
# • AGE - proportion of owner-occupied units built prior to 1940
# • DIS - weighted distances to five Boston employment centres
# • RAD - index of accessibility to radial highways
# • TAX - full-value property-tax rate per $10,000
# • PTRATIO - pupil-teacher ratio by town
# • B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# • LSTAT - % lower status of the population
# • CMEDV - Corrected Median value of owner-occupied homes in $1000's

# Part1: Neighborhood recommendation
# Recommendation Function Development
recommend_neighborhoods <- function(budget, 
                                    number_of_rooms,
                                    max_Lower_Status_Population,
                                    max_Non_retail_Business_Acres,
                                    max_Weighted_Distance_to_Employment,
                                    max_Proportion_of_Old_Units,
                                    max_Nitric_Oxides_Concentration,
                                    max_Property_Tax_Rate,
                                    max_Pupil_Teacher_Ratio,
                                    max_Per_Capita_Crime_Rate,
                                    min_Residential_Land_Proportion,
                                    min_Index_of_Highway_Accessibility) {
  # Filter the data based on user preferences
  filtered_data <- Boston %>%
    filter(cmedv <= budget,
           rm >= number_of_rooms,
           lstat <= max_Lower_Status_Population,
           indus <= max_Non_retail_Business_Acres,
           dis <= max_Weighted_Distance_to_Employment,
           age <= max_Proportion_of_Old_Units,
           nox <= max_Nitric_Oxides_Concentration,
           tax <= max_Property_Tax_Rate,
           ptratio <= max_Pupil_Teacher_Ratio,
           crim <= max_Per_Capita_Crime_Rate,
           zn >= min_Residential_Land_Proportion,
           rad >= min_Index_of_Highway_Accessibility)
  
  # Filter neighborhoods within the budget and sort by median price
  recommendations <- filtered_data %>%arrange(cmedv)
  
  return(recommendations)
}

# Example of customer input and recommendation
# For a customer who has a budget of 30 and cares most about number of rooms, lower status population and crime rate:
recommendations <- recommend_neighborhoods(
  budget = 30,
  number_of_rooms = 5.5, 
  max_Lower_Status_Population = 5,
  max_Non_retail_Business_Acres = 25,
  max_Weighted_Distance_to_Employment = 13,
  max_Proportion_of_Old_Units = 90,
  max_Nitric_Oxides_Concentration = 0.7,  
  max_Property_Tax_Rate = 700,  
  max_Pupil_Teacher_Ratio = 30,  
  max_Per_Capita_Crime_Rate = 0.1,  
  min_Residential_Land_Proportion = 0,  
  min_Index_of_Highway_Accessibility = 0  
)

print(recommendations)


# Part 2: CART model to predict the median price of newly built houses which are not in the data set
# Divide data set into train and test sets
set.seed(2014)
trainIndex <- createDataPartition(Boston$cmedv, p = 0.7, list = FALSE)
train_data <- Boston[trainIndex, ]
test_data <- Boston[-trainIndex, ]

cart1 <- rpart(cmedv ~ .-town -tract, data = train_data, method = 'anova', control = rpart.control(cp = 0))

printcp(cart1)

plotcp(cart1) # 8th tree is optimal

print(cart1)

cp1 <- sqrt(0.01575797*0.00846756)

# Prune the max tree using a particular CP value
cart2 <- prune(cart1, cp = cp1)
printcp(cart2, digits = 3)

print(cart2)

rpart.plot(cart2, nn = T, main = "Optimal Tree in Boston")

# Use pruned model to predict medv
cart_predictions <- predict(cart2, test_data)
cart_rmse <- sqrt(mean((cart_predictions - test_data$cmedv)^2))
print(cart_rmse)

# Visualization of prediction accuracy
ggplot(data = test_data, aes(x = cart_predictions, y = cmedv)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(title = "Observed vs. Predicted Median value of owner-occupied homes in $1000's",
       x = "Predicted medv",
       y = "Observed medv") +
  theme_minimal()

# Visualization of variable importance
cart2$variable.importance # average number of rooms per dwelling is the most important in determining house price
round(100*cart2$variable.importance/sum(cart2$variable.importance), 1)
var_importance <- cart2$variable.importance
rel_importance <- round(100 * var_importance / sum(var_importance), 1)
descriptive_names <- c(
  "Average Number of Rooms",          # rm
  "Lower Status Population",          # lstat
  "Non-retail Business Acres",        # indus
  "Weighted Distance to Employment",  # dis
  "Proportion of Old Units",          # age
  "Nitric Oxides Concentration",      # nox
  "Property-Tax Rate",                # tax
  "Pupil-Teacher Ratio",              # ptratio
  "Per Capita Crime Rate",            # crim
  "Residential Land Proportion",      # zn
  "Index of Highway Accessibility"    # rad
)

# Create a data frame for plotting
df <- data.frame(
  Variable = descriptive_names,
  Importance = rel_importance
)

# Plot using ggplot2
ggplot(df, aes(x = reorder(Variable, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Importance), vjust = -0.3, size = 3) +  # Add labels
  labs(title = "Relative Importance of Variables",
       x = "Variables", y = "Importance (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))

# Part 3: Facet chart of important variables
Boston_long <- Boston %>%
  tidyr::pivot_longer(cols = c(rm, lstat, indus, dis, age, nox, tax, ptratio, crim, zn, rad),
                      names_to = "variable",
                      values_to = "value")

# Create the faceted plot
facet_plot <- ggplot(Boston_long, aes(x = value, y = cmedv)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Factors vs Median Home Value", y = "Median Home Value") +
  facet_wrap(~ variable, scales = "free_x", 
             labeller = labeller(variable = c(rm = "Average Number of Rooms", 
                                              lstat = "Lower Status Population (%)", 
                                              indus = "Non-retail Business Acres (%)",
                                              dis = "Weighted Distance to Employment",
                                              age = "Proportion of Old Units (%)",
                                              nox = "Nitric Oxides Concentration",
                                              tax = "Property-Tax Rate",
                                              ptratio = "Pupil-Teacher Ratio",
                                              crim = "Per Capita Crime Rate (%)",
                                              zn = "Residential Land Proportion",
                                              rad = "Index of Highway Accessibility"
                                              ))) +
  theme(axis.title.x = element_blank())

# Print the faceted plot
print(facet_plot)