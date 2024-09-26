#CODE BY WEI SIJIA

library(mlbench)
data("BostonHousing2", package = "mlbench")

library(data.table)
library(ggplot2)

Boston_dt <- as.data.table(BostonHousing2)
class(Boston_dt)

summary(Boston_dt$crim)

ggplot(Boston_dt, aes(x = rm, y = cmedv, color = lstat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "orange") + 
  labs(
    title = "Effect of Room Count and Lower-Status Population on Housing Values",
    x = "Average Number of Rooms per Dwelling",
    y = "Median Value of Homes ($1000s)",
    color = "Lower-Status Population (%)"
  ) +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")




# Create a scatter plot with color representing dis
ggplot(Boston_dt, aes(x = age, y = cmedv, color = dis)) +
  geom_point() +
  geom_smooth(method = "lm", col = "orange") +  # Adding a line of best fit
  labs(
    title = "Relationship Between Building Age, Distance to Employment Centers, and Housing Values",
    x = "Proportion of Owner-Occupied Units Built Prior to 1940 (%)",
    y = "Median Value of Homes ($1000s)",
    color = "Distance to Employment Centers"
  ) +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")


ggplot(Boston_dt, aes(x = zn, y = cmedv, color = indus)) +
  geom_point() +
  geom_smooth(method = "lm", col = "orange") +  # Adding a line of best fit
  labs(
    title = "Residential Land Zoning and Non-Retail Business Acres vs. Median Value of Homes",
    x = "Proportion of Residential Land Zoned for Lots Over 25,000 sq.ft.",
    y = "Median Value of Homes ($1000s)",
    color = "Proportion of Non-Retail Business Acres"
  ) +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")

ggplot(Boston_dt, aes(x = tax, y = cmedv, color = ptratio)) +
  geom_point() +
  geom_smooth(method = "lm", col = "orange") +  # Adding a line of best fit
  labs(
    title = "Property Tax Rates and Pupil-Teacher Ratios vs. Median Value of Homes",
    x = "Full-Value Property Tax Rate per $10,000",
    y = "Median Value of Homes ($1000s)",
    color = "Pupil-Teacher Ratio by Town"
  ) +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")


ggplot(Boston_dt, aes(x = nox, y = medv, color = crim)) +
  geom_point() +
  geom_smooth(method = "lm", col = "orange") +  # Adding a line of best fit
  labs(
    title = "Crime Rate, Nitric Oxides Concentration vs. Median Value of Homes",
    x = "Nitric Oxides Concentration (parts per 10 million)",
    y = "Median Value of Homes ($1000s)",
    color = "Per Capita Crime Rate"
  ) +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")



