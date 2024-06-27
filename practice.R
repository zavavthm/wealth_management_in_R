vector <- c(112,121,131,141,15,16)
vector[6]
matrix <- rep(vector, each=3)
my_matrix <- matrix(data=vector,nrow=3,ncol=2)
# in matrix you can only store same data types, but in data frames, we can store different data types as well

my_data_frame <- as.data.frame(my_matrix)

# vector of names, data type -> character
roommates <- c("S", "M", "A")
s_salary <- c(324,234,12354) #salaries for 3 years
m_salary <- c(2344,4532,5345) #salaries for 3 years
a_salary <- c(54363, 345236, 3565473) #salaries for 3 years
dates <- c("01-13-2021","01-13-2022","01-13-2023")
typeof(dates)
formatted_dates <- as.Date(dates, format="%m - %d - %Y")

# in salary matrix 3x3 matrix
matrix_salary <- matrix(c(s_salary, m_salary, a_salary), nrow = 3, ncol = 3)

# converting all numbers to character data type
matrix_salary_new <- rbind(matrix_salary, roommates)

# create a data frame of the above matrix
salary_df <- as.data.frame(matrix_salary)

#add a new column to data frame
salary_df$V4 <- formatted_dates

# setting the names of the columns
colnames(salary_df) <- c(roommates, "Date")


##basic data types ium
# char
# date

## complex data types:
# vector -> a list of items with same data type

#factors
## -> like a python set, contains only unique values and in alphabetical order
my_factor_roommates <- as.factor(roommates)

final_data <- as.character(my_factor_roommates)

# is.numeric() will tell if the vector is numeric or not
# typeof() to get the type of the object
# as.numeric(), as.logical(), as.date(), as.character(), ... etc to convert vectors from one datatype to another

## trying to understand the distribution of sigma for a stock over the years
library(quantmod)
library(dplyr)

#AGG stock - low risk

agg_stock <- getSymbols(Symbols = "AGG", auto.assign = F)
agg <- dailyReturn(getSymbols(Symbols = "AGG", auto.assign = F))
agg_returns <- merge.xts(agg_stock, agg)
agg_final <- agg_returns[,7]

df_agg <- as.data.frame(agg_final)
index_date <- index(agg_final)

df_agg <- df_agg %>%
  mutate(Date=index_date)

df_agg <- tail(df_agg, 250)

# WFC Stock - high risk
wfc_stock <- getSymbols(Symbols = "WFC", auto.assign = F)
wfc <- dailyReturn(getSymbols(Symbols = "WFC", auto.assign = F))
wfc_returns <- merge.xts(wfc_stock, wfc)
wfc_final <- wfc_returns[,7]

df_wfc <- as.data.frame(wfc_final)
index_date <- index(wfc_final)
df_wfc <- df_wfc %>%
  mutate(Date=index_date)

df_wfc <- tail(df_wfc, 250)

# install.packages("ggplot2")
library(ggplot2)

#plotting wfc - scatter plot
ggplot(df_wfc, aes(x=Date, y=daily.returns)) + 
  geom_point(color="blue", shape=21, size=3, fill="lightblue") +
  labs(title="WFC stock", x="Date", y="daily returns") +
  theme_minimal()


#plotting agg - scatter plot
ggplot(df_agg, aes(x=Date, y=daily.returns)) + 
  geom_point(color="blue", shape=21, size=3, fill="lightblue") +
  labs(title="AGG stock", x="Date", y="daily returns") +
  theme_minimal()

# plotting distribution curve
ggplot(df_wfc, aes(x = daily.returns)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(title = "WFC", x = "Daily Returns Sigma", y = "Density") +
  theme_minimal()

ggplot(df_agg, aes(x = daily.returns)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(title = "AGG", x = "Daily Returns Sigma", y = "Density") +
  theme_minimal()
