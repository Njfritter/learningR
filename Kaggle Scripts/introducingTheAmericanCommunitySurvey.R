# Beginning the "Introducing the American Community Survey

# First Exercise

# We have the AC Survey Subset Data as an RData file

# Load in your data
acs_url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_810/AC_Survey_Subset.RData"

load(url(acs_url))

# Investigate first 20 observations
head(AC_Survey_Subset, n = 20)

# Second Exercise

# Our Data is still messy, let us clean it up a bit by loading in the dplyr package
# This allows us to easily filter data sets with multiple criteria at once 

# Load in the dplyr package and convert AC_Survey_Subset to tbl_df
library(dplyr)

AC_Survey_Subset <- tbl_df(AC_Survey_Subset) 

# Use the pipe operator and chaining 
# Omit empty spaces
# Filter by Schedule and where Schedule is either 21, 22 or 24 (Bachelor's, Master's, Doctorate)

AC_Survey_Subset_Cleaned <- AC_Survey_Subset %>% na.omit(NULL) %>% filter(SCHL %in% c(21, 22, 24)) %>% group_by(SCHL)

# Third Exercise

# Want to sum the number of degree holders, merge with degree codes
# dplyr is loaded, AC_Survey_Subset_Cleaned and degree_codes are available

# Count the number of Bachelor, Master and PhD holders using dplyr aggregate function "n()"
degree_holders <- summarize(AC_Survey_Subset_Cleaned, count = n())

# Join degree_codes with degree_holders, assign to degree_holders_2 using "inner_join"
degree_holders_2 <- inner_join(degree_holders, degree_codes)

# Fourth Exercise 

# We will make use of ggplot2 to rapidly visualize the data to explain data to peers
# Will recreate a simple bar graph with number of degrees on vertical axis, type on horizontal

# # Load the ggplot2 package
library(ggplot2)

# Visualize the number of Bachelor, Master and PhD holders  
# ggplot takes in two arguments: dataset, and aes which means aesthetics of graph
# Includes geom_bar (error prevention?), x and y axis labeling and Graph Title
ggplot(degree_holders_2, aes(x = Degree, y = count, fill = Degree)) +                        
  geom_bar(stat = "identity") +
  xlab("Degree") + 
  ylab("No of People") + 
  ggtitle("Comparing Degree Holders in the US")
  
# Fifth Exercise

# Now we will determine whether or not it is smart financially to pursue a PhD
# Will be working with MedianIncomes compared to Degree Type
# Will make use of the ggplot2 packages

# income is available in the workspace, ggplot2 is pre-loaded
    
# Create the boxplots
ggplot(income, aes(x = Degrees, y= MedianIncome, fill = Degrees)) +  
  geom_boxplot( ) + ggtitle("Comparing Income of Degrees Holders")
  
# And we're done! Woo!!!