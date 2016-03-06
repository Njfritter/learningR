# loading our data
	
	timeSeries <- read.csv("/Users/njfritter/myProjects/econIndicators.csv")

# download libraries (skip if you have already downloaded)

	install.packages("forecast")
	install.packages("astsa")
	install.packages("ts")

# load the abovelibraries
	
	require(forecast)
	require(astsa)
	require(ts)

# check top entries and structure
	
	head(timeSeries)
	str(timeSeries)

# running ts function on stock indexes and indicator, with start parameter and frequency
	
	m1 <- ts(timeSeries$m1, start=c(1995, 1), freq=12)
	m2 <- ts(timeSeries$m2, start=c(1995, 1), freq=12)
	consumerSentiment <- ts(timeSeries$consumerSentiment, start=c(1995, 1), freq=12)
	imports <- ts(timeSeries$imports, start=c(1995, 1), freq=12)
	exports <- ts(timeSeries$exports, start=c(1995, 1), freq=12)
	oilPrices <- ts(timeSeries$oilPrices, start=c(1995, 1), freq=12)
	cpi <- ts(timeSeries$cpi, start=c(1995, 1), freq=12)
	ppi <- ts(timeSeries$ppi, start=c(1995, 1), freq=12)
	fedFunds <- ts(timeSeries$fedFunds, start=c(1995, 1), freq=12)
	capUtilization <- ts(timeSeries$capUtilization, start=c(1995, 1), freq=12)
	unemploymentRate <- ts(timeSeries$unemploymentRate, start=c(1995, 1), freq=12)
	sp_500 <- ts(timeSeries$sp_500, start=c(1995, 1), freq=12)
	nasdaq <- ts(timeSeries$nasdaq, start=c(1995, 1), freq=12)
	nyse <- ts(timeSeries$nyse, start=c(1995, 1), freq=12)
	gdp_us <- ts(timeSeries$gdp_us, start=c(1995, 1), freq=12)



# printing some data

	print("Here are nasdaq values for 1995-2015! ---------------------------------------------------")
	nasdaq

	print("Here are gdp_us values for 1995-2015! ---------------------------------------------------")
	gdp_us

	print("Here are cpi values for 1995-2015! ---------------------------------------------------")
	cpi

	print("Here are ppi values for 1995-2015! ---------------------------------------------------")
	ppi

# printing mean & standard deviation of sp_500

	print("This is the average sp_500 value from years 1995 to 2015 ---------------------------------------------------")
	mean(sp_500)

	print("This is the standard deviation of sp_500 values from years 1995 to 2015 ---------------------------------------------------")
	sd(sp_500)

# plotting sp_500
	
	print("Outputting plot of sp_500: ")
	plot.ts(sp_500, main="Plotting sp_500 values")

# plotting nasdaq
	
	print("Outputting plot of NASDAQ: ")
	plot.ts(nasdaq, main="Plotting NASDAQ values")

# plotting nyse
	
	print("Outputting plot of NYSE: ")
	plot.ts(nyse, main="Plotting NYSE values")

# linear model on nasdaq as a function of every economic indicator
# every variable is accessed as a time series object from the main data set
# We will be using backwards elimination to find the most significant economic indicators

	nasdaq_fit1 <- lm(nasdaq ~ m1 + m2 + consumerSentiment + imports + oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = timeSeries)
	print("Printing summary of initial regression on economic indicators vs NASDAQ ---------------------------------------------------")
	summary(nasdaq_fit1)

# Looking at the summary for the fit, m2 has the lowest p value
# Using the process of backwards elimination, we will drop m2

	nasdaq_fit2 <- lm(nasdaq ~ m1 + consumerSentiment + imports + oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = timeSeries)
	summary(nasdaq_fit2)

# Looking again, we find that ppi has the lowest p value; it will be dropped

	nasdaq_fit3 <- lm(nasdaq ~ m1 + consumerSentiment + imports + oilPrices + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = timeSeries)
	summary(nasdaq_fit3)

# Next variable with lowest p value that we will drop is: imports

	nasdaq_fit4 <- lm(nasdaq ~ m1 + consumerSentiment + oilPrices + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = timeSeries)
	summary(nasdaq_fit4)

# Next variable to be dropped with lowest p value: oil prices

	nasdaq_fit5 <- lm(nasdaq ~ m1 + consumerSentiment + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = timeSeries)
	print("Printing summary of final regression on economic indicators vs NASDAQ ---------------------------------------------------")
	summary(nasdaq_fit5)

# All p values are now < 0.01
# So the above fit is in terms of only significant variables

# Next we will do the linear fit for the S & P 500
# This time we will use the BIC Method
# We will also be using dot notation for the initial fit
# The dot represents every variable in the file

	sp500_fit1 <- lm(sp_500 ~ ., data=timeSeries)
	print("Printing summary of initial regression on economic indicators vs S & P 500 ---------------------------------------------------")
	summary(sp500fit1)

# BIC Method
	
	n <- nrow(timeSeries)
	drop1(sp500_fit1, k=log(n))

# Using the BIC method, we will find and eliminate the variable with the lowest AIC value 
# According to the summary, cpi has the lowest AIC value
# cpi will be dropped

	sp500_fit2 <- lm(sp_500 ~ . - cpi,  data=timeSeries)
	summary(sp500_fit2)
	drop1(sp500_fit2, k=log(n))

# According to the drop1 summary, m2 has the lowest AIC value and will be dropped

	sp_500fit3 <- lm(sp_500 ~ . - cpi - m2, data=timeSeries)
	summary(sp_500fit3)
	drop1(sp_500fit3, k=log(n))

# According to the drop1 summary, ppi has the lowest AIC value and will be dropped

	sp_500fit4 <- lm(sp_500 ~ . - cpi - m2 - ppi, data=timeSeries)
	summary(sp_500fit4)
	drop1(sp_500fit4, k=log(n))

# According to the drop1 summary, customerSentiment has the lowest AIC value and will be dropped

	sp_500fit5 <- lm(sp_500 ~ . - cpi - m2 - ppi - consumerSentiment, data=timeSeries)
	summary(sp_500fit5)
	drop1(sp_500fit5, k=log(n))

# inflation is the next variable with the lowest AIC value, so it will be dropped

	sp_500fit6 <- lm(sp_500 ~ . - cpi - m2 - ppi - consumerSentiment - inflation, data=timeSeries)
	summary(sp_500fit6)
	drop1(sp_500fit6, k=log(n))

# Now the intercept value of AIC is lower than any other variable
# Thus the model above is in terms of the most significant variables

# Last we will analyze the nyse 
# linear model on nyse as function of every economic indicator

	nysefit <- lm(nyse ~ ., data = timeSeries)
	print("Printing summary of initial regression on economic indicators vs NYSE ---------------------------------------------------")
	summary(nysefit)

# linear model on nyse as function of more influential economic indicators
# variables with higher p values removed

	nysefit2<- lm(nyse ~ m1 + imports + oilPrices + exports + cpi +  capUtilization + sp_500, data = timeSeries)
	print("Printing summary of regression on more influential economic indicators vs NYSE ---------------------------------------------------")
	summary(nysefit2)

# Convert the timeSeries data into a dataframe

#	timeSeries_df <- data.frame(m1, m2, consumerSentiment, imports, exports, oilPrices, )
# still to do
# collaborate with others on project to get best fit data for each one
# do time series calculations for each one
# learn ts(), lm(), summary(), arima(), 
# use datacamp.com for R tutorial