# Welcome to our Stock Market Project
# We will be using Time Series and Regression Analysis 
# To analyze trends in stock market indexes against various economic indicators

# Stock Market Indexes we will analyze:

# 	NASDAQ
# 	S & P 500
# 	NYSE

# Economic Indicators we are analyzing:

# 	m1
#	m2
#	consumerSentiment
#	inflation
#	imports
#	oilPrices
#	ppi
#	exports
#	cpi
#	unemploymentRate
#	fedFunds
#	capUtilization
#	sp_500Dividends
#	gdp_us
#	nasdaq
#	nyse
#	sp_500

# Let's get to it!
# Loading our Data
	
	timeSeries <- read.csv("/Users/njfritter/myProjects/learningR/econIndicators.csv")

# Download these libraries (skip if you have already downloaded)

	install.packages("forecast")
	install.packages("astsa")
	install.packages("ggplot2")
	install.packages("car")
	install.packages("MTS")
	install.packages("plm")

# Load the above libraries locally for your project
	
	require(forecast)
	require(astsa)
	require(ggplot2)
	require(car)
	require(MTS)
	require(plm)

# Checking the top entries and structure to make sure the data loaded properly
	
	head(timeSeries)
	str(timeSeries)

# Running ts function on stock indexes and Indicators, with start parameter and frequency
# Turns every variable into a Time Series object
	
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
	dividends <- ts(timeSeries$sp_500Dividends, start=c(1995, 1), freq=12)
	inflation <- ts(timeSeries$inflation, start=c(1995, 1), freq=12)
	gdp_us <- ts(timeSeries$gdp_us, start=c(1995, 1), freq=12)
	sp_500 <- ts(timeSeries$sp_500, start=c(1995, 1), freq=12)
	nasdaq <- ts(timeSeries$nasdaq, start=c(1995, 1), freq=12)
	nyse <- ts(timeSeries$nyse, start=c(1995, 1), freq=12)
	



# Print out some of the Time Series variables

	print("Here are nasdaq values for 1995-2015! ---------------------------------------------------")
	nasdaq

	print("Here are gdp_us values for 1995-2015! ---------------------------------------------------")
	gdp_us

	print("Here are cpi values for 1995-2015! ---------------------------------------------------")
	cpi

	print("Here are ppi values for 1995-2015! ---------------------------------------------------")
	ppi

# Print the Mean & Standard Deviation of sp_500

	print("This is the average sp_500 value from years 1995 to 2015 ---------------------------------------------------")
	mean(sp_500)

	print("This is the standard deviation of sp_500 values from years 1995 to 2015 ---------------------------------------------------")
	sd(sp_500)

# Plotting sp_500
	
	print("Outputting plot of sp_500: ")
	plot.ts(sp_500, main="Plotting sp_500 values")

# Plotting nasdaq
	
	print("Outputting plot of NASDAQ: ")
	plot.ts(nasdaq, main="Plotting NASDAQ values")

# Plotting nyse
	
	print("Outputting plot of NYSE: ")
	plot.ts(nyse, main="Plotting NYSE values")


# FIRST SECTION: NASDAQ LINEAR MODEL


# Linear model on nasdaq as a function of every economic indicator
# Every variable is accessed as a time series object from the main data set
# We will be using backwards elimination to find the most significant economic indicators

	nasdaq_fit1 <- lm(nasdaq ~ m1 + m2 + consumerSentiment + imports + oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = timeSeries)
	print("Printing summary of initial regression on economic indicators vs NASDAQ ---------------------------------------------------")
	summary(nasdaq_fit1)

# Looking at the summary for the fit, m2 has the highest p value
# Using the process of backwards elimination, we will drop m2

	nasdaq_fit2 <- lm(nasdaq ~ m1 + consumerSentiment + imports + oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = timeSeries)
	summary(nasdaq_fit2)

# Looking again, we find that ppi has the highest p value; it will be dropped

	nasdaq_fit3 <- lm(nasdaq ~ m1 + consumerSentiment + imports + oilPrices + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = timeSeries)
	summary(nasdaq_fit3)

# Next variable with highest p value that we will drop is: imports

	nasdaq_fit4 <- lm(nasdaq ~ m1 + consumerSentiment + oilPrices + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = timeSeries)
	summary(nasdaq_fit4)

# Next variable to be dropped with highest p value: oil prices

	nasdaq_fit5 <- lm(nasdaq ~ m1 + consumerSentiment + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = timeSeries)
	print("Printing summary of final regression on economic indicators vs NASDAQ ---------------------------------------------------")
	summary(nasdaq_fit5)

# All p values are now < 0.01
# So the above fit is in terms of only significant variables


# Now we will plot and obtain a confidence interval for the significant variables vs. the NYSE

	final_nasdaq_fit <- nasdaq_fit5
	plot(final_nasdaq_fit)
	confint(final_nasdaq_fit)


# NEXT SECTION: S & P 500 LINEAR MODEL


# Next we will do the linear fit for the S & P 500
# This time we will use the BIC Method
# We will also be using dot notation for the initial fit
# The dot represents every variable in the file

	sp500_fit1 <- lm(sp_500 ~ ., data=timeSeries)
	print("Printing summary of initial regression on economic indicators vs S & P 500 ---------------------------------------------------")
	summary(sp500_fit1)

# BIC Method
	
	n <- nrow(timeSeries)
	drop1(sp500_fit1, k=log(n))

# Using the BIC method, we will find and eliminate the variable with the lowest AIC value 
# According to the summary, US GDP has the lowest AIC value
# US GDP will be dropped

	sp500_fit2 <- lm(sp_500 ~ . - gdp_us,  data=timeSeries)
	summary(sp500_fit2)
	drop1(sp500_fit2, k=log(n))

# According to the drop1 summary, cpi has the lowest AIC value and will be dropped

	sp500_fit3 <- lm(sp_500 ~ . - gdp_us - cpi,  data=timeSeries)
	summary(sp500_fit3)
	drop1(sp500_fit3, k=log(n))

# According to the drop1 summary, m2 has the lowest AIC value and will be dropped

	sp500_fit4 <- lm(sp_500 ~ . - gdp_us - cpi - m2, data=timeSeries)
	summary(sp500_fit4)
	drop1(sp500_fit4, k=log(n))

# According to the drop1 summary, ppi has the lowest AIC value and will be dropped

	sp500_fit5 <- lm(sp_500 ~ . - gdp_us - cpi - m2 - ppi, data=timeSeries)
	summary(sp500_fit5)
	drop1(sp500_fit5, k=log(n))

# According to the drop1 summary, customerSentiment has the lowest AIC value and will be dropped

	sp500_fit6 <- lm(sp_500 ~ . - gdp_us - cpi - m2 - ppi - consumerSentiment, data=timeSeries)
	summary(sp500_fit6)
	drop1(sp500_fit6, k=log(n))

# inflation is the next variable with the lowest AIC value, so it will be dropped

	sp500_fit7 <- lm(sp_500 ~ . - gdp_us - cpi - m2 - ppi - consumerSentiment - inflation, data=timeSeries)
	summary(sp500_fit7)
	drop1(sp500_fit7, k=log(n))

# Now the intercept value of AIC is lower than any other variable
# Thus the model above is in terms of the most significant variables

# Now we will plot and obtain a confidence interval for the significant variables vs. the S & P 500

	final_sp500_fit <- sp500_fit7
	plot(final_sp500_fit)
	confint(final_sp500_fit)


# LAST SECTION: NYSE LINEAR MODEL


# Last we will analyze the nyse 
# linear model on nyse as function of every economic indicator

	nyse_fit1 <- lm(nyse ~ ., data = timeSeries)
	print("Printing summary of initial regression on economic indicators vs NYSE ---------------------------------------------------")
	summary(nyse_fit1)

# We will use backwards elimination for the nyse
# According to the summary, cpi has the highest p value
# cpi will be dropped

	nyse_fit2 <- lm(nyse ~ . - cpi, data = timeSeries)
	summary(nyse_fit2)

# According to the summary, consumerSentiment has the highest p value and will be dropped

	nyse_fit3 <- lm(nyse ~ . - cpi - consumerSentiment, data = timeSeries)
	summary(nyse_fit3)

# According to the summary, sp_500 dividends have the highest p value and will be dropped

	nyse_fit4 <- lm(nyse ~ . - cpi - consumerSentiment - dividends, data = timeSeries)
	summary(nyse_fit4)

# According to the summary, imports have the highest p value and will be dropped

	nyse_fit5 <- lm(nyse ~ . - cpi - consumerSentiment - dividends - imports, data = timeSeries)
	summary(nyse_fit5)

# According to the summary, US GDP has the highest p value and will be removed

	nyse_fit6 <- lm(nyse ~ . - cpi - consumerSentiment - dividends - imports - gdp_us, data = timeSeries)
	summary(nyse_fit6)

# According to the summary, inflation has the highest p value and will be dropped

	nyse_fit7 <- lm(nyse ~ . - cpi - consumerSentiment - dividends - imports - gdp_us - inflation, data = timeSeries)
	summary(nyse_fit7)

# Now all of the variables have p values < 0.01
# Therefore the model above is based solely on significant variables

# Now we will plot and obtain a confidence interval for the significant variables vs. the NYSE

	final_nyse_fit <- nyse_fit7
	plot(final_nyse_fit)
	confint(final_nyse_fit)

# Convert the timeSeries data into a dataframe

#timeSeries_df <- data.frame(m1, m2, consumerSentiment, imports, exports, oilPrices, )

# still to do
# collaborate with others on project to get best fit data for each one
# do multivariate/univariate models and/or panel data???
# learn ts(), lm(), summary(), arima(), 
# use datacamp.com for R tutorial