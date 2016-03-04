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

# running ts function on stock index columns, with start parameter and frequency
	
	sp_500 <- ts(timeSeries$sp_500, start=c(1995, 1), freq=12)
	nasdaq <- ts(timeSeries$nasdaq, start=c(1995, 1), freq=12)
	nyse <- ts(timeSeries$nyse, start=c(1995, 1), freq=12)
	gdp_us <- ts(timeSeries$gdp_us, start=c(1995, 1), freq=12)
	cpi <- ts(timeSeries$cpi, start=c(1995, 1), freq=12)
	ppi <- ts(timeSeries$ppi, start=c(1995, 1), freq=12)


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

# linear model on sp_500 as a function of every economic indicator
# every variable is accessed as a column from the main data set

	sp500fit <- lm(sp_500 ~ timeSeries$m1 + timeSeries$m2 + timeSeries$consumerSentiment + timeSeries$imports + timeSeries$oilPrices + timeSeries$ppi + timeSeries$exports + timeSeries$cpi + timeSeries$unemploymentRate + timeSeries$fedFunds + capUtilization + timeSeries$nyse, data = timeSeries)
	print("Printing summary of initial regression on economic indicators vs S & P 500 ---------------------------------------------------")
	summary(sp500fit)

# linear model on sp_500 as a function of more significant variables 
# variables with higher p values were removed
# But only a few to maintain high R^2 value 

	sp500fit2 <- lm(sp_500 ~ timeSeries$m1 + timeSeries$consumerSentiment + timeSeries$ppi + timeSeries$exports + timeSeries$unemploymentRate + timeSeries$capUtilization + timeSeries$nyse,  data = timeSeries)
	print("Printing summary of linear regression on more significant economic variables vs. S & P 500 ---------------------------------------------------")
	summary(sp500fit2)

# linear model on nasdaq as a function of every economic indicator

	nasdaqfit <- lm(nasdaq ~ timeSeries$m1 + timeSeries$m2 + timeSeries$consumerSentiment + timeSeries$imports + timeSeries$oilPrices + timeSeries$ppi + timeSeries$exports + timeSeries$cpi + timeSeries$unemploymentRate + timeSeries$fedFunds + timeSeries$capUtilization + timeSeries$nyse, data = timeSeries)
	print("Printing summary of initial regression on economic indicators vs NASDAQ ---------------------------------------------------")
	summary(nasdaqfit)

# linear model on nasdaq as function of more influential economic indicators
# variables with higher p values were removed 

	nasdaqfit2 <- lm(nasdaq ~ timeSeries$consumerSentiment + timeSeries$imports + timeSeries$exports + timeSeries$cpi + timeSeries$unemploymentRate + timeSeries$fedFunds + timeSeries$capUtilization + timeSeries$nyse, data = timeSeries)
	print("Printing summary of regression on more significant economic indicators vs NASDAQ ---------------------------------------------------")
	summary(nasdaqfit2)

# linear model on nyse as function of every economic indicator

	nysefit <- lm(nyse ~ timeSeries$m1 + timeSeries$m2 + timeSeries$consumerSentiment + timeSeries$imports + timeSeries$oilPrices + timeSeries$ppi + timeSeries$exports + timeSeries$cpi + timeSeries$unemploymentRate + timeSeries$fedFunds + timeSeries$capUtilization + timeSeries$sp_500, data = timeSeries)
	print("Printing summary of initial regression on economic indicators vs NYSE ---------------------------------------------------")
	summary(nysefit)

# linear model on nyse as function of more influential economic indicators
# variables with higher p values removed

	nysefit2<- lm(nyse ~ timeSeries$m1 + timeSeries$imports + timeSeries$oilPrices + timeSeries$exports + timeSeries$cpi +  timeSeries$capUtilization + timeSeries$sp_500, data = timeSeries)
	print("Printing summary of regression on more influential economic indicators vs NYSE ---------------------------------------------------")
	summary(nysefit2)
# still to do
# collaborate with others on project to get best fit data for each one
# do time series calculations for each one
# learn ts(), lm(), summary(), arima(), 
# use datacamp.com for R tutorial