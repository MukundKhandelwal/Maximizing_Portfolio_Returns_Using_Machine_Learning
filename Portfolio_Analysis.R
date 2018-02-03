# Equal Weight Portfolio Weights
# Amount invested $500 in 3 stocks
# Current Prices: Stock 1 (Microsoft)= $83.34 | Stock 2 (Bank Of America)=$28.28 | Stock 3 (ANI Pharmaceuticals $71.02
# NASDAQ Index to be backtested
weight=rep(1/3,3)
weight


# Market Capitalization Portfolio Weights
# Market Cap (In billion dollars): Stock 1: $654.81 | Stock 2: $288.30 | Stock 3: 0.826 
mc=c(654.81,288.30,0.826)
weight_mc=mc/sum(mc)
weight_mc
summary(weight_mc)
barplot(weight_mc,xlab="Stocks",ylab="Market Capitalization based weights",ylim=c(0,0.7))


#Calculating Time Series returns on Stocks
#"values.csv" is a time series of 3 stocks with daily closed prices
#Install packages zoo and PerformanceAnalytics
library(zoo)
library(PerformanceAnalytics) 
value=read.zoo(file="values.csv",header=TRUE, format="%m/%d/%Y", sep=",")
value_ts=as.xts(value)
value_ts
asset_return=Return.calculate(value_ts)
#Remove First row of NA values
asset_return=asset_return[-1,]

#Calculating Time Series returns on Portfolio
#Buy and Hold
PR_BH=Return.portfolio(R=asset_return, weights=weight_mc)
#Daily Rebalance
PR_DR=Return.portfolio(R=asset_return, weights=weight_mc, rebalance_on="days")
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(PR_BH)
plot.zoo(PR_DR)

#Reducing frequency from Daily close to monthly close values. 
#Eg.Convert a stocks end of day prices of Microsoft, BAC, and ANI and convert it to monthly values (Works on Univariate distribution)
#toMonthly Stock 1
spd1=read.zoo(file="MSFT.csv",header=TRUE, format="%m/%d/%Y", sep=",")
spd_ts1=as.xts(spd1)
spm1=to.monthly(spd_ts1)
spm1
spm_returns1=Return.calculate(spm1[,4])
plot.zoo(spm_returns1)

#toMonthly Stock 2
spd2=read.zoo(file="BAC.csv",header=TRUE, format="%m/%d/%Y", sep=",")
spd_ts2=as.xts(spd2)
spm2=to.monthly(spd_ts2)
spm2
spm_returns2=Return.calculate(spm2[,4])
plot.zoo(spm_returns2)


#toMonthly Stock 3
spd3=read.zoo(file="ANI.csv",header=TRUE, format="%m/%d/%Y", sep=",")
spd_ts3=as.xts(spd3)
spm3=to.monthly(spd_ts3)
spm3
spm_returns3=Return.calculate(spm3[,4])
plot.zoo(spm_returns3)


#Evaluating Risk vs Reward on the portfolio (Comparison with a benchmark-US Treasury Bill)
#rf: Monthly rates for US Treasury Bill
rfm=read.zoo(file="Risk Free.csv",header=TRUE, format="%m/%d/%Y", sep=",")
rf=as.xts(rfm)

#Computing performance evaluation parameters
mean(spm_returns1[-1,])
mean.geometric(spm_returns1[-1,])
sd(spm_returns1[-1,])

sp_excess1 =spm_returns1 -rf
mean(sp_excess1[-1,])
# Compute the Sharpe ratio
sp_sharpe1 = mean(sp_excess1[-1,]) / sd(spm_returns1[-1,])

#Computing performance evaluation parameters
mean(spm_returns2[-1,])
mean.geometric(spm_returns2[-1,])
sd(spm_returns2[-1,])

sp_excess2 =spm_returns2 -rf
mean(sp_excess2[-1,])
# Compute the Sharpe ratio
sp_sharpe2 = mean(sp_excess2[-1,]) / sd(spm_returns2[-1,])

#Computing performance evaluation parameters
mean(spm_returns3[-1,])
mean.geometric(spm_returns3[-1,])
sd(spm_returns3[-1,])

sp_excess3 =spm_returns3 -rf
mean(sp_excess3[-1,])
# Compute the Sharpe ratio
sp_sharpe3 = mean(sp_excess3[-1,]) / sd(spm_returns3[-1,])

#Compute all of the above at once using table.AnnualizedReturns()
table.AnnualizedReturns(spm_returns1,scale=12)

#Compute all of the above at once using table.AnnualizedReturns()
table.AnnualizedReturns(spm_returns2,scale=12)

#Compute all of the above at once using table.AnnualizedReturns()
table.AnnualizedReturns(spm_returns3,scale=12)

#Plotting the 12-month rolling annualized Sharpe ratio
chart.RollingPerformance(R = spm_returns1, width = 12, FUN = "SharpeRatio.annualized", Rf = rf, main = "Rolling 12-Month Sharpe Ratio - Microsoft")

chart.RollingPerformance(R = spm_returns2, width = 12, FUN = "SharpeRatio.annualized", Rf = rf, main = "Rolling 12-Month Sharpe Ratio - Bank of America")

chart.RollingPerformance(R = spm_returns3, width = 12, FUN = "SharpeRatio.annualized", Rf = rf, main = "Rolling 12-Month Sharpe Ratio - ANI Pharmaceuticals")


# Subperiod Performance Analysis using Window function
#Fill in window for, eg. 2008
m_2017 =window(spm_returns1, start= "2017-01-01", end = "2017-10-31")
b_2017 =window(spm_returns2, start= "2017-01-01", end = "2017-10-31")
a_2017 =window(spm_returns3, start= "2017-01-01", end = "2017-10-31")


#Plotting settings 
par( mfrow = c(3, 1) , mar=c(3, 2, 2, 2))

plot(m_2017, main="Subperiod Analysis (2017) - Microsoft", xlab="Months", ylab="Returns", pch=19)

plot(b_2017, main="Subperiod Analysis (2017) -Bank of America", xlab="Months", ylab="Returns", pch=19)

plot(a_2017, main="Subperiod Analysis (2017) - ANI", xlab="Months", ylab="Returns", pch=19)

# Detecting Non-Normality of Returns using Skewness and Kurtosis
#Compute the skewness
skewness(spm_returns1)
kurtosis(spm_returns1)

skewness(spm_returns2)
kurtosis(spm_returns2)

skewness(spm_returns3)
kurtosis(spm_returns3)

# Visualizing Drawdowns
#Plot of drawdowns
par( mfrow = c(3, 1) , mar=c(3, 2, 2, 2))
chart.Drawdown(spm_returns1,main="Drawdown from Peak Equity Attained-Microsoft")
chart.Drawdown(spm_returns2,main="Drawdown from Peak Equity Attained-Bank of America")
chart.Drawdown(spm_returns3,main="Drawdown from Peak Equity Attained-ANI")

#Create a correlation matrix of returns
cov_mat=cov(asset_return[-1,])
cor_matrix=cor(asset_return[-1,])

#Create volatility budget
vol_budget <- StdDev(asset_return, portfolio_method = "component", weights =weight_mc)

#Make a table of weights and risk contribution
percrisk=cbind(weight_mc,vol_budget$pct_contrib_StdDev)
colnames(percrisk) = c("weights", "perc vol contrib")

#Print the table
print(percrisk)

library(tseries)

df_est=window(asset_return, start = "2014-12-01", end = "2016-12-31")
df_eval=window(asset_return,start="2017-01-01",end="2017-11-27")
pf_estim=portfolio.optim(df_est,covmat=cov(asset_return[-1,]))
pf_eval=portfolio.optim(df_eval,covmat=cov(asset_return[-1,]))
returns_pf_estim <- Return.portfolio(df_est, pf_estim$pw, rebalance_on = "days")
returns_pf_eval <- Return.portfolio(df_eval, pf_estim$pw, rebalance_on = "days")
plot(pf_estim$pw,pf_eval$pw)



#OutofSample Performance Evaluation
#Create returns_pf_estim
returns_pf_estim <- Return.portfolio(df_est, pf_estim$pw, rebalance_on = "days")

#Create returns_pf_eval
returns_pf_eval <- Return.portfolio(df_eval, pf_estim$pw, rebalance_on = "days")

#Print a table of your estimation sample returns 
table.AnnualizedReturns(returns_pf_estim)

#Print a table of your evaluation sample returns
table.AnnualizedReturns(returns_pf_eval)


**********


#Calculating Time Series returns on NASDAQ
#Install packages zoo and PerformanceAnalytics
library(zoo)
library(PerformanceAnalytics) 
value=read.zoo(file="NASDAQ.csv",header=TRUE, format="%m/%d/%Y", sep=",")
value_ts=as.xts(value)
value_ts
nasdaq_return=Return.calculate(value_ts)
#Remove First row of NA values
nasdaq_return=nasdaq_return[-1,]

