##### Information #####
# A positive batting average trend follower? Introducing Trend Vigor.
# Posted on May 25, 2014 by Ilya Kipnis
# Posted in Dr. John Ehlers, ETFs, QuantStrat, R, Trading
# https://quantstrattrader.wordpress.com/2014/05/25/a-positive-batting-average-trend-follower-introducing-trend-vigor-2/
# https://github.com/IlyaKipnis

##### Motivation #####
# Some common wisdom about both mean-reverting and trend-following strategies.
# Mean reverting strategies often sport a high percentage of positive trades,
# but the few negative trades can hammer the equity curve.
# On the other side of the equation, trend-following strategies run on a philosophy of 
# "let your winners run and cut your losers", resulting in many small losses and a few wins that make up for them. 
# However, what if there were an indicator that behaved like the best of both worlds?
# That is, capitalize on trends, while cutting out a good portion of whipsaws?
# The Trend Vigor Indicator, or TVI as in DSTrading package (available ob github), attempts to do just that. 

# It takes in a time series, and two parameters: a period, which is identical to the n parameter in indicators such as 
# SMA and so on, and a delta, which is a trigonometric parameter to adjust the computation of the bandpass filter.
# TVI then outputs a time series of a 0-centered trend vigor indicator, along with a pair of oscillators (signal and lead).

##### Strategy #####
# The indicator will be the trend vigor calculation,
# and the strategy will go long on the next open when the trend vigor crosses above 1,
# sell the next open when it crosses below 1.4,
# and will stop out on the next open when the trend vigor crosses under 1
# if it never crossed 1.4 within the duration of the position.

##### Syntax #####
# add.indicator/add.signal/add.rule is a function which takes in the name of the strategy as the first argument 
# (which I always use strategy.st for), a name of an R function in the name argument, the arguments to the function
# as the argument to arguments (that is, arguments=list(x=quote(Cl(mktdata)), period=100, delta=0.2) is the argument 
# to the TVI function which is the value for the name argument in the add.indicator function), and finally, a label. 
# While the labels may seem like so much window dressing, they are critical in terms of linking indicators to signals,
# signals to rules, and everything to any optimization/robustness testing you may decide to do. Do not forget them.

##### Notes #####
# While I kept Dr. Ehlers's default period setting, I found that the strategy works best starting at 
# 60 days for the period, and has a solid performance until the mid-hundreds, at which point it generates
# too few trades per instrument to really be able to look at any individual statistics.

##### Initialisation #####
setwd("~/Alpha Modelling/QuantStrat/Strategies/Trading")
require(DSTrading)
require(quantstrat)
options(width=80)
source("demoData.R") #contains all of the data-related boilerplate.

##### Strategy implementation #####
strategy.st <- portfolio.st <- account.st <- "TVItrendFollowingLong"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

# Indicator

add.indicator(strategy.st, name="TVI", arguments=list(x=quote(Cl(mktdata)), period=100, delta=0.2), label="TVI")

# Signals

add.signal(strategy.st, name="sigThreshold", 
           arguments=list(threshold=1, column="vigor.TVI", relationship="gte", cross=TRUE),
           label="longEntry")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(threshold=1.4, column="vigor.TVI", relationship="lt", cross=TRUE),
           label="longExit")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(threshold=1, column="vigor.TVI", relationship="lt", cross=TRUE),
           label="wrongExit")

# Rules

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, orderqty=100, 
                        ordertype="market", orderside="long", replace=FALSE, prefer="Open"), 
         type="enter", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", 
                        ordertype="market", orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="wrongExit", sigval=TRUE, orderqty="all", 
                        ordertype="market", orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

##### Strategy application #####

t1 <- Sys.time()
out <- applyStrategy(strategy=strategy.st,portfolios=portfolio.st, verbose=FALSE) # verbose=FALSE to suspend the order log
t2 <- Sys.time()
print(t2-t1)

##### Trade Stats #####

updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades", inclZeroDays = FALSE)
print(data.frame(t(tStats[, -c(1:2)])))

dStats <- dailyStats(Portfolios = portfolio.st, use = "Equity")
rownames(dStats) <- gsub("\\.DailyEndEq", "", rownames(dStats))
print(data.frame(t(dStats[, -1])))

tmp <- TVI(Cl(XLB), period = 100)
# add_TA(tmp$vigor)
chart_Series(tmp$vigor)

portPL <- .blotter$portfolio.TVItrendFollowingLong$summary$Net.Trading.PL
(SharpeRatio.annualized(portPL, geometric=FALSE))

plot(cumsum(portPL)["2003::"])

##### Summary #####
#This program has shown a few things:
# 1) It is possible to create a trend-following strategy with a win percentage greater than that of 50%.
# 2) While the trend vigor indicator is a filter, it may be possible to put a dedicated trend-following filter 
# on top of it (such as the FRAMA–more on that in the future), to possibly get even better results.
# 3) This is (definitely) not a complete trading system. The exit logic definitely leaves something to be desired 
# (for instance, if the trend vigor reaches its maximum (2)–which seems to be every time, waiting for it to drop 
# under 1.4 is definitely suboptimal), and it seems more improvements can be made.

# There is more to investigate in the study of this indicator:
# 1) In this case, given the smoothness of the trend vigor, is it possible to be more aggressive with it?
# For instance, although it avoided the financial crisis completely, it did not re-enter the market until late 2009,
# missing a chunk of the trend, and furthermore, it also managed to give back most of those profits (but not all).
# Can this be rectified?
# 2) How does the strategy perform on the short end?
# 3) What can be done to keep strategies built on this indicator from "giving back open equity"?