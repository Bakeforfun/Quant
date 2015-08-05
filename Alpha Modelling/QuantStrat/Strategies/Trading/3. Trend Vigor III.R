##### Information #####
# Trend Vigor Part III: ATR position sizing, Annualized Sharpe above 1.4, and Why Leverage Is Pointless
# Posted on June 11, 2014 by Ilya Kipnis
# Posted in Dr. John Ehlers, ETFs, QuantStrat, R, Trading
# https://quantstrattrader.wordpress.com/2014/06/11/trend-vigor-part-iii-atr-position-sizing-annualized-sharpe-above-1-4-and-why-leverage-is-pointless/
# https://github.com/IlyaKipnis

# http://www.followingthetrend.com/2014/01/why-leverage-is-pointless/

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

##### What's new? #####
# By tuning the delta parameter, it's possible to tune how fast Trend Vigor oscillates, and thereby,
# how fast it judges that newer observations are part of a new trend
# (but at the same time, how much longer the indicator classifies a trend as ongoing).
# This would allow a user to tune the Trend Vigor indicator in a similar fashion as other trend following indicators
# to capture different sorts of market phenomena, such as long term trend-following/filtering, to shorter-term phenomena.
# On the downside, this means that as a user tunes the period and the delta, that he or she could easily overfit.

# Furthermore, this program is going to make the portfolio analysis portion of the analytics make far more sense
# than a roughly-sketched, price-weighted equity curve. The results were that the benefits of diversification 
# were certainly less than expected. This backtest is going to implement an equal-weight asset allocation scheme,
# which will also showcase that quantstrat can implement custom order-sizing functions.

##### Strategy (updated) #####
# The indicator will be the trend vigor calculation,
# and the strategy will go long on the next open not only when the indicator crosses above 1,
# but also if it should cross over its one-day lagged computation, but still be above 1;
# sell the next open when it crosses under its one-day lagged trigger;
# and will stop out on the next open when the trend vigor crosses under 1
# if it never crossed 1.4 within the duration of the position.

# This is so that the strategy would stay in what it perceives as a trend, rather than sell and not come back
# until the computation dipped back below 1 (an arbitrary value, to be sure) and came back up again.
# The idea for the exit came from one of Dr. Ehlers's presentations, in which he presented several more
# trend-following smoothers, some of which are already in the DSTrading package
# (FRAMA, KAMA, VIDYA which I call VIDA, and the Ehlers filters).

##### Syntax #####
# add.indicator/add.signal/add.rule is a function which takes in the name of the strategy as the first argument 
# (which I always use strategy.st for), a name of an R function in the name argument, the arguments to the function
# as the argument to arguments (that is, arguments=list(x=quote(Cl(mktdata)), period=100, delta=0.2) is the argument 
# to the TVI function which is the value for the name argument in the add.indicator function), and finally, a label. 
# While the labels may seem like so much window dressing, they are critical in terms of linking indicators to signals,
# signals to rules, and everything to any optimization/robustness testing you may decide to do. Do not forget them.

# sigAND function is a basic AND operator for signals in quantstrat. While quantstrat itself has a signal function
# called sigFormula, I myself am not exactly a fan of it.

# osMaxDollar function a strategy to control positions by their total current dollar value. 
# What it doesn't do, however, is remember the initial dollar value of a position. That is, if you invested $10,000, 
# with a limit of $20,000 in a position, and your initial position went to $11,000, this function would only 
# allow up to another $9,000 worth of the instrument to be ordered. Negative quantities should be used for shorting.
# That is, if the goal is to short a security, rather than input 20000 as the maxSize and 10000 as the tradeSize,
# the parameters should be -20000 and -10000, respectively.
# Again, the order sizing function takes into account the value of the position at the time of a new order,
# not the original value of the transaction. That is, the function does not do this: 
#"You have an existing position that you entered into at some previous date, and your position then was $10,000,
# but now it's $15,000, but I'll order another $10,000 anyway". The function currently does this:
# "You have a position with a current market value of $15,000. You want to add $10,000 to your position, 
# but have a limit of $20,000. I'll order the nearest integer quantity to the difference ($5,000).”

##### Notes #####
# Trend Vigor indicator was changed to backfill any leading NA's as zero, since this is an indicator centered around zero. 
# This will allow the strategy to instantly jump into the middle of a trend, if one was under way 
# the moment the first true value of the indicator was computed.

# Delta is a parameter involved in an involved trigonometric calculation, lending to the idea 
# that there may be some sort of circular relationship with delta, rather than simply a "trend sensitivity" as I described.
# That stated, considering that the default value that Dr. Ehlers used was .2, it implies that the parameter 
# should probably be held between 0 and 1. Once beyond that, there's no guarantee that the algorithm even runs 
# (at some point, it will just make later computations come out as NaNs).

# We can see that the equal dollar weighting scheme may not have been the greatest. 
# For instance, the gross profits (that is, total dollars gained, not net profit) vary wildly,
# which means that either a few instruments were just fortunate, or more likely, that $10,000 in different instruments 
# buys different levels of risk. Ideally, with trend following (or any investment strategy, for that matter),
# order sizing should be done with some measure of volatility in mind. Jeff Swanson of System Trader Success 
# uses position sizing scaled with ATR, for instance.

##### Initialisation #####
setwd("~/Alpha Modelling/QuantStrat/Strategies/Trading")
require(DSTrading)
require(IKTrading)
require(quantstrat)

source("demoData.R") #contains all of the data-related boilerplate.

##### Strategy implementation #####
# Trade sizing and initial equity settings
tradeSize <- 10000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "TVI_TF_2"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, 
         currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

# Parameters (trigger lag unchanged, defaulted at 1)
delta=.2
#delta=.05
#delta=0
period=100

# Indicators
add.indicator(strategy.st, name="TVI", arguments=list(x=quote(Cl(mktdata)), 
                                                      period=period, delta=delta), label="TVI")

# Signals
add.signal(strategy.st, name="sigThreshold", 
           arguments=list(threshold=1, column="vigor.TVI", 
                          relationship="gte", cross=FALSE),
           label="TVIgtThresh")
add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("vigor.TVI","trigger.TVI"), 
                          relationship="gt"),
           label="TVIgtLag")
add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("TVIgtThresh","TVIgtLag"), 
                          cross=TRUE),
           label="longEntry")
add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("vigor.TVI","trigger.TVI"), 
                          relationship="lt"),
           label="longExit")

# Rules
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, orderqty=100, 
                        ordertype="market", orderside="long", 
                        replace=FALSE, prefer="Open", osFUN=osMaxDollar,
                        tradeSize=10000, maxSize=10000), 
         type="enter", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", 
                        ordertype="market", orderside="long", replace=FALSE, 
                        prefer="Open"), 
         type="exit", path.dep=TRUE)

##### Strategy application #####
t1 <- Sys.time()
out <- applyStrategy(strategy=strategy.st,portfolios=portfolio.st)
t2 <- Sys.time()
print(t2-t1)

##### Analytics #####
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)

# Trade stats
tStats <- tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=FALSE)
tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)])))

summary(tStats$Gross.Profit)

# Daily stats
dStats <- dailyStats(Portfolios = portfolio.st, use="Equity")
rownames(dStats) <- gsub(".DailyEndEq","", rownames(dStats))
print(data.frame(t(dStats)))

# Portfolio cash PL
portPL <- .blotter$portfolio.TVI_TF_2$summary$Net.Trading.PL

# Cash Sharpe
(SharpeRatio.annualized(portPL, geometric=FALSE))

# Graph
instRets <- PortfReturns(account.st)
portfRets <- xts(rowMeans(instRets)*ncol(instRets), order.by=index(instRets))
cumPortfRets <- cumprod(1+portfRets)-1
firstNonZeroDay <- index(portfRets)[min(which(portfRets!=0))]
getSymbols("SPY", from=firstNonZeroDay, to="2010-12-31")
SPYrets <- diff(log(Cl(SPY)))[-1]
cumSPYrets <- cumprod(1+SPYrets)-1
comparison <- cbind(cumPortfRets, cumSPYrets)
colnames(comparison)  <- c("strategy", "SPY")
chart.TimeSeries(comparison, legend.loc = "topleft", 
                 main=paste0("Period=", period, ", Delta=",delta))


# Sharpe based on returns, annualized returns, and max drawdown
SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

chart.Posn(portfolio.st, "XLB")
tmp <- TVI(Cl(XLB), period=period, delta=delta, triggerLag=30)
add_TA(tmp$vigor)
add_TA(tmp$trigger, on=5, col="red")

# Some other stats
mean(tStats$Percent.Positive)
(aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses))
(aggCorrect <- mean(tStats$Percent.Positive))
(numTrades <- sum(tStats$Num.Trades))
(meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio))


##### Summary #####
# Essentially, 4% annualized return for a max drawdown of nearly 10%, with a Sharpe Ratio nowhere close to 1.
# Definitely not the greatest strategy, but nevertheless, compared to SPY, far less volatile, when comparing equity curves.
# Once again, however, keep in mind that currently, the Trend Vigor is being used as a market mode indicator,
# rather than a dedicated trend follower. Keep that in the back of your mind as you look
# at the value of the actual indicator over time.

# Results for delta = 0.05
# This time, while you take on more risk, your returns are definitely better–essentially keeping pace with SPY 
# in its bullish phases while missing the financial crisis (as any decent trend-follower does).
# That stated, the drawdowns between this strategy and SPY happen at similar times, 
# meaning that given that there are more equity indices than the SPY (as well as bond indices), 
# that position sizing for the strategy can be improved for diversification.

# Results for delta = 0
# At this point, profit factors become obscene, and even the aggregate profit factor 
# (sum of all gross profits divided by the negative sum of all gross losses) clocks in above 9, 
# with the average percentage correct being above 70% (mean of the percent positive). 
# In reality, this turns Trend Vigor into an up-or-down classifier (to use some machine-learning terminology),
# with no-in betweens, as you'll see in a moment.

# As you can see, as the delta parameter becomes smaller and smaller, the sensitivity to a trend increases. 
# At the limit, it essentially becomes akin to an either-or classifier. 
# So basically, for those with the statistics backgrounds (and if you've understood everything thus far, you have one), 
# then the confusion matrix becomes "go long in a trend", "go long without a trend", "don't go long but miss a trend",
# "stay out of the market in which there's no trend”, and this variant essentially leans towards the idea of 
# "I have a slight hunch there's a trend. Oh well. That's enough! Time to buy!" 
# And so long as so much as even a hunch persists, the strategy will stay long.

# Results for a period of 20, and a delta of 0
# This changes the characteristics of the strategy in terms of what trade statistics considerably. 
# It sacrifices the win-over-the-long-haul philosophy in favor of a style more akin to spray-and-pray,
# rat-ta-tat-tat, or twitch trading.

# In other words, over nearly a thousand relatively short-term momentum trades (can you say "whipsaw"?),
# the strategy still gets it right more than half the time, and the trades it *does* get right, on average, 
# it gets them right by a ratio of 2.5 to 1. Of course, this backtest doesn't take transaction costs into account,
# and when dealing with these shorter-term strategies, if you're a retail investor out to try and rub two pennies together
# and you're getting taken for a ride on the order of $10 per buy or sell order from ETrade, 
# you'll have to look at the average trade P&L. (Also, if you're a retail investor, 
# your bigger obstacle would be the $3,000,000 to be able to even trade something like this.)

# lot of the cash Sharpe ratios (now in the *daily* statistics, not the trade statistics) are nearing 1. 
# Not exactly spectacular, by any stretch of the imagination, when considering a short-term trading strategy
# before commissions/slippage, but definitely nothing to scoff at. Something I notice just by eyeballing the statistics 
# is that there definitely seems to be an edge in the percentage of days positive. 
# Of course, I won't rule out the fact that all of this may simply be the fact that many of these securities 
# are equity indices, and thus, may have a slightly inherent long bias in them.

# Looking at the equity curve, the first thing to note is that it's strikingly similar to the SPY equity curve 
# up until the crisis, and the drawdowns happen at about the same exact times. Maybe this means that 
# all of my global equity ETFs that I chose for the data were correlated because at the end of the day, 
# aside from the few fixed income ETFs available before 2003, that they're still separate slices of 
# the equities asset class, or that my volatility (and returns?) are mainly driven by the 9 XL sectors. 
# What's also worth noting is that the strategy attempted to go "dumpster diving", to put it kindly–that is, 
# still try to find those positive-momentum trades in the depths of the financial crisis. 
# While it certainly seemed to try, in this case, the best move would have been to do nothing at all. 
# However, when the market rebounded, the strategy quickly made up its losses (and then some).

# Portfolio statistics: an 11% return, at a maximum drawdown of 18%. I am of course, still not pleased with
# the last two numbers, since a maximum drawdown larger than the annualized return means that it's quite possible to 
# simply have a down year. However, the idea of an 18% drawdown while seemingly keeping pace with an S&P 500 
# (with a simplistic asset allocation and order-sizing strategy, no less) in its good years is nothing to scoff at. 

# Conclusion
# By adjusting the period and delta parameters, it's possible to take what was originally a market mode filter and:
# A) keep it as a double-purpose trend follower and market mode indicator
# B) turn it into a long-term trend indicator
# C) turn it into a short(er) term momentum trader.

# Paradoxically (at least according to the basic risk/reward blabber found in typical academic finance), 
# it seems that by lowering delta and having a willingness to take on "more" risk in the form of greater market exposure,
# one actually achieves a better annualized Sharpe Ratio than with the default, more risk-averse, setting.