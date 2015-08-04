##### Information #####
# Trend Vigor Part III: ATR position sizing, Annualized Sharpe above 1.4, and Why Leverage Is Pointless
# Posted on May 29, 2014 by Ilya Kipnis
# Posted in Dr. John Ehlers, ETFs, QuantStrat, R, Trading, Uncategorized
# https://quantstrattrader.wordpress.com/2014/06/11/trend-vigor-part-iii-atr-position-sizing-annualized-sharpe-above-1-4-and-why-leverage-is-pointless/
# https://github.com/IlyaKipnis

# http://www.followingthetrend.com/2014/01/why-leverage-is-pointless/

##### Initialisation #####
setwd("~/Alpha Modelling/QuantStrat/Strategies/Trading")
require(DSTrading)
require(IKTrading)
require(quantstrat)

initDate="1990-01-01"
from="2003-01-01"
to="2010-12-31"

source("demoData.R") #contains all of the data-related boilerplate.

##### Correlations #####
tmp <- list()
length(tmp) <- length(symbols)
for (i in 1:length(symbols)) {
  tmp[[i]] <-Cl(get(symbols[i]))
}
tmp <- do.call(cbind, tmp)
baseCors <- cor(tmp)
diag(baseCors) <- NA
instrumentAverageBaseCors <- rowMeans(baseCors, na.rm=TRUE)
names(instrumentAverageBaseCors) <- gsub(".Close", "", names(instrumentAverageBaseCors))
instrumentAverageBaseCors
(grandMeanBaseCors <- mean(instrumentAverageBaseCors))

##### Strategy implementation #####
# Trade sizing and initial equity settings
tradeSize <- 10000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "TVI_osATR"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

# Parameters (trigger lag unchanged, defaulted at 1)
delta=0
period=20
pctATR=.02 #control risk with this parameter
#pctATR=.04

# Indicators
add.indicator(strategy.st, name="TVI", arguments=list(x=quote(Cl(mktdata)), period=period, delta=delta), label="TVI")
add.indicator(strategy.st, name="lagATR", arguments=list(HLC=quote(HLC(mktdata)), n=period), label="atrX")

# Signals
add.signal(strategy.st, name="sigThreshold", 
           arguments=list(threshold=1, column="vigor.TVI", relationship="gte", cross=FALSE),
           label="TVIgtThresh")
add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("vigor.TVI","trigger.TVI"), relationship="gt"),
           label="TVIgtLag")
add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("TVIgtThresh","TVIgtLag"), cross=TRUE),
           label="longEntry")
add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("vigor.TVI","trigger.TVI"), relationship="lt"),
           label="longExit")

# Rules
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open", osFUN=osDollarATR,
                        tradeSize=tradeSize, pctATR=pctATR, atrMod="X"), 
         type="enter", path.dep=TRUE)
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open"), 
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
(aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses))
(aggCorrect <- mean(tStats$Percent.Positive))
(numTrades <- sum(tStats$Num.Trades))
(meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio))

# Daily stats
dStats <- dailyStats(Portfolios = portfolio.st, use="Equity")
rownames(dStats) <- gsub(".DailyEndEq","", rownames(dStats))
print(data.frame(t(dStats)))

# Portfolio cash PL
portPL <- .blotter$portfolio.TVI_osATR$summary$Net.Trading.PL

# Cash Sharpe
(SharpeRatio.annualized(portPL, geometric=FALSE))

# Portfolio comparisons to SPY
instRets <- PortfReturns(account.st)

# Correlations
instCors <- cor(instRets)
diag(instRets) <- NA
corMeans <- rowMeans(instCors, na.rm=TRUE)
names(corMeans) <- gsub(".DailyEndEq", "", names(corMeans))
print(round(corMeans,3))
mean(corMeans)

# Graph
cumPortfRets <- cumprod(1+portfRets)
firstNonZeroDay <- index(portfRets)[min(which(portfRets!=0))]
getSymbols("SPY", from=firstNonZeroDay, to="2010-12-31")
SPYrets <- diff(log(Cl(SPY)))[-1]
cumSPYrets <- cumprod(1+SPYrets)
comparison <- cbind(cumPortfRets, cumSPYrets)
colnames(comparison)  <- c("strategy", "SPY")
chart.TimeSeries(comparison, legend.loc = "topleft", 
                 main=paste0("Period=", period, ", Delta=",delta), colors=c("green","red"))


# Sharpe based on returns, annualized returns, and max drawdown
SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

# Individual instrument equity curve
chart.Posn(portfolio.st, "XLB")
# The triggerLag is NOT 30 for the strategy, just amplified in this case to illustrate exit logic.
# The actual trigger lag is defaulted at 1.
tmp <- TVI(Cl(XLB), period=period, delta=delta, triggerLag=1)
add_TA(tmp$vigor, lwd=3)
add_TA(tmp$trigger, on=5, col="red", lwd=1.5)
tmp2 <- lagATR(HLC=HLC(XLB), n=period)
add_TA(tmp2$atr, col="blue", lwd=2)


##### Summary #####
# Essentially, 4% annualized return for a max drawdown of nearly 10%, with a Sharpe Ratio nowhere close to 1.
# Definitely not the greatest strategy, but nevertheless, compared to SPY, far less volatile, when comparing equity curves.
# Once again, however, keep in mind that currently, the Trend Vigor is being used as a market mode indicator,
# rather than a dedicated trend follower. Keep that in the back of your mind as you look
# at the value of the actual indicator over time.

# Results for delta = 0.05
# This time, while you take on more risk, your returns are definitely better?essentially keeping pace with SPY 
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
# "stay out of the market in which there's no trend?, and this variant essentially leans towards the idea of 
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
# What's also worth noting is that the strategy attempted to go "dumpster diving", to put it kindly?that is, 
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