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