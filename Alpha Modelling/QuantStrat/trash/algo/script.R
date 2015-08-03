require(quantmod)
require(PerformanceAnalytics)
getSymbols(c("SPY", "TLT"), from="1990-01-01")
returns <- merge(Return.calculate(Ad(SPY)), Return.calculate(Ad(TLT)), join='inner')
returns <- returns[-1,]
configs <- list()
for(i in 1:21) {
  weightSPY <- (i-1)*.05
  weightTLT <- 1-weightSPY
  config <- Return.portfolio(R = returns, weights=c(weightSPY, weightTLT), rebalance_on = "months")
  configs[[i]] <- config
}
configs <- do.call(cbind, configs)
cumRets <- cumprod(1+configs)
period <- 72

roll72CumAnn <- (cumRets/lag(cumRets, period))^(252/period) - 1
roll72SD <- sapply(X = configs, runSD, n=period)*sqrt(252)


sd_f_factor <- 2.5
modSharpe <- roll72CumAnn/roll72SD^sd_f_factor
monthlyModSharpe <- modSharpe[endpoints(modSharpe, on="months"),]

findMax <- function(data) {
  return(data==max(data))
}

weights <- t(apply(monthlyModSharpe, 1, findMax))
weights <- weights*1
weights <- xts(weights, order.by=as.Date(rownames(weights)))
weights[is.na(weights)] <- 0
weights$zeroes <- 1-rowSums(weights)
configs$zeroes <- 0



stratRets <- Return.portfolio(R = configs, weights = weights)
rbind(table.AnnualizedReturns(stratRets), maxDrawdown(stratRets))
charts.PerformanceSummary(stratRets)


rbind(table.AnnualizedReturns(stratRets), maxDrawdown(stratRets))


stratAndComponents <- merge(returns, stratRets, join='inner')
charts.PerformanceSummary(stratAndComponents)
rbind(table.AnnualizedReturns(stratAndComponents), maxDrawdown(stratAndComponents))
apply.yearly(stratAndComponents, Return.cumulative)


weightSPY <- apply(monthlyModSharpe, 1, which.max)
weightSPY <- do.call(rbind, weightSPY)
weightSPY <- (weightSPY-1)*.05
align <- cbind(weightSPY, stratRets)
align <- na.locf(align)
chart.TimeSeries(align[,1], date.format="%Y", ylab="Weight SPY", main="Weight of SPY in SPY-TLT pair")

