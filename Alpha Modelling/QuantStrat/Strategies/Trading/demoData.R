# These symbols represents a fairly broad aspect of both the U.S. economy and other developed economies abroad
# (E.G. Japan, Germany), and contain a fair amount of "staple" ETFs (SPDR sectors, EFA, internationals, etc.).

options("getSymbols.warning4.0"=FALSE)
rm(list=ls(.blotter), envir=.blotter)
initDate='1990-12-31'

currency('USD')
Sys.setenv(TZ="UTC")

symbols <- c("XLB", #SPDR Materials sector
             "XLE", #SPDR Energy sector
             "XLF", #SPDR Financial sector
             "XLP", #SPDR Consumer staples sector
             "XLI", #SPDR Industrial sector
             "XLU", #SPDR Utilities sector
             "XLV", #SPDR Healthcare sector
             "XLK", #SPDR Tech sector
             "XLY", #SPDR Consumer discretionary sector
             "RWR", #SPDR Dow Jones REIT ETF
             
             "EWJ", #iShares Japan
             "EWG", #iShares Germany
             "EWU", #iShares UK
             "EWC", #iShares Canada
             "EWY", #iShares South Korea
             "EWA", #iShares Australia
             "EWH", #iShares Hong Kong
             "EWS", #iShares Singapore
             "IYZ", #iShares U.S. Telecom
             "EZU", #iShares MSCI EMU ETF
             "IYR", #iShares U.S. Real Estate
             "EWT", #iShares Taiwan
             "EWZ", #iShares Brazil
             "EFA", #iShares EAFE
             "IGE", #iShares North American Natural Resources
             "EPP", #iShares Pacific Ex Japan
             "LQD", #iShares Investment Grade Corporate Bonds
             "SHY", #iShares 1-3 year TBonds
             "IEF", #iShares 3-7 year TBonds
             "TLT" #iShares 20+ year Bonds
)

#SPDR ETFs first, iShares ETFs afterwards
if(!"XLB" %in% ls()) { 
  suppressMessages(getSymbols(symbols, from="2003-01-01", to="2010-12-31", src="yahoo", adjust=TRUE))  
}

stock(symbols, currency="USD", multiplier=1)