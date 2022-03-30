#Hsieh, David A. and Fung, William,
#The Risk in Hedge Fund Strategies: Theory and Evidence from Trend Followers.
#The Review of Financial Studies, Vol. 14, No. 2, Summer 2001 .
#Available at SSRN: http://ssrn.com/abstract=250542
#http://faculty.fuqua.duke.edu/~dah7/DataLibrary/TF-Fac.xls

library("gdata")
library("quantmod")
library("PerformanceAnalytics")
library("FactorAnalytics")

require(gdata)
require(quantmod)
require(PerformanceAnalytics)
require(FactorAnalytics)

URL <- "http://faculty.fuqua.duke.edu/~dah7/DataLibrary/TF-Fac.xls"
#get xls sheet TF-Fac starting at the row with yyyymm
hsieh_factor <- read.xls(URL,sheet="TF-Fac",pattern="yyyymm",stringsAsFactors=FALSE)
hsieh_factor.clean <- hsieh_factor
#clean up date to get to yyyy-mm-dd
hsieh_factor.clean[,1] <- as.Date(paste(substr(hsieh_factor[,1],1,4),
                                        substr(hsieh_factor[,1],5,6),
                                        "01",sep="-"))
#remove percent sign and make numeric
hsieh_factor.clean[,2:6] <- apply(
  apply(hsieh_factor[,2:6],
        MARGIN=2,
        FUN=function(x) {gsub("%", "", x)}),
  MARGIN=2,
  as.numeric)/100

#get rid of NAs
hsieh_factor.clean <- hsieh_factor.clean[,1:6]

hsieh_factor.xts <- as.xts(hsieh_factor.clean[,2:6],order.by=hsieh_factor.clean[,1])

chart.CumReturns(hsieh_factor.xts, 
                 main="Hsieh and Fung Trend Following Factors",
                 xlab=NA,
                 legend.loc="topleft")
mtext(text="Source: http://faculty.fuqua.duke.edu/~dah7/DataLibrary/TF-Fac.xls",
      side=3,adj=0.10,outer=TRUE, col="purple",cex=0.75,line=-4)

chart.Correlation(hsieh_factor.xts,main="Hsieh and Fung Trend Following Factors")
mtext(text="Source: http://faculty.fuqua.duke.edu/~dah7/DataLibrary/TF-Fac.xls",
      side=1,adj=0.10,outer=TRUE, col="purple",cex=0.75,line=-1.5)


#get edhec data for sample factor analysis
data(edhec)
cta <- edhec[,1]
index(cta)=as.Date(format(index(cta),"%Y-%m-01"))
cta.factors <- na.omit(merge(cta,hsieh_factor.xts))
chart.RollingStyle(cta.factors[,1],cta.factors[,2:NCOL(cta.factors)],
                   width=36,
                   colorset=c("darkseagreen1","darkseagreen3","darkseagreen4","slateblue1","slateblue3","slateblue4"),
                   main="Edhec CTA by Trend Following Factors Rolling 36 Months")
mtext(text="Source: http://faculty.fuqua.duke.edu/~dah7/DataLibrary/TF-Fac.xls",
      side=1,adj=0.10,outer=TRUE, col="purple",cex=0.75,line=-5)

