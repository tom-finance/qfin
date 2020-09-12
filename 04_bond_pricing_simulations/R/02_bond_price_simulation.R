
# Program to Calculate potential Bond Prices with various term structures

# ------------------------------------------------------------------------------

# Attention:
# currently only working with positive interest rates!

# ------------------------------------------------------------------------------

library(readxl)
library(RQuantLib)
library(dplyr)

# ------------------------------------------------------------------------------

# load and clean data

setwd("C:/Users/User/Desktop/Bond_Pricing")

daten <- read_excel("Italian_BBB_Banks_YC_daily_2019_06_11.xlsx")[, -1] # exclude date!

# consider only positive rates --> exclude all negative rates
daten <- daten[daten$`3M` > 0, ]

# plot

matplot(daten, type = "l",  main = "Interest Rates")
abline(h = 0, col = "red", lwd = 2) # currently no negative interest rates...


# ------------------------------------------------------------------------------

# exctract all curves into large list to pass into bond pricing function

# are we having problems with NAs??

alle_kurven <- function(x) { 
  
  res <- list(d3m = x[1]/100, # convert in correct percentual format
              d6m = x[2]/100, 
              d1y = x[3]/100, 
              s2y = x[4]/100, 
              s3y = x[5]/100, 
              s4y = x[6]/100, 
              s5y = x[7]/100) 
  
  return(res) 
  
} 

all_curves <- apply(daten, MARGIN = 1, FUN = alle_kurven)

# ------------------------------------------------------------------------------

# bond <- list(settlementDays=0, # is this setting correct?
#              issueDate=as.Date("2018-10-31"), # this should be ok
#              faceAmount=100, # this should be ok
#              dayCounter='ActualActual', # should be ok!
#              paymentConvention='Unadjusted') # is this ok?
# 
# # check all of this in more detail...
# 
# schedule <- list(effectiveDate=as.Date("2018-10-31"), # ok?
#                  maturityDate=as.Date("2023-10-31"), # ok?
#                  period='Annual', # ok!
#                  calendar='Italy', #ok?
#                  businessDayConvention='Unadjusted',
#                  terminationDateConvention='Unadjusted',
#                  dateGeneration='Forward',
#                  endOfMonth=1)
# 
# calc=list(dayCounter='ActualActual',
#           compounding='Compounded',
#           freq='Annual',
#           durationType='Modified')
# 
# # insert coupon rates for cash flow
# coupon.rate <- c(0.014, 0.016, 0.018, 0.02, 0.022)
# 
# params <- list(tradeDate=as.Date('2019-06-11'), # what is this date?
#                settleDate=as.Date('2019-06-11'), # what is this date?
#                dt=.25, # what is this?
#                interpWhat="discount", # is this correct?
#                interpHow="spline") # whci method for interpolation is used?
# 
# # define evaluation date
# setEvaluationDate(as.Date("2019-06-13"))
# 
# 
# tsQuotes <- list(d3m = 0.00035,
#                  d6m = 0.00058,
#                  d1y = 0.0013,
#                  s2y = 0.00474,
#                  s3y = 0.0079,
#                  s4y = 0.01102,
#                  s5y = 0.01364) # problems with negative rates!!!
# 
# t <- seq(0, 4.75, 0.25) # for which t calculate discount curve - what is the impact of this paramter??
# 
# discountCurve <- DiscountCurve(params, tsQuotes, times = t)
# 
# 
# bond_value <- FixedRateBond(bond,
#                             coupon.rate,
#                             schedule,
#                             calc,
#                             discountCurve=discountCurve)


# ------------------------------------------------------------------------------

# function or bond price calculation

bond_price <- function(x, 
                       eval_date = as.Date("2019-06-13"), 
                       trade_date = as.Date("2019-06-11"), 
                       cf = c(0.014, 0.016, 0.018, 0.02, 0.022)) {
  
  bond <- list(settlementDays=0, # is this setting correct?
               issueDate=as.Date("2018-10-31"), # this should be ok
               faceAmount=100, # this should be ok
               dayCounter='ActualActual', # should be ok!
               paymentConvention='Unadjusted') # is this ok?
  
  # check all of this in more detail...
  
  schedule <- list(effectiveDate=as.Date("2018-10-31"), # ok?
                   maturityDate=as.Date("2023-10-31"), # ok?
                   period='Annual', # ok!
                   calendar='Italy', #ok?
                   businessDayConvention='Unadjusted',
                   terminationDateConvention='Unadjusted',
                   dateGeneration='Forward',
                   endOfMonth=1)
  
  calc=list(dayCounter='ActualActual',
            compounding='Compounded',
            freq='Annual',
            durationType='Modified')
  
  # insert coupon rates for cash flow
  coupon.rate <- cf
  
  params <- list(tradeDate=trade_date, # what is this date?
                 settleDate=trade_date, # what is this date?
                 dt=.25, # what is this?
                 interpWhat="discount", # is this correct?
                 interpHow="spline") # whci method for interpolation is used?
  
  # define evaluation date
  setEvaluationDate(eval_date)
  
  tsQuotes <- x
  
  t <- seq(0, 4.75, 0.25) # for which t calculate discount curve - what is the impact of this paramter??
  
  discountCurve <- DiscountCurve(params, tsQuotes, times = t)
  
  bond_value <- FixedRateBond(bond,
                              coupon.rate,
                              schedule,
                              calc,
                              discountCurve=discountCurve)
  
  bond_price <- bond_value[[2]]
  
  return(bond_price)
  
}


# ------------------------------------------------------------------------------

# compute results (here I have to exclude all negative rates...)
# curves are stored in large list "all_curves"

result <- sapply(all_curves, bond_price)

# compute future price paths with all available interest rate term structures!

result_1y <- sapply(all_curves, bond_price, 
                    eval_date = as.Date("2020-06-13"), 
                    trade_date = as.Date('2020-06-11'))

result_2y <- sapply(all_curves, bond_price, eval_date = as.Date("2021-06-13"), trade_date = as.Date('2021-06-11'))

result_3y <- sapply(all_curves, bond_price, eval_date = as.Date("2022-06-13"), trade_date = as.Date('2022-06-11'))

result_4y <- sapply(all_curves, bond_price, eval_date = as.Date("2023-06-13"), trade_date = as.Date('2023-06-11'))

# construct result

finale <- rbind(rep(101, length(result)),
                result,
                result_1y,
                result_2y,
                result_3y,
                result_4y,
                rep(100, length(result)))

# some row-wise descriptive statistics to compute expectation

stats <- apply(finale, 
               MARGIN = 1, 
               FUN = function(x) cbind(mean = mean(x),
                                                  sd = sd(x),
                                                  median = median(x)))


# price expectation considering all past yield curves
matplot(t(stats)[, -2], x = 0:6, type = "l", main = "E[P(t, T)]", ylab = "P", xlab = "t")
abline(h = 100, col = "blue")

# ------------------------------------------------------------------------------

# plot results

# extract sample for plot

set.seed(1234)
data_sample <- finale[, sample(1:820, 150, replace = FALSE)]

matplot(data_sample, x = 0:6, type = "l", ylab = "P(t, T)", xlab = "t",
        main = "possible Bond Price Evolution", lwd = 1)
abline(h = 100, col = "red", lwd = 2)

# ------------------------------------------------------------------------------