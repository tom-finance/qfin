
# Bond Valuation with sophisticated bond pricing engine from QuantLib

# ------------------------------------------------------------------------------

# Source: https://rdrr.io/cran/RQuantLib/man/FixedRateBond.html

# Check: https://rdrr.io/cran/RQuantLib/man/DiscountCurve.html


# package manual
# https://cran.r-project.org/web/packages/RQuantLib/RQuantLib.pdf

# ------------------------------------------------------------------------------

library(RQuantLib) # sophisticated bond pricing tool


# goal is to value this bond:
# http://www.himtf.com/sites/himtf/files/isin_schede/IT0005347304.pdf

# ------------------------------------------------------------------------------

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
coupon.rate <- c(0.014, 0.016, 0.018, 0.02, 0.022)

params <- list(tradeDate=as.Date('2019-06-11'), # what is this date?
               settleDate=as.Date('2019-06-11'), # what is this date?
               dt=.25, # what is this?
               interpWhat="discount", # is this correct?
               interpHow="spline") # whci method for interpolation is used?

# define evaluation date
setEvaluationDate(as.Date("2019-06-13"))

# ------------------------------------------------------------------------------

# Valuation with flat curve (only to show how the calculation engine works)

discountCurve.flat <- DiscountCurve(params, list(flat=0.05))
FixedRateBond(bond,
              coupon.rate,
              schedule,
              calc,
              discountCurve=discountCurve.flat) # here we have a flat curve!

# ------------------------------------------------------------------------------

#Same bond with a discount curve constructed from market quotes

# Enter here the current market quotes (As Of: 11/06/2019) - is this the correct way???

tsQuotes <- list(d3m = 0.00035,
                 d6m = 0.00058,
                 d1y = 0.0013,
                 s2y = 0.00474,
                 s3y = 0.0079,
                 s4y = 0.01102,
                 s5y = 0.01364)

t <- seq(0, 4.75, 0.25) # for which t calculate discount curve - what is the impact of this paramter??

# clean vs. dirty price --> https://www.investopedia.com/terms/c/cleanprice.asp

discountCurve <- DiscountCurve(params, tsQuotes, times = t)

bond_value <- FixedRateBond(bond,
              coupon.rate,
              schedule,
              calc,
              discountCurve=discountCurve)

# ------------------------------------------------------------------------------

# static shift in the yield curve and full revaluation of the bond

shock_static <- lapply(tsQuotes, FUN = function(x) x + 0.01)


discountCurve_shock <- DiscountCurve(params, shock_static, times = t)

bond_value_shock <- FixedRateBond(bond,
                            coupon.rate,
                            schedule,
                            calc,
                            discountCurve=discountCurve_shock)

# ------------------------------------------------------------------------------
