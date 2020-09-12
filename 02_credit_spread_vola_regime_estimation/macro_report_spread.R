
# Visualisation of Volatility Regimes for Italian Credit Spread

# ------------------------------------------------------------------------------

# idea is based on the following article:
# http://keplerianfinance.com/2013/06/the-relevance-of-history/

# read data

data <- read.csv("test.csv", stringsAsFactors = FALSE)

data$Date <- as.Date(data$Date, "%Y-%m-%d")

# ------------------------------------------------------------------------------

# PLOT FÜR DEN TEIL ZUM MARKTRISIKO 
# (CREDIT SPREAD RISK FOR SOVEREIGN BONDS IN ITALY)

# show how credit spread changes over time affect bond prices

par(mar = c(4,4,3,3))

par(mfrow = c(2,1))

plot(data$close ~ data$Date, type = "l", col = "purple", main = "BTP-Bund Spread")
grid(col = "mediumpurple1")


plot(diff(log(data$close), 1) ~ data$Date[-1], type = "l", col = "purple", 
     main = "BTP-Bund Spread LOG RETURNS", xlab = "Zeit",
     ylab = "log-Rendite")
grid(col = "mediumpurple1")
abline(h = max(diff(log(data$close), 1)), lwd = 1, col = "red", lty = 2)
abline(h = min(diff(log(data$close), 1)), lwd = 1, col = "red", lty = 2)


# mean return over the observed time frame is approximately zero (slightly negative though...)
mean(diff(log(data$close), 1))
median(diff(log(data$close), 1))

# plot squared returns as volatility proxy

plot(diff(log(data$close), 1)^2 ~ data$Date[-1], type = "l", col = "red", main = "BTP-Bund Spread LOG RETURNS")

# plot absolute returns as additional volatility proxy

plot(abs(diff(log(data$close), 1)) ~ data$Date[-1], type = "l", col = "red", main = "BTP-Bund Spread LOG RETURNS")

summary(diff(log(data$close), 1))

par(mfrow = c(1,1))


hist(diff(log(data$close), 1), breaks = 35, main = "Histogramm", col = "red")

# price of bonds? --> inverse relationship between yield and price

# how to scale a daily variance proxy to an annual variance proxy for that day?
# use "square root of time" rule? V(x)_{annual} = (V(x)*250^0.5)^0.5

# ------------------------------------------------------------------------------

# GARCH MODELLING - COULD BE AN ALTERNATIVE??

# returns <- diff(log(data$close), 1)
# 
# install.packages("rugarch")
#  
# library(rugarch)
# data(sp500ret)
# spec <- ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)), distribution = 'std')
# fit <- ugarchfit(spec, returns, solver = 'hybrid')
# plot(fit)

# ------------------------------------------------------------------------------

# some additional sources:

# https://quant.stackexchange.com/questions/7000/squared-and-absolute-returns
# https://stats.stackexchange.com/questions/213395/using-absolute-value-vs-variance-in-garch

# formula of variance: V(x) = E[x^2] -  E[x]^2 --> if E[x]^2 is assumed to be zero --> V(x) = E[x^2]

# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

# calculate log returns
returns <- diff(log(data$close), 1)

# histogram of log returns
hist(returns, breaks = 50)

# plot cumulative log return
plot(cumsum(returns), type = "l")

returns_squared <- returns^2

plot(cumsum(returns_squared) ~ data$Date[-1], type = "l", 
     xlab = "Zeit", ylab = "kumulativer squared return", col = "purple",
     lwd = 1)

# create data frame with cumulated squared returns

dati <- data.frame(y = cumsum(returns_squared),
                   x = 1:length(returns_squared),
                   stringsAsFactors = F)

# plot cumulative squared returns as function of "time"
plot(dati$y ~ dati$x, type = "l", xaxt="n")
axis(1, at = seq(0, 1683, by = 50), las=2)

# ------------------------------------------------------------------------------

# fit piecewise model

library(segmented)

out.lm <- lm(y ~ x, data = dati)


o <- segmented(out.lm, seg.Z = ~x, psi = list(x = c(275, 500, 550, 700, 710, 800, 825, 1200, 1210, 1310, 1330)),
               control = seg.control(display = FALSE)
)

####

dat2 = data.frame(x = dati$x, y = broken.line(o)$fit)

library(ggplot2)


ggplot(dati, aes(x = x, y = y)) +
  geom_line(data = dat2, color = 'blue') +
  theme_classic()


ggplot(dati, aes(x = x, y = y)) +
  geom_point(size = 4, color = "black") +
  geom_line(data = dat2, color = "yellow", size = 0.5) +
  theme_classic()




# extract slope --> this the correct way??
coefficients <- o$coefficients[3:7]

slope_coef <- slope(o)[[1]]

# annualize

sd_annual <- (coefficients*250)^0.5 # not possible!

var_annual <- (slope_coef[, 1] * 250)^0.5

# vector with identified regime changes
regimes <- c(275, 500, 550, 700, 710, 800, 825, 1200, 1210, 1310, 1330)


# collect results
res <- c(rep(var_annual[1], 275),
        rep(var_annual[2], 225),
        rep(var_annual[3], 50),
        rep(var_annual[4], 150),
        rep(var_annual[5], 10),
        rep(var_annual[6], 90),
        rep(var_annual[7], 25),
        rep(var_annual[8], 375),
        rep(var_annual[9], 10),
        rep(var_annual[10], 100),
        rep(var_annual[11], 20),
        rep(var_annual[12], 353))

# ------------------------------------------------------------------------------

# plot results

# some data preperation for plot
y2 <- rep(res, each=2)
y2 <- y2[-length(y2)]
x2 <- rep(data$Date[-1], each=2)[-1]
x3 <- c(min(x2), x2, max(x2))
y3 <- c(0, y2, 0)

# plot with filled area under the function
plot(data$Date[-1], res, ylim=c(0, max(res)), type="n",
     xlab = "Zeit", ylab = "annualisierte Volatilität",
     main = "ITA Gov Yield Credit Spread Volatility Regimes")
grid(col = "mediumpurple1")
polygon(x3, y3, border=NA, col="mediumpurple1")
lines(x2, y2, lwd = 2, col = "purple")

# plot as "step function"
plot(res ~ data$Date[-1], type = "s", lwd = 2, ylim=c(0, max(res)),
     xlab = "Zeit", ylab = "annualisierte Volatilität",
     main = "ITA Gov Yield Credit Spread Volatility Regimes",
     col = "purple")
grid(col = "mediumpurple1")

# ------------------------------------------------------------------------------