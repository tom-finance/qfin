################################################################################
# CDS Spread and CDS Spread implied PD as Model Factors
################################################################################

data_rat_cds <- read_html("http://www.worldgovernmentbonds.com/sovereign-cds/") %>%
  html_table(fill = TRUE)
data_rat_cds <- data_rat_cds[[1]]

data_rat_cds <- data_rat_cds[-1, c(2,4,7)]
names(data_rat_cds) <- c("Country", "CDS Spread", "implied PD")

# correct representation of implied PD
data_rat_cds$`implied PD` <- as.numeric(gsub(pattern = "%", "", data_rat_cds$`implied PD`))/100

data_rat_cds$`CDS Spread` <- as.numeric(data_rat_cds$`CDS Spread`)


test <- data_rat

hoi <- left_join(test,
                 data_rat_cds)

hoi <- hoi[complete.cases(hoi), ]

# check: what happens with NA in lm model??

##########################

# model with implied PD as explanatory variable...

# plot yield spread as "function" of rating by using a linear model
plot(`Spread vsBund` ~ `implied PD`, data = hoi, col = "blue",
     pch = 19, cex = 1, lty = "solid", lwd = 1, main = "Yield Spread vs. Credit Rating (S&P Ratings)",
     ylab = "Spread vs. Germany 10Y", xlab = "Credit Rating")
text(hoi$`Spread vsBund` ~ hoi$`implied PD`, labels=hoi$Country, cex= 0.4, pos=3)
model1 <- lm(`Spread vsBund` ~ `implied PD`, data = hoi)
abline(model1, col = "orange", lwd = 2, lty = 3)

############################

# model with CDS spread as explanatory variable

plot(`Spread vsBund` ~ `CDS Spread`, data = hoi, col = "blue",
     pch = 19, cex = 1, lty = "solid", lwd = 1, main = "Yield Spread vs. Credit Rating (S&P Ratings)",
     ylab = "Spread vs. Germany 10Y", xlab = "Credit Rating")
text(hoi$`Spread vsBund` ~ hoi$`CDS Spread`, labels=hoi$Country, cex= 0.4, pos=3)
model1 <- lm(`Spread vsBund` ~ `CDS Spread`, data = hoi)
abline(model1, col = "orange", lwd = 2, lty = 3)

###########################

# some basic plotting...
plot(log(data_rat_cds$`CDS Spread`),
     log(data_rat_cds$`implied PD`))

################################################################################