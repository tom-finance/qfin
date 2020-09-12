################################################################################
# Paper on Credit Ratings, Yield Spreads, CDS and implied Default Probabilities
################################################################################

# packages

library(rvest)
library(dplyr)

# ------------------------------------------------------------------------------

# load data from Website

data_rat <- read_html("http://www.worldgovernmentbonds.com/") %>%
  html_table()
data_rat <- data_rat[[1]]

# data cleansing

# eliminate first column which is empty
data_rat[, 1] <- NULL

# convert yield to proper format
data_rat$`10Y Yield` <- as.numeric(gsub(pattern = "%", "", data_rat$`10Y Yield`))/100

# convert yield spread vs Ger and vs US to proper format
data_rat$`Spread vsBund` <- as.numeric(gsub(pattern = " bp", "", data_rat$`Spread vsBund`))
data_rat$`Spread vsT-Note` <- as.numeric(gsub(pattern = " bp", "", data_rat$`Spread vsT-Note`))

# if not rated we assing NA to the data
data_rat$`S&PRating`[data_rat$`S&PRating`==""] <- NA

# load rating scale from Standard & Poor's
scale_s_p <- c("AAA", "AA+", "AA", "AA-", 
               "A+", "A", "A-", "BBB+", "BBB", "BBB-", 
               "BB+", "BB", "BB-", "B+", "B", "B-", "CCC+", 
               "CCC", "CCC-", "CC", "C", "D", "SD")


# create data frame which matches rating with numeric value for plot
scale_comp <- data.frame(`S&PRating` = scale_s_p,
                         `S&PNumeric` = 1:length(scale_s_p),
                         stringsAsFactors = FALSE,
                         check.names = FALSE)

# keep only complete cases for analyses
data_rat <- data_rat[complete.cases(data_rat), ]

# match with numeric values
data_rat <- left_join(data_rat,
                 scale_comp)

# keep only rows which are not in default
# check here the S&P rating definitions:
# https://www.standardandpoors.com/en_US/web/guest/article/-/view/sourceId/504352
data_rat <- data_rat[which(data_rat$`S&PRating` != "SD"), ]

# convert ratings to factor variable for nice plotting
data_rat$`S&PRating` <- factor(data_rat$`S&PRating`,
                               levels = scale_s_p,
                               ordered = TRUE)

# data without "outliers"
data_rat_clean <- filter(data_rat, 
                        !Country %in% 
                        c("Zambia", "Argentina(*)"))

################################################################################

labels_rat <- sort(unique(data_rat$`S&PRating`))

# plot yield spread as "function" of rating by using a linear model
plot(`Spread vsBund` ~ `S&PNumeric`, data = data_rat, col = "blue",
     pch = 19, cex = 1, lty = "solid", lwd = 1, main = "Yield Spread vs. Credit Rating (S&P Ratings)",
     ylab = "Spread vs. Germany 10Y", xlab = "Credit Rating", xaxt = "n", yaxt="n")
axis(2, cex.axis=0.7, col.axis="blue")
axis(1, at=1:length(labels_rat), labels=labels_rat, cex.axis=0.7, col.axis="blue")
text(data_rat$`Spread vsBund` ~ data_rat$`S&PNumeric`, labels=data_rat$Country, cex= 0.4, pos=3)
model <- lm(`Spread vsBund` ~ `S&PNumeric`, data = data_rat)
abline(model, col = "orange", lwd = 2, lty = 3)

################################################################################

# plot rating distribution
plot(data_rat$`S&PRating`,
     main = "Country Rating Distribution Standard & Poor's",
     cex.names = 0.7,
     cex.axis=0.7)

################################################################################

# EXPERIMENTAL STUFF - NEED TO BE FINISHED!!

# plot(data_rat$`Spread vsBund` ~ data_rat$`S&PRating`)
# spineplot(data_rat$`S&PRating` ~ data_rat$`Spread vsBund`)


################################################################################