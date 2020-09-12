

getwd()

alles <- read.csv2("alles.csv",
                   stringsAsFactors = FALSE,
                   na.strings = "#NV")


alles$Numeric <- as.numeric(alles$Numeric)

# run regression model

alles$Srpead_vs_Ger <- as.numeric(gsub(" bp", "", alles$Srpead_vs_Ger))


alles <- alles[complete.cases(alles), ]

# exclude zambia

alles <- alles[-66, ]

# linear model

# labels - be careful, they are hard coded!!
labels_rat <- c("AAA", "AA+", "AA", "AA-", "A+", "A", "A-", "BBB+", "BBB", 
                "BBB-", "BB+", "BB", "BB-", "B+", "B", "B-")

# create plot using base R

plot(Srpead_vs_Ger ~ Numeric, data = alles, col = "blue",
     pch = 19, cex = 1, lty = "solid", lwd = 1, main = "Yield Spread vs. Credit Rating (S&P Ratings)",
     ylab = "Spread vs. Germany 10Y", xlab = "Credit Rating", xaxt = "n", yaxt="n")
axis(2, cex.axis=0.7, col.axis="blue")
axis(1, at=1:16, labels=labels_rat, cex.axis=0.7, col.axis="blue")
text(alles$Srpead_vs_Ger ~ alles$Numeric, labels=alles$Country, cex= 0.4, pos=3)
model <- lm(Srpead_vs_Ger ~ Numeric, data = alles)
abline(model, col = "orange", lwd = 2, lty = 3)

# abline(h = 0, col = "red", lwd = 2, lty = 3)


# to do: more ticks on the y axis!

min(alles$Srpead_vs_Ger)
max(alles$Srpead_vs_Ger)

# 
# at = (seq(min(alles$Srpead_vs_Ger),
#     max(alles$Srpead_vs_Ger), by = 100))
# 
# at = c(1,5,10)

# looks nice!

# output of the regression model
summary(model)


################################################################################

################################################################################


# how to add nice labels to plot:

# https://stackoverflow.com/questions/13229546/how-can-i-label-points-in-this-scatterplot