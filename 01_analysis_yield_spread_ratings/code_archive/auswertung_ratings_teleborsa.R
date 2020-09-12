################################################################################
# Country Credit Ratings
################################################################################


library(rvest)
library(dplyr)


# only needed if read_html is not working (e.g. due to internal firewall)
# download.file("https://www.teleborsa.it/Quotazioni/Rating", destfile = "scrapedpage.html", quiet=TRUE)


content <- read_html("https://www.teleborsa.it/Quotazioni/Rating") %>%
  html_table()

content <- content[[1]]
content[, 1] <- NULL


# rating scales were manually exported from HTML table which can be found on
# https://www.teleborsa.it/Quotazioni/Rating/Legenda.

# Not Rated was added

scales <- data.frame(
  stringsAsFactors = FALSE,
  check.names = FALSE,
  `Moody's` = c("Aaa",
                "Aa1","Aa2","Aa3","A1","A2","A3","Baa1",
                "Baa2","Baa3","Ba1","Ba2","Ba3","B1","B2",
                "B3","Caa1","Caa2","Caa3","Ca","C", "Not Rated", NA, NA),
  `S&Poor's` = c("AAA",
            "AA+","AA","AA-","A+","A","A-","BBB+","BBB",
            "BBB-","BB+","BB","BB-","B+","B","B-",
            "CCC+","CCC","CCC-","CC","C","D", "Not Rated", "SD"),
  Fitch = c("AAA",
            "AA+","AA","AA-","A+","A","A-","BBB+","BBB",
            "BBB-","BB+","BB","BB-","B+","B","B-",
            "CCC","DDD","DD","D", "Not Rated", NA, NA, NA)
)

# some more data cleansing
content[content==""]<-"Not Rated"
content[content=="NR"]<-"Not Rated"
content[content=="Not rated"]<-"Not Rated"

################################################################################

# plot data

s_p <- factor(content$`S&Poor's`,
            levels = na.omit(scales$`S&Poor's`))

plot(s_p[which(s_p != "Not Rated")],
     main = "Country Rating Distribution Standard & Poor's",
     cex.names = 0.7)

# compare distribution with Moody's

moodys <- factor(content$`Moody's`,
                 levels = na.omit(scales$`Moody's`))

plot(moodys[which(moodys != "Not Rated")],
     main = "Country Rating Distribution Moody's",
     cex.names = 0.7)

# compare distribution with Fitch

fitch <- factor(content$Fitch,
                 levels = na.omit(scales$Fitch))

plot(fitch[which(fitch != "Not Rated")],
     main = "Country Rating Distribution Fitch",
     cex.names = 0.7)

################################################################################

# make some queries

# AAA countries
content %>%
  filter(`S&Poor's` == "AAA")

# Italy
content %>%
  filter(Nazione == "Italia")

# other countries with Italy's rating

content %>%
  filter(`S&Poor's` == "BBB" |
           `S&Poor's` == "BBB+" |
           `S&Poor's` == "BBB-")

#################

# how often assign rating agencies the same rating?

# asign numeric vector to the rating scales
scales$test <- 1:24

# match with the categorical ratings
num_s_p <- merge(content[, c(2,1)],
              scales[, c(2,4)])

num_fitch <- merge(content[, c(4,1)],
                   scales[, c(3,4)])

num_moodys <- merge(content[, c(3,1)],
                    scales[, c(1,4)])

rat_comp <- cbind(num_s_p[, 3],
                  c(num_fitch[, 3], NA, NA),
                  num_moodys[, 3])

################################################################################

# to do: make map of Europe
# compare credit ratings

################################################################################