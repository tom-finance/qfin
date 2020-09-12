
# Test to Join

################################################################################

full <- data.frame(Country = c("Italy", "Germany", "Austria"),
                   Rating = c("BB", "AAA", "BB+"),
                   stringsAsFactors = F)

match <- data.frame(
                    Number = 1:4,
                    Rating = c("AAA", "AA", "BB+", "BB"),
                    stringsAsFactors = F)


# load package
library(dplyr)

# seems to work ok
all <- left_join(full, match)

################################################################################