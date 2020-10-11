################################################################################
# Analysis ECB Eligible Assets Database
################################################################################

# package
library(rvest)
library(dplyr)
library(ggplot2)


# check this website
# http://doc989.consiglioveneto.it/oscc/resources/ecb_2018_4_f_sign.pdf
# https://www.ecb.europa.eu/paym/coll/risk/liquidity/html/index.en.html

# Decoding Coupon Definition:
# fixed coupon (incl. inflation linked bonds): COUPON_DEFINITION: CD4
# zero coupon: COUPON_DEFINITION: CD1
# floating: COUPON_DEFINITION: CD1


# target website for automated download
# https://www.ecb.europa.eu/paym/coll/assets/html/list-MID.en.html

# extract most recent download link
link <- read_html("https://www.ecb.europa.eu/paym/coll/assets/html/list-MID.en.html") %>%
  html_nodes("tr:nth-child(1) .csv") %>% # html node extracted with selector gadget
  html_attr("href") %>% # extract link to download
  paste0("https://www.ecb.europa.eu", .) # create full path from relative path

# alternative: direkt import into R without file download
data <- read.csv(link, skipNul = TRUE, sep = '\t',header = TRUE, fileEncoding = "UTF-16LE")

################################################################################
# data manipulation and preperation

data$ISSUANCE_DATE <- as.Date(data$ISSUANCE_DATE, "%d/%m/%Y")
data$MATURITY_DATE <- as.Date(data$MATURITY_DATE, "%d/%m/%Y")


data$RUNNING_TIME_YEARS <- as.numeric(difftime(data$MATURITY_DATE, 
                                               data$ISSUANCE_DATE, 
                                               unit="weeks"))/52.25

data$REMAINING_TIME_YEARS <- as.numeric(difftime(data$MATURITY_DATE, 
                                                 Sys.Date(), 
                                                 unit="weeks"))/52.25


# create time buckets running time years and remaining time years
data <- data %>%
  mutate(time_bucket_running = case_when(
    RUNNING_TIME_YEARS <= 1 ~ "<1Y",
    RUNNING_TIME_YEARS > 1 & RUNNING_TIME_YEARS <= 5 ~ ">1-5Y",
    RUNNING_TIME_YEARS > 5 & RUNNING_TIME_YEARS <= 10 ~ ">5-10Y",
    RUNNING_TIME_YEARS > 10 ~ ">10Y"
  )
) %>%
  mutate(time_bucket_remaining = case_when(
    REMAINING_TIME_YEARS <= 1 ~ "<1Y",
    REMAINING_TIME_YEARS > 1 & REMAINING_TIME_YEARS <= 5 ~ ">1-5Y",
    REMAINING_TIME_YEARS > 5 & REMAINING_TIME_YEARS <= 10 ~ ">5-10Y",
    REMAINING_TIME_YEARS > 10 ~ ">10Y"
  )
)

# plot some results
plot(as.factor(data$time_bucket_remaining))
plot(as.factor(data$time_bucket_running))

data$ISSUANCE_DATE_YEAR <- as.numeric(format(data$ISSUANCE_DATE,"%Y"))

################################################################################

# some data analysis

# let's have a loom on govis bonds in 2020 issued with fixed coupon

data_govis_2020 <- data %>%
  filter(ISSUANCE_DATE_YEAR == 2020) %>%
  filter(COUPON_DEFINITION == "CD4") %>%
  filter(ISSUER_GROUP == "IG2") %>%
  arrange(desc(COUPON_RATE....)) %>%
  select(ISIN_CODE, ISSUER_NAME, COUPON_RATE...., RUNNING_TIME_YEARS,
         DENOMINATION, ISSUANCE_DATE, MATURITY_DATE, HAIRCUT, TYPE)


data_all_2020 <- data %>%
  filter(ISSUANCE_DATE_YEAR == 2020) %>%
  filter(COUPON_DEFINITION == "CD4") %>%
  arrange(desc(COUPON_RATE....)) %>%
  #filter(TYPE == "AT01") %>%
  select(ISIN_CODE, ISSUER_NAME, COUPON_RATE...., time_bucket_running,
         DENOMINATION, ISSUANCE_DATE, MATURITY_DATE, HAIRCUT, TYPE)

data_all_2020 <- data %>%
  filter(ISSUANCE_DATE_YEAR == 2020) %>%
  filter(COUPON_DEFINITION == "CD4") %>%
  arrange(desc(ISSUANCE_DATE)) %>%
  #filter(TYPE == "AT01") %>%
  select(ISIN_CODE, ISSUER_NAME, COUPON_RATE...., time_bucket_running,
         DENOMINATION, ISSUANCE_DATE, MATURITY_DATE, HAIRCUT, TYPE)


coupon <- data %>%
  filter(!is.na(COUPON_RATE....)) %>%
  filter(COUPON_DEFINITION == "CD4") %>% # use only fixed rate instruments
  filter(ISSUANCE_DATE_YEAR > 1990) %>%
  group_by(ISSUANCE_DATE_YEAR) %>%
  summarise(m = mean(COUPON_RATE....))

plot(coupon$m ~ coupon$ISSUANCE_DATE_YEAR, type = "l", 
     main = "average coupon rate ECB eligible assets")
abline(h = 0, col = "orange")


###########

haircut <- data %>%
  filter(!is.na(HAIRCUT)) %>%
  filter(ISSUANCE_DATE_YEAR > 1990) %>%
  group_by(ISSUANCE_DATE_YEAR) %>%
  summarise(m = mean(HAIRCUT))

plot(haircut$m ~ haircut$ISSUANCE_DATE_YEAR, type = "l", 
     main = "average haircut ECB eligible assets")
abline(h = 0, col = "orange")



#############

violine <- data %>%
  filter(ISSUANCE_DATE_YEAR > 2015) %>%
  mutate(ISSUANCE_DATE_YEAR = as.factor(ISSUANCE_DATE_YEAR)) %>%
  filter(!is.na(COUPON_RATE....)) %>%
  select(ISSUANCE_DATE_YEAR, COUPON_RATE....)

p <- ggplot(violine, aes(x=ISSUANCE_DATE_YEAR, y=COUPON_RATE...., color = ISSUANCE_DATE_YEAR)) + 
  geom_violin(show.legend = FALSE) +
  labs(x = "Jahre") +
  labs(y = "Abschlusszeit in Jahren") +
  labs(title = "Verteilung der Abschlusszeit 2009 - 2019") +
  theme_minimal()
p

################################################################################

# show all supranational issuers
supranational <- data %>%
  filter(ISSUER_GROUP == "IG6")


coupon_issuer <- data %>%
  #filter(TYPE == "AT01") %>% # consider only bonds for plot
  filter(COUPON_DEFINITION == "CD4") %>% # consider only fixed rate bonds
  filter(!is.na(COUPON_RATE....)) %>% # exclude records without coupon rate
  filter(ISSUANCE_DATE_YEAR > 1990) %>%
  group_by(ISSUANCE_DATE_YEAR, ISSUER_GROUP) %>%
  summarise(average_coupon = mean(COUPON_RATE....))

coupon_issuer %>%
  ggplot(aes(x=ISSUANCE_DATE_YEAR, 
             y=average_coupon, 
             group=ISSUER_GROUP, 
             color=ISSUER_GROUP)) +
  geom_line() +
  ggtitle("durch. Kupon EZB Eligible Assets nach Issuer Group und Ausgabejahr") +
  theme_minimal()

#######################

haircut_issuer <- data %>%
  #filter(TYPE == "AT01") %>% # consider only bonds for plot
  filter(!is.na(HAIRCUT)) %>% # exclude records without coupon rate
  filter(ISSUANCE_DATE_YEAR > 1990) %>%
  group_by(ISSUANCE_DATE_YEAR, ISSUER_GROUP) %>%
  summarise(average_haircut = mean(HAIRCUT))

haircut_issuer %>%
  ggplot(aes(x=ISSUANCE_DATE_YEAR, 
             y=average_haircut, 
             group=ISSUER_GROUP, 
             color=ISSUER_GROUP)) +
  geom_line() +
  ggtitle("durch. Haircut EZB Eligible Assets nach Issuer Group und Ausgabejahr") +
  theme_minimal()

