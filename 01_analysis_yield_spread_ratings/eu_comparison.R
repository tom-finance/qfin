################################################################################
# Analysis seperated by EU
################################################################################

# scrape current list of countries under ECB supervision
sample2 <- "https://en.wikipedia.org/wiki/Member_state_of_the_European_Union" %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[5]') %>%
  html_table(fill = TRUE)

# data cleansing - eliminate strange bracktes from wikipedia
sample2$Name <- gsub(pattern = "(\\[.*?\\])",
                     replacement = "",
                     sample2$Name)

# rename column for joining with other table
names(sample2)[names(sample2) == 'Name'] <- 'Country'

# flag_eu <- data.frame(Country = sample2$Country,
#                       Flag = rep(1, length(sample2$Country)),
#                       stringsAsFactors = FALSE)


flag_eu <- data.frame(Country = sample2$Country,
                      Flag = rep("EU", length(sample2$Country)),
                      stringsAsFactors = FALSE)


sers <- left_join(hoi, flag_eu)

sers$Flag[is.na(sers$Flag)] <- "not EU"
#sers$Flag[is.na(sers$Flag)] <- 0


sers$Flag <- as.factor(sers$Flag)

# sers_EU <- sers %>%
#   filter(Flag == 1)
# 
# sers_not <- sers %>%
#   filter(Flag != 1)
# 
# model2 <- lm(`Spread vsBund` ~ `CDS Spread` , data = sers_EU)
# model3 <- lm(`Spread vsBund` ~ `CDS Spread` , data = sers_not)


###########

library(ggplot2)

# plot results

ggplot(sers, aes(x=`CDS Spread`, y=`Spread vsBund`, col=Flag, label = Country)) + 
  geom_point() +
  scale_color_manual(values=c("orange", "steelblue")) +
  geom_smooth(method="lm", se=FALSE, linetype = "dotted") +
  theme_classic() +
  geom_text(check_overlap = TRUE, size = 3)

#################

# plot several regression lines
# https://stackoverflow.com/questions/24651464/how-to-plot-several-regression-lines-in-same-scatter-plot-in-r

################################################################################