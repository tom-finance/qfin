################################################################################
# Performance Comparison Tool for Raiffeisen Pension Fund

# (c) Thomas Ludwig
################################################################################

# load packages

library(rvest)
library(dplyr)
library(plotly)
library(htmlwidgets)

################################################################################

# (1) Downloand Price Data from Website into R

# read all tables from website into R
content <- read_html("http://www.raiffeisenpensionsfonds.it/nc/offener-pensionsfonds/finanz-verwaltung/wert-der-anteile/nav-investitionslinie-dynamic.html?tx_rolstockgraph_pi1%5Byear%5D=all") %>%
  html_table()

# construct price table
price_dynamic <- rbind(content[[1]],
                        content[[2]],
                        content[[3]])

# data cleansing
price_dynamic$Wert <- as.numeric(gsub(",", ".", gsub("\\.", "", price_dynamic$Wert)))
price_dynamic$Datum <- as.Date(price_dynamic$Datum, "%d.%m.%Y")

content <- read_html("http://www.raiffeisenpensionsfonds.it/nc/offener-pensionsfonds/finanz-verwaltung/wert-der-anteile/nav-investitionslinie-activity.html?tx_rolstockgraph_pi1%5Byear%5D=all") %>%
  html_table()

# construct price table
price_activity <- rbind(content[[1]],
                        content[[2]],
                        content[[3]])

# data cleansing
price_activity$Wert <- as.numeric(gsub(",", ".", gsub("\\.", "", price_activity$Wert)))
price_activity$Datum <- as.Date(price_activity$Datum, "%d.%m.%Y")

colnames(price_activity) <- c("Datum", "Wert_aktivity")

################################################################################

# (2) Merge and prepare data for application generation

price_all <- left_join(price_activity, price_dynamic) %>%
  filter(complete.cases(.))

colnames(price_activity) <- c("Datum", "Wert")

price_activity$Symbol <- "Activity"
price_dynamic$Symbol <- "Dynamic"

all <- rbind(price_activity,
             price_dynamic)

################################################################################

# (3) Set up application

pltly <- 
  all %>% 
  dplyr::group_by(Symbol) %>% 
  dplyr::mutate(Wert = Wert / Wert[1L] * 100) %>% 
  plotly::plot_ly(x = ~Datum, y = ~Wert, color = ~Symbol,
                  type = "scatter", mode = "lines", colors = c("#132B43", "#56B1F7")) %>% 
  plotly::layout(dragmode = "zoom", 
                 datarevision = 0,
                 xaxis = list(
                   rangeselector = list(
                     buttons = list(
                       list(
                         count = 3,
                         label = "3 mo",
                         step = "month",
                         stepmode = "backward"),
                       list(
                         count = 6,
                         label = "6 mo",
                         step = "month",
                         stepmode = "backward"),
                       list(
                         count = 1,
                         label = "1 yr",
                         step = "year",
                         stepmode = "backward"),
                       list(
                         count = 1,
                         label = "YTD",
                         step = "year",
                         stepmode = "todate"),
                       list(step = "all"))),
                   
                   rangeslider = list(type = "date"))) %>% 
  rangeslider(borderwidth = 1)

onRenderRebaseTxt <- "
  function(el, x) {
el.on('plotly_relayout', function(rlyt) {
        var nrTrcs = el.data.length;
        // array of x index to rebase to; defaults to zero when all x are shown, needs to be one per trace
        baseX = Array.from({length: nrTrcs}, (v, i) => 0);
        // if x zoomed, increase baseX until first x point larger than x-range start
        if (el.layout.xaxis.autorange == false) {
            for (var trc = 0; trc < nrTrcs; trc++) {
                while (el.data[[trc]].x[baseX[trc]] < el.layout.xaxis.range[0]) {baseX[trc]++;}
            }   
        }
        // rebase each trace
        for (var trc = 0; trc < nrTrcs; trc++) {
            el.data[trc].y = el.data[[trc]].y.map(x => x / el.data[[trc]].y[baseX[trc]]);
        }
        el.layout.yaxis.autorange = true; // to show all traces if y was zoomed as well
        el.layout.datarevision++; // needs to change for react method to show data changes
        Plotly.react(el, el.data, el.layout);

});
  }
"
# render final result
htmlwidgets::onRender(pltly, onRenderRebaseTxt)

# save application to output directory
saveWidget(onRender(pltly, onRenderRebaseTxt), # render plot
           "../Output/fund_performance.html") # save widget to output directory

################################################################################

# BACKUP PLOTLY PLOT

# pltly <- 
#   all %>% 
#   dplyr::group_by(Symbol) %>% 
#   dplyr::mutate(Wert = Wert / Wert[1L] * 100) %>% 
#   plotly::plot_ly(x = ~Datum, y = ~Wert, color = ~Symbol,
#                   type = "scatter", mode = "lines", colors = c("#132B43", "#56B1F7")) %>% 
#   plotly::layout(dragmode = "zoom", 
#                  datarevision = 0) %>% 
#   rangeslider(borderwidth = 1)

################################################################################