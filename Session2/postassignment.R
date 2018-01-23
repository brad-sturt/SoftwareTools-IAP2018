#' We'll need a new package to help handle dates, so install it and load it:
install.packages('lubridate', repos = 'http://cran.rstudio.org')
library(lubridate)

#' Next, load up some data:
listings <- read.csv("data/listings.csv")  # Data per listing
calendar <- read.csv("data/calendar.csv")  # Prices per listing per day

#' Now, we'll join these together to give you a starting dataset to work with.
clean_price <- function(price) as.numeric(gsub('\\$|,', '', price))
prices <- calendar %>%
  left_join(listings, by = c('listing_id' = 'id' )) %>%
  mutate(date = ymd(date),
         price = clean_price(price.x))

#' Let's plot daily average prices:
prices %>%
  group_by(date) %>%
  summarise(mean_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(date, mean_price)) +
  geom_line()

#' What's up with that spike in prices in mid-April? Your task is to investigate the data and come up with a theory for why we might be seeing this. Write up a short report, including at least two ggplot visualizations.
