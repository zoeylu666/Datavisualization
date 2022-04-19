library(ggplot2)
library(dplyr)
library(gapminder)
library(trelliscopejs)
space_to_dash <- function(x) gsub(" ", "-", x)

# Group by country and create the two new variables
gap <- gapminder %>%
  group_by(country) %>%
  mutate(
    delta_lifeExp = tail(lifeExp, 1) - head(lifeExp, 1),
    ihme_link = paste0("http://www.healthdata.org/", space_to_dash(country)))

# Add the description
gap$delta_lifeExp <- cog(gap$delta_lifeExp, desc = "Overall change in life expectancy")
# Specify the default label
gap$ihme_link <- cog(gap$ihme_link, default_label = TRUE)

ggplot(gap, aes(year, lifeExp)) +
  geom_point() +
  facet_trelliscope(~ country + continent,
                    name = "lifeExp_by_country",
                    desc = "Life expectancy vs. year.",
                    nrow = 1, ncol = 2,
                    scales = c("same", "sliced"))