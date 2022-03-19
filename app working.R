library(tidyverse)
library(gsheet)
library(magrittr)

athlete <- c("Emmanuel", "Max")
tests <- c("CMJ (mm)", "LCMJ (mm)")
season <- c("21-22")

url <- c("https://docs.google.com/spreadsheets/d/1fiRUwYv8FtysQQg7Z25Py-FW6KJeuMeoFCXuPmf89EE/edit?usp=sharing")
dat <- gsheet::gsheet2tbl(url)

dat$Athlete %<>% as.factor()
dat$Type %<>% as.factor()

# %in% is used for multiple inputs
dat <- dat %>%
  filter(Athlete %in% athlete &
           Type %in% tests &
           Season %in% season)

jump.graph <- ggplot(data = dat, mapping = aes(x = Date, y = Score)) +
  geom_point(mapping = aes(colour = Athlete, shape = Type)) +
  geom_line(mapping = aes(colour = Athlete, group = interaction(Athlete, Type))) +
  theme_classic()

print(jump.graph)