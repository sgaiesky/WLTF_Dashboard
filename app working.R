rm(list = ls())

library(tidyverse)
library(gsheet)
library(magrittr)
library(lubridate)

athlete <- c("Max", "Emmanuel")
tests <- c("CMJ (mm)", "LCMJ (mm)", "RCMJ (mm)")
season <- c("21-22", "20-21", "19-20")

url <- c("https://docs.google.com/spreadsheets/d/1fiRUwYv8FtysQQg7Z25Py-FW6KJeuMeoFCXuPmf89EE/edit?usp=sharing")
dat <- gsheet::gsheet2tbl(url)

dat$Date %<>% as.Date(format("%d/%m/%Y"))
dat$Athlete %<>% as.factor()
dat$Type %<>% as.factor()
dat$Season %<>% as.factor()

# %in% is used for multiple inputs
dat <- dat %>%
  filter(Athlete %in% athlete &
           Type %in% tests &
           Season %in% season)

jump.graph <- ggplot(data = dat, mapping = aes(x = Date, y = Score)) +
  geom_point(mapping = aes(colour = Athlete, shape = Type)) +
  geom_line(mapping = aes(colour = Athlete, group = interaction(Athlete, Type))) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B %Y") +
  labs(y = "Height (mm)",
       x = "Date") +
  guides(x = guide_axis(angle = 90)) +
  theme_classic()

print(jump.graph)