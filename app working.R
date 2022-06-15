rm(list = ls())

library(tidyverse)
library(gsheet)
library(magrittr)
library(lubridate)
library(plotly)

athlete <- c("Holly", "Jahde", "Synne")
tests <- c("CMJ (mm)", "SJ (mm)")
season <- c("21-22")
date.range <- as.Date(c("2021-01-01", "2022-01-01"))

url <- c("https://docs.google.com/spreadsheets/d/1fiRUwYv8FtysQQg7Z25Py-FW6KJeuMeoFCXuPmf89EE/edit?usp=sharing")
dat <- gsheet::gsheet2tbl(url)

dat$Date %<>% as.Date(format("%d/%m/%Y"))
dat$Athlete %<>% as.factor()
dat$Type %<>% as.factor()
dat$Season %<>% as.factor()

dat <- dat %>%
  filter(Athlete %in% athlete &
         Type %in% tests &
           Date >= date.range[1] &
           Date <= date.range[2])

dat$Athlete <- droplevels(dat$Athlete)
dat$Type <- droplevels(dat$Type)

#plotly functions


pal <- c("red", "blue", "orange")
pal <- setNames(pal, c(athlete[1], athlete[2], athlete[3]))
sym <- c("circle", "o", "x", "x-open")
sym <- setNames(sym, c("CMJ (mm)", "SJ (mm)", "RCMJ (mm)", "LCMJ (mm)"))

fig <- plot_ly(
  data = dat,
  x = ~Date,
  y = ~Score,
  color = ~Athlete,
  text = ~paste("", Athlete,
                "<br>", Date,
                "<br>", Type,
                "<br>", Score),
  hoverinfo = "text",
  type = "scatter",
  mode = "markers+lines",
  line = list(width = 0.5, shape = "spline"),
  symbol = ~Type,
  symbols = sym,
  colors = pal
) %>%
  layout(yaxis = list(
           title = "Score (mm)"),
         xaxis = list(
           title = ""),
         legend = list(
           orientation = "h",
           font = list(
             size = 12
           )
           )
         )

fig

# %in% is used for multiple inputs
dat <- dat %>%
  filter(Athlete %in% athlete &
           Type %in% tests &
           Season %in% season)

jump.graph <- ggplot(data = dat, mapping = aes(x = Date, y = Score)) +
  geom_point(mapping = aes(colour = Type, shape = Athlete)) +
  geom_line(mapping = aes(colour = Type, group = interaction(Athlete, Type))) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B %Y") +
  coord_cartesian(xlim = date.range) +
  labs(y = "Height (mm)",
       x = "Date") +
  guides(x = guide_axis(angle = 90)) +
  theme_classic()

print(jump.graph)

dat.tbl <- dat %>%
  filter(Date >= date.range[1] & Date <= date.range[2]) %>%
  group_by(Athlete, Type, Season) %>%
  summarise(PB = max(Score),
            Average = mean(Score)) %>%
  mutate_if(is.numeric, round, 0) %>%
  ungroup()

pb.tbl <- dat.tbl %>%
  select(!Average) %>%
  pivot_wider(names_from = Type, values_from = PB)

avg.tbl <- dat.tbl %>%
  select(!PB) %>%
  pivot_wider(names_from = Type, values_from = Average)

x <- date.range[2]

mth.tbl <- dat %>%
  filter(Date >= date.range[2]-31) %>%
  group_by(Athlete, Type, Season) %>%
  summarise(PB = max(Score),
            Average = mean(Score)) %>%
  mutate_if(is.numeric, round, 0) %>%
  ungroup()
