library(tidyverse)

# c(Year,Fires,Acres)
bind_rows(
  c("year" = 2021,"Fires" = 58985,"Acres burned" = 7125643),
  c("year" = 2020,"Fires" = 58950,"Acres burned" = 10122336),
  c("year" = 2019,"Fires" = 50477,"Acres burned" = 4664364),
  c("year" = 2018,"Fires" = 58083,"Acres burned" = 8767492),
  c("year" = 2017,"Fires" = 71499,"Acres burned" = 10026086),
  c("year" = 2016,"Fires" = 67743,"Acres burned" = 5509995),
  c("year" = 2015,"Fires" = 68151,"Acres burned" = 10125149),
  c("year" = 2014,"Fires" = 63312,"Acres burned" = 3595613),
  c("year" = 2013,"Fires" = 47579,"Acres burned" = 4319546),
  c("year" = 2012,"Fires" = 67774,"Acres burned" = 9326238),
  c("year" = 2011,"Fires" = 74126,"Acres burned" = 8711367),
  c("year" = 2010,"Fires" = 71971,"Acres burned" = 3422724),
  c("year" = 2009,"Fires" = 78792,"Acres burned" = 5921786),
  c("year" = 2008,"Fires" = 78979,"Acres burned" = 5292468),
  c("year" = 2007,"Fires" = 85705,"Acres burned" = 9328045),
  c("year" = 2006,"Fires" = 96385,"Acres burned" = 9873745),
  c("year" = 2005,"Fires" = 66753,"Acres burned" = 8689389),
  c("year" = 2004,"Fires" = 65461,"Acres burned" = 8097880),
  c("year" = 2003,"Fires" = 63629,"Acres burned" = 3960842),
  c("year" = 2002,"Fires" = 73457,"Acres burned" = 7184712),
  c("year" = 2001,"Fires" = 84079,"Acres burned" = 3570911),
  c("year" = 2000,"Fires" = 92250,"Acres burned" = 7393493),
  c("year" = 1999,"Fires" = 92487,"Acres burned" = 5626093),
  c("year" = 1998,"Fires" = 81043,"Acres burned" = 1329704),
  c("year" = 1997,"Fires" = 66196,"Acres burned" = 2856959),
  c("year" = 1996,"Fires" = 96363,"Acres burned" = 6065998),
  c("year" = 1995,"Fires" = 82234,"Acres burned" = 1840546),
  c("year" = 1994,"Fires" = 79107,"Acres burned" = 4073579),
  c("year" = 1993,"Fires" = 58810,"Acres burned" = 1797574),
  c("year" = 1992,"Fires" = 87394,"Acres burned" = 2069929),
  c("year" = 1991,"Fires" = 75754,"Acres burned" = 2953578),
  c("year" = 1990,"Fires" = 66481,"Acres burned" = 4621621),
  c("year" = 1989,"Fires" = 48949,"Acres burned" = 1827310),
  c("year" = 1988,"Fires" = 72750,"Acres burned" = 5009290),
  c("year" = 1987,"Fires" = 71300,"Acres burned" = 2447296),
  c("year" = 1986,"Fires" = 85907,"Acres burned" = 2719162),
  c("year" = 1985,"Fires" = 82591,"Acres burned" = 2896147),
  c("year" = 1984,"Fires" = 20493,"Acres burned" = 1148409),
  c("year" = 1983,"Fires" = 18229,"Acres burned" = 1323666)
) |>
  pivot_longer(c(Fires, `Acres burned`)) |>
  group_by(name) |>
  arrange(year) |>
  mutate(value = value/value[1]*100) |>
  ggplot(aes(x = year, y = value, color = name)) +
    geom_line(aes(group = name, linetype = "Yearly observations")) +
    tidyquant::geom_ma(n = 10, aes(linetype = "10 year moving average")) +
    theme_bw() +
    theme(legend.position = "top") +
    labs(x = "Year", y = "Index (base year 1983)", linetype = "Measure", color = "Variable")

ggsave("./figures_for_report/wildfires_in_us.pdf", width = 8, height = 3.5)

