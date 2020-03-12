
setwd("/media/mburu/mburu/R/kenya debt")

setwd("E:/R/kenya debt")
library(tidyverse)

library(data.table)

kenya_debt <- read_csv( "Public Debt (Ksh Million).csv") %>%setDT()

kenya_debt[, Date := as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")]

ggplot(kenya_debt, aes(Date, Total)) +geom_line()
        

kenya_debt <- kenya_debt[order(kenya_debt$Date)]

x <- diff(kenya_debt$Total,lag = 1)
y <- c(0, x)

kenya_debt[, lag_diff := y]
dec <- decompose(ts(kenya_debt$lag_diff, start = c(1999, 09), end = c(2018, 09), frequency = 12))
trend <- dec$trend

plot(trend)
ggplot(kenya_debt, aes(Date, lag_diff)) +geom_line()

start <- as.Date("1999-09-01")
end <- as.Date("2019-12-01")
x2 <- seq.Date(start, end, by = "year" )

x3 <- c()
for (i in 1:21) {
    
    if( i%%2 !=0 ) x3 <- append(x3, i)
}

x2 <- x2[x3]
library(ggthemes)
library(gganimate)

options(gganimate.dev_args = list(width = 700, height = 500))
ggplot(kenya_debt, aes(Date, Total * 1e-06)) + 
    geom_line(size = 2) + 
    geom_label(aes(Date+100, Total*1e-06, label = round(Total*1e-06, 1)), size = 5,
              face = "bold") + 
    transition_reveal(Date) + 
    labs(title = 'Kenya Public Debt Sept-1999 - Sept-2018', y = 'Debt(in Trillions, KES)',
         caption = "@mmburu_w") +
    scale_x_date(breaks = x2, date_labels = "%b-%y", limits = c(start, end))+
    theme_hc() +
    theme(#plot.margin = margin(10.5, 2, 3, 2),
           axis.text = element_text(face = "bold", size =12),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(size =12, face = "bold"))



    ggplot(kenya_debt, aes(Date, lag_diff)) +geom_bar(stat = "identity")
