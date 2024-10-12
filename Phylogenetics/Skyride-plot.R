library(ggplot2)
library(lubridate)

setwd("~/skyrides/")
# read in skyride ouputs from Tracer
pda <- read.csv("American-2011-2020gmrf.tsv", sep = '\t')
rand1 <- read.csv("American-2011-2020-rand1.tsv", sep = '\t')
rand2 <- read.csv("American-2011-2020-rand2.tsv", sep = '\t')



combined_sky_data <- rbind(
  transform(pda, dataset = "pda"),
  transform(rand1, dataset = "rand1"),
  transform(rand2, dataset = "rand2")
)


combined_sky_data$date <- as.Date(combined_sky_data$date)


skyride_plot <- ggplot(combined_sky_data, aes(x = date, y = mean, color = dataset)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = dataset), alpha = 0.3) +
  labs(title = "Skyride effective population size esitmates A(H3N2)", subtitle = "2011-2020",
       x = "Date",
       y = "log(Ne)") +
  scale_y_log10() +
  scale_x_date(limits = c(as.Date("2011-05-01"), as.Date("2020-01-01"))) +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  scale_color_moma_d("VanGogh")


skyride_plot


