library(mgcv)
library(dplyr)
library(gratia)
library(ggplot2)
library(easyGgplot2)
library(graphics)

df <- read.csv('https://raw.githubusercontent.com/ozim-ml/r_gam-households/main/households_pl.csv',
               header = TRUE)

#convert first column to date object 
df$t <- as.Date(df$t)
df$t_year <- format(df$t, "%Y")


# plotting households sales
p1 <- ggplot(df, aes(x = t)) +
  geom_line(aes(y = y1, 
                color = "Primary market"), 
            linewidth = 0.75) +
  geom_line(aes(y = y2, 
                color = "Secondary market"), 
            linewidth = 0.75) +
  labs(x = "Year", 
       y = "Households sales (units)", 
       title = "Households sales on the primary and secondary markets in Poland") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "bottom") +
  theme(panel.grid = element_line(color = "lightgrey",
                                  linewidth  = 0.75,
                                  linetype = 1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_color_manual(name = NULL, 
                     values = c("Primary market" = "red",
                                "Secondary market" = "blue"))

# subtract 100 from inflation rate in order to plot it together with reference rate
df$x2 <- df$x2-100

# plotting reference and inflation rates
p2 <- ggplot(df, aes(x = t)) +
  geom_line(aes(y = x1, 
                color = "Reference rate"), 
            linewidth = 0.75) +
  geom_line(aes(y = x2, 
                color = "Inflation rate"), 
            linewidth = 0.75) +
  labs(x = "Year", 
       y = "PCT value", 
       title = "Reference and inflation rates in Poland") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "bottom") +
  theme(panel.grid = element_line(color = "lightgrey",
                                  linewidth  = 0.75,
                                  linetype = 1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_color_manual(name = NULL, 
                     values = c("Reference rate" = "darkgreen",
                                "Inflation rate" = "orange"))


ggplot2.multiplot(p1,p2, cols = 1)


# GAM for the primary market
gam1 <- gam(log(y1) ~ te(x1, x2), data = df)
gam.check(gam1)
summary(gam1)
AIC(gam1)
logLik(gam1)
appraise(gam1)

# GAM for the secondary market
gam2 <- gam(log(y2) ~ te(x1, x2), data = df)
gam.check(gam2)
summary(gam2)
AIC(gam2)
logLik(gam2)
appraise(gam2)

# visualization of models
g1 <-  draw(gam1, dist = 0) + labs(title = "te(x1, x2)",
                                   subtitle = "Primary market",
                                   x = "Reference rate",
                                   y = "Inflation rate")
g2 <-  draw(gam2, dist = 0) + labs(title = "te(x1, x2)", 
                                   subtitle = "Secondary market",
                                   x = "Reference rate",
                                   y = "Inflation rate")

ggplot2.multiplot(g1,g2, cols = 2)

