library(tidyverse)
library(scales)
library(RColorBrewer)


## Colors and theme
cb_palette <- c("#D55E00", "#0072B2")

theme_set(theme_minimal())

## Create a figures/ subdir if one doesn't exist
ifelse(!dir.exists(file.path("figures")), dir.create(file.path("figures")), FALSE)

### --------------------------------------------------
### Data
### --------------------------------------------------

data <- read.csv("data/oecd-assault-series-per-100k-standardized-to-2015.csv",
                 header=TRUE)

colnames(data) <- c("Country", as.character(c(1960:2015)))

ind_us <- data$Country %in% "United States"

ind_exclude <- data$Country %in% c("Estonia", "Mexico", "Chile", "Israel")

data$Group <- "Other OECD"
data$Group[ind_us] <- "United States"

data$Dropped <- FALSE
data$Dropped[ind_exclude] <- TRUE



## Convert to long format
data_m <- gather(data, key = "Year", value = "Deaths", `1960`:`2015`)
data_m$Year <- as.numeric(data_m$Year)




###--------------------------------------------------
### Plots
###--------------------------------------------------

credit <- function() {
  return(makeFootnote("\n\nData: OECD. Excludes Estonia and Mexico. http://kieranhealy.org"))
}


pdf(file="figures/assault-deaths-oecd-ts-all.pdf", width=10, height=8)

p <- ggplot(data = subset(data_m, Dropped == FALSE),
            mapping = aes(x = Year, y = Deaths,
                          group = Country,
                          color = Group,
                          fill = Group))

p0 <- p + geom_point(size=0.8) + geom_smooth(method="loess") +
    scale_x_continuous(breaks = seq(1960, 2010, by = 10))
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))

p1 <- p0 + scale_colour_manual(name = NULL,
                        breaks=c("United States", "Other OECD"),
                        labels=c("United States", "23 other OECD Countries"),
                               values=rev(cb_palette)) +
    scale_fill_manual(name  = NULL,
                      breaks=c("United States", "Other OECD"),
                      labels=c("United States", "23 other OECD Countries"),
                      values=rev(cb_palette))

p1 + labs(title = "Assault Deaths in the OECD, 1960-2015",
          caption = "Excludes Estonia and Mexico. http://kieranhealy.org",
          y = "Assault Deaths per 100,000 population",
          color = NULL,
          fill = NULL) +
    theme(legend.position="top")

dev.off()
