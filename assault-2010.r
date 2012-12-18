library(ggplot2)
library(reshape2)
library(scales)

### --------------------------------------------------
### Cosmetics
### --------------------------------------------------
makeFootnote <- function(footnoteText=
                         format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x = unit(1,"npc") - unit(2, "mm"),
             y= unit(2, "mm"),
             just=c("right", "bottom"),
             gp=gpar(cex= size, col=color))
   popViewport()
}

credit <- function() {
  return(makeFootnote("\nKieran Healy. http://kieranhealy.org"))
}

### --------------------------------------------------
### Data
### --------------------------------------------------

data <- read.csv("data/oecd-assault-series-per-100k-standardized.csv",
                 header=TRUE)

colnames(data) <- c("Country", as.character(c(1960:2010)))
usdum <- c(rep("Other OECD", 8), rep("Other OECD", 13),
           rep("Other OECD", 11), "United States")
data$usdum <- usdum

ccode <- c("Australia", "Austria", "Belgium", "Canada", "Denmark",
         "Finland", "France", "Germany", "Greece", "Hungary",
         "Ireland", "Italy", "Japan", "Korea", "Luxembourg",
         "Netherlands", "New Zealand", "Norway", "Portugal", "Spain",
         "Sweden", "Switzerland", "United Kingdom", "United States")

ind <- data$Country %in% ccode
data.cc <- data[ind,]

##
data.m <- melt(data.cc, id.vars=c("Country","usdum"))
colnames(data.m) <- c("Country", "Dummy", "Year", "Deaths")
data.m$Year <- strptime(data.m$Year, "%Y")

png(file="figures/assault-deaths-oecd-ts-facet.png", height=1900, width=1100,
    res=100, pointsize=9)
p <- ggplot(data.m, aes(Year, Deaths, group=Country))
p + geom_point() + geom_smooth(method="loess") + labs(y = "Assault Deaths per 100k") + facet_wrap(~ Country, ncol=5) + theme_bw()
credit()
dev.off()

pdf(file="figures/assault-deaths-oecd-ts-facet.pdf", width=14,
    height=18, pointsize=9)
p <- ggplot(data.m, aes(Year, Deaths, group=Country))
p + geom_point() + geom_smooth(method="loess") + labs(y = "Assault Deaths per 100k") + facet_wrap(~ Country, ncol=4) + theme_bw()
credit()
dev.off()


credit <- function() {
  return(makeFootnote("\n\nData: OECD. Excludes Estonia and Mexico. http://kieranhealy.org"))
}

png(file="figures/assault-deaths-oecd-ts-all.png", height=900, width=1100,
    res=140, pointsize=11)
p <- ggplot(data.m, aes(Year, Deaths, group=Country, color=Dummy))
p + geom_point(size=0.8) + geom_smooth(method="loess") + guides(color=guide_legend(title="Country")) + labs(y =
    "Assault Deaths per 100k population") + theme(legend.position="top") + theme_bw()
credit()
dev.off()

pdf(file="figures/assault-deaths-oecd-ts-all.pdf", width=10, height=8)
p <- ggplot(data.m, aes(Year, Deaths, group=Country, color=Dummy))
p + geom_point(size=0.8) + geom_smooth(method="loess") + labs(y =
    "Assault Deaths per 100k population") + theme(legend.position="top") + theme_bw()

credit()
dev.off()

###--------------------------------------------------
### US Data
###--------------------------------------------------
credit <- function() {
  return(makeFootnote("\nSource: CDC WONDER. Age-adjusted. Kieran Healy. http://kieranhealy.org"))
}

## Region
data.us.region <-
  read.csv("data/cdc-wonder-assault-deaths-by-region.csv",
  header=TRUE)
colnames(data.us.region) <- c("Region", "Year", "Deaths",
  "Population", "Rate", "U95", "L95", "Adjusted")
data.us.region$Year <- strptime(data.us.region$Year, "%Y")

library(gdata)
ind.o <- order(data.us.region$Adjusted, decreasing=TRUE)
data.us.region$Region <- reorder.factor(data.us.region[,"Region"], new.order=data.us.region$Region[ind.o])
detach(package:gdata)

library(Hmisc)
library(mgcv)
library(MASS)
p <- ggplot(data.us.region, aes(Year, Adjusted, group=Region, color=Region))
p0 <- p + geom_point(size=0.8) + geom_smooth(method="rlm", formula=y~ns(x,3),
                 aes(group=Region, fill=Region)) +
  labs(y="Assault Deaths per 100,000 population") +
  guides(color=guide_legend(title="Region")) + theme_bw()

png(file="figures/assault-deaths-us-ts-region.png", height=900, width=1100,
    res=140, pointsize=11)
print(p0)
credit()
dev.off()

pdf(file="figures/assault-deaths-us-ts-region.pdf", width=10, height=8)
print(p0)
credit()
dev.off()

## Race
data.us.race <-
  read.csv("data/CDC-WONDER-AssaultDeaths-By-Race.csv",
  header=TRUE)
colnames(data.us.race) <- c("Race", "Year", "Deaths",
  "Population", "Rate", "U95", "L95", "Adjusted")

library(gdata)
ind.o <- order(data.us.race$Adjusted, decreasing=TRUE)
data.us.race$Race <- reorder.factor(data.us.race[,"Race"], new.order=data.us.race$Race[ind.o])
detach(package:gdata)


data.us.race$Year <- strptime(data.us.race$Year, "%Y")
p <- ggplot(data.us.race, aes(Year, Adjusted, group=Race, color=Race))
p0 <- p + geom_point(size=0.8) + geom_smooth(method="rlm", formula=y~ns(x,3),
                 aes(group=Race, fill=Race)) +
  labs(y="Assault Deaths per 100,000 population") +
  guides(color=guide_legend(title="Race")) + theme_bw() +
  theme(legend.position="right")

png(file="figures/assault-deaths-us-ts-race.png", height=900, width=1100,
    res=140, pointsize=11)
print(p0)
credit()
dev.off()

pdf(file="figures/assault-deaths-us-ts-race.pdf", width=10, height=8)
print(p0)
credit()
dev.off()


### By State
data.us.state <-
  read.csv("data/CDC-WONDER-AssaultDeaths-By-State.csv",
  header=TRUE)
colnames(data.us.state) <- c("State", "Code", "Year", "Deaths", "Population", "Rate", "Adjusted")
data.us.state$Year <- strptime(data.us.state$Year, "%Y")

library(gdata)
ind.o <- order(data.us.state$Adjusted, decreasing=TRUE)
data.us.state$State <- reorder.factor(data.us.state[,"State"], new.order=data.us.state$State[ind.o])
detach(package:gdata)

p <- ggplot(data.us.state, aes(Year, Adjusted, group=State, color=State))
p0 <- p + geom_point(size=0.8) + geom_smooth(method="rlm", formula=y~ns(x,3),
                 aes(group=State, fill=State)) +
  labs(y="Assault Deaths per 100,000 population") +
  guides(color=guide_legend(title="State"), fill=guide_legend(nrow=5)) + theme_bw()

q <- ggplot(data.us.state, aes(Year, Adjusted, group=State,
                                     color=State, fill=State)) +
                                     geom_smooth(method="rlm",
                                     formula=y~ns(x,3),
                                     aes(group=State,
                                     fill=State, color=State),
                                     alpha=0.1) +
                                     labs(y="Assault Deaths per 100,000 population")

q1 <- q + guides(fill = guide_legend(ncol = 5), colour =
                 guide_legend(override.aes = list(alpha = 1))) +
  theme_bw() + theme(legend.position="bottom")


credit <- function() {
  return(makeFootnote("\nSource: CDC WONDER. Age-adjusted.\nEstimates are unavailable for some\nstates with very low assault rates.\nKieran Healy. http://kieranhealy.org"))
}


png(file="figures/assault-deaths-us-ts-state.png", height=1800, width=1200,
    res=140, pointsize=11)
print(q1)
credit()
dev.off()

pdf(file="figures/assault-deaths-us-ts-state.pdf", width=10, height=13)
print(q1)
credit()
dev.off()

## By state excluding DC
no.dc <- data.us.state$State!="District of Columbia"
q <- ggplot(data.us.state[no.dc,], aes(Year, Adjusted, group=State,
                                     color=State, fill=State)) +
geom_smooth(method="rlm", formula=y~ns(x,3), aes(group=State,
                            fill=State, color=State), alpha=0.1) + labs(y="Assault Deaths per 100,000 population")

q1 <- q + guides(fill = guide_legend(ncol = 5), colour =
                 guide_legend(override.aes = list(alpha = 1))) +
  theme_bw() + theme(title="Death Rates from Assault by State (excluding DC), 1999-2009", legend.position="bottom")

png(file="figures/assault-deaths-us-ts-state-exdc.png", height=1800, width=1200,
    res=140, pointsize=11)
print(q1)
credit()
dev.off()

pdf(file="figures/assault-deaths-us-ts-state-exdc.pdf", width=10, height=13)
print(q1)
credit()
dev.off()

### Facet states excluding DC
q2 <- ggplot(data.us.state[no.dc,], aes(Year, Adjusted, group=State))
q3 <- q2 + geom_smooth(method="rlm", formula=y~ns(x,3), size=2) + labs(y = "Assault Deaths per 100,000") + facet_wrap(~ State, ncol=5) + theme_bw()

credit <- function() {
  return(makeFootnote("Source: CDC WONDER. Age-adjusted. Estimates are unavailable for some states with very low assault rates. Kieran Healy. http://kieranhealy.org"))
}

pdf(file="figures/assault-deaths-states-ts-facet.pdf", width=15,
    height=30, pointsize=9)
print(q3)
credit()
dev.off()

png(file="figures/assault-deaths-states-ts-facet.png", height=3200, width=1100,
    res=100, pointsize=9)
print(q3)
credit()
dev.off()

###--------------------------------------------------
### Merge US region and OECD
###--------------------------------------------------
head(data.us.region)

data.rm <- data.us.region[,c(1,1,2,8)]
colnames(data.rm) <- c("Country", "Dummy", "Year", "Deaths")
data.comp <- rbind(data.m, data.rm)
colnames(data.comp) <- c("Country", "Region", "Year", "Deaths")
library(gdata)
ind.o <- order(data.comp$Deaths, decreasing=TRUE)
data.comp$Region <- reorder.factor(data.comp[,"Region"], new.order=data.comp$Region[ind.o])
detach(package:gdata)

##Short year
data.comp$Yr <- strftime(data.comp$Year, format="%Y")

p <- ggplot(data.comp, aes(Year, Deaths, group=Country, color=Region))
p1 <- p + geom_point(size=0.8) + geom_smooth(method="loess",
    aes(fill=Region)) + labs(y =
    "Assault Deaths per 100,000 population") + theme(legend.position="top") + theme_bw()


credit <- function() {
  return(makeFootnote("\nSource: OECD/CDC WONDER. Age-adjusted.\nKieran Healy. http://kieranhealy.org"))
}

png(file="figures/assault-deaths-oecd-vs-us-regions.png", height=900, width=1100,
    res=140, pointsize=11)
print(p1)
credit()
dev.off()

pdf(file="figures/assault-deaths-oecd-vs-us-regions.pdf", width=10, height=8)
print(p1)
credit()
dev.off()

### US Race and OECD (req. by @aaronsw)
head(data.us.race)

data.race.oecd <- data.us.race[,c(1,1,2,8)]
colnames(data.race.oecd) <- c("Country", "Dummy", "Year", "Deaths")
data.comp <- rbind(data.m, data.race.oecd)
colnames(data.comp) <- c("Country", "Group", "Year", "Deaths")
library(gdata)
ind.o <- order(data.comp$Deaths, decreasing=TRUE)
data.comp$Group <- reorder.factor(data.comp[,"Group"], new.order=data.comp$Group[ind.o])
detach(package:gdata)

p <- ggplot(data.comp, aes(Year, Deaths, group=Country, color=Group))
p1 <- p + geom_point(size=0.8) + geom_smooth(method="rlm",
                       formula=y~ns(x,3),
    aes(fill=Group)) + labs(y =
    "Assault Deaths per 100,000 population") + theme(legend.position="top") + theme_bw()


credit <- function() {
  return(makeFootnote("\nSource: OECD/CDC WONDER. Age-adjusted.\nKieran Healy. http://kieranhealy.org"))
}

png(file="figures/assault-deaths-oecd-vs-us-race.png", height=900, width=1100,
    res=140, pointsize=11)
print(p1)
credit()
dev.off()

pdf(file="figures/assault-deaths-oecd-vs-us-race.pdf", width=9, height=8)
print(p1)
credit()
dev.off()

###--------------------------------------------------
### All countries in the OECD data
###--------------------------------------------------
dum3 <- c(rep("Other OECD", 7), "Estonia", rep("Other OECD", 12), "Mexico",
           rep("Other OECD", 11), "United States")

data$dum3 <- dum3
ccode.all <- data$Country
ind.all <- data$Country %in% ccode.all
data.all <- data[ind.all,]

## Not strictly necessary
data.m.all <- melt(data.all, id.vars=c("Country","usdum","dum3"))
colnames(data.m.all) <- c("Country", "US-Dummy","US.Mex.Est", "Year", "Deaths")
data.m.all$Year <- strptime(data.m.all$Year, "%Y")
data.m.all$US.Mex.Est <- as.factor(data.m.all$US.Mex.Est)

library(gdata)
data.m.all$Region <- reorder.factor(data.m.all[,"US.Mex.Est"], new.order=c("Mexico", "Estonia", "United States", "Other OECD"))
detach(package:gdata)


credit <- function() {
  return(makeFootnote("\n\nData: OECD. http://kieranhealy.org"))
}

png(file="figures/assault-deaths-oecd-ts-all-2.png", height=900, width=1100,
    res=140, pointsize=11)
p <- ggplot(data.m.all, aes(Year, Deaths, group=Country, color=US.Mex.Est))
p + geom_point(size=0.8) + geom_smooth(method="loess", aes(fill = US.Mex.Est)) + guides(color=guide_legend(title="Country")) + labs(y ="Assault Deaths per 100k population") + theme(legend.position="top") + theme_bw() + scale_colour_discrete(name  ="Country",
                            breaks=c("Mexico", "Estonia", "United States", "Other OECD"),
                            labels=c("Mexico", "Estonia", "United States", "Other OECD")) + scale_fill_discrete(name  ="Country",
                            breaks=c("Mexico", "Estonia", "United States", "Other OECD"),
                            labels=c("Mexico", "Estonia", "United States", "Other OECD"))
credit()
dev.off()

pdf(file="figures/assault-deaths-oecd-ts-all-2.pdf", width=10, height=8)
p <- ggplot(data.m.all, aes(Year, Deaths, group=Country, color=US.Mex.Est))
p + geom_point(size=0.8) + geom_smooth(method="loess", aes(fill = US.Mex.Est)) + guides(color=guide_legend(title="Country")) + labs(y ="Assault Deaths per 100k population") + theme(legend.position="top") + theme_bw() + scale_colour_discrete(name  ="Country",
                            breaks=c("Mexico", "Estonia", "United States", "Other OECD"),
                            labels=c("Mexico", "Estonia", "United States", "Other OECD")) + scale_fill_discrete(name  ="Country",
                            breaks=c("Mexico", "Estonia", "United States", "Other OECD"),
                            labels=c("Mexico", "Estonia", "United States", "Other OECD"))
credit()
dev.off()


png(file="figures/assault-deaths-oecd-ts-facet-2.png", height=2100, width=1100,
    res=100, pointsize=9)
p <- ggplot(data.m.all, aes(Year, Deaths, group=Country))
p + geom_point() + geom_smooth(method="loess") + labs(y = "Assault Deaths per 100k") + facet_wrap(~ Country, ncol=5) + theme_bw()
credit()
dev.off()

pdf(file="figures/assault-deaths-oecd-ts-facet-2.pdf", width=14,
    height=22, pointsize=9)
p <- ggplot(data.m.all, aes(Year, Deaths, group=Country))
p + geom_point() + geom_smooth(method="loess") + labs(y = "Assault Deaths per 100k") + facet_wrap(~ Country, ncol=5) + theme_bw()
credit()
dev.off()
