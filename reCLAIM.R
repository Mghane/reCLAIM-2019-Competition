options(java.parameters = "-Xmx6g")
library(XLConnect)
library(rJava)
library(xlsx)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lattice)
library(scales)
library(plotly)
library(htmlwidgets)
library(data.table)
library(stats)
library(mosaic)
library(tidyselect)
setwd("C:/Users/Mehdi/Desktop/Desktop/Data Science/Projects/reCLAIM/regression_comp_starter_kit")
train <- fread("./Header - train.csv", na.strings = c("Unspecified","Not Available"))
vvalidation <- fread("./Viking - Validation.csv", na.strings = c("Unspecified","Not Available"))
vtrain <- fread("./Viking - Train.csv", na.strings = c("Unspecified","Not Available"))
tall <- tally(group_by(train,Province, Formation, Pool, Field), sort = TRUE, na.rm=T)
nr <- contains("Viking", ignore.case = TRUE, vars = train$Formation)
ftrain <- train[nr,]
mviking <- rbind(vtrain,vvalidation)
count(isTRUE(duplicated(mviking$EPAssetsId)))
df <- merge(ftrain, mviking, by="EPAssetsId", all = TRUE)
df <- mutate(df, Hlength = TotalDepth - TVD)
df <- mutate(df, Hlength2 = sqrt((Surf_Longitude-BH_Longitude)^2+(Surf_Latitude-BH_Latitude)^2))
nrn <- count(is.na(df$Hlength))
nrn2 <- count(is.na(df$Hlength2))
count(df$Hlength<0)
min(df$Hlength,na.rm = TRUE)
head <- colnames(df)
head[87] <- "MaxOil_BOE"
head[88] <- "Frac_Stages"
head[89] <- "OpenHole"
head[90] <- "Comp_Events"
head[92] <- "Norm_BOE"
head[93] <- "Norm_Oil"
head[94] <- "Norm_Gas"
head[95] <- "Norm_Water"
colnames(df) <- head
df$Hlength[which(df$Hlength<0)] <- 0
favstats(df$Hlength)
quantile(df$Hlength, c(0,0.01,0.05,0.1,0.25,0.5,0.75,1), na.rm = T)
par(mfrow=c(2,1), mar=c(4,3,1,2), oma=c(0,0,3,0))
fdf <- filter(df, WellSymbPt1==c("Oil"))
hist(df$Hlength, col = "Blue")
hist(df$Hlength2, col = factor(df$Province))
ggplot(data = df, aes(y = MaxOil_BOE, x = Hlength)) + geom_point(col="blue",size = 2,position = "jitter") +xlim(c(1,2000))+ylim(c(1,500)) + ylab("Max Oil") + xlab("Horizontal Length") + ggtitle("Scatterplot")
ggplot(data = df, aes(y = Norm_Oil, x = Hlength2)) + geom_point(col="blue",size = 2,position = "jitter") +xlim(c(0,0.05))+ylim(c(1,100))
qplot(Norm_Oil, data = df, bins=20, binwidth=2, fill=Province, facets = WellSymbPt1~.)
qplot(df$WellSymbPt1, Hlength2, data = df, geom = "boxplot")
qplot(Norm_Oil, Hlength2, data = fdf, color=WellSymbPt1, shape=WellSymbPt1,facets = Surf_QuarterUnit~.) + geom_smooth(method = "lm")



