options(java.parameters = "-Xmx6g")
library(XLConnect)
library(rJava)
library(xlsx)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(htmlwidgets)
library(data.table)
library(stats)
library(mosaic)
library(tidyselect)
setwd("C:/Users/Mehdi/Desktop/Desktop/Data Science/Projects/reCLAIM/regression_comp_starter_kit")
df <- fread("./Header - train.csv", na.strings = c("Unspecified","Not Available"))
##aggregate(df$UWI, by=list(df$Province,df$Formation, df$Field, df$Pool),FUN=count, na.rm=T)
tall <- tally(group_by(df,Province, Formation, Pool, Field), sort = TRUE, na.rm=T)
nr <- contains("Viking", ignore.case = TRUE, vars = df$Formation)
dff <- df[nr,]