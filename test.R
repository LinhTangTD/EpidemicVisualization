#library(shiny)
#library(shinythemes)
library(readr)
#library(ggplot2)
library(dplyr)
library(tidyr)
#library(gdata)
#library(stats)
#library(curl)
library(lubridate)
library(rpart)
library(rpart.plot)
library(vcd)

#Pull data from server and select data since Dec 19, 2019
data <- read_csv("https://www.stat2games.sites.grinnell.edu/data/epidemic/getdata.php")
benchmark <- as.Date("2019-12-19")
data = data %>% filter(Date > benchmark)

#Filter by Level
data <- data %>% filter(Level > 0)

#Converting PlayerID and GroupID to lower case
data$PlayerID <- tolower(data$PlayerID)
data$GroupID <- tolower(data$GroupID)

#Convering Certain Columns to Factors
data$TurretType <- as.factor(data$TurretType)
data$Level <- as.factor(data$Level)
data$Upgrade <- as.factor(data$Upgrade)
data$Medicine <- as.factor(data$Medicine)
data$Virus <- as.factor(data$Virus)
data$Wave <- as.factor(data$Wave)
data$Location <- as.factor(data$Location)

#Creating Additional Columns
data <- mutate(data, PercDestroyed = Destroyed/Shot)
data <- mutate(data, Missed = Shot - Destroyed)
data <- mutate(data, PerMissed = Missed/Shot)
data <- mutate(data, PerDefective = Destroyed/Shot)

#Splitting data by level
level1 <- data %>% filter(Level == "1")
level2 <- data %>% filter(Level == "2")
level2B <- data %>% filter(Level == "2B")
level3 <- data %>% filter(Level == "3")

#Building regression tree and mosaic plot

#Level 1
model_level1 = rpart(PercDestroyed ~ Medicine + Virus, method = "anova", data = level1)
#rpart.plot(model_level1, uniform=TRUE, main="Level 1")
prp(model_level1, type = 5)
tbl <- xtabs(PercDestroyed ~ Medicine + Virus, level1)

#Level 2
model_level2 = rpart(PercDestroyed ~ Medicine, method = "anova", data = level2)
#rpart.plot(model_level2, uniform=TRUE, main="Level 2")
prp(model_level2, type = 5)

#Level 2B
model_level2B = rpart(PercDestroyed ~ TurretType + Medicine, method = "anova", data = level2B, control=rpart.control(minbucket = 10, minsplit = 1))
#rpart.plot(model_level2B, uniform=TRUE, main="Level 2B")
prp(model_level2B, type = 5)

#Level 3
model_level3 = rpart(PercDestroyed ~ Medicine + Virus, method = "anova", data = level3)
#rpart.plot(model_level3, box.palette="RdBu", main="Level 3")
prp(model_level3, type = 5)
