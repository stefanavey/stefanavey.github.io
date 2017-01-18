#################################################################################
## traffic.R
## 
##
## Short Summary: Explore traffic fatalities data from US Gov
##
## Copyright 2016 Stefan Avey
##
## Author: Stefan Avey <stefan.avey@yale.edu>
## Date Created: 2016/09/16 13:57:58
## Version: $Id: traffic.R, v 0.0
## Project Directory: stefanavey.github.io/_posts/opendata/TrafficFatalities/
## Keywords: traffic; opendata; gov
##
## Description:
##
## EDA
##
## Header generated automatically by TEMPLATE.R.tpl
#################################################################################


###################
## Load packages ##
###################
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)

###############
## Load Data ##
###############
datDir <- "FARS2015NationalCSV"
accFile <- file.path(datDir, "ACC_AUX.CSV")
stateFile <- file.path(datDir, "states.csv")
popFile <- file.path(datDir, "popSize.csv")
regVehFile <- file.path(datDir, "registered_vehicles_by_state.xlsx")

acc <- read.csv(accFile)
states <- read.csv(stateFile)
pop <- read.csv(popFile)
regVeh <- read.xlsx(regVehFile, sheet = 2, startRow = 6, colNames = FALSE)
colnames(regVeh) <- c("STATE_NAME", "REGISTERED_VEHICLES")
regVeh$STATE_NAME <- gsub("*", "", regVeh$STATE_NAME, fixed = TRUE)
regVeh$STATE_NAME <- gsub("Dist. of Col.", "District of Columbia",
                          regVeh$STATE_NAME, fixed = TRUE)

dat <- acc
dat$STATE_NAME <- factor(states[match(dat$STATE, states$STATE_CODE), "STATE_NAME"])
dat$STATE_ABBRV <- factor(states[match(dat$STATE, states$STATE_CODE), "STATE_ABBRV"])
dat$POP_ESTIMATE_2015 <- pop$POPESTIMATE2015[match(dat$STATE_NAME, pop$NAME)]
dat$REGISTERED_VEHICLES <- regVeh$REGISTERED_VEHICLES[match(dat$STATE_NAME,
                                                            regVeh$STATE_NAME)]
  
## Visualize accidents by state
plotDat <- dat %>%
  group_by(STATE_ABBRV) %>%
  summarize(TotalFatalities = sum(FATALS))

ggplot(data = plotDat, aes(x = STATE_ABBRV, y = TotalFatalities)) +
  geom_bar(stat = "identity") +
  xlab("US State or Territory") +
  ylab("2015 Traffic Fatalities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Visualize accidents by state scaled by state population size
plotDat2 <- dat %>%
  group_by(STATE_ABBRV) %>%
  summarize(PercentOfPopFatalities = sum(FATALS)/mean(POP_ESTIMATE_2015))

ggplot(data = plotDat2, aes(x = STATE_ABBRV, y = PercentOfPopFatalities)) +
  geom_bar(stat = "identity") +
  xlab("US State or Territory") +
  ylab("2015 Traffic Fatalities (as a Percent of State Population)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Visualize accidents by state scaled by # of registered vehicles in each state
plotDat3 <- dat %>%
  group_by(STATE_ABBRV) %>%
  summarize(PercentOfPopFatalities = sum(FATALS)/mean(REGISTERED_VEHICLES))

ggplot(data = plotDat3, aes(x = STATE_ABBRV, y = PercentOfPopFatalities)) +
  geom_bar(stat = "identity") +
  xlab("US State or Territory") +
  ylab("2015 Traffic Fatalities (as a Percent of Registered Motor Vehicles)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

