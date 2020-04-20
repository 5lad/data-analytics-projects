# import data (cleaned)
StatsFinal <- read.csv("DA_Project_Group8_MLR_Data_Cleaning.csv")

# remove rows with all 0s
StatsFinal = StatsFinal[which(rowSums(StatsFinal) > 0),]

# Normalize values in all columns:

VS_mean = mean(StatsFinal$teamVS)
VS_stdev = sd(StatsFinal$teamVS)
StatsFinal$teamVS = (StatsFinal$teamVS-VS_mean) / VS_stdev

TDC_mean = mean(StatsFinal$teamTDC)
TDC_stdev = sd(StatsFinal$teamTDC)
StatsFinal$teamTDC = (StatsFinal$teamTDC-TDC_mean) / TDC_stdev

TDT_mean = mean(StatsFinal$teamTDT)
TDT_stdev = sd(StatsFinal$teamTDT)
StatsFinal$teamTDT = (StatsFinal$teamTDT-TDT_mean) / TDT_stdev

Kills_mean = mean(StatsFinal$teamKills)
Kills_stdev = sd(StatsFinal$teamKills)
StatsFinal$teamKills = (StatsFinal$teamKills-Kills_mean) / Kills_stdev

Assists_mean = mean(StatsFinal$teamAssists)
Assists_stdev = sd(StatsFinal$teamAssists)
StatsFinal$teamAssists = (StatsFinal$teamAssists-Assists_mean) / Assists_stdev

Deaths_mean = mean(StatsFinal$teamDeaths)
Deaths_stdev = sd(StatsFinal$teamDeaths)
StatsFinal$teamDeaths = (StatsFinal$teamDeaths-Deaths_mean) / Deaths_stdev

# create a column for team KDA with the normalized values
StatsFinal$teamKDA = (StatsFinal$teamKills+StatsFinal$teamAssists) / StatsFinal$teamDeaths

# remove the columns for Kills, Assists and Deaths (since we already included KDA)
StatsFinal = StatsFinal[,-c(1:3)]

# Add in interaction terms:

StatsFinal$VS_TDC = StatsFinal$teamVS * StatsFinal$teamTDC
StatsFinal$VS_TDT = StatsFinal$teamVS * StatsFinal$teamTDT
StatsFinal$TDT_TDC = StatsFinal$teamTDT * StatsFinal$teamTDC

# Run MLR:
m2 = lm(teamKDA~., data=StatsFinal)
summary(m2)

# Perform BSS:

# load library leaps, which has the regsubsets() command for subset selection
library(leaps)

regfit.2 = regsubsets(teamKDA~., data=StatsFinal, nvmax=6)
summary(regfit.2)