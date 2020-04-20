library (dplyr)
library(ISLR)
library(writexl)

#read in our csv files
Stats <- read.csv("stats1.csv")
Champs <- read.csv("champs.csv")
Matches <- read.csv("matches.csv")
Stats2 <- read.csv("Stats2.csv")
Participants <- read.csv('participants.csv')

#Manually add the one missing player from the other excel file back to the one we are using
Stats[1000000,] = Stats2[1,]
Participants <- Participants[1:1000000,]
Stats$Champion_id = Participants$championid
Stats_Test <- Stats

#Cleanup:---------------------------------------------------

#Create a MatchID
Stats$match_index <- c(0, rep(1:(nrow(Stats)-1)%/%10))
Stats_Test$match_index <- c(0, rep(1:(nrow(Stats)-1)%/%10))

#Create a new column for summed KDA
Stats$AFK <- Stats$kills + Stats$deaths + Stats$assists

#Find those who did not afk in the game, or who had a aggregrate KDA score of > 0 (cant be negative)
Stats_Test <- Stats[!(Stats$AFK == 0),]

#Delete all matches which don't have 10 participants
Stats_Final <- Stats_Test[Stats_Test$match_index %in% names(which(table(Stats_Test$match_index) > 9)), ]


#Select the 3 columns that are important to us
Stats_final <- Stats_Final%>%select(id,win,match_index)


#Create 6 classifications for all the champions
Champs$is_Fighter = as.integer(Champs$Champion.Class == 'Fighter')
Champs$is_Mage = as.integer(Champs$Champion.Class == 'Mage')
Champs$is_Controller = as.integer(Champs$Champion.Class == 'CC')
Champs$is_Marksman = as.integer(Champs$Champion.Class == 'Marksman')
Champs$is_Tank = as.integer(Champs$Champion.Class == 'Tank')
Champs$is_Slayer = as.integer(Champs$Champion.Class == 'Slayer')

#check
#table(Champs$is_Fighter)
#table(Champs$is_Controller)
#table(Champs$is_Mage)
#table(Champs$is_Marksman)
#table(Champs$is_Tank)
#table(Champs$is_Slayer)

#Found blitzcrank needed to be manually assigned
subset(Champs,is_Fighter ==0 &is_Controller ==0 &is_Mage ==0 & is_Marksman ==0 &is_Tank ==0 & is_Slayer ==0)
Champs[61,6] =1

Stats_final$Champions = Stats_Final$Champion_id


#make column to represent which team a player is on

Stats_final$team <- rep(0, nrow(Stats_final))
Stats_final$team <- c(0, rep(1:(nrow(Stats_final)-1)%/%5))

#Assign 0's to all of the classification of champions' column

Stats_final$is_Fighter = rep(0, nrow(Stats_final))
Stats_final$is_Mage = rep(0, nrow(Stats_final))
Stats_final$is_Controller = rep(0, nrow(Stats_final))
Stats_final$is_Marksman = rep(0, nrow(Stats_final))
Stats_final$is_Tank = rep(0, nrow(Stats_final))
Stats_final$is_Slayer = rep(0, nrow(Stats_final))

#create 6 lists that contain the champion id number and place them in their classification list
list_marksman = list(18, 110, 202, 15, 21, 119, 96, 222, 81, 236, 51, 203, 133, 42, 67, 498, 22, 429, 22)
list_Controller = list(37, 27, 223, 412, 43, 516, 78, 12, 113, 89, 53, 267, 497, 111, 40, 16, 432, 427, 117)
list_Fighter = list(24, 114, 68, 77, 64, 83, 141, 157, 58, 120, 6, 80, 41, 150, 82, 92, 266, 420, 5, 86, 48, 126, 122, 23, 104, 102, 19, 240, 2, 39, 62, 254, 421, 164, 11, 59)
list_Mage = list(7, 38, 10, 127, 9, 17, 1, 268, 85, 163, 74, 76, 161, 245, 50, 134, 45, 101, 103, 60, 63, 30, 8, 26, 115, 4, 84, 131, 136, 143, 112, 69, 61, 99, 34, 90, 79)
list_Slayer = list(35, 105, 91, 238, 55, 121, 107, 28)
list_Tank = list(57, 3, 31, 106, 33, 44, 72, 201, 20, 32, 56, 36, 14, 54, 75, 98, 154)

#Categorizing the champs for each player
Stats_final$is_Marksman <- as.integer(Stats_final[,4] %in% list_marksman)
Stats_final$is_Controller <- as.integer(Stats_final[,4] %in% list_Controller)
Stats_final$is_Fighter <- as.integer(Stats_final[,4] %in% list_Fighter)
Stats_final$is_Mage <- as.integer(Stats_final[,4] %in% list_Mage)
Stats_final$is_Slayer <- as.integer(Stats_final[,4] %in% list_Slayer)
Stats_final$is_Tank <- as.integer(Stats_final[,4] %in% list_Tank)

#Find the sum of each champion classification on a team
attach(Stats_final)
Aggregate_Fighter <-aggregate(is_Fighter~ team, data=Stats_final, FUN=sum)
Aggregate_Marksman <-aggregate(is_Marksman~ team, data=Stats_final, FUN=sum)
Aggregate_Controller <-aggregate(is_Controller~ team, data=Stats_final, FUN=sum)
Aggregate_Mage <-aggregate(is_Mage~ team, data=Stats_final, FUN=sum)
Aggregate_Slayer <-aggregate(is_Slayer~ team, data=Stats_final, FUN=sum)
Aggregate_Tank <-aggregate(is_Tank~ team, data=Stats_final, FUN=sum)
Aggregate_Win <-aggregate(win~ team, data=Stats_final, FUN=mean)

#Aggregate all of the values created above and put it into a dataframe labeled Overview
Overview <- data.frame("Team"= Aggregate_Win$team,"Win" = Aggregate_Win$win,"NumFighter" =Aggregate_Fighter$is_Fighter,'NumMarksman' = Aggregate_Marksman$is_Marksman,'NumController' = Aggregate_Controller$is_Controller
                       , "NumMage" = Aggregate_Mage$is_Mage,"NumSlayer" = Aggregate_Slayer$is_Slayer,"NumTank" = Aggregate_Tank$is_Tank)


#----------------------------------------------------------------------------------------------------------------------------------------------------------

#names(Overview)
#write_xlsx(Overview, "Overview.xlsx")

#Split data into train and test
set.seed(100)
train = sample(1:nrow(Overview), nrow(Overview)/2)
Overview.train = Overview[train,]
Result.train = Overview$Win[train,]
Overview.test = Overview[-train,]
Result.test = Overview$Win[-train,]

#run a logisitic regression
train_logreg = glm(Win~ NumFighter + NumMarksman + NumController + NumMage + NumSlayer + NumTank, family=quasibinomial, data=Overview.train)
summary(train_logreg)

#find the accuracy of our model
test_prob = predict(train_logreg,Overview.test,type = "response")
View(test_prob)
glm.pred = rep (0,length(test_prob))
glm.pred[test_prob >=0.5] = 1

head(glm.pred)
mean(glm.pred == Overview.test$Win)
