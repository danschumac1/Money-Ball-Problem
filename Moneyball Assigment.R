library(ggplot2)
library(dplyr)
library(plotly)

# Data
# We'll be using data from Sean Lahaman's Website a very useful source for baseball statistics. 
# The documentation for the csv files is located in the readme2013.txt file. 
# You may need to reference this to understand what acronyms stand for.
# 
# Use R to open the Batting.csv file and assign it to a dataframe called batting using read.csv

batting <- read.csv('Batting.csv')

#Use head() to check out the batting

head(batting)

# Use str() to check the structure. 
# Pay close attention to how columns that start with a number get an 'X' in front of them! 
# You'll need to know this to call those columns!

str(batting)

#Call the head() of the first five rows of AB (At Bats) column

head(batting$AB)

#Call the head of the doubles (X2B) column

head(batting$X2B)

# Feature Engineering
# We need to add three more statistics that were used in Moneyball! These are:
#   
#   Batting Average
#   On Base Percentage
#   Slugging Percentage
#   
# Click on the links provided and search the wikipedia page for the formula for creating the new statistic! 

batting$BA <- batting$H / batting$AB
tail(batting$BA,5)

#Create an OBP Column

H <- batting$H
BB <- batting$BB
HBP <- batting$HBP
AB <- batting$AB
SF <- batting$SF

batting$OBP <- (H+BB+HBP)/(AB+BB+HBP+SF)

head(batting$OBP)

#Create an SLG Column

X2B <- batting$X2B
X3B <- batting$X3B
HR <- batting$HR
AB <- batting$AB
batting$x1B <- batting$H - batting$X2B - batting$X3B - batting$HR
x1B <- batting$x1B
batting$SLG <- (x1B)+(2*X2B)+(3*X3B)+(4*HR)/(AB)

head(batting$SLG)

#Check the structure of your data frame using str()

str(batting)

#Load the Salaries.csv file into a dataframe called sal using read.csv

sal <- read.csv('Salaries.csv')

#starts in 1871!
summary(batting)
#only starts in 1985
summary(sal)

#Use subset() to reassign batting to only contain data from 1985 and onwards

batting <- subset(batting,yearID >= 1985)

#Now use summary again to make sure the subset reassignment worked, 
#your yearID min should be 1985
summary(batting$yearID)

#Use the merge() function to merge the batting and sal data frames by
#c('playerID','yearID'). Call the new data frame combo

combo <- merge(batting,sal, by=c('playerID','yearID'))

#Use summary to check the data
summary(combo)

#lost players
# codes are giambja01 damonjo01 saenzol01

x <- c('giambja01', 'damonjo01', 'saenzol01')

lost_players <- subset(combo, playerID %in% x)
#check it
unique(lost_players$playerID)

#Since all these players were lost in after 2001 in the offseason, 
#let's only concern ourselves with the data from 2001.
#Use subset again to only grab the rows where the yearID was 2001.

lost_players <- subset(lost_players, yearID == 2001)
#check it
unique(lost_players$yearID)

#Reduce the lost_players data frame to the following columns: 
#playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB

reduction <- c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')
lost_players <- lost_players[reduction]

head(lost_players)

# Replacement Players
# Now we have all the information we need! Here is your final task 
# - Find Replacement Players for the key three players we lost! 
#   
#   However, you have three constraints:
#   
#   The total combined salary of the three players can not exceed 15 million dollars.
#   Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
#   Their mean OBP had to equal to or greater than the mean OBP of the lost players
#   
# Use the combo dataframe you previously created as the source of information! 
# Remember to just use the 2001 subset of that dataframe. 
# There's lost of different ways you can do this, so be creative! 
# It should be relatively simple to find 3 players that satisfy the requirements, 
# note that there are many correct combinations available!

#let's break down each criteria
#1: The total combined salary of the three players can not exceed 15 million dollars.

Money.goal <- 15000000
money.goal.pp <- 5000000

# 2: The total combined salary of the three players can not exceed 15 million dollars.
#What's the total at bat that we need to beat?
AB.goal <- sum(lost_players$AB)
AB.goal

#or you could think we need players on avg higher than this number
AB.goal.pp <- AB.goal/3
AB.goal.pp

# 2: Their mean OBP had to equal to or greater than the mean OBP of the lost players
OBP.goal <- mean(lost_players$OBP)
OBP.goal

#let's filter down our data to start picking new players
# I remember in the book the player's on base percentage was the most important thing.

combo <- subset(combo, yearID == 2001)



fits.requirements <- combo %>% subset(OBP>OBP.goal) %>% subset(AB > AB.goal.pp) %>% subset(salary < money.goal.pp+3000000)
summary(fits.requirements)

  
pl <- ggplot(fits.requirements, aes(x=OBP,y=salary,text=playerID))
pl <- pl + geom_point(aes(color=AB)) + scale_colour_gradient(high='red',low = "green")
gpl <- ggplotly(pl,tooltip = 'text')
print(gpl)

subset(fits.requirements,playerID=='boggswa01')



#CONCLUSION
# I really wanted to get giambja01 before I remembered he is one of the lost players!
#the three players that I would hire would be:
  #heltoto01
  #berkmla01
  #gonzalu01

#these three players meet the at bat, salary, and OBP requirements.
#berkmla01 is in particular a great great bargain.
