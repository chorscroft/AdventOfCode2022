# Advent of Code Day 2
# Part 1

## Read in the data
mydata<-read.table("data/day02.txt",col.names=c("Them","Strategy"))

## Initialise score at 0
score<-0

## Iterate over each game
for(i in 1:nrow(mydata)){
  
  ## Increase the score based on the the result of the game
  # 1 for Rock, 2 for Paper, and 3 for Scissors
  # 0 for lose, 3 for draw, 6 for win
  # A&X = Rock, B&Y = Paper, C&Z = Scissors
  if(mydata$Them[i] == "A" & mydata$Strategy[i] == "X"){
    score<-score+3+1
  } else if(mydata$Them[i] == "A" & mydata$Strategy[i] == "Y"){
    score<-score+6+2
  } else if(mydata$Them[i] == "A" & mydata$Strategy[i] == "Z"){
    score<-score+0+3
  } else if(mydata$Them[i] == "B" & mydata$Strategy[i] == "X"){
    score<-score+0+1
  } else if(mydata$Them[i] == "B" & mydata$Strategy[i] == "Y"){
    score<-score+3+2
  } else if(mydata$Them[i] == "B" & mydata$Strategy[i] == "Z"){
    score<-score+6+3
  } else if(mydata$Them[i] == "C" & mydata$Strategy[i] == "X"){
    score<-score+6+1
  } else if(mydata$Them[i] == "C" & mydata$Strategy[i] == "Y"){
    score<-score+0+2
  } else {
    score<-score+3+3
  }
}

## Output the final score
score

###########################
# Part 2

## Initialise score at 0
score<-0

## Iterate over each game
for(i in 1:nrow(mydata)){
  ## Increase the score based on the the result of the game
  # 1 for Rock, 2 for Paper, and 3 for Scissors
  # 0 for lose, 3 for draw, 6 for win
  # A = Rock, B = Paper, C = Scissors
  # X = lose, Y = draw, Z = win
  if(mydata$Them[i] == "A" & mydata$Strategy[i] == "X"){
    score<-score+0+3
  } else if(mydata$Them[i] == "A" & mydata$Strategy[i] == "Y"){
    score<-score+3+1
  } else if(mydata$Them[i] == "A" & mydata$Strategy[i] == "Z"){
    score<-score+6+2
  } else if(mydata$Them[i] == "B" & mydata$Strategy[i] == "X"){
    score<-score+0+1
  } else if(mydata$Them[i] == "B" & mydata$Strategy[i] == "Y"){
    score<-score+3+2
  } else if(mydata$Them[i] == "B" & mydata$Strategy[i] == "Z"){
    score<-score+6+3
  } else if(mydata$Them[i] == "C" & mydata$Strategy[i] == "X"){
    score<-score+0+2
  } else if(mydata$Them[i] == "C" & mydata$Strategy[i] == "Y"){
    score<-score+3+3
  } else {
    score<-score+6+1
  }
}

## Output the final score
score

