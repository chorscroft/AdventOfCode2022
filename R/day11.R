# Advent of Code Day 11
# Part 1

## Read in the data
mydata<-read.table("data/day11.txt",sep=":")

## get number of monkeys in the dataset
nomonkeys<-nrow(mydata)/6

## Create a list where each element is the information for a monkey
monkeys<-vector("list",nomonkeys)

## initialise the current monkey as 0
monkey<-0

## Loop over each row in the data
for (i in 1:nrow(mydata)){
  if(i %% 6 == 1){
    ## iterate the monkey number
    monkey<-monkey+1
  } else if (i %% 6 == 2){
    ## store the current items as a vector
    monkeys[[monkey]]$items<-as.numeric(unlist(strsplit(mydata$V2[i],",")))
  } else if (i %% 6 == 3){
    ## store the operation as a vector (operator and amount)
    monkeys[[monkey]]$operation<-unlist(strsplit(mydata$V2[i]," "))[5:6]
  } else if (i %% 6 == 4){
    ## store the test value
    monkeys[[monkey]]$test<-as.numeric(unlist(strsplit(mydata$V2[i]," "))[4])
  } else if (i %% 6 == 5){
    ## store which monkey to throw to if test is passed (1-indexed)
    monkeys[[monkey]]$iftrue<-as.numeric(unlist(strsplit(mydata$V2[i]," "))[5])+1
  } else if (i %% 6 == 0){
    ## store which monkey to throw to if test is failed (1-indexed)
    monkeys[[monkey]]$iffalse<-as.numeric(unlist(strsplit(mydata$V2[i]," "))[5])+1
  }
}

## initialise the number of items inspected by each monkey as 0
inspections<-rep(0,nomonkeys)

## loop over each round
for(round in 1:20){
  ## loop over each monkey
  for(monkey in 1:nomonkeys){
    ## if the monkey has items, loop over each item
    if(length(monkeys[[monkey]]$items)>0){
      for(i in 1:length(monkeys[[monkey]]$items)){
        ## iterate the number of items this monkey has inspected
        inspections[monkey]<-inspections[monkey]+1
        ## get the worry value for the current item
        worry<-monkeys[[monkey]]$items[i]
        ## do the operation on the worry value
        if(monkeys[[monkey]]$operation[1]=="+"){
          if(monkeys[[monkey]]$operation[2]=="old"){
            worry <- worry + worry
          } else {
            worry<- worry + as.numeric(monkeys[[monkey]]$operation[2])
          }
        } else if (monkeys[[monkey]]$operation[1]=="*"){
          if(monkeys[[monkey]]$operation[2]=="old"){
            worry <- worry * worry
          } else {
            worry<- worry * as.numeric(monkeys[[monkey]]$operation[2])
          }         
        }
        ## divide the worry value by three (rounded down)
        worry <- floor(worry/3)
        ## do the test on the current worry value
        if(worry %% monkeys[[monkey]]$test == 0){
          ## add item to the list of the relevant monkey
          monkeys[[monkeys[[monkey]]$iftrue]]$items<-c(monkeys[[monkeys[[monkey]]$iftrue]]$items,worry)
        } else {
          ## add item to the list of the relevant monkey
          monkeys[[monkeys[[monkey]]$iffalse]]$items<-c(monkeys[[monkeys[[monkey]]$iffalse]]$items,worry)
        }
      }
      ## remove all items from the current monkey's list
      monkeys[[monkey]]$items <- NULL
    }
  }
}

## sort the number of inspections for each monkey from highest to lowest
inspections<-sort(inspections,decreasing = T)
## multiply the two highest inspection counts to get the answer
inspections[1]*inspections[2]



###########################
# Part 2

## Create a list where each element is the information for a monkey
monkeys<-vector("list",nomonkeys)

## initialise the current monkey as 0
monkey<-0

## Loop over each row in the data
for (i in 1:nrow(mydata)){
  if(i %% 6 == 1){
    ## iterate the monkey number
    monkey<-monkey+1
  } else if (i %% 6 == 2){
    ## store the current items as a vector
    monkeys[[monkey]]$items<-as.numeric(unlist(strsplit(mydata$V2[i],",")))
  } else if (i %% 6 == 3){
    ## store the operation as a vector (operator and amount)
    monkeys[[monkey]]$operation<-unlist(strsplit(mydata$V2[i]," "))[5:6]
  } else if (i %% 6 == 4){
    ## store the test value
    monkeys[[monkey]]$test<-as.numeric(unlist(strsplit(mydata$V2[i]," "))[4])
  } else if (i %% 6 == 5){
    ## store which monkey to throw to if test is passed (1-indexed)
    monkeys[[monkey]]$iftrue<-as.numeric(unlist(strsplit(mydata$V2[i]," "))[5])+1
  } else if (i %% 6 == 0){
    ## store which monkey to throw to if test is failed (1-indexed)
    monkeys[[monkey]]$iffalse<-as.numeric(unlist(strsplit(mydata$V2[i]," "))[5])+1
  }
}

## This next piece of code transforms each item so that instead of holding a single
## worry value, it is instead held as a vector of length nomonkeys, where each
## element is the worry value modulus the test value for each monkey

## Loop over each monkey
for(monkey in 1:nomonkeys){
  ## Initialise new item vector
  newitems<-NULL
  ## loop over each item
  for(i in 1:length(monkeys[[monkey]]$items)){
    ## get the test value for each monkey, and record the modulus of the worry
    ## value for each test value
    for(m in 1:nomonkeys){
      newitems<-c(newitems,monkeys[[monkey]]$items[i] %% monkeys[[m]]$test)
    }
  }
  ## record this new vector as the items for this monkey
  monkeys[[monkey]]$items<-newitems
}

## initialise the number of items inspected by each monkey as 0
inspections<-rep(0,nomonkeys)

## loop over each round
for(round in 1:10000){
  ## loop over each monkey
  for(monkey in 1:nomonkeys){
    ## if the monkey has items, loop over each item (each item is a vector of length nomonkeys)
    if(length(monkeys[[monkey]]$items)>0){
      for(i in 1:(length(monkeys[[monkey]]$items)/nomonkeys)){
        ## iterate the number of items this monkey has inspected
        inspections[monkey]<-inspections[monkey]+1
        ## get the worry values for this item
        worry<-monkeys[[monkey]]$items[((i-1)*nomonkeys+1):(i*nomonkeys)]
        ## do the operation on the worry values
        if(monkeys[[monkey]]$operation[1]=="+"){
          if(monkeys[[monkey]]$operation[2]=="old"){
            worry <- worry + worry
          } else {
            worry<- worry + as.numeric(monkeys[[monkey]]$operation[2])
          }
        } else if (monkeys[[monkey]]$operation[1]=="*"){
          if(monkeys[[monkey]]$operation[2]=="old"){
            worry <- worry * worry
          } else {
            worry<- worry * as.numeric(monkeys[[monkey]]$operation[2])
          }         
        }
        ## reduce the worry values by finding the modulus of the test values 
        for(w in 1:length(worry)){
          worry[w]<-worry[w] %% monkeys[[w]]$test
        }
        ## do the test on the current worry value
        if(worry[monkey] == 0){
          ## add item to the list of the relevant monkey
          monkeys[[monkeys[[monkey]]$iftrue]]$items<-c(monkeys[[monkeys[[monkey]]$iftrue]]$items,worry)
        } else {
          ## add item to the list of the relevant monkey
          monkeys[[monkeys[[monkey]]$iffalse]]$items<-c(monkeys[[monkeys[[monkey]]$iffalse]]$items,worry)
        }
      }
      ## remove all items from the current monkey's list
      monkeys[[monkey]]$items <- NULL
    }
  }
}

## sort the number of inspections for each monkey from highest to lowest
inspections<-sort(inspections,decreasing = T)
## multiply the two highest inspection counts to get the answer
inspections[1]*inspections[2]
