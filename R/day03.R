# Advent of Code Day 3
# Part 1

## Read in the data
mydata<-read.table("data/day03.txt")

## Initialise the sum of priorities
sumpriority<-0

## Loop over each bag
for(i in 1:nrow(mydata)){
  ## Split bag into a vector of individual letters
  bag<-unlist(strsplit(mydata$V1[i],""))
  ## Find which letter from the first half is in the second half of the bag
  same<-bag[1:(length(bag)/2)][which(bag[1:(length(bag)/2)] %in% bag[(length(bag)/2+1):length(bag)])]
  ## Get the priority value of the letter 
  ## (take [1] in case there are multiples of the object in the second bag)
  value<-which(c(letters,LETTERS)==same[1])
  ## Add the priority value to the total
  sumpriority<-sumpriority+value
}

## Return the sum of the priority values
sumpriority

###########################
# Part 2

## Read in the data
mydata<-read.table("data/day03.txt")

## Initialise the sum of priorities
sumpriority<-0

## Start at bag 1
i<-1
## Loop over each bag
while(i<nrow(mydata)){
  ## Split next three bags into vectors of individual letters
  bag1<-unlist(strsplit(mydata$V1[i],""))
  i<-i+1
  bag2<-unlist(strsplit(mydata$V1[i],""))
  i<-i+1
  bag3<-unlist(strsplit(mydata$V1[i],""))
  
  ## Loop through each letter in the first bag until the letter in all three
  ## bags is found
  for(j in 1:length(bag1)){
    if(bag1[j] %in% bag2 & bag1[j] %in% bag3){
      break
    }
  }
  
  ## Get the priority value of the letter
  value<-which(c(letters,LETTERS)==bag1[j])
  ## Add the priority value to the total
  sumpriority<-sumpriority+value
  i<-i+1
}

## Return the sum of the priority values
sumpriority