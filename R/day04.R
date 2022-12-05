# Advent of Code Day 4
# Part 1

## Read in the data
mydata<-read.table("data/day04.txt",sep=",")

## Initialise the count of overlaps at 0
count<-0

## Loop over each pair
for(i in 1:nrow(mydata)){
  ## Get the ranges for each elf in the pair
  e1<-as.numeric(unlist(strsplit(mydata$V1[i],"-")))
  e2<-as.numeric(unlist(strsplit(mydata$V2[i],"-")))
  
  ## Sort the ranges from lowest to highest
  if(e1[1]>e1[2]){
    temp<-e1[2]
    e1[2]<-e1[1]
    e1[1]<-temp
  }
  if(e2[1]>e2[2]){
    temp<-e2[2]
    e2[2]<-e2[1]
    e2[1]<-temp
  }
  
  ## Check if one range completely contains the other
  if((e1[1]>=e2[1] & e1[2]<=e2[2]) | (e2[1]>=e1[1] & e2[2]<=e1[2])){
    ## Count if one range completely contains the other
    count<-count+1
  }
}

## Output the final count
count

###########################
# Part 2

## Initialise the count of overlaps at 0
count<-0

## Loop over each pair
for(i in 1:nrow(mydata)){
  ## Get the ranges for each elf in the pair
  e1<-as.numeric(unlist(strsplit(mydata$V1[i],"-")))
  e2<-as.numeric(unlist(strsplit(mydata$V2[i],"-")))
  
  ## Sort the ranges from lowest to highest
  if(e1[1]>e1[2]){
    temp<-e1[2]
    e1[2]<-e1[1]
    e1[1]<-temp
  }
  if(e2[1]>e2[2]){
    temp<-e2[2]
    e2[2]<-e2[1]
    e2[1]<-temp
  }
  
  ## Check if one range overlaps the other at all
  if((e2[1]<=e1[1] & e1[1]<=e2[2]) | 
     (e2[1]<=e1[2] & e1[2]<=e2[2]) | 
     (e1[1]<=e2[1] & e2[1]<=e1[2]) | 
     (e1[1]<=e2[2] & e2[2]<=e1[2])){
    ## Count if one range completely contains the other
    count<-count+1
  }
}

## Output the final count
count