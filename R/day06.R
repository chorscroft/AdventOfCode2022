# Advent of Code Day 6
# Part 1

## Read in the movement data
mydata<-read.table("data/day06.txt")

## Split into a vector of individual characters
signal<-unlist(strsplit(mydata$V1,""))

## start at the first character and loop until marker is found
i<-1
while(TRUE){
  ## check if the next four letters are unique
  if(length(unique(signal[i:(i+3)]))==4){
    ## if they are then stop searching
    break
  }
  ## iterate i
  i<-i+1
}

## output the marker location
i+3

###########################
# Part 2

## start at the first character and loop until marker is found
i<-1
while(TRUE){
  ## check if the next fourteen letters are unique
  if(length(unique(signal[i:(i+13)]))==14){
    ## if they are then stop searching
    break
  }
  ## iterate i
  i<-i+1
}

## output the marker location
i+13


