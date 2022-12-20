# Advent of Code Day 20
# Part 1

## Read in the data
mydata<-read.table("data/day20.txt",col.names = "numbers")

## Get number of lines in the data
no_lines<-no_lines

## Initialise the locations of each of the numbers
mydata$locations<-c(1:no_lines)

## Loop over each number
for(i in 1:no_lines){
  ## Get the number
  number<-mydata$numbers[i]
  ## Get its current location
  location<-mydata$locations[i]
  ## Get its new location
  new_location<-location+number
  ## If it is left of the start, loop around
  if(new_location<=1){
    while(new_location<=1){
      new_location<-no_lines+new_location-1
    }
  ## If it is right of the end, loop around
  } else if (new_location>no_lines){
    while(new_location>no_lines){
      new_location<-new_location - no_lines+1
    }
  }
  ## move the location of any other numbers that have been displaced
  if(location<new_location){
    mydata$locations[mydata$locations<=new_location & mydata$locations>location]<-mydata$locations[mydata$locations<=new_location & mydata$locations>location]-1
  } else if(location>new_location){
    mydata$locations[mydata$locations>=new_location & mydata$locations<location]<-mydata$locations[mydata$locations>=new_location & mydata$locations<location]+1
  }
  ## record the new location of the number
  mydata$locations[i]<-new_location
}

## sort the numbers by their final location
mixed<-mydata$numbers[order(mydata$locations)]

## initialise the numbers to keep at 1000, 2000, and 3000
keep<-c(0,0,0)
## start at the location of the zero
j<-which(mixed==0)
## loop 3000 times
for(i in 1:3000){
  ## increase the index
  j<-j+1
  #'# if the index is too big, loop around to the start
  if(j>no_lines){
    j<-1
  }
  ## record the numbers in position 1000, 2000, and 3000
  if(i==1000){
    keep[1]<-mixed[j]    
  } else if (i ==2000){
    keep[2]<-mixed[j]
  } else if (i==3000){
    keep[3]<-mixed[j]
  }
}
## return the sum of these numbers
sum(keep)

###########################
# Part 2

## Read in the data
mydata<-read.table("data/day20.txt",col.names = "numbers")

## Get number of lines in the data
no_lines<-no_lines

## Initialise the locations of each of the numbers
mydata$locations<-c(1:no_lines)

## Multiply numbers by the decryption key
mydata$numbers<-mydata$numbers*811589153

## mix the file ten times
for(l in 1:10){
  ## Loop over each number
  for(i in 1:no_lines){
    ## Get the number
    number<-mydata$numbers[i]
    ## Get its current location
    location<-mydata$locations[i]
    ## Get its new location
    new_location<-location+number
    ## If it is left of the start, use the modulus to find the new location
    if(new_location<=1){
      new_location<-new_location %% (no_lines-1)
      if(new_location==0){
        new_location<-no_lines-1
      }
    ## If it is right of the end, use the modulus to find the new location
    } else if (new_location>no_lines){
      new_location<-new_location %% (no_lines-1)
      if(new_location==0){
        new_location<-no_lines-1
      }
    }
    ## move the location of any other numbers that have been displaced
    if(location<new_location){
      mydata$locations[mydata$locations<=new_location & mydata$locations>location]<-mydata$locations[mydata$locations<=new_location & mydata$locations>location]-1
    } else if(location>new_location){
      mydata$locations[mydata$locations>=new_location & mydata$locations<location]<-mydata$locations[mydata$locations>=new_location & mydata$locations<location]+1
    }
    ## record the new location of the number
    mydata$locations[i]<-new_location
  }
}

## sort the numbers by their final location
mixed<-mydata$numbers[order(mydata$locations)]

## initialise the numbers to keep at 1000, 2000, and 3000
keep<-c(0,0,0)
## start at the location of the zero
j<-which(mixed==0)
## loop 3000 times
for(i in 1:3000){
  ## increase the index
  j<-j+1
  #'# if the index is too big, loop around to the start
  if(j>no_lines){
    j<-1
  }
  ## record the numbers in position 1000, 2000, and 3000
  if(i==1000){
    keep[1]<-mixed[j]    
  } else if (i ==2000){
    keep[2]<-mixed[j]
  } else if (i==3000){
    keep[3]<-mixed[j]
  }
}
## return the sum of these numbers
sum(keep)
