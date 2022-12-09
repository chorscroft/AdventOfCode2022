# Advent of Code Day 9
# Part 1

## Read in the data
mydata<-read.table("data/day09.txt")

## Initialise list of knot locations
pos<-data.frame(x=0,y=0)

## Initialise positions of the tail and the head
currentTpos<-c(0,0)
currentHpos<-c(0,0)

## function to update the positions the tail has visited
## Inputs: current list of past positions & current tail position
updatePos<-function(pos,x){
  ## check if current knot position is new
  if(is.na(which(pos$x==x[1] & pos$y==x[2],arr.ind = T)[1])){
    ## if it is new, append it to the list
    pos<-rbind(pos,x)
  }
  ## return new position list
  return(pos)
}

## Loop over each movement
for(i in 1:nrow(mydata)){
  ## Head moves left
  if(mydata$V1[i]=="L"){
    ## Loop over each step
    for(j in 1:mydata$V2[i]){
      ## update head location
      currentHpos[1]<-currentHpos[1]-1
      ## check if tail needs to move
      if(abs(currentTpos[1]-currentHpos[1])<=1 & abs(currentTpos[2]-currentHpos[2])<=1){
        ## do nothing
      } else {
        ## move tail
        currentTpos[1]<-currentHpos[1]+1
        currentTpos[2]<-currentHpos[2]
        ## update tail position list
        pos<-updatePos(pos,currentTpos)
      }
    }
  ## Head moves right
  } else if(mydata$V1[i]=="R"){
    ## Loop over each step
    for(j in 1:mydata$V2[i]){
      ## update head location
      currentHpos[1]<-currentHpos[1]+1
      ## check if tail needs to move
      if(abs(currentTpos[1]-currentHpos[1])<=1 & abs(currentTpos[2]-currentHpos[2])<=1){
        ## do nothing
      } else {
        ## move tail
        currentTpos[1]<-currentHpos[1]-1
        currentTpos[2]<-currentHpos[2]
        ## update tail position list
        pos<-updatePos(pos,currentTpos)
      }
    }
  ## Head moves up
  } else if(mydata$V1[i]=="U"){
    ## Loop over each step
    for(j in 1:mydata$V2[i]){
      ## update head location
      currentHpos[2]<-currentHpos[2]+1
      ## check if tail needs to move
      if(abs(currentTpos[1]-currentHpos[1])<=1 & abs(currentTpos[2]-currentHpos[2])<=1){
        ## do nothing
      } else {
        ## move tail
        currentTpos[2]<-currentHpos[2]-1
        currentTpos[1]<-currentHpos[1]
        ## update tail position list
        pos<-updatePos(pos,currentTpos)
      }
    }
  ## Head moves down
  } else if(mydata$V1[i]=="D"){
    ## Loop over each step
    for(j in 1:mydata$V2[i]){
      ## update head location
      currentHpos[2]<-currentHpos[2]-1
      ## check if tail needs to move
      if(abs(currentTpos[1]-currentHpos[1])<=1 & abs(currentTpos[2]-currentHpos[2])<=1){
        ## do nothing
      } else {
        ## move tail
        currentTpos[2]<-currentHpos[2]+1
        currentTpos[1]<-currentHpos[1]
        ## update tail position list
        pos<-updatePos(pos,currentTpos)
      }
    }
  }
}

## Return the number of locations the tail visited
nrow(pos)

###########################
# Part 2

## Initialise list of final knot locations
pos<-data.frame(x=0,y=0)

## Initialise positions of the 9 knots and the head
currentTpos<-matrix(0,9,2)
currentHpos<-c(0,0)

## Loop over each movement
for(i in 1:nrow(mydata)){
  ## Loop over each step
  for(j in 1:mydata$V2[i]){
    
    ## Move the head
    ## Head moves left
    if(mydata$V1[i]=="L"){
      ## update head location
      currentHpos[1]<-currentHpos[1]-1
      ## check if first knot needs to move
      if(abs(currentTpos[1,1]-currentHpos[1])<=1 & abs(currentTpos[1,2]-currentHpos[2])<=1){
        ## do nothing
      } else {
        ## move first knot
        currentTpos[1,1]<-currentHpos[1]+1
        currentTpos[1,2]<-currentHpos[2]
      }
    ## Head moves right
    } else if(mydata$V1[i]=="R"){
      ## update head location
      currentHpos[1]<-currentHpos[1]+1
      ## check if first knot needs to move
      if(abs(currentTpos[1,1]-currentHpos[1])<=1 & abs(currentTpos[1,2]-currentHpos[2])<=1){
        #do nothing
      } else {
        ## move first knot
        currentTpos[1,1]<-currentHpos[1]-1
        currentTpos[1,2]<-currentHpos[2]
      }
    ## Head moves up
    } else if(mydata$V1[i]=="U"){
      ## update head location
      currentHpos[2]<-currentHpos[2]+1
      ## check if first knot needs to move
      if(abs(currentTpos[1,1]-currentHpos[1])<=1 & abs(currentTpos[1,2]-currentHpos[2])<=1){
        #do nothing
      } else {
        ## move first knot
        currentTpos[1,2]<-currentHpos[2]-1
        currentTpos[1,1]<-currentHpos[1]
      }
    ## Head moves down   
    } else if(mydata$V1[i]=="D"){
      ## update head location
      currentHpos[2]<-currentHpos[2]-1
      ## check if first knot needs to move
      if(abs(currentTpos[1,1]-currentHpos[1])<=1 & abs(currentTpos[1,2]-currentHpos[2])<=1){
        #do nothing
      } else {
        ## move first knot
        currentTpos[1,2]<-currentHpos[2]+1
        currentTpos[1,1]<-currentHpos[1]
      }
    }
    
    ## Move eahc subsequent knot
    ## Loop over knots 2 to 9
    for (t in 2:9){
      ## check if knot needs to move
      if(abs(currentTpos[t,1]-currentTpos[t-1,1])<=1 & abs(currentTpos[t,2]-currentTpos[t-1,2])<=1){
        ## do nothing
      ## Move diagonal
      } else if(abs(currentTpos[t,1]-currentTpos[t-1,1])==2 & abs(currentTpos[t,2]-currentTpos[t-1,2])==2){
        ## check if knot is moving left and up
        if(currentTpos[t,1]-currentTpos[t-1,1]==2 & currentTpos[t,2]-currentTpos[t-1,2]==-2){
          ## move knot
          currentTpos[t,1]<-currentTpos[t-1,1]+1
          currentTpos[t,2]<-currentTpos[t-1,2]-1
        ## check if knot is moving left and down
        } else if(currentTpos[t,1]-currentTpos[t-1,1]==2 & currentTpos[t,2]-currentTpos[t-1,2]==2){
          ## move knot
          currentTpos[t,1]<-currentTpos[t-1,1]+1
          currentTpos[t,2]<-currentTpos[t-1,2]+1
        ## check if knot is moving right and up
        } else if(currentTpos[t,1]-currentTpos[t-1,1]==-2 & currentTpos[t,2]-currentTpos[t-1,2]==-2){
          ## move knot
          currentTpos[t,1]<-currentTpos[t-1,1]-1
          currentTpos[t,2]<-currentTpos[t-1,2]-1
        ## check if knot is moving right and down
        } else if (currentTpos[t,1]-currentTpos[t-1,1]==-2 & currentTpos[t,2]-currentTpos[t-1,2]==2){
          ## move knot
          currentTpos[t,1]<-currentTpos[t-1,1]-1
          currentTpos[t,2]<-currentTpos[t-1,2]+1
        }
      ## Move normally
      } else {
        ## check if knot is moving left
        if(currentTpos[t,1]-currentTpos[t-1,1]==2){
          ## move knot
          currentTpos[t,1]<-currentTpos[t-1,1]+1
          currentTpos[t,2]<-currentTpos[t-1,2]
        ## check if knot is moving right
        } else if(currentTpos[t,1]-currentTpos[t-1,1]==-2){
          ## move knot
          currentTpos[t,1]<-currentTpos[t-1,1]-1
          currentTpos[t,2]<-currentTpos[t-1,2]
        ## check if knot is moving up
        } else if(currentTpos[t,2]-currentTpos[t-1,2]==-2){
          ## move knot
          currentTpos[t,2]<-currentTpos[t-1,2]-1
          currentTpos[t,1]<-currentTpos[t-1,1]
        ## check if knot is moving down
        } else if(currentTpos[t,2]-currentTpos[t-1,2]==2){
          ## move knot
          currentTpos[t,2]<-currentTpos[t-1,2]+1
          currentTpos[t,1]<-currentTpos[t-1,1]
        }
      }
      ## If this is the last knot, update the list of locations it has visited
      if (t==9){
        pos<-updatePos(pos,currentTpos[9,])
      }
    }
  }
}

## Return the number of locations the final knot visited
nrow(pos)