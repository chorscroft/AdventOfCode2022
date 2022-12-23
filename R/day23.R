# Advent of Code Day 23
# Parts 1 and 2

## Read in the data
mydata<-read.table("data/day23.txt",comment.char = "@")

## Get the initial map
map<-t(apply(mydata,1,function(x)unlist(strsplit(x,""))))

## Initialise a data frame where each row is an elf
elves<-data.frame(x=as.numeric(),y=as.numeric(),move=as.logical(),propx=as.numeric(),propy=as.numeric())

## Fill in the data frame with the locations of each of the elves
for(i in 1:nrow(map)){
  for(j in 1:ncol(map)){
    if(map[i,j]=="#"){
      elves<-rbind(elves,data.frame(x=i,y=j,move=F,propx=NA,propy=NA))
    }
  }
}

## Get the total number of elves
no_elves<-nrow(elves)

## Initialise the order in which to consider moving
considerOrder<-c("N","S","W","E")

## Start at round 0
round<-0
while(T){
  ## Iterate the round count
  round<-round+1
  ## Assume the elf will not move
  elves$move<-F
  elves$propx<-NA
  elves$propy<-NA
  
  ## loop over each elf
  for(e in 1:no_elves){
    ## Continue if there is at least one elf next to it
    if(sum(abs(elves$x-elves$x[e])<=1 & abs(elves$y-elves$y[e])<=1)>1){
      ## loop over the directions to consider
      for(o in 1:4){
        ## if the elf hasn't already proposed a move, check north
        if(elves$move[e]==F && considerOrder[o]=="N"){
          if(sum(elves$x[e]-elves$x==1 & abs(elves$y-elves$y[e])<=1)==0){
            ## If there is no elf there, mark as moving and record proposed location
            elves$move[e]<-T
            elves$propx[e]<-elves$x[e]-1
            elves$propy[e]<-elves$y[e]
          }
        }
        ## if the elf hasn't already proposed a move, check south
        if(elves$move[e]==F && considerOrder[o]=="S"){
          if(sum(elves$x-elves$x[e]==1 & abs(elves$y-elves$y[e])<=1)==0){
            ## If there is no elf there, mark as moving and record proposed location
            elves$move[e]<-T
            elves$propx[e]<-elves$x[e]+1
            elves$propy[e]<-elves$y[e]
          }
        }
        ## if the elf hasn't already proposed a move, check east
        if(elves$move[e]==F && considerOrder[o]=="E"){
          if(sum(abs(elves$x[e]-elves$x)<=1 & elves$y-elves$y[e]==1)==0){
            ## If there is no elf there, mark as moving and record proposed location
            elves$move[e]<-T
            elves$propx[e]<-elves$x[e]
            elves$propy[e]<-elves$y[e]+1
          }
        }
        ## if the elf hasn't already proposed a move, check west
        if(elves$move[e]==F && considerOrder[o]=="W"){
          if(sum(abs(elves$x-elves$x[e])<=1 & elves$y[e]-elves$y==1)==0){
            ## If there is no elf there, mark as moving and record proposed location
            elves$move[e]<-T
            elves$propx[e]<-elves$x[e]
            elves$propy[e]<-elves$y[e]-1
          }
        }  
      }
    }
  }
  
  ## loop over each elf
  for(e in 1:no_elves){
    if(elves$move[e]==T && sum(elves$propx[e]==elves$propx & elves$propy[e]==elves$propy,na.rm = T)==1){
      ## move the elf to the proposed location if they are the only elf moving there
      elves$x[e]<-elves$propx[e]
      elves$y[e]<-elves$propy[e]
    } else {
      ## record they didn't move if they didn't for any reason
      elves$move[e]<-F
    }
  }
  
  ## Part 1
  if(round==10){
    break
  }
  ## Part 2
  # if(sum(elves$move)==0){
  #   break
  # }
  
  ## Rotate the order the directions are considered in
  considerOrder<-c(considerOrder[2:4],considerOrder[1])
}

## Part 1
(max(elves$x)-min(elves$x)+1)*(max(elves$y)-min(elves$y)+1)-no_elves
## Part 2
# round

