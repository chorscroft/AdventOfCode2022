# Advent of Code Day 8
# Part 1

## Read in the data
mydata<-read.fwf("data/day08.txt",rep(1,99))

## Initialise number of trees visible as the count of trees around the edge
countVisible<-99+99+97+97

## Loop over each internal tree
for(i in 2:(nrow(mydata)-1)){
  for(j in 2:(ncol(mydata)-1)){
    ## get height of tree
    height<-mydata[i,j]
    ## Assume it is visible
    visible<-T
    ## Look at all trees to the left
    for(k in 1:(i-1)){
      ## If tree blocks the view, tree isn't visible & stop searching
      if(mydata[k,j]>=height){
        visible<-F
        break
      }
    }
    ## if tree isn't visible yet, check trees to the right 
    if(visible==F){
      visible<-T
      ## Look at all trees to the right
      for(k in (i+1):nrow(mydata)){
        ## If tree blocks the view, tree isn't visible & stop searching
        if(mydata[k,j]>=height){
          visible<-F
          break
        }
      }
    }
    ## if tree isn't visible yet, check trees above 
    if(visible==F){
      visible<-T
      ## Look at all trees above
      for(k in 1:(j-1)){
        ## If tree blocks the view, tree isn't visible & stop searching
        if(mydata[i,k]>=height){
          visible<-F
          break
        }
      }
    }
    ## if tree isn't visible yet, check trees below 
    if(visible==F){
      visible<-T
      ## Look at all trees below
      for(k in (j+1):ncol(mydata)){
        ## If tree blocks the view, tree isn't visible & stop searching
        if(mydata[i,k]>=height){
          visible<-F
          break
        }
      }
    }
    ## If tree is visible from at least one direction, count it
    if (visible==T){
      countVisible<-countVisible+1
    }
  }
}

## Return number of visible trees
countVisible

###########################
# Part 2

## Initialise the overal best scenic score at zero
bestScenicScore<-0

## Loop over every tree
for(i in 1:nrow(mydata)){
  for(j in 1:ncol(mydata)){
    ## get height of tree
    height<-mydata[i,j]
    ## initalise scenic score in each direction as zero
    scenicScore<-c(0,0,0,0)
    if(i==1){
      ## If tree is on edge there is no view
      scenicScore[1]<-scenicScore[1]+0
    } else {
      for(k in (i-1):1){
        ## count every tree until line of sight is blocked
        scenicScore[1]<-scenicScore[1]+1
        if(mydata[k,j]>=height){
          break
        }
      }
    }
    if(i==nrow(mydata)){
      ## If tree is on edge there is no view
      scenicScore[2]<-scenicScore[2]+0
    } else {
      for(k in (i+1):nrow(mydata)){
        ## count every tree until line of sight is blocked
        scenicScore[2]<-scenicScore[2]+1
        if(mydata[k,j]>=height){
          break
        }
      }
    }   
    if(j==1){
      ## If tree is on edge there is no view
      scenicScore[3]<-scenicScore[3]+0
    } else {
      for(k in (j-1):1){
        ## count every tree until line of sight is blocked
        scenicScore[3]<-scenicScore[3]+1
        if(mydata[i,k]>=height){
          break
        }
      }
    }
    if(j==nrow(mydata)){
      ## If tree is on edge there is no view
      scenicScore[4]<-scenicScore[4]+0
    } else {
      for(k in (j+1):nrow(mydata)){
        ## count every tree until line of sight is blocked
        scenicScore[4]<-scenicScore[4]+1
        if(mydata[i,k]>=height){
          break
        }
      }
    }
    ## Calculate scenic score for the tree
    totalScenicScore<-scenicScore[1]*scenicScore[2]*scenicScore[3]*scenicScore[4]
    ## If this is the highest scenic score found so far, record it as the best
    if (totalScenicScore> bestScenicScore){
      bestScenicScore<-totalScenicScore
    }
  }
}

## Return the best scenic score
bestScenicScore