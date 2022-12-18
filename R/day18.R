# Advent of Code Day 18
# Part 1

## Read in the data
mydata<-read.table("data/day18.txt",sep=",")

## assume each cube is open on all sides
mydata$sides<-6

## compare each pair of cubes
for(i in 1:(nrow(mydata)-1)){
  for(j in (i+1):nrow(mydata)){
  ## if they touch on any side, remove one from the count of open sides for both cubes
   if((mydata$V1[i]==mydata$V1[j] & mydata$V2[i]==mydata$V2[j] & abs(mydata$V3[i]-mydata$V3[j])==1) |
      (mydata$V1[i]==mydata$V1[j] & abs(mydata$V2[i]-mydata$V2[j])==1 & mydata$V3[i]==mydata$V3[j]) |
      (abs(mydata$V1[i]-mydata$V1[j])==1 & mydata$V2[i]==mydata$V2[j] & mydata$V3[i]==mydata$V3[j])){
      mydata$sides[i]<-mydata$sides[i]-1
      mydata$sides[j]<-mydata$sides[j]-1
    } 
  }
}

## return the total count of open sides
sum(mydata$sides)


###########################
# Part 2

## Assume none if the sides of any cube is open to the outside
mydata$sides<-0

## Find the maximum and minimum coordinates to check for steam
maxsteam<-max(mydata)+1
minsteam<-min(mydata)-1

## Start steam expanding from the bottom corner
steam<-data.frame(x=minsteam,y=minsteam,z=minsteam,checked=F)

## Loop until the steam has fully enveloped all the cubes
## Only stop once there are no more potential steam locations to check
while(sum(steam$checked)<nrow(steam)){
  ## find the first steam coordinate in the list that has not been checked yet
  index<-which(steam$checked==F)[1]
  ## mark it as checked
  steam$checked[index]<-T
  ## get the coordinates of the location to check
  coords<-steam[index,c(1,2,3)]  
  
  ## check the location left of the current steam location
  if(coords[1]-1>=minsteam){
    ## get the location coordinates
    checkvec<-unlist(c(coords[1]-1,coords[2],coords[3]))
    ## check if it is already in the list of steam locations
    insteam<-sum(apply(steam,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0
    ## check if it is in the list of lava locations
    if (insteam==F){
      inlava<-sum(apply(mydata,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0      
    }
    ## continue if it is not already a steam or lava location
    if(insteam== F&& inlava==F){
      ## for each lava cube
      for(i in 1:nrow(mydata)){
        ## if this location touches a side of a lava cube, count this side
        if(abs(checkvec[1]-mydata[i,1])<=1 && abs(checkvec[2]-mydata[i,2])<=1 && abs(checkvec[3]-mydata[i,3])<=1){
          if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]+1){
            mydata$sides[i]<-mydata$sides[i]+1
          }else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]-1){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]+1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]-1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]+1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]-1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          }
        }
      }
      ## add this location to the list of steam locations to check
      steam<-rbind(steam,c(checkvec,F))
    }
  }
  ## check the location right of the current steam location
  if(coords[1]+1<=maxsteam){
    ## get the location coordinates
    checkvec<-unlist(c(coords[1]+1,coords[2],coords[3]))
    ## check if it is already in the list of steam locations
    insteam<-sum(apply(steam,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0
    ## check if it is in the list of lava locations
    if (insteam==F){
      inlava<-sum(apply(mydata,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0      
    }
    ## continue if it is not already a steam or lava location
    if(insteam== F&& inlava==F){
      ## for each lava cube
      for(i in 1:nrow(mydata)){
        ## if this location touches a side of a lava cube, count this side
        if(abs(checkvec[1]-mydata[i,1])<=1 && abs(checkvec[2]-mydata[i,2])<=1 && abs(checkvec[3]-mydata[i,3])<=1){
          if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]+1){
            mydata$sides[i]<-mydata$sides[i]+1
          }else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]-1){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]+1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]-1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]+1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]-1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          }
        }
      }
      ## add this location to the list of steam locations to check
      steam<-rbind(steam,c(checkvec,F))
    }
  }
  ## check the location below the current steam location
  if(coords[2]-1>=minsteam){
    ## get the location coordinates
    checkvec<-unlist(c(coords[1],coords[2]-1,coords[3]))
    ## check if it is already in the list of steam locations
    insteam<-sum(apply(steam,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0
    ## check if it is in the list of lava locations
    if (insteam==F){
      inlava<-sum(apply(mydata,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0      
    }
    ## continue if it is not already a steam or lava location
    if(insteam== F&& inlava==F){
      ## for each lava cube
      for(i in 1:nrow(mydata)){
        ## if this location touches a side of a lava cube, count this side
        if(abs(checkvec[1]-mydata[i,1])<=1 && abs(checkvec[2]-mydata[i,2])<=1 && abs(checkvec[3]-mydata[i,3])<=1){
          if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]+1){
            mydata$sides[i]<-mydata$sides[i]+1
          }else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]-1){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]+1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]-1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]+1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]-1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          }
        }
      }
      ## add this location to the list of steam locations to check
      steam<-rbind(steam,c(checkvec,F))
    }
  }
  ## check the location above the current steam location
  if(coords[2]+1<=maxsteam){
    ## get the location coordinates
    checkvec<-unlist(c(coords[1],coords[2]+1,coords[3]))
    ## check if it is already in the list of steam locations
    insteam<-sum(apply(steam,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0
    ## check if it is in the list of lava locations
    if (insteam==F){
      inlava<-sum(apply(mydata,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0      
    }
    ## continue if it is not already a steam or lava location
    if(insteam== F&& inlava==F){
      ## for each lava cube
      for(i in 1:nrow(mydata)){
        ## if this location touches a side of a lava cube, count this side
        if(abs(checkvec[1]-mydata[i,1])<=1 && abs(checkvec[2]-mydata[i,2])<=1 && abs(checkvec[3]-mydata[i,3])<=1){
          if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]+1){
            mydata$sides[i]<-mydata$sides[i]+1
          }else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]-1){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]+1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]-1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]+1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]-1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          }
        }
      }
      ## add this location to the list of steam locations to check
      steam<-rbind(steam,c(checkvec,F))
    }
  }
  ## check the location behind the current steam location
  if(coords[3]-1>=minsteam){
    ## get the location coordinates
    checkvec<-unlist(c(coords[1],coords[2],coords[3]-1))
    ## check if it is already in the list of steam locations
    insteam<-sum(apply(steam,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0
    ## check if it is in the list of lava locations
    if (insteam==F){
      inlava<-sum(apply(mydata,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0      
    }
    ## continue if it is not already a steam or lava location
    if(insteam== F&& inlava==F){
      ## for each lava cube
      for(i in 1:nrow(mydata)){
        ## if this location touches a side of a lava cube, count this side
        if(abs(checkvec[1]-mydata[i,1])<=1 && abs(checkvec[2]-mydata[i,2])<=1 && abs(checkvec[3]-mydata[i,3])<=1){
          if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]+1){
            mydata$sides[i]<-mydata$sides[i]+1
          }else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]-1){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]+1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]-1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]+1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]-1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          }
        }
      }
      ## add this location to the list of steam locations to check
      steam<-rbind(steam,c(checkvec,F))
    }
  }
  ## check the location in front of the current steam location
  if(coords[3]+1<=maxsteam){
    ## get the location coordinates
    checkvec<-unlist(c(coords[1],coords[2],coords[3]+1))
    ## check if it is already in the list of steam locations
    insteam<-sum(apply(steam,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0
    ## check if it is in the list of lava locations
    if (insteam==F){
      inlava<-sum(apply(mydata,1,function(x)(checkvec[1]==x[1] && checkvec[2]==x[2] && checkvec[3]==x[3])))>0      
    }
    ## continue if it is not already a steam or lava location
    if(insteam== F&& inlava==F){
      ## for each lava cube
      for(i in 1:nrow(mydata)){
        ## if this location touches a side of a lava cube, count this side
        if(abs(checkvec[1]-mydata[i,1])<=1 && abs(checkvec[2]-mydata[i,2])<=1 && abs(checkvec[3]-mydata[i,3])<=1){
          if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]+1){
            mydata$sides[i]<-mydata$sides[i]+1
          }else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]-1){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]+1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1] && mydata[i,2]==checkvec[2]-1 && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]+1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          } else if(mydata[i,1]==checkvec[1]-1 && mydata[i,2]==checkvec[2] && mydata[i,3]==checkvec[3]){
            mydata$sides[i]<-mydata$sides[i]+1
          }
        }
      }
      ## add this location to the list of steam locations to check
      steam<-rbind(steam,c(checkvec,F))
    }
  }
}

## Get the total number of exposed sides
sum(mydata$sides)
