# Advent of Code Day 14
# Part 1

## Read in the data
mydata<-read.table("data/day14.txt",sep=":")

## Initialise the cave
cave<-matrix(".",nrow=200,ncol=1000)

## initialise vectors to record the limits of the paths in the cave
colsindata<-c(Inf,-Inf)
rowsindata<-c(Inf,-Inf)

## loop over each path
for(i in 1:nrow(mydata)){
  ## extract the x and y coordinates for the path
  path<-unlist(strsplit(mydata$V1[i]," -> "))
  path<-as.numeric(unlist(strsplit(path,",")))

  ## loop over each step in the path    
  j<-1
  while (j<length(path)) {
    ## record the limits of the path in the cave (not directly used in this part)
    if(path[j]<colsindata[1]){
      colsindata[1]<-path[j]
    }
    if(path[j]>colsindata[2]){
      colsindata[2]<-path[j]
    }
    if(path[j+1]<rowsindata[1]){
      rowsindata[1]<-path[j+1]
    }
    if(path[j+1]>rowsindata[2]){
      rowsindata[2]<-path[j+1]
    }
    
    ## Draw the paths in the cave
    if((j+2)<length(path)){
      ## Draw path horizontally
      if(path[j+1]==path[j+3]){
        for(k in path[j]:path[j+2]){
          cave[path[j+1],k]<-"#"
        }
      ## Draw path vertically
      } else {
        for(k in path[j+1]:path[j+3]){
          cave[k,path[j]]<-"#"
        }        
      }
    }
    ## iterate to next coordinates
    j<-j+2
  }
}

## add row 0 to cave
cave<-rbind(rep(".",1000),cave)

## loop until the sand starts to fall into the abyss
stop<-FALSE
while(stop==FALSE){
  ## sand appears in this location
  sandLoc<-c(1,500)
  while(T){
    ## if sand falls into the abyss, stop generating sand
    if(sandLoc[1]==200){
      stop<-TRUE
      break
    }
    ## if the square below is empty, sand moves there
    if(cave[sandLoc[1]+1,sandLoc[2]]=="."){
      sandLoc[1]<-sandLoc[1]+1
    ## otherwise, if square below and to the left is empty, sand moves there
    } else if (cave[sandLoc[1]+1,sandLoc[2]-1]=="."){
      sandLoc[1]<-sandLoc[1]+1
      sandLoc[2]<-sandLoc[2]-1
    ## otherwise, if square below and to the right is empty, sand moves there
    } else if (cave[sandLoc[1]+1,sandLoc[2]+1]=="."){
      sandLoc[1]<-sandLoc[1]+1
      sandLoc[2]<-sandLoc[2]+1
    ## otherwise, sand comes to a rest
    } else {
      cave[sandLoc[1],sandLoc[2]]<-"o"
      ## move onto next sand particle
      break
    }
  }
}

## return the number of sand particles that came to a rest
sum(cave=="o")

###########################
# Part 2

## initialise the cave
cave<-matrix(".",nrow=200,ncol=1000)

## loop over each path
for(i in 1:nrow(mydata)){
  ## extract the x and y coordinates for the path
  path<-unlist(strsplit(mydata$V1[i]," -> "))
  path<-as.numeric(unlist(strsplit(path,",")))
  ## loop over each step in the path  
  j<-1
  while (j<length(path)) {
    ## Draw the paths in the cave
    if((j+2)<length(path)){
      ## Draw path horizontally
      if(path[j+1]==path[j+3]){
        for(k in path[j]:path[j+2]){
          cave[path[j+1],k]<-"#"
        }
      ## Draw path vertically
      } else {
        for(k in path[j+1]:path[j+3]){
          cave[k,path[j]]<-"#"
        }        
      }
    }
    ## iterate to next coordinates
    j<-j+2
  }
}

## draw in the cave floor based on lowest point scanned
for(i in 1:1000){
  cave[rowsindata[2]+2,i]<-"#"
}

## add row 0 to cave
cave<-rbind(rep(".",1000),cave)

## loop until no more sand can be generated
while(T){
  ## sand appears in this location
  sandLoc<-c(1,500)
  ## if there is already sand in the starting location, stop generating sand
  if (cave[sandLoc[1],sandLoc[2]]=="o"){
    break
  }
  
  while(T){
    ## if the square below is empty, sand moves there
    if(cave[sandLoc[1]+1,sandLoc[2]]=="."){
      sandLoc[1]<-sandLoc[1]+1
    ## otherwise, if square below and to the left is empty, sand moves there
    } else if (cave[sandLoc[1]+1,sandLoc[2]-1]=="."){
      sandLoc[1]<-sandLoc[1]+1
      sandLoc[2]<-sandLoc[2]-1
    ## otherwise, if square below and to the right is empty, sand moves there
    } else if (cave[sandLoc[1]+1,sandLoc[2]+1]=="."){
      sandLoc[1]<-sandLoc[1]+1
      sandLoc[2]<-sandLoc[2]+1
    ## otherwise, sand comes to a rest
    } else {
      cave[sandLoc[1],sandLoc[2]]<-"o"
      ## move onto next sand particle
      break
    }
  }
}

## return the number of sand particles that came to a rest
sum(cave=="o")
