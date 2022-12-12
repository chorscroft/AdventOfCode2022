# Advent of Code Day 12
# Part 1

## Read in the data
mydata<-read.fwf("data/day12.txt",rep(1,163))

## Transform each letter into a number
for(i in 1:nrow(mydata)){
  for (j in 1:ncol(mydata)){
    mydata[i,j]<-which(c(letters,LETTERS)==mydata[i,j],arr.ind = T)
  }
}

## Make the elements numeric from strings
mydata<-apply(mydata,1,as.numeric)

## find the starting location (S) and record its location
start<-which(mydata==which(c(letters,LETTERS)=="S"),arr.ind = T)[1,]

## find the end location (E) and record its location
end<-which(mydata==which(c(letters,LETTERS)=="E"),arr.ind = T)[1,]

## Change the starting location to a 1
mydata[start[1],start[2]]<-1

## Change the end location to a 26
mydata[end[1],end[2]]<-26

## Use Dijkstra's algorithm to find the shortest path from S to E

## Initialise all locations to be unvisited
unvisited<-matrix(T,nrow=nrow(mydata),ncol(mydata))

## Initialise tentative shortest paths to each location as infinity
tent<-matrix(Inf,nrow=nrow(mydata),ncol(mydata))
## Change the start location to having a shortest path of zero
tent[start[1],start[2]]<-0

## Loop until the end location has been visited
while(unvisited[end[1],end[2]]==T){
  ## Find a location that is unvisited and has the minimum tentative shortest path
  curr_loc<-which(unvisited==T & tent==min(tent[unvisited==T]),arr.ind=T)[1,]
  ## Set the current location as visited
  unvisited[curr_loc[1],curr_loc[2]]<-FALSE
  
  ## Check the location to the left
  if(curr_loc[1]>1){
    ## Only do anything if the location is unvisited
    if(unvisited[curr_loc[1]-1,curr_loc[2]]==T){
      ## If the neighbouring location is at most one step higher, check to see if
      ## the path from the current location is the shortest path found so far.
      ## If so, overwrite it
      if(mydata[curr_loc[1],curr_loc[2]]+1>=mydata[curr_loc[1]-1,curr_loc[2]]){
        if(tent[curr_loc[1]-1,curr_loc[2]]>tent[curr_loc[1],curr_loc[2]]+1){
          tent[curr_loc[1]-1,curr_loc[2]]<-tent[curr_loc[1],curr_loc[2]]+1
        }
      }
    }
  }
  ## Check the location to the right
  if(curr_loc[1]<nrow(mydata)){
    ## Only do anything if the location is unvisited
    if(unvisited[curr_loc[1]+1,curr_loc[2]]==T){
      ## If the neighbouring location is at most one step higher, check to see if
      ## the path from the current location is the shortest path found so far.
      ## If so, overwrite it
      if(mydata[curr_loc[1],curr_loc[2]]+1>=mydata[curr_loc[1]+1,curr_loc[2]]){
        if (tent[curr_loc[1]+1,curr_loc[2]]>tent[curr_loc[1],curr_loc[2]]+1){
          tent[curr_loc[1]+1,curr_loc[2]]<-tent[curr_loc[1],curr_loc[2]]+1
        }
      }
    }
  }
  ## Check the location above
  if(curr_loc[2]>1){
    ## Only do anything if the location is unvisited
    if(unvisited[curr_loc[1],curr_loc[2]-1]==T){
      ## If the neighbouring location is at most one step higher, check to see if
      ## the path from the current location is the shortest path found so far.
      ## If so, overwrite it
      if(mydata[curr_loc[1],curr_loc[2]]+1>=mydata[curr_loc[1],curr_loc[2]-1]){
        if(tent[curr_loc[1],curr_loc[2]-1]>tent[curr_loc[1],curr_loc[2]]+1){
          tent[curr_loc[1],curr_loc[2]-1]<-tent[curr_loc[1],curr_loc[2]]+1
        }
      }
    }
  }
  ## Check the location below
  if(curr_loc[2]<ncol(mydata)){
    ## Only do anything if the location is unvisited
    if(unvisited[curr_loc[1],curr_loc[2]+1]==T){
      ## If the neighbouring location is at most one step higher, check to see if
      ## the path from the current location is the shortest path found so far.
      ## If so, overwrite it
      if(mydata[curr_loc[1],curr_loc[2]]+1>=mydata[curr_loc[1],curr_loc[2]+1]){
        if(tent[curr_loc[1],curr_loc[2]+1]>tent[curr_loc[1],curr_loc[2]]+1){
          tent[curr_loc[1],curr_loc[2]+1]<-tent[curr_loc[1],curr_loc[2]]+1
        }
      }
    }
  }
}

## Output the shortest path to the end location
tent[end[1],end[2]]


###########################
# Part 2

## Use Dijkstra's algorithm to find the shortest path from E 
## to each other location possible

## Initialise all locations to be unvisited
unvisited<-matrix(T,nrow=nrow(mydata),ncol(mydata))

## Initialise tentative shortest paths to each location as infinity
tent<-matrix(Inf,nrow=nrow(mydata),ncol(mydata))

## Change the end location to having a shortest path of zero
tent[end[1],end[2]]<-0

## Loop until every location has been visited
while(sum(unvisited==T)!=0){
 
  ## Find a location that is unvisited and has the minimum tentative shortest path
  curr_loc<-which(unvisited==T & tent==min(tent[unvisited==T]),arr.ind=T)
  ## If every location possile to visit has been visited, stop searching
  if(nrow(curr_loc)==0){
    break
  }
  ## pick the first unvisited location with the minimum shortest path 
  curr_loc<-curr_loc[1,]
  
  ## Set the current location as visited
  unvisited[curr_loc[1],curr_loc[2]]<-FALSE
  
  ## Check the location to the left
  if(curr_loc[1]>1){
    ## Only do anything if the location is unvisited
    if(unvisited[curr_loc[1]-1,curr_loc[2]]==T){
      ## If the neighbouring location is higher or at most one step lower, 
      ## check to see if the path from the current location is the 
      ## shortest path found so far. If so, overwrite it
      if(mydata[curr_loc[1],curr_loc[2]]-1<=mydata[curr_loc[1]-1,curr_loc[2]]){
        if(tent[curr_loc[1]-1,curr_loc[2]]>tent[curr_loc[1],curr_loc[2]]+1){
          tent[curr_loc[1]-1,curr_loc[2]]<-tent[curr_loc[1],curr_loc[2]]+1
        }
      }
    }
  }
  ## Check the location to the right
  if(curr_loc[1]<nrow(mydata)){
    ## Only do anything if the location is unvisited
    if(unvisited[curr_loc[1]+1,curr_loc[2]]==T){
      ## If the neighbouring location is higher or at most one step lower, 
      ## check to see if the path from the current location is the 
      ## shortest path found so far. If so, overwrite it
      if(mydata[curr_loc[1],curr_loc[2]]-1<=mydata[curr_loc[1]+1,curr_loc[2]]){
        if (tent[curr_loc[1]+1,curr_loc[2]]>tent[curr_loc[1],curr_loc[2]]+1){
          tent[curr_loc[1]+1,curr_loc[2]]<-tent[curr_loc[1],curr_loc[2]]+1
        }
      }
    }
  }
  ## Check the location above
  if(curr_loc[2]>1){
    ## Only do anything if the location is unvisited
    if(unvisited[curr_loc[1],curr_loc[2]-1]==T){
      ## If the neighbouring location is higher or at most one step lower, 
      ## check to see if the path from the current location is the 
      ## shortest path found so far. If so, overwrite it
      if(mydata[curr_loc[1],curr_loc[2]]-1<=mydata[curr_loc[1],curr_loc[2]-1]){
        if(tent[curr_loc[1],curr_loc[2]-1]>tent[curr_loc[1],curr_loc[2]]+1){
          tent[curr_loc[1],curr_loc[2]-1]<-tent[curr_loc[1],curr_loc[2]]+1
        }
      }
    }
  }
  ## Check the location below
  if(curr_loc[2]<ncol(mydata)){
    ## Only do anything if the location is unvisited
    if(unvisited[curr_loc[1],curr_loc[2]+1]==T){
      ## If the neighbouring location is higher or at most one step lower, 
      ## check to see if the path from the current location is the 
      ## shortest path found so far. If so, overwrite it
      if(mydata[curr_loc[1],curr_loc[2]]-1<=mydata[curr_loc[1],curr_loc[2]+1]){
        if(tent[curr_loc[1],curr_loc[2]+1]>tent[curr_loc[1],curr_loc[2]]+1){
          tent[curr_loc[1],curr_loc[2]+1]<-tent[curr_loc[1],curr_loc[2]]+1
        }
      }
    }
  }
}

## Find the shortest path to any location that is only 1 high
min(tent[mydata==1])