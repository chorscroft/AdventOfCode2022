# Advent of Code Day 22
# Part 1

## initialise the map
map_init<-NULL
## intialise a flag for when the map stops and the instructions start
init_instruct<-F

## read in data libe by line
mydata<-file("data/day22.txt","r")
while (TRUE){
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else if (line==""){
    ## instructions are starting on the next line
    init_instruct<-T
  } else {
    if(init_instruct==F){
      ## add line to the map
      map_init<-c(map_init,line)
    } else {
      ## read in the instructions
      instructions<-line
    }
  }
}
close(mydata)

## split the instructions by individual characters
instructions<-unlist(strsplit(instructions,""))
## bring numbers of nultiple digits back together into a single element
combineMultipleDigits<-function(x){
  ## initialise the index along the vector
  j<-1
  ## loop along the vector
  while(j < length(x)){
    ## if both this element and the next are numbers then join them
    if(!is.na(as.numeric(x[j])) & !is.na(as.numeric(x[j+1]))){
      ## join numbers together
      x[j]<-paste0(x[j],x[j+1])
      ## remove second number
      x<-x[-(j+1)]     
    } else {
      ## iterate along the vector
      j<-j+1
    }
  }
  ## return the final vector
  return(x)
}
instructions<-combineMultipleDigits(instructions)

## get the width of the map
maxwidth<-max(sapply(map_init,nchar))
## create a map in matrix format
map<-matrix(" ",nrow=length(map_init),ncol=maxwidth)
for(i in 1:length(map_init)){
  map[i,1:nchar(map_init[i])]<-unlist(strsplit(map_init[i],""))
}

## find the top left "." location on the map
startPos<-1
while(map[1,startPos] != "."){
  startPos<-startPos+1
}
currentPos<-c(1,startPos)

## initialise the direction facing as East
facing<-"E"

## loop over each step in the instructions
i<-1
while(i<=length(instructions)){
  ## Move
  ## get number of steps to move
  steps<-as.numeric(instructions[i])
  ## If facing North try to move
  if(facing=="N"){
    ## do as many steps as possible, stop if hit a wall
    stop<-F
    while(steps>0 & stop==F){
      ## if not looping around, step if no wall
      if(currentPos[1]>1 && map[currentPos[1]-1,currentPos[2]] != " "){
        if(map[currentPos[1]-1,currentPos[2]]=="."){
          currentPos[1]<-currentPos[1]-1
          steps<-steps-1
        } else {
          stop<-T
        }
      } else {
        ## Loop around if no wall on other side
        checkloop<-nrow(map)
        while(map[checkloop,currentPos[2]]==" "){
          checkloop<-checkloop-1
        }
        if(map[checkloop,currentPos[2]]=="."){
          currentPos[1]<-checkloop
          steps<-steps-1
        } else {
          stop<-T
        }
      }
    }
  ## If facing South try to move
  } else if(facing=="S"){
    ## do as many steps as possible, stop if hit a wall
    stop<-F
    while(steps>0 & stop==F){
      ## if not looping around, step if no wall
      if(currentPos[1]<nrow(map) && map[currentPos[1]+1,currentPos[2]] != " "){
        if(map[currentPos[1]+1,currentPos[2]]=="."){
          currentPos[1]<-currentPos[1]+1
          steps<-steps-1
        } else {
          stop<-T
        }
      } else {
        ## Loop around if no wall on other side
        checkloop<-1
        while(map[checkloop,currentPos[2]]==" "){
          checkloop<-checkloop+1
        }
        if(map[checkloop,currentPos[2]]=="."){
          currentPos[1]<-checkloop
          steps<-steps-1
        } else {
          stop<-T
        }
      }
    }
  ## If facing West try to move
  } else if(facing=="W"){
    ## do as many steps as possible, stop if hit a wall
    stop<-F
    while(steps>0 & stop==F){
      ## if not looping around, step if no wall
      if(currentPos[2]>1 && map[currentPos[1],currentPos[2]-1] != " "){
        if(map[currentPos[1],currentPos[2]-1]=="."){
          currentPos[2]<-currentPos[2]-1
          steps<-steps-1
        } else {
          stop<-T
        }
      } else {
        ## Loop around if no wall on other side
        checkloop<-ncol(map)
        while(map[currentPos[1],checkloop]==" "){
          checkloop<-checkloop-1
        }
        if(map[currentPos[1],checkloop]=="."){
          currentPos[2]<-checkloop
          steps<-steps-1
        } else {
          stop<-T
        }
      }
    }
  ## If facing East try to move
  } else if(facing=="E"){
    ## do as many steps as possible, stop if hit a wall
    stop<-F
    while(steps>0 & stop==F){
      ## if not looping around, step if no wall
      if(currentPos[2]<ncol(map) && map[currentPos[1],currentPos[2]+1] != " "){
        if(map[currentPos[1],currentPos[2]+1]=="."){
          currentPos[2]<-currentPos[2]+1
          steps<-steps-1
        } else {
          stop<-T
        }
      } else {
        ## Loop around if no wall on other side
        checkloop<-1
        while(map[currentPos[1],checkloop]==" "){
          checkloop<-checkloop+1
        }
        if(map[currentPos[1],checkloop]=="."){
          currentPos[2]<-checkloop
          steps<-steps-1
        } else {
          stop<-T
        }
      }
    }
  }
  ## Turn
  ## Get rotation direction if there is another one
  i<-i+1
  if(i <= length(instructions)){
    ## Rotate right
    if(instructions[i]=="R"){
      facing<-switch(facing,
                     "E"="S",
                     "S"="W",
                     "W"="N",
                     "N"="E")
    } else {
      ## Rotate left
      facing<-switch(facing,
                     "E"="N",
                     "N"="W",
                     "W"="S",
                     "S"="E")
    }
  }
  i<-i+1
}

## Get the final direction value
direction_value<-switch(facing,
                        "E"=0,
                        "S"=1,
                        "W"=2,
                        "N"=3)

## Get the final password
currentPos[1]*1000+currentPos[2]*4+direction_value

###########################
# Part 2

## find the top left "." location on the map
startPos<-1
while(map[1,startPos] != "."){
  startPos<-startPos+1
}
currentPos<-c(1,startPos)

## initialise the direction facing as East
facing<-"E"

## loop over each step in the instructions
i<-1
while(i<=length(instructions)){
  ## Move
  ## get number of steps to move
  steps<-as.numeric(instructions[i])
  ## do as many steps as possible, stop if hit a wall
  stop<-F
  while(steps>0 & stop==F){
    ## If facing North try to move
    if(facing=="N"){
      ## if not looping around, step if no wall
      if(currentPos[1]>1 && map[currentPos[1]-1,currentPos[2]] != " "){
        if(map[currentPos[1]-1,currentPos[2]]=="."){
          currentPos[1]<-currentPos[1]-1
          steps<-steps-1
        } else {
          stop<-T
        }
      } else {
        ## Loop around if no wall on other side
        if(currentPos[1]==101 && currentPos[2]<=50){
          checkloop<-c(50+currentPos[2],51) #nusd
          tempfacing<-"E"
        } else if (currentPos[1]==1 && currentPos[2]>=51 && currentPos[2]<=100){
          checkloop<-c(currentPos[2]-50+150,1) #nusd
          tempfacing<-"E"
        } else if (currentPos[1]==1 && currentPos[2]>=101){
          checkloop<-c(200,currentPos[2]-100) #nusd
          tempfacing<-"N"
        }
        if(map[checkloop[1],checkloop[2]]=="."){
          currentPos<-checkloop
          facing<-tempfacing
          steps<-steps-1
        } else {
          stop<-T
        }
      }
    ## If facing South try to move
    } else if(facing=="S"){
      ## if not looping around, step if no wall
      if(currentPos[1]<nrow(map) && map[currentPos[1]+1,currentPos[2]] != " "){
        if(map[currentPos[1]+1,currentPos[2]]=="."){
          currentPos[1]<-currentPos[1]+1
          steps<-steps-1
        } else {
          stop<-T
        }
      } else {
        ## Loop around if no wall on other side
        if(currentPos[1]==200 & currentPos[2]<=50){
          checkloop<-c(1,currentPos[2]+100) #nusd
          tempfacing<-"S"
        } else if (currentPos[1]==150 & currentPos[2]>=51 & currentPos[2]<=100){
          checkloop<-c(currentPos[2]-50+150,50) #nusd
          tempfacing<-"W"
        } else if (currentPos[1]==50 & currentPos[2]>=101 & currentPos[2]<=150){
          checkloop<-c(currentPos[2]-100+50,100) #nusd
          tempfacing<-"W"
        }
        if(map[checkloop[1],checkloop[2]]=="."){
          currentPos<-checkloop
          facing<-tempfacing
          steps<-steps-1
        } else {
          stop<-T
        }
      }
    ## If facing West try to move
    } else if(facing=="W"){
      ## if not looping around, step if no wall
      if(currentPos[2]>1 && map[currentPos[1],currentPos[2]-1] != " "){
        if(map[currentPos[1],currentPos[2]-1]=="."){
          currentPos[2]<-currentPos[2]-1
          steps<-steps-1
        } else {
          stop<-T
        }
      } else {
        ## Loop around if no wall on other side
        if(currentPos[1]<=50 & currentPos[2]==51){
          checkloop<-c(151-currentPos[1],1) #usd
          tempfacing<-"E"
        } else if (currentPos[1]>=51 & currentPos[1]<=100 & currentPos[2]==51){
          checkloop<-c(101,currentPos[1]-50) #nusd
          tempfacing<-"S"
        } else if (currentPos[1]>=101 & currentPos[1]<=150 & currentPos[2]==1){
          checkloop<-c(51-(currentPos[1]-100),51) #usd
          tempfacing<-"E"
        } else if (currentPos[1]>=151 & currentPos[1]<=200 & currentPos[2]==1){
          checkloop<-c(1,currentPos[1]-150+50) #nusd
          tempfacing<-"S"
        }
        if(map[checkloop[1],checkloop[2]]=="."){
          currentPos<-checkloop
          facing<-tempfacing
          steps<-steps-1
        } else {
          stop<-T
        }
      }
    ## If facing East try to move
    } else if(facing=="E"){
      ## if not looping around, step if no wall
      if(currentPos[2]<ncol(map) && map[currentPos[1],currentPos[2]+1] != " "){
        if(map[currentPos[1],currentPos[2]+1]=="."){
          currentPos[2]<-currentPos[2]+1
          steps<-steps-1
        } else {
          stop<-T
        }
      } else {
        ## Loop around if no wall on other side
        if(currentPos[1]>=1 & currentPos[1]<=50 & currentPos[2]==150){
          checkloop<-c(151-currentPos[1],100) #usd
          tempfacing<-"W"
        } else if (currentPos[1]>=51 & currentPos[1]<=100 & currentPos[2]==100){
          checkloop<-c(50,currentPos[1]-50+100) #nusd
          tempfacing<-"N"
        } else if (currentPos[1]>=101 & currentPos[1]<=150 & currentPos[2]==100){
          checkloop<-c(51-(currentPos[1]-100),150) #usd
          tempfacing<-"W"
        } else if (currentPos[1]>=151 & currentPos[1]<=200 & currentPos[2]==50){
          checkloop<-c(150,currentPos[1]-150+50) #nusd
          tempfacing<-"N"
        }
        if(map[checkloop[1],checkloop[2]]=="."){
          currentPos<-checkloop
          facing<-tempfacing
          steps<-steps-1
        } else {
          stop<-T
        }
      }
    }
  }
  
  ## Turn
  ## Get rotation direction if there is another one
  i<-i+1
  if(i <= length(instructions)){
    ## Rotate right
    if(instructions[i]=="R"){
      facing<-switch(facing,
                     "E"="S",
                     "S"="W",
                     "W"="N",
                     "N"="E")
    } else {
      ## Rotate left
      facing<-switch(facing,
                     "E"="N",
                     "N"="W",
                     "W"="S",
                     "S"="E")
    }
  }
  i<-i+1
}

## Get the final direction value
direction_value<-switch(facing,
                        "E"=0,
                        "S"=1,
                        "W"=2,
                        "N"=3)

## Get the final password
currentPos[1]*1000+currentPos[2]*4+direction_value

