# Advent of Code Day 15
# Part 1

## Read in the data
mydata<-read.table("data/day15.txt")

## get the x and y coordinates of the sensors
sensorx<-as.numeric(sapply(mydata$V3,function(x)substr(x,3,(nchar(x)-1))))
sensory<-as.numeric(sapply(mydata$V4,function(x)substr(x,3,(nchar(x)-1))))

## get the x and y coordinates of the beacons
beaconx<-as.numeric(sapply(mydata$V9,function(x)substr(x,3,(nchar(x)-1))))
beacony<-as.numeric(sapply(mydata$V10,function(x)substr(x,3,(nchar(x)))))

## initialse distance matrix for recording manhattan distances between
## sensor and its closest beacon
distance<-rep(0,length(mydata))

## row to check
checkrow<-2000000
## initialise vector of x coordinates that cannot contain a beacon
## in the given row
xs<-NULL

## loop over each sensor
for(i in 1:nrow(mydata)){

## calculate the manhattan distance between the sensor and the beacon
distance[i]<-abs(sensorx[i]-beaconx[i])+abs(sensory[i]-beacony[i])

## if the distance reaches the given row, continue
if(abs(sensory[i]-checkrow)<=distance[i]){
  ## calculate the range of x values that cannot contain a beacon for this sensor
  temp<-distance[i]-abs(sensory[i]-checkrow)
  range<-(sensorx[i]-temp):(sensorx[i]+temp)
  ## record range in vector
  xs<-c(xs,range)
  ## remove any x coordinates that were already in the vector
  xs<-unique(xs)
  }
}

## Remove any coordinates that already contain a beacon in the given row
## loop over beacons
for(i in 1:nrow(mydata)){
  ## if the beacon is in the given row, remove it
  if(beacony[i]==checkrow && beaconx[i] %in% xs){
    xs<-xs[-which(xs==beaconx[i])]
  }
}

## return the number of positions that cannot contain a beacon
length(xs)

###########################
# Part 2

## The unchecked location must be exactly one square outside the range of
## at least one sensor-beacon distance
## Start by finding all the possible coordinates that this applies to

## initialse x and y coordinates to check
checkx<-NULL
checky<-NULL
## loop over each sensor
for(i in 1:nrow(mydata)){
  ## get all the x and y coordinates around the sensor that are the distance
  ## to the nearest beacon plus one
  checkx<-c(checkx,sensorx[i] + -(distance[i]+1):(distance[i] + 1))
  checkx<-c(checkx,sensorx[i] + (distance[i]):-(distance[i]))
  checky<-c(checky,sensory[i]+0:(distance[i]+1))
  checky<-c(checky,sensory[i]+(distance[i]):0)
  checky<-c(checky,sensory[i]+(-1):-(distance[i]+1))
  checky<-c(checky,sensory[i]+-(distance[i]):(-1))
}

## Only check coordinates that are in the given area
checky<-checky[checkx>=0 & checkx<=4000000]
checkx<-checkx[checkx>=0 & checkx<=4000000]
checkx<-checkx[checky>=0 & checky<=4000000]
checky<-checky[checky>=0 & checky<=4000000]

## Loop over each coordinate to check
for(cr in 1:length(checkx)){
  ## assume the coordinate is not covered by a sensor
  covered<-F
  ## loop over each sensor
  for(i in 1:nrow(mydata)){
    ## if the distance reaches the given y coordinate, continue
    if(abs(sensory[i]-checky[cr])<=distance[i]){
      temp<-distance[i]-abs(sensory[i]-checky[cr])
      ## If the x value begin checked falls inside the range of the sensor then
      ## move onto the next coordinate to check
      if(checkx[cr]>=sensorx[i]-temp & checkx[cr]<=sensorx[i]+temp){
        covered<-T
        break
      }
    }
  }
  ## if the coordinate to check wasn't covered by any of the sensors,
  ## stop looking
  if(covered==F){
    break
  }
}

## force R to output the full number (not in scientific notation)
options(scipen=999)
## return the tuning frequency
checkx[cr]*4000000+checky[cr]