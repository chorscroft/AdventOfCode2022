# Advent of Code Day 10
# Part 1

## Read in the data
mydata<-read.table("data/day10.txt",fill=T,header = F,col.names = c("instruction","value"))

## Initialise the X register at 1
X<-1

## Initialise the vector of signal values
signals<-NULL

## Loop over each instruction in the data set
for(i in 1:nrow(mydata)){
  if(mydata$instruction[i]=="addx"){
    ## if addx, X will stay the same for two cycles
    signals<-c(signals,X,X)
    ## then X will be changed by the addx amount
    X <- X + mydata$value[i]
  } else {
    ## if noop, X stays the same for one cycle
    signals<-c(signals,X)
  }
}

## initialise the final sum as 0
finalsum<-0
## for each of the required cycles, add the cycle number multiipled by the signal
for(i in seq(20,220,40)){
     finalsum<-finalsum+i*signals[i]
}

## return the final sum
finalsum

###########################
# Part 2

## initialise the CRT screen with 6 rows and 40 columns of dots
endmatrix<-matrix(".",nrow=6,ncol=40)

## start in the top left of the CRT screen
x<-1
y<-1

## Loop over each cycle
for(i in 1:240){
  ## sprite location based on signal value at this cycle
  sprite<-signals[i]+c(0,1,2)
  ## if the sprite is in the right location, draw a "#" symbol on the CRT screen
  if(x==sprite[1] | x==sprite[2] | x==sprite[3]){
    endmatrix[y,x]<-"#"
  }
  ## iterate the x value
  x<-x+1
  ## if we get to the end of the row, go to the next one
  if(x>40){
    x<-1
    y<-y+1
  }
}

## print the final CRT screen, read letters
endmatrix