# Advent of Code Day 1
# Part 1

## Open a connection to the file
mydata<-file("data/day01.txt","r")

## Initialise the maximum calories found so far as 0
maxcal<-0

## Intitialse the current calorie count as 0
currcal<-0

## Loop through every line in the file
while (TRUE){
  ## Read in the line
  line = readLines(mydata, n = 1)
  
  if (length(line) == 0){
    ## If the length of the line is 0, the end of the file has been reached
    break
  } else if (line==""){
    ## If the line is blank, then we have reached the end for that elf
    if (maxcal < currcal ){
      ## If the elf has the most calories so far, record it
      maxcal<-currcal
    }
    ## Start ounting the calories for the next elf
    currcal<-0
  } else {
    ## Add on the calories in the line to the current count
    currcal<-currcal+as.numeric(line)
  }
}
## Close the file connection
close(mydata)

## Read out the result
maxcal

###########################
# Part 2

## Open a connection to the file
mydata<-file("data/day01.txt","r")

## Initialise the top three calorie counts found so far as 0
maxcal1<-0
maxcal2<-0
maxcal3<-0

## Intitialse the current calorie count as 0
currcal<-0

## Loop through every line in the file
while (TRUE){
  ## Read in the line
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    ## If the length of the line is 0, the end of the file has been reached
    break
  } else if (line==""){
    # If the line is blank, then we have reached the end for that elf
    if (maxcal1 < currcal ){
      ## If the elf has the most calories so far, record it
      maxcal3<-maxcal2
      maxcal2<-maxcal1
      maxcal1<-currcal
    } else if (maxcal2 < currcal ){
      ## If the elf has the 2nd most calories so far, record it
      maxcal3<-maxcal2
      maxcal2<-currcal
    } else if (maxcal3 < currcal ){
      ## If the elf has the 3rd most calories so far, record it
      maxcal3<-currcal
    } 
    ## Start ounting the calories for the next elf
    currcal<-0
  } else {
    ## Add on the calories in the line to the current count
    currcal<-currcal+as.numeric(line)
  }
}
## Close the file connection
close(mydata)

## Read out the result
maxcal1+maxcal2+maxcal3