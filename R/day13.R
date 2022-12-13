# Advent of Code Day 13
# Functions

## function to parse out the next element from the packet
parseNextElement<-function(x){
  ## if the packet is empty, then return an empty packet and and empty element
  if (length(x)==0){
    x<-NULL
    e_x<-NULL
  ## if the next element in the packet is a number
  }else if(!is.na(as.numeric(x[1]))){
    ## return the number as the element
    e_x<-x[1]
    ## remove it from the packet
    x<-x[-1]
    ## if there is more in the packet, remove the comma between elements
    if(length(x)>0){
      x<-x[-1]
    }
  ## if the next element in the packet is a list
  } else {
    ## count the number of open brackets
    countopen<-1
    ## initialise the index
    j<-1
    ## loop until the brackets are all closed
    while(countopen>0){
      ## iterate the index
      j<-j+1
      ## if there is another open bracket, count it
      if(x[j]=="["){
        countopen<-countopen+1
      ## if there is a close bracket, remove it from the count
      } else if(x[j]=="]") {
        countopen<-countopen-1
      }
    }
    ## if the list is bigger than 2 (i.e. it isn't just "[]")
    if(j>2){
      ## keep everything inside the brackets as the element
      e_x<-x[2:(j-1)]
    } else {
      ## otherwise the element is null
      e_x<-NULL
    }
    ## remove the element from the packet
    x<-x[-c(1:j)]
    ## if there is more in the packet, remove the comma between elements
    if(length(x)>0){
      x<-x[-1]
    }
  }
  
  ## if the element is a number, store it as numeric
  if(length(e_x)==1){
    e_x<-as.numeric(e_x)
  }
  
  ## return the updated packet and the next element
  return(list(x=x,e_x=e_x))
}

## function to compare the next left element to the next right element
compare<-function(e1,e2){
  
  ## elements can be lists, numbers or null

  ## if left is null and right isn't, then the order is correct
  if(is.null(e1) && !is.null(e2)){
    return("CORRECT")
  ## if right is null and left isn't, then the order is incorrect
  } else if (!is.null(e1) && is.null(e2)){
    return("INCORRECT")
  ## if they are both null, then continue on the check the next element
  } else if(is.null(e1) && is.null(e2)){
    return("SAME")
  ## if both are a number
  } else if (is.numeric(e1) & is.numeric(e2)){
    ## if left is less than right, then the order is correct
    if(e1<e2){
      return("CORRECT")
    ## if right is less than left, then the order is incorrect
    } else if (e1>e2){
      return("INCORRECT")
    ## if they are the same, then continue on the check the next element
    } else {
      return("SAME")
    }
  ## if one is a number and the other is a list, then turn the number into a list
  } else if(is.numeric(e1) & !is.numeric(e2)){
    e1<-c("[",e1,"]")
    ## compare the lists
    return(compare(e1,e2))
  } else if(!is.numeric(e1) & is.numeric(e2)){
    e2<-c("[",e2,"]")
    ## compare the lists
    return(compare(e1,e2))
  } else {
    ## do nothing: both must be lists so parse out next element
  }
  
  ## get the next element
  ## loop until a resolution is found
  while(T){
    ## parse the next element from the left packet
    parse<-parseNextElement(e1)
    ## update the packet
    e1<-parse$x
    ## get the next element
    e_e1<-parse$e_x
    
    ## parse the next element from the right packet
    parse<-parseNextElement(e2)
    ## update the packet
    e2<-parse$x
    ## get the next element
    e_e2<-parse$e_x
    
    ## compare the elements
    comp_res<-compare(e_e1,e_e2)
    
    ## if the comparison resulted in the ordered to be determined as correct or
    ## incorrect, then return that.
    ## Also stop looping if the current packet list is empty
    if(comp_res %in% c("CORRECT","INCORRECT") | (is.null(e1) & is.null(e2))){
      return(comp_res)
    }
  }
}

## function to combine multiple digits together from a vector into the same element
## e.g. c(1,2,3,"a","b") becomes c(123,"a","b")
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

###########################
# Part 1

## Read in the data
mydata<-read.table("data/day13.txt")

## initialise a vector to store if a pair of packets is in the correct order
isCorrect<-rep(FALSE,nrow(mydata)/2)

## loop over each pair of packets
for(i in 1:(nrow(mydata)/2)){
  ## get the left and right packets from the pair
  p1<-mydata$V1[i*2-1]
  p2<-mydata$V1[i*2]
  
  ## split the packets into vectors with one character per element in the vector
  p1<-unlist(strsplit(p1,""))
  p2<-unlist(strsplit(p2,""))
  
  ## the previous step will have split numbers 10 or greater into separate 
  ## elements. This step sticks numbers with multiple digits back together
  p1<-combineMultipleDigits(p1)
  p2<-combineMultipleDigits(p2)
  
  ## compare the two packets
  comp_res<-compare(p1,p2)
  
  ## if the order is correct, then record that this pair is in the correct order
  if(comp_res=="CORRECT"){
    isCorrect[i] <- T
  }
}

## add the indices of the pairs that are in the correct order
sum(which(isCorrect))

###########################
# Part 2

## Count the number of packets in the data that would be before the [[2]]
## and [[6]] packets once sorted into order

## initialise the counts at zero
countpos2<-0
countpos6<-0

## compare with each packet in the data
for(i in 1:(nrow(mydata))){
  ## get the packet from the data
  p1<-mydata$V1[i]
  p2<-"[[2]]"
  p3<-"[[6]]"
  
  ## split the packets into vectors with one character per element in the vector
  p1<-unlist(strsplit(p1,""))
  p2<-unlist(strsplit(p2,""))
  p3<-unlist(strsplit(p3,""))
  
  ## the previous step will have split numbers 10 or greater into separate 
  ## elements. This step sticks numbers with multiple digits back together
  p1<-combineMultipleDigits(p1)
  
  ## compare the the packet from the data with packet [[2]]
  comp_res<-compare(p1,p2)
  ## if the packet in the data would go before packet [[2]], count it
  if(comp_res=="CORRECT"){
    countpos2<-countpos2+1
  }
  
  ## compare the the packet from the data with packet [[6]]
  comp_res<-compare(p1,p3)
  ## if the packet in the data would go before packet [[2]], count it
  if(comp_res=="CORRECT"){
    countpos6<-countpos6+1
  }
}

## Add one to get the final position of the [[2]] packet
countpos2<-countpos2+1

## Add two to get the final position of the [[6]] packet ([[2]] will also be
## before it)
countpos6<-countpos6+2

## Get the final value
countpos2*countpos6