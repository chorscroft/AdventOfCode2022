# Advent of Code Day 5
# Part 1

## Read in the movement data
mydata<-read.table("data/day05.txt",skip=10)

## Read in the crates
crates<-read.fwf("data/day05.txt",rep(1,35),nrows=8)

## Put piles of crates into a list, where each element in the list is 
## a vector representing the pile of crates from bottom to top
lcrates<-list(
  rev(crates$V2),
  rev(crates$V6),
  rev(crates$V10),
  rev(crates$V14),
  rev(crates$V18),
  rev(crates$V22),
  rev(crates$V26),
  rev(crates$V30),
  rev(crates$V34)
)
## Trim each vector in th list to the number of crates in the pile
lcrates<-lapply(lcrates,function(x)x[x!=" "])

## Loop over each movement instruction
for(i in 1:nrow(mydata)){
  ## Get the number of crates to move
  number<-mydata$V2[i]
  ## Get the location crates are moving from
  from<-mydata$V4[i]
  ## Get the location crates are moving to
  to<-mydata$V6[i]
  
  ## Get vector of crates that are being moved
  movingcrates<-lcrates[[from]][(length(lcrates[[from]])-number+1):length(lcrates[[from]])]
  ## Remove crates from their pile
  lcrates[[from]]<-lcrates[[from]][1:(length(lcrates[[from]])-number)]
  ## Add crates to their new pile in reverse order
  lcrates[[to]]<-c(lcrates[[to]],rev(movingcrates))
}

## Return the top crates in each pile
paste0(lcrates[[1]][length(lcrates[[1]])],
       lcrates[[2]][length(lcrates[[2]])],
       lcrates[[3]][length(lcrates[[3]])],
       lcrates[[4]][length(lcrates[[4]])],
       lcrates[[5]][length(lcrates[[5]])],
       lcrates[[6]][length(lcrates[[6]])],
       lcrates[[7]][length(lcrates[[7]])],
       lcrates[[8]][length(lcrates[[8]])],
       lcrates[[9]][length(lcrates[[9]])])


###########################
# Part 2

## Read in the movement data
mydata<-read.table("data/day05.txt",skip=10)

## Read in the crates
crates<-read.fwf("data/day05.txt",rep(1,35),nrows=8)

## Put piles of crates into a list, where each element in the list is 
## a vector representing the pile of crates from bottom to top
lcrates<-list(
  rev(crates$V2),
  rev(crates$V6),
  rev(crates$V10),
  rev(crates$V14),
  rev(crates$V18),
  rev(crates$V22),
  rev(crates$V26),
  rev(crates$V30),
  rev(crates$V34)
)
## Trim each vector in th list to the number of crates in the pile
lcrates<-lapply(lcrates,function(x)x[x!=" "])

## Loop over each movement instruction
for(i in 1:nrow(mydata)){
  ## Get the number of crates to move
  number<-mydata$V2[i]
  ## Get the location crates are moving from
  from<-mydata$V4[i]
  ## Get the location crates are moving to
  to<-mydata$V6[i]
  
  ## Get vector of crates that are being moved
  movingcrates<-lcrates[[from]][(length(lcrates[[from]])-number+1):length(lcrates[[from]])]
  ## Remove crates from their pile
  lcrates[[from]]<-lcrates[[from]][1:(length(lcrates[[from]])-number)]
  ## Add crates to their new pile
  lcrates[[to]]<-c(lcrates[[to]],movingcrates)
}

## Return the top crates in each pile
paste0(lcrates[[1]][length(lcrates[[1]])],
       lcrates[[2]][length(lcrates[[2]])],
       lcrates[[3]][length(lcrates[[3]])],
       lcrates[[4]][length(lcrates[[4]])],
       lcrates[[5]][length(lcrates[[5]])],
       lcrates[[6]][length(lcrates[[6]])],
       lcrates[[7]][length(lcrates[[7]])],
       lcrates[[8]][length(lcrates[[8]])],
       lcrates[[9]][length(lcrates[[9]])])
