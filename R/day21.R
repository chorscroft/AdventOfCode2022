# Advent of Code Day 21
# Part 1

## Read in the data
mydata<-read.table("data/day21.txt",sep=":")

## Get the number of monkeys
nomonkeys<-nrow(mydata)

## initialise a list where each element is the thing they will yell
monkeylist<-vector("list",nrow(mydata))

## Identify which is the root monkey
rootmonkey<-which(mydata$V1=="root")

## Loop over each monkey
for(i in 1:nomonkeys){
  ## if they have an equation, store it in a vector
  if(is.na(as.numeric(mydata$V2[i]))){
    monkeylist[[i]]<-unlist(strsplit(mydata$V2[i]," "))
    monkeylist[[i]]<-monkeylist[[i]][-1]
  ## if they have a number, store it as numeric
  } else {
    monkeylist[[i]]<-as.numeric(mydata$V2[i])
  }
}

## loop over each monkey
for(i in 1:nomonkeys){
  ## if they have a number, continue
  if(length(monkeylist[[i]])==1){
    ## get the name of the monkey
    name<-mydata$V1[i]
    ## get the number they yell
    number<-monkeylist[[i]]
    ## loop over all the monkeys
    for(j in 1:nomonkeys){
      ## if they have an equation, look for the monkey's name in the equation
      ## if found, replace the name with the number
      if(length(monkeylist[[j]])>1){
        if(monkeylist[[j]][1]==name){
          monkeylist[[j]][1]<-number
        } else if (monkeylist[[j]][3]==name){
          monkeylist[[j]][3]<-number
        }
      }
    }
  }
}

## loop until the root monkey yells a number
while(length(monkeylist[[rootmonkey]])>1){
  ## loop over each monkey
  for(i in 1:nomonkeys){
    ## if the monkey has an equation, then check if the elements are all numbers
    if(length(monkeylist[[i]])>1){
      ## if they are all numbers, resolve the equation
      if(!is.na(as.numeric(monkeylist[[i]][1])) && !is.na(as.numeric(monkeylist[[i]][3]))){
        if(monkeylist[[i]][2]=="+"){
          monkeylist[[i]]<-as.numeric(monkeylist[[i]][1])+as.numeric(monkeylist[[i]][3])
        } else if(monkeylist[[i]][2]=="-"){
          monkeylist[[i]]<-as.numeric(monkeylist[[i]][1])-as.numeric(monkeylist[[i]][3])
        } else if(monkeylist[[i]][2]=="*"){
          monkeylist[[i]]<-as.numeric(monkeylist[[i]][1])*as.numeric(monkeylist[[i]][3])
        } else if(monkeylist[[i]][2]=="/"){
          monkeylist[[i]]<-as.numeric(monkeylist[[i]][1])/as.numeric(monkeylist[[i]][3])
        }
        ## get the name of the monkey that is now yelling a number
        name<-mydata$V1[i]
        ## get the number it is yelling
        number<-monkeylist[[i]]
        ## loop over all the monkeys
        for(j in 1:nomonkeys){
          ## if they have an equation, look for the monkey's name in the equation
          ## if found, replace the name with the number
          if(length(monkeylist[[j]])>1){
            if(monkeylist[[j]][1]==name){
              monkeylist[[j]][1]<-number
            } else if (monkeylist[[j]][3]==name){
              monkeylist[[j]][3]<-number
            }
          }
        }
      }
    }
  }
}

## return the number the root monkey yells
monkeylist[[rootmonkey]]

###########################
# Part 2

## initialise a list where each element is the thing they will yell
monkeylist<-vector("list",nrow(mydata))

## Loop over each monkey
for(i in 1:nomonkeys){
  ## if they have an equation, store it in a vector
  if(is.na(as.numeric(mydata$V2[i]))){
    monkeylist[[i]]<-unlist(strsplit(mydata$V2[i]," "))
    monkeylist[[i]]<-monkeylist[[i]][-1]
  } else {
  ## if they have a number, store it as numeric
    monkeylist[[i]]<-as.numeric(mydata$V2[i])
  }
  ## if the monkey's name is humn, replace its number with X
  if(mydata$V1[i]=="humn"){
    monkeylist[[i]]<-"X"
  ## if the monkey is the root monkey, replace it's operator with ==
  } else if(mydata$V1[i]=="root"){
    monkeylist[[i]][2]<-"=="
  }
}

## loop over each monkey
for(i in 1:nomonkeys){
  ## if they have a number, continue
  if(length(monkeylist[[i]])==1){
    ## get the name of the monkey
    name<-mydata$V1[i]
    ## get the number they yell
    number<-monkeylist[[i]]
    ## loop over all the monkeys
    for(j in 1:nomonkeys){
      ## if they have an equation, look for the monkey's name in the equation
      ## if found, replace the name with the number
      if(length(monkeylist[[j]])>1){
        if(monkeylist[[j]][1]==name){
          monkeylist[[j]][1]<-number
        } else if (monkeylist[[j]][3]==name){
          monkeylist[[j]][3]<-number
        }
      }
    }
  }
}

## First, resolve all the numbers as far as possible

## record if something changes in a loop
change<-T
## loop until there is no more changes
while(change==T){
  ## assume there will be no change
  change<-F
  ## loop over each monkey
  for(i in 1:nomonkeys){
    ## if the monkey has an equation, then check if the elements are all numbers
    if(length(monkeylist[[i]])>1){
      ## if they are all numbers, resolve the equation
      if(!is.na(as.numeric(monkeylist[[i]][1])) && !is.na(as.numeric(monkeylist[[i]][3]))){
        if(monkeylist[[i]][2]=="+"){
          monkeylist[[i]]<-as.numeric(monkeylist[[i]][1])+as.numeric(monkeylist[[i]][3])
        } else if(monkeylist[[i]][2]=="-"){
          monkeylist[[i]]<-as.numeric(monkeylist[[i]][1])-as.numeric(monkeylist[[i]][3])
        } else if(monkeylist[[i]][2]=="*"){
          monkeylist[[i]]<-as.numeric(monkeylist[[i]][1])*as.numeric(monkeylist[[i]][3])
        } else if(monkeylist[[i]][2]=="/"){
          monkeylist[[i]]<-as.numeric(monkeylist[[i]][1])/as.numeric(monkeylist[[i]][3])
        }
        ## record that something has changed
        change<-T       
        ## get the name of the monkey that is now yelling a number
        name<-mydata$V1[i]
        ## get the number it is yelling
        number<-monkeylist[[i]]
        ## loop over all the monkeys
        for(j in 1:nomonkeys){
          ## if they have an equation, look for the monkey's name in the equation
          ## if found, replace the name with the number
          if(length(monkeylist[[j]])>1){
            if(monkeylist[[j]][1]==name){
              monkeylist[[j]][1]<-number
            } else if (monkeylist[[j]][3]==name){
              monkeylist[[j]][3]<-number
            }
          }
        }
      }
    }
  }
}

## Now create the final equation with X to be resolved

## record if something changes in a loop
change<-T
## loop until there is no more changes
while(change==T){
  ## assume there will be no change
  change<-F
  ## loop over each monkey
  for(i in 1:nomonkeys){
    ## if monkey has an equation an is not the root monkey, continue
    if(length(monkeylist[[i]])>1 && i != rootmonkey){
      ## If the monkey has X in its equation, continue
      if("X" %in% monkeylist[[i]]){
        ## if the monkey has a name still in its equation, skip it
        skip<-F
        for(k in 1:length(monkeylist[i])){
          if(!(monkeylist[[i]][k] %in% c("X","+","-","/","(",")") | !is.na(as.numeric(monkeylist[[i]][k])))){
            skip<-T
          }
        }
        if(skip==F){
          ## record that something changed
          change<-T
          ## get the name of the monkey
          name<-mydata$V1[i]
          ## get the equation form the monkey, and surround it with brackets
          Xvector<-c("(",monkeylist[[i]],")")
          ## zero out the monkey's equation
          monkeylist[[i]]<-0
          ## loop over all the monkeys
          for(j in 1:nomonkeys){
            ## if they have an equation, look for the monkey's name in the equation
            ## if found, replace the name with the equation vector
            if(length(monkeylist[[j]])>1){
              for(k in 1:length(monkeylist[[j]])){
                if(monkeylist[[j]][k]==name){
                  if(k==1){
                    monkeylist[[j]]<-c(Xvector,monkeylist[[j]][(k+1):length(monkeylist[[j]])])
                  } else if (k==length(monkeylist[[j]])){
                    monkeylist[[j]]<-c(monkeylist[[j]][1:(k-1)],Xvector)
                  } else {
                    monkeylist[[j]]<-c(monkeylist[[j]][1:(k-1)],Xvector,monkeylist[[j]][(k+1):length(monkeylist[[j]])])
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

## Finally left with an equation to solve
## Solve it by removing the brackets one by one and doing the operation
## to the number on the right hand side

## Get the number on the right hand side
endnumber<-as.numeric(monkeylist[[rootmonkey]][length(monkeylist[[rootmonkey]])])
## Remove the number and the ==
equation<-monkeylist[[rootmonkey]][1:(length(monkeylist[[rootmonkey]])-2)]
## loop until the equation is resolved
while(length(equation)>1){
  ## remove ()
  equation<-equation[-c(1,length(equation))]
  ## if the number and operator are on the right
  if(equation[1]=="(" || equation[1]=="X"){
    ## get the number
    number<-as.numeric(equation[length(equation)])
    ## get the operator
    symbol<-equation[length(equation)-1]
    ## remove them from the equation
    equation<-equation[1:(length(equation)-2)]
    ## do the applicable operation
    if(symbol=="+"){
      endnumber<-endnumber-number
    } else if(symbol=="-"){
      endnumber<-endnumber+number
    } else if(symbol=="*"){
      endnumber<-endnumber/number
    } else if(symbol=="/"){
      endnumber<-endnumber*number
    }
  ## if the number and operator are on the left
  } else {
    ## get the number
    number<-as.numeric(equation[1])
    ## get the operator
    symbol<-equation[2]
    ## remove them from the equation
    equation<-equation[3:length(equation)]
    ## do the applicable operation
    if(symbol=="+"){
      endnumber<-endnumber-number
    } else if(symbol=="-"){
      endnumber<-number-endnumber
    } else if(symbol=="*"){
      endnumber<-endnumber/number
    } else if(symbol=="/"){
      endnumber<-number/endnumber
    }
  }
}

## Return the final result
endnumber


