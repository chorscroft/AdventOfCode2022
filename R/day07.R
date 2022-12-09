# Advent of Code Day 7
# Part 1

## Read in the data
mydata<-file("data/day07.txt","r")
lines<-NULL
while (TRUE){
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else {
    lines<-c(lines,line)
  }
}
close(mydata)

## Get a vector of all the directory names
## Also, change all the directory names so they are absolute rather than relative

## initialise the vector of directories 
directories<-"/"
## initialise the current directory structure
dirs<-"/"
## set the current directory level to one
level<-1
## loop over each command
for(i in 2:length(lines)){
  ## Check if we are moving directories
  if (substr(lines[i],1,4)=="$ cd"){
    if(substr(lines[i],1,7)=="$ cd .."){
      ## If we are going up a level, minus one from the current directory level
      level<-level-1
      ## Only keep the current directories we are in
      dirs<-dirs[1:level]
    } else {
      ## If we are going down a level, add one to the current directory level
      level<-level+1
      ## Add the new directory to the current directories that we are inside
      dirs<-c(dirs,substr(lines[i],6,nchar(lines[i])))
      ## Change the command to the relative directory name
      lines[i]<-paste0("$ cd ",paste0(dirs,collapse = "_"))
    }
  ## If the line is a result of the $ls command and contains a directory, change the
  ## directory name to be relative rather than absolute
  } else if (substr(lines[i],1,4)=="dir "){
    lines[i]<-paste0("dir ",paste0(dirs,collapse = "_"),"_",substr(lines[i],5,nchar(lines[i])))
    ## Record the directory in the vector of directory names
    directories<-c(directories,substr(lines[i],5,nchar(lines[i])))
  }
}

## Create a list, where each element corresponds to a directory
## Each element in the list is vector of all the files and folders within the directory

## Initialise list of directories
dir_cont<-vector("list",length(directories))
## Current directory number (index from directories vector)
curr_dir<-0
## Loop over each command
for(i in 1:length(lines)){
  if (substr(lines[i],1,7)=="$ cd .."){
    ## if moving up do nothing
  } else if(substr(lines[i],1,4)=="$ cd") {
    ## if moving to a new directory, record all the things in the current directory in the list
    ## But not if this is the very first directory
    if(curr_dir != 0){
      dir_cont[[curr_dir]]<-curr_dir_cont
    }
    ## Get the new directory's index
    curr_dir<-which(directories==substr(lines[i],6,nchar(lines[i])))
    ## initialise the vector of items inside the new directory
    curr_dir_cont<-NULL
  } else if(substr(lines[i],1,4)=="dir "){
    ## record the name of a directory within the current directory
    curr_dir_cont<-c(curr_dir_cont,substr(lines[i],5,nchar(lines[i])))
  } else if(substr(lines[i],1,4)=="$ ls"){
    ## if listing, do nothing
  } else {
    ## record the size of a file within the current directory
    curr_dir_cont<-c(curr_dir_cont,unlist(strsplit(lines[i]," "))[1])
  }
}
## record the things inside the final directory in the list
dir_cont[[curr_dir]]<-curr_dir_cont

## Function to find the sum of file/folder sizes inside a directory
findsum<-function(x){
  if(!is.na(sum(as.numeric(x)))){
    x<-sum(as.numeric(x))
  }
  return(x)
}

## This section of code finds any directories that just contain numbers, and finds the total
## size. Then any time that directory appears inside another, change it to the number.
## Then repeat, until the root directory has become just a number.

## For each directory, if it only contains numbers then make it so it only contains the sum
## Put this into a new list so it doesn't overwrite the original
new_dir_cont<-lapply(dir_cont,findsum)

## Loop until the root directory is just a number
while(!is.numeric(new_dir_cont[[1]])){
  ## Loop over each directory
  for(i in 1:length(new_dir_cont)){
    ## If the directory isn't already just a number, then continue
    if(!is.numeric(new_dir_cont[[i]])){
      ## loop over each element inside the directory
      for(j in 1:length(new_dir_cont[[i]])){
        ## If the element is another directory that hasn't been resolved, continue
        if(is.na(as.numeric(new_dir_cont[[i]][j]))){
          ## Check to see if the inner directory contains just a number
          if(is.numeric(new_dir_cont[[which(directories==new_dir_cont[[i]][j])]])){
            ## if it contains just a number, update the entry to just be the number
            new_dir_cont[[i]][j]<-new_dir_cont[[which(directories==new_dir_cont[[i]][j])]]
          }
        }
      }
    }
  }
  ## For each directory, if it only contains numbers then make it so it only contains the sum
  new_dir_cont<-lapply(new_dir_cont,findsum)
}

## Initialise the final sum of the size of directories that are less than 100,000
finalsum<-0
## Loop over each directory
for(i in 1:length(new_dir_cont)){
  if(new_dir_cont[[i]]<=100000){
    ## If the size of the directory is less than 100,000 then add its size to the final sum
    finalsum<-finalsum+new_dir_cont[[i]]
  }
}

## Output the combined size of all the directories less than 100,000
finalsum

###########################
# Part 2

## Find the space required to clear:
## 30000000, minus the current available space
space<-30000000-(70000000-new_dir_cont[[1]])
## Initialise the minimum directory size that exists to clear the required space at infinity
min_dir_size<-Inf
## Loop over directories
for(i in 1:length(new_dir_cont)){
  ## If the directory size is greater than or equal to the size needed to clear then continue
  if(new_dir_cont[[i]]>=space){
    if(new_dir_cont[[i]]<min_dir_size){
      ## If the directory is the smallest found so far, record its size
      min_dir_size<-new_dir_cont[[i]]
    }
  }
}

## Output the smallest directory that can be deleted to clear the required space
min_dir_size


