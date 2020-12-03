### Advent of Code ###

setwd("C:/Users/ch19g17/Documents/AdventOfCode2020")

###################################################################

## Day 1

## define function findValue. 
## This function sums all the numbers in a given vector, 
## and if they match the value given for the result, 
## the product of the numbers in the vector is returned. 
findValue<-function(vector,result){
  if (sum(vector)==result){
    return(prod(vector))
  } else {
    return(0)
  }
}

##define function to iterate a vector
iterate<-function(testVector,index,maxValue){
  testVector[index]<-testVector[index]+1
  ## check this index is okay
  if (testVector[index]<=maxValue){
    i<-1
    while(index+i<=length(testVector)){
      testVector[index+i]<-testVector[index]+i
      i<-i+1
    }
  } else if (index==1) {
    return(NA)
  } else {
    testVector<-iterate(testVector,index-1,maxValue-1)
  }
  return(testVector)
}

## define function to find the answer, 
## given the data in vector format, 
## and the number of values that must sum to the given result
getAnswer<-function(data,noValues,result){
  testVector<-c(1:noValues)
  testProduct<-findValue(data[testVector],result)
  while(testProduct==0){
    testVector<-iterate(testVector,length(testVector),length(data))
    if (is.na(testVector[1])){
      return(NA)
    }
    testProduct<-findValue(data[testVector],result)
  }
  return(testProduct)
}

## get data
day1<-read.table("day1.txt")
## generate answers
getAnswer(day1$V1,2,2020)
getAnswer(day1$V1,3,2020)

###################################################################

### Day 2

checkValid<-function(number,letter,password){
  letter<-substr(letter,1,nchar(letter)-1)
  number<-as.numeric(unlist(strsplit(number,"-")))
  count<-sum(grepl(letter,unlist(strsplit(password,""))))
  return((count>=number[1] & count<=number[2]))
}

checkValid2<-function(number,letter,password){
  letter<-substr(letter,1,nchar(letter)-1)
  number<-as.numeric(unlist(strsplit(number,"-")))
  count<-sum(c(substr(password,number[1],number[1])==letter,substr(password,number[2],number[2])==letter))
  return(count==1)
}

##get data
day2<-read.table("day2.txt",col.names=c("number","letter","password"))
## generate answers
answer1<-sum(apply(day2,1,function(x) checkValid(x[1],x[2],x[3])))
answer2<-sum(apply(day2,1,function(x) checkValid2(x[1],x[2],x[3])))

###################################################################

### Day 3
day3<-read.table("day3.txt",col.names=c("pattern"),comment.char = "")

# Iterate the column based on the given slope
nextCol<-function(col,slope,maxCol){
  col <- col + slope
  if (col > maxCol){
    col <- col - maxCol
  }
  return(col)
}
# Iterate the row based on the given slope
nextRow<-function(row,slope){
  row<-row+slope
}
# Count the number of trees given the slope and the pattern
countTrees<-function(slope,pattern){
  col<-1
  row<-1
  countTree<-0
  maxCol<-nchar(pattern[1])
  while (row<=nrow(day3)){
    if(substr(pattern[row],col,col)=="#"){
      countTree<-countTree+1
    }
    col<-nextCol(col,slope[1],maxCol)
    row<-nextRow(row,slope[2])
  }
  return(countTree)
}

answer1<-countTrees(c(3,1),day3$pattern)
answer2<-countTrees(c(1,1),day3$pattern)*
         countTrees(c(3,1),day3$pattern)*
         countTrees(c(5,1),day3$pattern)*
         countTrees(c(7,1),day3$pattern)*
         countTrees(c(1,2),day3$pattern)