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
         countTrees

###################################################################

### Day 4

## Read data into a vector - one entry for each passport
## Each passport is concatenated into a single string
day4<-file("day4.txt","r")
passports<-NULL
currentPassport<-NULL
while (TRUE){
  line = readLines(day4, n = 1)
  if (length(line) == 0){
      passports<-c(passports,currentPassport)
      break
  } else if (line==""){
    passports<-c(passports,currentPassport)
    currentPassport<-NULL
  } else {
    currentPassport<-paste(currentPassport,line)
  }
}
close(day4)

## Checks that all required fields are present
checkValid<-function(passport,checks){
  valid<-sapply(checks,function(x)grepl(x,passport))
  return(sum(valid)==length(checks))
}

## Checks that each of the seven fields have valid entries
checkValid2<-function(passport,checks){
  if (checkValid(passport,checks)==FALSE){
    return(FALSE)
  } else{
    passport<-unlist(strsplit(passport,c(" ")))
    #byr
    byr<-as.numeric(substring(passport[grep("byr",passport)],5))
    if (byr==""){
      return(FALSE)
    }
    if (is.na(byr)){
      return(FALSE)
    } else if (byr<1920 | byr>2002) {
      return(FALSE)
    }
    #iyr
    iyr<-as.numeric(substring(passport[grep("iyr",passport)],5))
    if (iyr==""){
      return(FALSE)
    }
    if (is.na(iyr)){
      return(FALSE)
    } else if (iyr<2010 | iyr>2020) {
      return(FALSE)
    }
    #eyr
    eyr<-as.numeric(substring(passport[grep("eyr",passport)],5))
    if (eyr==""){
      return(FALSE)
    }
    if (is.na(eyr)){
      return(FALSE)
    } else if (eyr<2020 | eyr>2030) {
      return(FALSE)
    }
    #hgt
    hgt<-substring(passport[grep("hgt",passport)],5)
    if (hgt==""){
      return(FALSE)
    }
    if (substr(hgt,nchar(hgt)-1,nchar(hgt))=="cm"){
      hgt<-as.numeric(substr(hgt,1,nchar(hgt)-2))
      if (is.na(hgt)){
        return(FALSE)
      } else if (hgt<150 | hgt>193) {
        return(FALSE)
      }
    } else if (substr(hgt,nchar(hgt)-1,nchar(hgt))=="in"){
      hgt<-as.numeric(substr(hgt,1,nchar(hgt)-2))
      if (is.na(hgt)){
        return(FALSE)
      } else if (hgt<59 | hgt>76) {
        return(FALSE)
      }      
    } else {
      return(FALSE)
    }
    #hcl
    hcl<-substring(passport[grep("hcl",passport)],5)
    if (hcl==""){
      return(FALSE)
    }
    if(substr(hcl,1,1)=="#" & nchar(hcl)==7){
      hcl<-substr(hcl,2,7)
      if (regexpr('[0-9a-f]{6,6}',hcl) != 1){
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
    #ecl
    ecl<-substring(passport[grep("ecl",passport)],5)
    if (ecl==""){
      return(FALSE)
    }
    colours<-c("amb","blu","brn","gry","grn","hzl","oth")
    if(sum(sapply(colours,function(x)x==ecl)) != 1){
      return(FALSE)
    }
    #pid
    pid<-substring(passport[grep("pid",passport)],5)
    if (pid==""){
      return(FALSE)
    }    
    if (regexpr('[0-9]{9,9}',pid) != 1 | nchar(pid) !=9){
      return(FALSE)
    }
    return(TRUE)
  }
}
checks<-c("byr:","iyr:","eyr:","hgt:","hcl:","ecl:","pid:")
answer1<-sum(sapply(passports,checkValid,checks))
answer2<-sum(sapply(passports,checkValid2,checks))

###################################################################

### Day 5
day5<-read.table("day5.txt",col.names=c("passes"),comment.char = "")

# Returns the seat number
getSeatNumber<-function(pass){
  pass<-unlist(strsplit(pass,""))
  row<-getRow(pass[1:7])
  col<-getCol(pass[8:10])
  return(row*8+col)
}
# Gets the column number
getCol<-function(binCol){
  set<-c(0:7)
  for (i in binCol){
    if (i =="L"){
      set<-set[1:(length(set)/2)]
    } else {
      set<-set[(length(set)/2+1):length(set)]
    }
  }
  return(set)
}
#Gets the row number
getRow<-function(binRow){
  set<-c(0:127)
  for (i in binRow){
    if (i =="F"){
      set<-set[1:(length(set)/2)]
    } else {
      set<-set[(length(set)/2+1):length(set)]
    }
  }
  return(set)
}

# Vector of seat numbers
seats<-sapply(day5$passes,getSeatNumber)
answer1<-max(seats)

# Find seat that isn't in the list
# But both seats either side are in the list
for(i in 1:answer1){
  if (sum(i==seats)==0 & sum((i-1)==seats)==1 & sum((i+1)==seats)==1){
    answer2<-i
    break
  }
}

###################################################################

### Day 6

## Read file into a list variable 
## One entry into the list per group
## Each group contains a vector where each item is a person's answers

day6<-file("day6.txt","r")
questions<-list()
currentQuestions<-NULL
while (TRUE){
  line = readLines(day6, n = 1)
  if (length(line) == 0){
    questions[[length(questions)+1]]<-currentQuestions
    break
  } else if (line==""){
    questions[[length(questions)+1]]<-currentQuestions
    currentQuestions<-NULL
  } else {
    currentQuestions<-c(currentQuestions,line)
  }
}
close(day6)

## Count all the different questions answered in a group
countAnyoneSaidYes<-function(question){
  return(length(unique(unlist(strsplit(question,"")))))
}
## Count each question that was answered by all members of the group
countEveryoneSaidYes<-function(question){
  return(sum(table(unlist(strsplit(question,"")))==length(question)))
}

answer1<-sum(unlist(lapply(questions,countAnyoneSaidYes)))
answer2<-sum(unlist(lapply(questions,countEveryoneSaidYes)))

###################################################################

### Day 7

## Create a vector of bag names called bags
## Create a list of data frames for the bags contained within the bags
## bags[index] corresponds to contains[[index]]
## contains[[index]] equals 0 if there are no bags inside
## Data frames in contains[[index]] have two columns:
## number of bags and colour of bags

day7<-file("day7.txt","r")
bags<-NULL
contains<-list()
while (TRUE){
  line = readLines(day7, n = 1)
  if (length(line) == 0){
    break
  } else {
    bag<-unlist(strsplit(line," "))
    bags<-c(bags,paste(bag[1],bag[2]))
    if (bag[5]=="no"){
      contains[[length(bags)]]<-0
    } else {
      number<-NULL
      colour<-NULL
      j<-5
      while (j<length(bag)){
        number<-c(number,as.numeric(bag[j]))
        colour<-c(colour,paste(bag[j+1],bag[j+2]))
        j<-j+4
      }
      contains[[length(bags)]]<-data.frame(number,colour)
    }
  }
}
close(day7)

## Returns a count of the number of bags containing
## the given bag colour
countBagsContaining<-function(myBagColour){
  count<-0
  for(i in 1:length(bags)){
    if (bags[i] != myBagColour){
      if(checkInsideBag(i,myBagColour)==TRUE){
        count<-count+1
      }
    }
  }
  return(count)
}

## Checks to see if the given bag colour is inside
## the bag at the given index, returns TRUE or FALSE
checkInsideBag<-function(index,myBagColour){
  if (is.data.frame(contains[[index]])){
    for (k in 1:nrow(contains[[index]])){
      if (contains[[index]]$colour[k]==myBagColour){
        return(TRUE)
      } else {
        if(checkInsideBag(which(bags==contains[[index]]$colour[k],TRUE),myBagColour)==TRUE){
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

## Finds the number of bags within a bag of the given colour
numberOfBagsInside<-function(myBagColour){
  return(bagsInside(which(bags==myBagColour,TRUE)))
}

## Finds the number of bags within a bag at the given index
bagsInside<-function(index){
  count<-0
  if(is.data.frame(contains[[index]])){
    for (k in 1:nrow(contains[[index]])){
      count<-count+contains[[index]]$number[k]+contains[[index]]$number[k]*bagsInside(which(bags==contains[[index]]$colour[k],TRUE))
    }
  }
  return(count)
}

answer1<-countBagsContaining("shiny gold")
answer2<-numberOfBagsInside("shiny gold")

###################################################################

### Day 8

## Read in data
day8<-read.table("day8.txt",col.names=c("instruction","parameter"))

# Vector to record if a line of code has been visited
visited<-rep(FALSE,nrow(day8))

# initialise accumulator
acc<-0
# start with the code at index 1
index<-1
# loop until a line of code has been visited twice
while(visited[index]==FALSE){
  visited[index]<-TRUE
  if(day8$instruction[index]=="nop"){
    index<-index+1
  } else if(day8$instruction[index]=="acc"){
    acc<-acc+day8$parameter[index]
    index<-index+1
  } else if(day8$instruction[index]=="jmp"){
    index<-index+day8$parameter[index]
  } else {
    break
  }
}
answer1<-acc

#initialise line to test
test<-1
# loop until the code is fixed, testing line by line
result<-FALSE
while(result==FALSE){
  if(day8$instruction[test]=="jmp" | day8$instruction[test]=="nop"){
    day8new<-day8
    if(day8new$instruction[test]=="jmp"){
      day8new$instruction[test]<-"nop"
    } else if (day8new$instruction[test]=="nop"){
      day8new$instruction[test]<-"jmp"
    }
    visited<-rep(FALSE,nrow(day8new))
    acc<-0
    index<-1
    while(visited[index]==FALSE){
      visited[index]<-TRUE
      if(day8new$instruction[index]=="nop"){
        index<-index+1
      } else if(day8new$instruction[index]=="acc"){
        acc<-acc+day8new$parameter[index]
        index<-index+1
      } else if(day8new$instruction[index]=="jmp"){
        index<-index+day8new$parameter[index]
      } else {
        break
      }
      if (index==(nrow(day8new)+1)){
        result<-TRUE
        break
      }
    }
  }
  test<-test+1
}
answer2<-acc