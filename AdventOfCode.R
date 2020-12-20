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

###################################################################

### Day 9

## Read in data
day9<-read.table("day9.txt",col.names=c("number"))

## finds if the number at the particular index is the sum
## of two numbers in the previous set (of size preamble)
isNumberSum<-function(index,preamble){
  number<-day9$number[index]
  for (i in 1:(preamble-1)){
    for (j in (i+1):preamble){
      print.default(day9$number[index-i]+day9$number[index-j])
      if ((day9$number[index-i]+day9$number[index-j])==number){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

## Find the first number which isn't the sum of two
## of the previous 25 numbers
preamble<-25
index<-preamble+1
while (isNumberSum(index,preamble)==TRUE){
  index<-index+1
}
answer1<-day9$number[index]

## search for a contiguous set of numbers that 
## sum to answer1
for(i in 1:nrow(day9)){
  summing<-day9$number[i]
  j<-i
  while(summing<answer1 & j<nrow(day9)){
    j<-j+1     
    summing<-summing+day9$number[j]
  }
  if (summing==answer1){
    # Get the sum of the min and max numbers in the contiguous set
    answer2<-min(day9$number[c(i:j)])+max(day9$number[c(i:j)])
    break
  }
}

###################################################################

### Day 10

## Read in data
day10<-read.table("day10.txt",col.names=c("adapters"))

## get device joltage
device<-max(day10$adapters+3)
## sort joltages
sorted<-sort(day10$adapters)
## get differences
differences<-c(sorted,device)-c(0,sorted)
diffs<-table(differences)
## multiply differences of 1 with differences of 3
answer1<-diffs["1"]*diffs["3"]

## Find sets of adapters that are grouped
currVec<-NULL
noSets<-0
sets<-list()
sorteds<-c(0,sorted,device)
for(i in 2:(length(sorteds)-1)){
  if (sorteds[i]-sorteds[i-1]==1 & sorteds[i+1]-sorteds[i]==1){
    currVec<-c(currVec,sorteds[i])
  } else if (is.vector(currVec)) {
    noSets<-noSets+1
    sets[[noSets]]<-currVec
    currVec<-NULL
  }
}

## Get the different combinations given the set size
combin<-rep(0,length(sets))
for (i in 1:length(sets)){
  if(length(sets[[i]])==1){
    combin[i]<-2
  } else if(length(sets[[i]])==2){
    combin[i]<-4
  } else if(length(sets[[i]])==3){
    combin[i]<-7
  }
}
## Multiply these combinations together
answer2<-prod(combin)

###################################################################

### Day 11

## Read in data
day11<-read.table("day11.txt",col.names=c("seats"))

init_layout<-sapply(day11$seats,function(x)unlist(strsplit(x,"")),USE.NAMES = FALSE)

## Part 1

## Are all 8 adjacent seats empty?
allAdjacentEmpty<-function(oldLayout,r,c){
  if (r>1){
    if (oldLayout[r-1,c]=="#"){
      return(FALSE)
    }
    if (c>1){
      if (oldLayout[r-1,c-1]=="#"){
        return(FALSE)
      }
    }
    if (c<ncol(oldLayout)){
      if(oldLayout[r-1,c+1]=="#"){
        return(FALSE)
      }
    }
  }
  if (r<nrow(oldLayout)){
    if (oldLayout[r+1,c]=="#"){
      return(FALSE)
    }
    if (c>1){
      if (oldLayout[r+1,c-1]=="#"){
        return(FALSE)
      }
    }
    if (c<ncol(oldLayout)){
      if(oldLayout[r+1,c+1]=="#"){
        return(FALSE)
      }
    }
  }
  if (c<ncol(oldLayout)){
    if(oldLayout[r,c+1]=="#"){
      return(FALSE)
    }
  }
  if (c>1){
    if (oldLayout[r,c-1]=="#"){
      return(FALSE)
    }
  }    
  return(TRUE)
}

## Are there 4 or more occupied adjacent seats?
fourOrMoreOccupied<-function(oldLayout,r,c){
  occupied<-0
  if (r>1){
    if (oldLayout[r-1,c]=="#"){
      occupied<-occupied+1
    }
    if (c>1){
      if (oldLayout[r-1,c-1]=="#"){
        occupied<-occupied+1
      }
    }
    if (c<ncol(oldLayout)){
      if(oldLayout[r-1,c+1]=="#"){
        occupied<-occupied+1
      }
    }
  }
  if (r<nrow(oldLayout)){
    if (oldLayout[r+1,c]=="#"){
      occupied<-occupied+1
    }
    if (c>1){
      if (oldLayout[r+1,c-1]=="#"){
        occupied<-occupied+1
      }
    }
    if (c<ncol(oldLayout)){
      if(oldLayout[r+1,c+1]=="#"){
        occupied<-occupied+1
      }
    }
  }
  if (c<ncol(oldLayout)){
    if(oldLayout[r,c+1]=="#"){
      occupied<-occupied+1
    }
  }
  if (c>1){
    if (oldLayout[r,c-1]=="#"){
      occupied<-occupied+1
    }
  }    
  if (occupied>=4){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## Gets the new layout by applying both rules to every seat simultaneously
getNewLayout<-function(oldLayout){
  newLayout<-oldLayout
  for (r in 1:nrow(oldLayout)){
    for (c in 1:ncol(oldLayout)){
      if(oldLayout[r,c]=="L"){
        if (allAdjacentEmpty(oldLayout,r,c)){
          newLayout[r,c]<-"#"
        }
      } else if (oldLayout[r,c]=="#"){
        if (fourOrMoreOccupied(oldLayout,r,c)){
          newLayout[r,c]<-"L"
        }
      }
    }
  }
  return(newLayout)
}

## Change the seating according to the rules until
## an equalibrium is found
layout<-init_layout
while(TRUE){
  newLayout<-getNewLayout(layout)
  if(isTRUE(all.equal(layout,newLayout))){
    break
  } else {
    layout<-newLayout
  }
}
## Count the number of occupied seats
answer1<-sum(newLayout=="#")

## Part 2

## Are all 8 visible seats empty?
allAdjacentVisibleEmpty<-function(oldLayout,r,c){
  #N
  tempr<-r
  while (tempr>1){
    tempr<-tempr-1
    if (oldLayout[tempr,c]=="#"){
      return(FALSE)
    } 
    if (oldLayout[tempr,c]=="L"){
      break
    }
  }
  #S
  tempr<-r
  while (tempr<nrow(oldLayout)){
    tempr<-tempr+1
    if (oldLayout[tempr,c]=="#"){
      return(FALSE)
    } 
    if (oldLayout[tempr,c]=="L"){
      break
    }
  }
  #W
  tempc<-c
  while (tempc>1){
    tempc<-tempc-1
    if (oldLayout[r,tempc]=="#"){
      return(FALSE)
    } 
    if (oldLayout[r,tempc]=="L"){
      break
    }
  }
  #E
  tempc<-c
  while (tempc<ncol(oldLayout)){
    tempc<-tempc+1
    if (oldLayout[r,tempc]=="#"){
      return(FALSE)
    }
    if (oldLayout[r,tempc]=="L"){
      break
    }
  }
  #NE
  tempr<-r
  tempc<-c
  while (tempr>1 & tempc<ncol(oldLayout)){
    tempr<-tempr-1
    tempc<-tempc+1
    if (oldLayout[tempr,tempc]=="#"){
      return(FALSE)
    } 
    if (oldLayout[tempr,tempc]=="L"){
      break
    }
  }
  #NW
  tempr<-r
  tempc<-c
  while (tempr>1 & tempc>1){
    tempr<-tempr-1
    tempc<-tempc-1
    if (oldLayout[tempr,tempc]=="#"){
      return(FALSE)
    }
    if (oldLayout[tempr,tempc]=="L"){
      break
    }
  }
  #SW
  tempr<-r
  tempc<-c
  while (tempr<nrow(oldLayout) & tempc>1){
    tempr<-tempr+1
    tempc<-tempc-1
    if (oldLayout[tempr,tempc]=="#"){
      return(FALSE)
    } 
    if (oldLayout[tempr,tempc]=="L"){
      break
    }
  }
  #SE
  tempr<-r
  tempc<-c
  while (tempr<nrow(oldLayout) & tempc<ncol(oldLayout)){
    tempr<-tempr+1
    tempc<-tempc+1
    if (oldLayout[tempr,tempc]=="#"){
      return(FALSE)
    } 
    if (oldLayout[tempr,tempc]=="L"){
      break
    }
  }
  return(TRUE)
  
}

## Are there 5 or more visibly occupied seats?
fiveOrMoreVisibleOccupied<-function(oldLayout,r,c){
  occupied<-0
  #N
  tempr<-r
  while (tempr>1){
    tempr<-tempr-1
    if (oldLayout[tempr,c]=="#"){
      occupied<-occupied+1
      break
    } 
    if (oldLayout[tempr,c]=="L"){
      break
    }
  }
  #S
  tempr<-r
  while (tempr<nrow(oldLayout)){
    tempr<-tempr+1
    if (oldLayout[tempr,c]=="#"){
      occupied<-occupied+1
      break
    } 
    if (oldLayout[tempr,c]=="L"){
      break
    }
  }
  #W
  tempc<-c
  while (tempc>1){
    tempc<-tempc-1
    if (oldLayout[r,tempc]=="#"){
      occupied<-occupied+1
      break
    } 
    if (oldLayout[r,tempc]=="L"){
      break
    }
  }
  #E
  tempc<-c
  while (tempc<ncol(oldLayout)){
    tempc<-tempc+1
    if (oldLayout[r,tempc]=="#"){
      occupied<-occupied+1
      break
    } 
    if (oldLayout[r,tempc]=="L"){
      break
    }
  }
  #NE
  tempr<-r
  tempc<-c
  while (tempr>1 & tempc<ncol(oldLayout)){
    tempr<-tempr-1
    tempc<-tempc+1
    if (oldLayout[tempr,tempc]=="#"){
      occupied<-occupied+1
      break
    } 
    if (oldLayout[tempr,tempc]=="L"){
      break
    }
  }
  #NW
  tempr<-r
  tempc<-c
  while (tempr>1 & tempc>1){
    tempr<-tempr-1
    tempc<-tempc-1
    if (oldLayout[tempr,tempc]=="#"){
      occupied<-occupied+1
      break
    } 
    if (oldLayout[tempr,tempc]=="L"){
      break
    }
  }
  #SW
  tempr<-r
  tempc<-c
  while (tempr<nrow(oldLayout) & tempc>1){
    tempr<-tempr+1
    tempc<-tempc-1
    if (oldLayout[tempr,tempc]=="#"){
      occupied<-occupied+1
      break
    } 
    if (oldLayout[tempr,tempc]=="L"){
      break
    }
  }
  #SE
  tempr<-r
  tempc<-c
  while (tempr<nrow(oldLayout) & tempc<ncol(oldLayout)){
    tempr<-tempr+1
    tempc<-tempc+1
    if (oldLayout[tempr,tempc]=="#"){
      occupied<-occupied+1
      break
    } 
    if (oldLayout[tempr,tempc]=="L"){
      break
    }
  }
  
  if (occupied>=5){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## Gets the new layout by applying both rules to every seat simultaneously
getNewLayout2<-function(oldLayout){
  newLayout<-oldLayout
  for (r in 1:nrow(oldLayout)){
    for (c in 1:ncol(oldLayout)){
      if(oldLayout[r,c]=="L"){
        if (allAdjacentVisibleEmpty(oldLayout,r,c)){
          newLayout[r,c]<-"#"
        }
      } else if (oldLayout[r,c]=="#"){
        if (fiveOrMoreVisibleOccupied(oldLayout,r,c)){
          newLayout[r,c]<-"L"
        }
      }
    }
  }
  return(newLayout)
}

## Change the seating according to the rules until
## an equalibrium is found
layout<-init_layout
while(TRUE){
  newLayout<-getNewLayout2(layout)
  if(isTRUE(all.equal(layout,newLayout))){
    break
  } else {
    layout<-newLayout
  }
}
## Count the number of occupied seats
answer2<-sum(newLayout=="#")

###################################################################

### Day 12

## Read in data
day12<-read.table("day12.txt",col.names=c("directions"))

## Part 1

facing<-"E"
compass<-c("N","E","S","W")
coord<-c(0,0) ##north,east
for (i in 1:nrow(day12)){
  letter<-substr(day12$directions[i],1,1)
  number<-as.numeric(substr(day12$directions[i],2,nchar(day12$directions[i])))
  if(letter=="E"){
    coord[2]<-coord[2]+number
  } else if(letter=="W"){
    coord[2]<-coord[2]-number
  } else if(letter=="N"){
    coord[1]<-coord[1]+number
  } else if(letter=="S"){
    coord[1]<-coord[1]-number
  } else if(letter=="R"){
    currentComp<-which(compass==facing)
    turn<-number/90
    currentComp<-currentComp+turn
    while (currentComp>4){
      currentComp<-currentComp-4
    }
    facing<-compass[currentComp]
  } else if(letter=="L"){
    currentComp<-which(compass==facing)
    turn<-number/90
    currentComp<-currentComp-turn
    while (currentComp<1){
      currentComp<-currentComp+4
    }
    facing<-compass[currentComp]
  } else if(letter=="F"){
    if(facing=="E"){
      coord[2]<-coord[2]+number
    } else if(facing=="W"){
      coord[2]<-coord[2]-number
    } else if(facing=="N"){
      coord[1]<-coord[1]+number
    } else if(facing=="S"){
      coord[1]<-coord[1]-number
    } 
  }
}

answer1<-abs(coord[1])+abs(coord[2])

## Part 2

coord<-c(0,0) ##north,east
waypoint<-c(1,10) ##north,east
for (i in 1:nrow(day12)){
  letter<-substr(day12$directions[i],1,1)
  number<-as.numeric(substr(day12$directions[i],2,nchar(day12$directions[i])))
  if(letter=="E"){
    waypoint[2]<-waypoint[2]+number
  } else if(letter=="W"){
    waypoint[2]<-waypoint[2]-number
  } else if(letter=="N"){
    waypoint[1]<-waypoint[1]+number
  } else if(letter=="S"){
    waypoint[1]<-waypoint[1]-number
  } else if(letter=="R"){
    turn<-number/90
    for (t in 1:turn){
      oldwaypoint<-waypoint
      waypoint[1]<--oldwaypoint[2]
      waypoint[2]<-oldwaypoint[1]
    }
  } else if(letter=="L"){
    turn<-number/90
    for (t in 1:turn){
      oldwaypoint<-waypoint
      waypoint[1]<-oldwaypoint[2]
      waypoint[2]<--oldwaypoint[1]
    }
  } else if(letter=="F"){
    coord[1]<-coord[1]+number*waypoint[1]
    coord[2]<-coord[2]+number*waypoint[2]
  }
}

answer2<-abs(coord[1])+abs(coord[2])

###################################################################

### Day 13

## Read in data
day13<-read.table("day13.txt",col.names=c("bus"))

time<-as.numeric(day13[1,1])
busses<-unlist(strsplit(day13[2,1],","))

offset<-0:(length(busses)-1)
foffset<-offset[busses!="x"]
fbusses<-as.numeric(busses[busses!="x"])

mintime<-time
minbus<-0
for (bus in fbusses){
  waittime<-bus-time%%bus
  if(waittime<mintime){
    mintime<-waittime
    minbus<-bus
  }
}

answer1<-minbus*mintime

getTime<-function(fbusses,foffset){
  number1<-fbusses[1]  
  offset1<-fbusses[1]-foffset[1]  
  for (busno in 2:length(fbusses)){

    number2<-fbusses[busno]
    offset2<-fbusses[busno]-foffset[busno]
    if (offset2==fbusses[busno]){
      offset2<-0
    }
    while(offset2<0){
      offset2<-offset2+fbusses[busno]
    }
    
    for (i in 1:number2){
      if(((number1*i+offset1)%%number2)==offset2){
        break
      }
    }
    offset1<-number1*i+offset1
    number1<-number1*number2
  }
  return(offset1)
}

answer2<-getTime(fbusses,foffset)

###################################################################

### Day 14

## Apply mask to the number
applyMask<-function(mask,number){
  splitMask<-unlist(strsplit(mask,""))
  binNumber<-convertToBinary(number,36)
  for (i in 1:36){
    if(splitMask[i]!="X"){
      binNumber[i]<-splitMask[i]
    }
  }
  newNumber<-convertFromBinary(binNumber)
  return(newNumber)
}
## convert decimal to binary number
convertToBinary<-function(number,len){
  bin<-rep(0,len)
  i<-1
  while(number>0){
    if (number>=2^(len-i)){
      bin[i]=1
      number<-number-2^(len-i)
    }
    i<-i+1
  }
  return(bin)
}
## convert from binary to decimal
convertFromBinary<-function(binNumber){
  newNumber<-0
  for (i in 1:36){
    newNumber<-newNumber+as.numeric(binNumber[i])*(2^(36-i))
  }
  return(newNumber)
}

## Create mem vector
mem<-rep(0,99999)
## read in file line by line 
day14<-file("day14.txt","r")
while (TRUE){
  line = readLines(day14, n = 1)
  if (length(line) == 0){
    break
  } else if (substring(line,1,4)=="mask") {
    mask<-substring(line,8,nchar(line))
  } else {
    memory<-unlist(strsplit(line," "))
    index<-as.numeric(substring(memory[1],5,nchar(memory[1])-1))
    number<-as.numeric(memory[3])
    newNumber<-applyMask(mask,number)
    mem[index]<-newNumber
  }
}
close(day14)
answer1<-sum(mem)

## Part 2

## Get new indexes after masking
getNewIndex<-function(mask,index){
  binIndex<-convertToBinary(index,36)
  splitMask<-unlist(strsplit(mask,""))
  for (i in 1:36){
    if (splitMask[i]!="0"){
      binIndex[i]<-splitMask[i]
    }
  }
  noX<-sum(binIndex=="X")
  if (noX>0){
    newIndex<-rep(0,noX)
    Xloc<-which(binIndex=="X")
    for (i in 1:(2^noX)){
      tempIndex<-binIndex
      replace<-convertToBinary(i-1,noX)
      tempIndex[Xloc]<-replace
      newIndex[i]<-convertFromBinary(tempIndex)
    }
  } else {
    newIndex<-convertFromBinary(binIndex)
  }
  return(newIndex)
}

## Initialise vector of indexes and corresponding numbers
memind<-0
memnum<-0
##read in file line by line
day14<-file("day14.txt","r")
while (TRUE){
  line = readLines(day14, n = 1)
  if (length(line) == 0){
    break
  } else if (substring(line,1,4)=="mask") {
    mask<-substring(line,8,nchar(line))
  } else {
    memory<-unlist(strsplit(line," "))
    index<-as.numeric(substring(memory[1],5,nchar(memory[1])-1))
    number<-as.numeric(memory[3])
    newIndex<-getNewIndex(mask,index)
    for (ind in 1:length(newIndex)){
      if (sum(memind==newIndex[ind])>0){
        #replace
        memnum[which(memind==newIndex[ind])]<-number
      } else {
        #add
        memind<-c(memind,newIndex[ind])
        memnum<-c(memnum,number)
      }
    }
  }
}
close(day14)
answer2<-sum(memnum)

###################################################################

### Day 15

## Read in data
numbers<-c(9,12,1,4,17,0,18)

while (length(numbers)<2020){
  before<-which(numbers[length(numbers)]==numbers[-length(numbers)])
  if (length(before)==0){
    numbers<-c(numbers,0)
  } else {
    numbers<-c(numbers,length(numbers)-before[length(before)])
  }
}
answer1<-numbers[2020]

## Part 2
numbers<-rep(0,30000000)
numbers[9]<-1
numbers[12]<-2
numbers[1]<-3
numbers[4]<-4
numbers[17]<-5
zero<-6

count<-7
number<-18
while (count<30000000){
  if (number==0){
    number<-count-zero
    zero<-count
  } else {
    if(numbers[number]==0){
      numbers[number]<-count
      number<-0
    } else {
      newnumber<-count-numbers[number]
      numbers[number]<-count
      number<-newnumber
    }
  }
  count<-count+1
}
answer2<-number

###################################################################

### Day 16

## Read in data
tickets<-read.csv("day16.txt",skip=25,header=FALSE)
## Read in data
myTicket<-read.csv("day16.txt",skip=22,nrows=1,header=FALSE)
##
rules<-read.table("day16.txt",nrows=20,sep = ":")

## get just the numbers from the rules
rules_param<-apply(sapply(unlist(strsplit(rules$V2," or ")),function(x)unlist(strsplit(x,"-")),USE.NAMES = FALSE),c(1,2),as.numeric)

# min and max numnber in rules
mini<-min(rules_param[1,])
maxi<-max(rules_param[2,])

# find any numbers that would not be allowed
isOkay<-rep(FALSE,maxi)
for (i in 1:ncol(rules_param)){
  isOkay[rules_param[1,i]:rules_param[2,i]]<-TRUE
}
which(isOkay==FALSE)
## only those smaller than the minimum number and larger than the
## maximum number are not allowed

notallowed<-tickets<mini | tickets>maxi
answer1<-sum(tickets[notallowed])

## Part 2

## filter out the invalid tickets
okayTickets<-apply(notallowed,1,sum)
newTickets<-tickets[which(okayTickets==0),]

## Matrix to say which rule (rows) could be in which position of the ticket (columns)
rulesPos<-matrix(FALSE,nrow=20,ncol=20)

for (i in 1:20){
  ## test rules against each position
  for (j in 1:20){
    if (sum((newTickets[,j]>=rules_param[1,i*2-1]& newTickets[,j]<=rules_param[2,i*2-1] ) | (newTickets[,j]>=rules_param[1,i*2] & newTickets[,j]<=rules_param[2,i*2]))==nrow(newTickets)){
      rulesPos[i,j]<-TRUE
    }
  }
}

## now find which rule goes with each position,
## given they must match one-to-one
uniqRules<-rulesPos
finalRulesPos<-rep(0,20)
for (i in 1:20){
  # find row with only one true
  for (x in 1:20){
    if (sum(uniqRules[x,])==TRUE){
      break
    }
  }
  #write to vector
  finalRulesPos[x]<-which(uniqRules[x,]==TRUE)
  #make column & row FALSE
  uniqRules[x,]<-FALSE
  uniqRules[,finalRulesPos[x]]<-FALSE
}

answer2<-prod(myTicket[finalRulesPos[1:6]])

###################################################################

### Day 17

## read in the initial layout
day17<-read.table("day17.txt",col.names=c("cubes"),comment.char = "/")
init_layout<-sapply(day17$cubes,function(x)unlist(strsplit(x,"")),USE.NAMES = FALSE)

## start with 8x8x1 grid, after the 6th cycle will have:
## 20*20*13
## give self wiggle room 22*22*15

init_array<-array(rep(".",22*22*15),dim=c(22,22,15))
init_array[8:15,8:15,8]<-init_layout

## get the number of active neighbours
countActiveNeighbours<-function(current_array,i,j,k){
  count<-0
  for (xi in (i-1):(i+1)){
    for (xj in (j-1):(j+1)){
      for (xk in (k-1):(k+1)){
        if (!(xi == i & xj == j & xk==k)){
          if (current_array[xi,xj,xk]=="#"){
            count<-count+1
          } 
        }
      }
    }
  }
  return(count)
}

## do cycles
current_array<-init_array
next_array<-current_array
for(cycle in 1:6){
  for(i in 2:21){
    for (j in 2:21){
      for (k in 2:14){
        tempcount<-countActiveNeighbours(current_array,i,j,k)
        if(current_array[i,j,k]=="."){
          if (tempcount==3){
            next_array[i,j,k]<-"#"
          }
        } else {
          if (tempcount != 3 & tempcount != 2){
            next_array[i,j,k]<-"."
          }
        }
      }
    }
  }
  current_array<-next_array
}

answer1<-sum(next_array=="#")

## Part 2

##Initialise array with 4 dimensions
init_array<-array(rep(".",22*22*15*15),dim=c(22,22,15,15))
init_array[8:15,8:15,8,8]<-init_layout

## count active neighbours in 4D
countActiveNeighbours<-function(current_array,i,j,k,l){
  count<-0
  for (xi in (i-1):(i+1)){
    for (xj in (j-1):(j+1)){
      for (xk in (k-1):(k+1)){
        for (xl in (l-1):(l+1)){
          if (!(xi == i & xj == j & xk==k & xl ==l)){
            if (current_array[xi,xj,xk,xl]=="#"){
              count<-count+1
            } 
          }
        }
      }
    }
  }
  return(count)
}

## do cycles
current_array<-init_array
next_array<-current_array
for(cycle in 1:6){
  for(i in 2:21){
    for (j in 2:21){
      for (k in 2:14){
        for (l in 2:14){
          tempcount<-countActiveNeighbours(current_array,i,j,k,l)
          if(current_array[i,j,k,l]=="."){
            if (tempcount==3){
              next_array[i,j,k,l]<-"#"
            }
          } else {
            if (tempcount != 3 & tempcount != 2){
              next_array[i,j,k,l]<-"."
            }
          }
        }
      }
    }
  }
  current_array<-next_array
}

answer2<-sum(next_array=="#")

###################################################################

### Day 18

## read in the initial layout
day18<-read.table("day18.txt",col.names=c("equations"),sep="#")

evaluateEquation<-function(equ){
  equ<-gsub(" ","",equ)
  total<-0
  operator<-"Add"
  i<-1
  while(i <= nchar(equ)){
    if(substring(equ,i,i)=="+"){
      operator<-"Add"
    } else if (substring(equ,i,i)=="*"){
      operator<-"Multi"
    } else if (substring(equ,i,i)=="("){
      values<-evaluateEquation(substring(equ,i+1,nchar(equ)))
      number<-values[1]
      if(operator=="Add"){
        total<-total+number
      } else {
        total<-total*number
      }
      i<-i+values[2]
    } else if (substring(equ,i,i)==")"){
      return(c(total,i))
    } else {
      number<-as.numeric(substring(equ,i,i))
      if(operator=="Add"){
        total<-total+number
      } else {
        total<-total*number
      }
    }
    i<-i+1
  }
  return(c(total,i))
}

answers<-unname(sapply(day18$equations,function(x)evaluateEquation(x)[1]))
answer1<-sum(answers)

## part 2

## brackets first

solveEqu<-function(equation){
  equation<-gsub(" ","",equation)
  while(is.na(as.numeric(equation))){
    ##bracket loop
    bracket<-regexpr("[(]{1}[0-9+*]+[)]{1}",equation)
    if (bracket>0){
      equ<-substring(equation,bracket+1,bracket+attr(bracket,"match.length")-2)
      
      ## all the x + y
      plus<-regexpr("[0-9]+[+]{1}[0-9]+",equ)
      while (plus>0){
        sum<-substring(equ,plus,plus+attr(plus,"match.length")-1)
        number<-as.numeric(substring(sum,1,regexpr("[+]",sum)-1))+as.numeric(substring(sum,regexpr("[+]",sum)+1,nchar(sum)))
        equ<-paste0(substring(equ,1,plus-1),number,substring(equ,plus+attr(plus,"match.length"),nchar(equ)))
        plus<-regexpr("[0-9]+[+]{1}[0-9]+",equ)
      }
      
      ## all the x * y
      multi<-regexpr("[0-9]+[*]{1}[0-9]+",equ)
      while (multi>0){
        produ<-substring(equ,multi,multi+attr(multi,"match.length")-1)
        number<-as.numeric(substring(produ,1,regexpr("[*]",produ)-1))*as.numeric(substring(produ,regexpr("[*]",produ)+1,nchar(produ)))
        equ<-paste0(substring(equ,1,multi-1),number,substring(equ,multi+attr(multi,"match.length"),nchar(equ)))
        multi<-regexpr("[0-9]+[*]{1}[0-9]+",equ)
      }
      equation<-paste0(substring(equation,1,bracket-1),equ,substring(equation,bracket+attr(bracket,"match.length"),nchar(equation)))
    } else {
      equ<-equation
      ## all the x + y
      plus<-regexpr("[0-9]+[+]{1}[0-9]+",equ)
      while (plus>0){
        sum<-substring(equ,plus,plus+attr(plus,"match.length")-1)
        number<-as.numeric(substring(sum,1,regexpr("[+]",sum)-1))+as.numeric(substring(sum,regexpr("[+]",sum)+1,nchar(sum)))
        equ<-paste0(substring(equ,1,plus-1),number,substring(equ,plus+attr(plus,"match.length"),nchar(equ)))
        plus<-regexpr("[0-9]+[+]{1}[0-9]+",equ)
      }
      
      ## all the x * y
      multi<-regexpr("[0-9]+[*]{1}[0-9]+",equ)
      while (multi>0){
        produ<-substring(equ,multi,multi+attr(multi,"match.length")-1)
        number<-as.numeric(substring(produ,1,regexpr("[*]",produ)-1))*as.numeric(substring(produ,regexpr("[*]",produ)+1,nchar(produ)))
        equ<-paste0(substring(equ,1,multi-1),number,substring(equ,multi+attr(multi,"match.length"),nchar(equ)))
        multi<-regexpr("[0-9]+[*]{1}[0-9]+",equ)
      }
      equation<-equ
    }
  }
  return(as.numeric(equation))
}
answers<-unname(sapply(day18$equations,solveEqu))
answer2<-sum(answers)

###################################################################

### Day 19 INCOMPLETE

## read in the data
rules<-read.table("day19.txt",col.names=c("number","rule"),sep=":",nrows=135)
messages<-read.table("day19.txt",col.names=c("messages"),skip=136)

rules<-rules[order(rules$number),]
rule0<-rules[1,2]
rule0<-unlist(strsplit(rule0," "))
rule0<-rule0[-1]
rules<-rules[-1,]

rules_split<-sapply(rules$rule,function(x)unlist(strsplit(x," ")),USE.NAMES = FALSE)
rules_split<-lapply(rules_split,function(x)x[-1])


rules_split[[6]]
rule1<-grep('[ab]',rules$rule)

message<-unlist(strsplit(messages$messages[1],""))

checkMessage<-function(message){
  for (i in 1:length(rule0)){
    rule0[i]
  }
}

###################################################################

### Day 20

## read tiles into a list of matrices
day20<-file("day20.txt","r")
tiles<-list()
while (TRUE){
  line = readLines(day20, n = 1)
  if (length(line) == 0){
    break
  } else if (line==""){
    tiles[[tileNo]]<-tileLayout
  } else if (substring(line,1,4)=="Tile") {
    tileNo<-substring(line,6,9)
    tileLayout<-matrix(".",10,10)
    tileline<-1
  } else {
    tileLayout[tileline,]<-unlist(strsplit(line,""))
    tileline<-tileline+1
  }
}
close(day20)

## Find the number of unique sides a tiles has
countOfUniqueSides<-function(tileIndex){
  side1<-tiles[[tileIndex]][1,] #top
  side2<- tiles[[tileIndex]][,10] #right
  side3<-tiles[[tileIndex]][10,10:1]  #bottom
  side4<-tiles[[tileIndex]][10:1,1] #left
  
  uniqueSides<-0
  thisUnique<-TRUE
  for (i in 1:144){
    if (i!=tileIndex){
      if (isTRUE(all.equal(side1,tiles[[i]][1,10:1])) | isTRUE(all.equal(side1,tiles[[i]][10:1,10])) | isTRUE(all.equal(side1,tiles[[i]][10,])) | isTRUE(all.equal(side1,tiles[[i]][,1]))
           | isTRUE(all.equal(side1,tiles[[i]][1,1:10])) | isTRUE(all.equal(side1,tiles[[i]][1:10,10])) | isTRUE(all.equal(side1,tiles[[i]][10,10:1])) | isTRUE(all.equal(side1,tiles[[i]][10:1,1]))){
        thisUnique<-FALSE
        break
      }
    }
  }
  if (thisUnique){
    uniqueSides<-uniqueSides+1
  }

  thisUnique<-TRUE
  for (i in 1:144){
    if (i!=tileIndex){
      if (isTRUE(all.equal(side2,tiles[[i]][1,10:1])) | isTRUE(all.equal(side2,tiles[[i]][10:1,10])) | isTRUE(all.equal(side2,tiles[[i]][10,])) | isTRUE(all.equal(side2,tiles[[i]][,1]))
        | isTRUE(all.equal(side2,tiles[[i]][1,1:10])) | isTRUE(all.equal(side2,tiles[[i]][1:10,10])) | isTRUE(all.equal(side2,tiles[[i]][10,10:1])) | isTRUE(all.equal(side2,tiles[[i]][10:1,1]))){        
        thisUnique<-FALSE
        break
      }
    }
  }
  if (thisUnique){
    uniqueSides<-uniqueSides+1
  }
  thisUnique<-TRUE
  for (i in 1:144){
    if (i!=tileIndex){
      if (isTRUE(all.equal(side3,tiles[[i]][1,10:1])) | isTRUE(all.equal(side3,tiles[[i]][10:1,10])) | isTRUE(all.equal(side3,tiles[[i]][10,])) | isTRUE(all.equal(side3,tiles[[i]][,1]))
        | isTRUE(all.equal(side3,tiles[[i]][1,1:10])) | isTRUE(all.equal(side3,tiles[[i]][1:10,10])) | isTRUE(all.equal(side3,tiles[[i]][10,10:1])) | isTRUE(all.equal(side3,tiles[[i]][10:1,1]))){
        thisUnique<-FALSE
        break
      }
    }
  }
  if (thisUnique){
    uniqueSides<-uniqueSides+1
  }
  thisUnique<-TRUE
  for (i in 1:144){
    if (i!=tileIndex){
      if (isTRUE(all.equal(side4,tiles[[i]][1,10:1])) | isTRUE(all.equal(side4,tiles[[i]][10:1,10])) | isTRUE(all.equal(side4,tiles[[i]][10,])) | isTRUE(all.equal(side4,tiles[[i]][,1]))
        | isTRUE(all.equal(side4,tiles[[i]][1,1:10])) | isTRUE(all.equal(side4,tiles[[i]][1:10,10])) | isTRUE(all.equal(side4,tiles[[i]][10,10:1])) | isTRUE(all.equal(side4,tiles[[i]][10:1,1]))){
        thisUnique<-FALSE
        break
      }
    }
  }
  if (thisUnique){
    uniqueSides<-uniqueSides+1
  }
  
  return(uniqueSides)
}

## find the number of unique sides for each tile
uniqueSides<-rep(0,144)
for (i in 1:144){
  uniqueSides[i]<-countOfUniqueSides(i)
}
## find the products of the corners (tiles with 2 unique sides)
answer1<-prod(as.numeric(names(tiles)[which(uniqueSides==2)]))

## Part 2

## rotate a tile
rotateTile<-function(tile){
  dims<-dim(tile) #assume square
  newTile<-matrix(".",dims[1],dims[1])
  for (i in 1:dims[1]){
    for (j in 1:dims[1]){
      newTile[j,dims[1]+1-i]<-tile[i,j]
    }
  }
  return(newTile)
}
## flip a tile
flipTileHoriz<-function(tile){
  dims<-dim(tile) #assume square
  newTile<-matrix(".",dims[1],dims[1])
  for (i in 1:dims[1]){
    for (j in 1:dims[1]){
      newTile[i,dims[1]+1-j]<-tile[i,j]
    }
  }
  return(newTile)
}
## find which of the sides of a tile are unique
whichUniqueSides<-function(tile,tileIndex){
  side<-c(0,0,0,0) #top, right, bottom, left
  side1<-tile[1,]
  side2<- tile[,10]
  side3<-tile[10,10:1]  
  side4<-tile[10:1,1]
  
  thisUnique<-TRUE
  for (i in 1:144){
    if (i!=tileIndex){
      if (isTRUE(all.equal(side1,tiles[[i]][1,10:1])) | isTRUE(all.equal(side1,tiles[[i]][10:1,10])) | isTRUE(all.equal(side1,tiles[[i]][10,])) | isTRUE(all.equal(side1,tiles[[i]][,1]))
          | isTRUE(all.equal(side1,tiles[[i]][1,1:10])) | isTRUE(all.equal(side1,tiles[[i]][1:10,10])) | isTRUE(all.equal(side1,tiles[[i]][10,10:1])) | isTRUE(all.equal(side1,tiles[[i]][10:1,1]))){
        thisUnique<-FALSE
        break
      }
    }
  }
  if (thisUnique){
    side[1]<-1
  }
  
  thisUnique<-TRUE
  for (i in 1:144){
    if (i!=tileIndex){
      if (isTRUE(all.equal(side2,tiles[[i]][1,10:1])) | isTRUE(all.equal(side2,tiles[[i]][10:1,10])) | isTRUE(all.equal(side2,tiles[[i]][10,])) | isTRUE(all.equal(side2,tiles[[i]][,1]))
          | isTRUE(all.equal(side2,tiles[[i]][1,1:10])) | isTRUE(all.equal(side2,tiles[[i]][1:10,10])) | isTRUE(all.equal(side2,tiles[[i]][10,10:1])) | isTRUE(all.equal(side2,tiles[[i]][10:1,1]))){        
        thisUnique<-FALSE
        break
      }
    }
  }
  if (thisUnique){
    side[2]<-1
  }
  thisUnique<-TRUE
  for (i in 1:144){
    if (i!=tileIndex){
      if (isTRUE(all.equal(side3,tiles[[i]][1,10:1])) | isTRUE(all.equal(side3,tiles[[i]][10:1,10])) | isTRUE(all.equal(side3,tiles[[i]][10,])) | isTRUE(all.equal(side3,tiles[[i]][,1]))
          | isTRUE(all.equal(side3,tiles[[i]][1,1:10])) | isTRUE(all.equal(side3,tiles[[i]][1:10,10])) | isTRUE(all.equal(side3,tiles[[i]][10,10:1])) | isTRUE(all.equal(side3,tiles[[i]][10:1,1]))){
        thisUnique<-FALSE
        break
      }
    }
  }
  if (thisUnique){
    side[3]<-1
  }
  thisUnique<-TRUE
  for (i in 1:144){
    if (i!=tileIndex){
      if (isTRUE(all.equal(side4,tiles[[i]][1,10:1])) | isTRUE(all.equal(side4,tiles[[i]][10:1,10])) | isTRUE(all.equal(side4,tiles[[i]][10,])) | isTRUE(all.equal(side4,tiles[[i]][,1]))
          | isTRUE(all.equal(side4,tiles[[i]][1,1:10])) | isTRUE(all.equal(side4,tiles[[i]][1:10,10])) | isTRUE(all.equal(side4,tiles[[i]][10,10:1])) | isTRUE(all.equal(side4,tiles[[i]][10:1,1]))){
        thisUnique<-FALSE
        break
      }
    }
  }
  if (thisUnique){
    side[4]<-1
  }
  
  return(side)
}
## Test if a tile matches an already placed puzzle piece, and that
## its unique side touches an edge
isMatch<-function(tileIndex,trows,tcols,puzzleVec,uniqueSides=NULL){
  tempTile<-tiles[[tileIndex]]
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec))){
    if (is.null(uniqueSides)){
      return(tempTile)
    } else {
      if(isTRUE(all.equal(whichUniqueSides(tempTile,tileIndex),uniqueSides))){
        return(tempTile)
      }
    }
  }    
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec))){
    if (is.null(uniqueSides)){
      return(tempTile)
    } else {
      if(isTRUE(all.equal(whichUniqueSides(tempTile,tileIndex),uniqueSides))){
        return(tempTile)
      }
    }
  }   
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec))){
    if (is.null(uniqueSides)){
      return(tempTile)
    } else {
      if(isTRUE(all.equal(whichUniqueSides(tempTile,tileIndex),uniqueSides))){
        return(tempTile)
      }
    }
  }   
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec))){
    if (is.null(uniqueSides)){
      return(tempTile)
    } else {
      if(isTRUE(all.equal(whichUniqueSides(tempTile,tileIndex),uniqueSides))){
        return(tempTile)
      }
    }
  }   
  tempTile<-flipTileHoriz(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec))){
    if (is.null(uniqueSides)){
      return(tempTile)
    } else {
      if(isTRUE(all.equal(whichUniqueSides(tempTile,tileIndex),uniqueSides))){
        return(tempTile)
      }
    }
  } 
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec))){
    if (is.null(uniqueSides)){
      return(tempTile)
    } else {
      if(isTRUE(all.equal(whichUniqueSides(tempTile,tileIndex),uniqueSides))){
        return(tempTile)
      }
    }
  }   
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec))){
    if (is.null(uniqueSides)){
      return(tempTile)
    } else {
      if(isTRUE(all.equal(whichUniqueSides(tempTile,tileIndex),uniqueSides))){
        return(tempTile)
      }
    }
  }   
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec))){
    if (is.null(uniqueSides)){
      return(tempTile)
    } else {
      if(isTRUE(all.equal(whichUniqueSides(tempTile,tileIndex),uniqueSides))){
        return(tempTile)
      }
    }
  }   
  return(FALSE)
}

## Test if a tile matches two already placed puzzle pieces
isDoubleMatch<-function(tileIndex,trows,tcols,puzzleVec,trows2,tcols2,puzzleVec2){
  tempTile<-tiles[[tileIndex]]
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec)) & isTRUE(all.equal(tempTile[trows2,tcols2],puzzleVec2))){
    return(tempTile)
  }    
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec)) & isTRUE(all.equal(tempTile[trows2,tcols2],puzzleVec2))){
    return(tempTile)
  }   
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec)) & isTRUE(all.equal(tempTile[trows2,tcols2],puzzleVec2))){
    return(tempTile)
  }   
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec)) & isTRUE(all.equal(tempTile[trows2,tcols2],puzzleVec2))){
    return(tempTile)
  }   
  tempTile<-flipTileHoriz(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec)) & isTRUE(all.equal(tempTile[trows2,tcols2],puzzleVec2))){
    return(tempTile)
  } 
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec)) & isTRUE(all.equal(tempTile[trows2,tcols2],puzzleVec2))){
    return(tempTile)
  }   
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec)) & isTRUE(all.equal(tempTile[trows2,tcols2],puzzleVec2))){
    return(tempTile)
  }   
  tempTile<-rotateTile(tempTile)
  if(isTRUE(all.equal(tempTile[trows,tcols],puzzleVec)) & isTRUE(all.equal(tempTile[trows2,tcols2],puzzleVec2))){
    return(tempTile)
  }   
  return(FALSE)
}

## put the puzzle together
corners<-which(uniqueSides==2)
edges<-which(uniqueSides==1)
middles<-which(uniqueSides==0)

## Make puzzle board
puzzle<-matrix(0,12,12)

#fit [1,1]
while(isTRUE(all.equal(whichUniqueSides(tiles[[corners[1]]],corners[1]),c(1,0,0,1)))==FALSE){
  tiles[[corners[1]]]<-rotateTile(tiles[[corners[1]]])
}
puzzle[1,1]<-corners[1]
#fit [2:11,1]
for(p in 2:11){
  i<-1
  while(i<=length(edges)){
    tempTile<-isMatch(edges[i],1,c(1:10),tiles[[puzzle[p-1,1]]][10,],c(0,0,0,1)) 
    if(is.matrix(tempTile)
       & sum(puzzle==edges[i])==0){
      tiles[[edges[i]]]<-tempTile
      break
    }
    i<-i+1
  }
  puzzle[p,1]<-edges[i]
}
#fit [12,1]
i<-2
while(i<=length(corners)){
  tempTile<-isMatch(corners[i],1,c(1:10),tiles[[puzzle[11,1]]][10,],c(0,0,1,1)) 
  if(is.matrix(tempTile)){
    tiles[[corners[i]]]<-tempTile
    break
  }
  i<-i+1
}
puzzle[12,1]<-corners[i]
#fit [1,2:11]
for(p in 2:11){
  i<-1
  while(i<=length(edges)){
    tempTile<-isMatch(edges[i],c(1:10),1,tiles[[puzzle[1,p-1]]][,10],c(1,0,0,0))
    if(is.matrix(tempTile)
       & sum(puzzle==edges[i])==0){
      tiles[[edges[i]]]<-tempTile
      break
    }
    i<-i+1
  }
  tiles[[edges[i]]]<-tempTile
  puzzle[1,p]<-edges[i]
}
#fit [1,12]
i<-2
while(i<=length(corners)){
  tempTile<-isMatch(corners[i],1:10,1,tiles[[puzzle[1,11]]][,10],c(1,1,0,0)) 
  if(is.matrix(tempTile)
     & sum(puzzle==corners[i])==0){
    tiles[[corners[i]]]<-tempTile
    break
    
  }
  i<-i+1
}
puzzle[1,12]<-corners[i]
#fit [12,2:11]
for(p in 2:11){
  i<-1
  while(i<=length(edges)){
    tempTile<-isMatch(edges[i],c(1:10),1,tiles[[puzzle[12,p-1]]][,10],c(0,0,1,0))
    if(is.matrix(tempTile)
       & sum(puzzle==edges[i])==0){
      tiles[[edges[i]]]<-tempTile
      break
    }
    i<-i+1
  }
  tiles[[edges[i]]]<-tempTile
  puzzle[12,p]<-edges[i]
}
#fit [12,12]
i<-2
while(i<=length(corners)){
  tempTile<-isMatch(corners[i],1:10,1,tiles[[puzzle[12,11]]][,10],c(0,1,1,0)) 
  if(is.matrix(tempTile)
     & sum(puzzle==corners[i])==0){
    tiles[[corners[i]]]<-tempTile
    break
  }
  i<-i+1
}
puzzle[12,12]<-corners[i]
#fit [2:11,12]
for(p in 2:11){
  i<-1
  while(i<=length(edges)){
    tempTile<-isMatch(edges[i],1,c(1:10),tiles[[puzzle[p-1,12]]][10,],c(0,1,0,0)) 
    if(is.matrix(tempTile)
       & sum(puzzle==edges[i])==0){
      tiles[[edges[i]]]<-tempTile
      break
    }
    i<-i+1
  }
  tiles[[edges[i]]]<-tempTile
  puzzle[p,12]<-edges[i]
}
## fit centre pieces
for(p in 2:11){
  for(q in 2:11){
    i<-1
    while(i<=length(middles)){
      tempTile<-isDoubleMatch(middles[i],1,c(1:10),tiles[[puzzle[p-1,q]]][10,],1:10,1,tiles[[puzzle[p,q-1]]][,10]) 
      if(is.matrix(tempTile)
         & sum(puzzle==middles[i])==0){
        tiles[[middles[i]]]<-tempTile
        break
      }
      i<-i+1
    }
    tiles[[middles[i]]]<-tempTile
    puzzle[p,q]<-middles[i]
  }
}

## Create Final Image
finalImage<-matrix(".",96,96)
for (i in 1:12){
  for (j in 1:12){
    finalImage[(i*8-7):(i*8),(j*8-7):(j*8)]<-tiles[[puzzle[i,j]]][2:9,2:9]
  }
}

# count Sea Monsters
#   .#...#.###...#.##.O#
#   O.##.OO#.#.OO.##.OOO
#   #O.#O#.O##O..O.#O##.

#returns TRUE if there is a sea monster in this 3x20 layout
seaMonster<-function(layout){
  if(layout[2,1]=="#" &
     layout[3,2]=="#" &
     layout[3,5]=="#" &
     layout[2,6]=="#" &
     layout[2,7]=="#" &
     layout[3,8]=="#" &
     layout[3,11]=="#" &
     layout[2,12]=="#" &
     layout[2,13]=="#" &
     layout[3,14]=="#" &
     layout[3,17]=="#" &
     layout[2,18]=="#" &
     layout[2,19]=="#" &
     layout[2,20]=="#" &
     layout[1,19]=="#"){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
## counts Sea Monsters in image
countSeaMonsters<-function(finalImage){
  count<-0
  for(i in 1:94){
    for(j in 1:77){
      if (seaMonster(finalImage[i:(i+2),j:(j+19)])){
        count<-count+1
      }
    }
  }
  return(count)
}
## find orientation of image that contains sea monsters
noSeaMonstersInImage<-function(finalImage){
  monsters<-countSeaMonsters(finalImage)
  if (monsters>0){
    return(monsters)
  }
  finalImage<-rotateTile(finalImage)
  monsters<-countSeaMonsters(finalImage)
  if (monsters>0){
    return(monsters)
  }
  finalImage<-rotateTile(finalImage)
  monsters<-countSeaMonsters(finalImage)
  if (monsters>0){
    return(monsters)
  }
  finalImage<-rotateTile(finalImage)
  if (countSeaMonsters(finalImage)>0){
    break
  }
  finalImage<-flipTileHoriz(finalImage)
  monsters<-countSeaMonsters(finalImage)
  if (monsters>0){
    return(monsters)
  }
  finalImage<-rotateTile(finalImage)
  monsters<-countSeaMonsters(finalImage)
  if (monsters>0){
    return(monsters)
  }
  finalImage<-rotateTile(finalImage)
  monsters<-countSeaMonsters(finalImage)
  if (monsters>0){
    return(monsters)
  }
  finalImage<-rotateTile(finalImage)
  monsters<-countSeaMonsters(finalImage)
  if (monsters>0){
    return(monsters)
  }
}

answer2<-sum(finalImage=="#")-noSeaMonstersInImage(finalImage)*15
