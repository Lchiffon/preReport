
## Check the class of inputData
dataFrameCheck = function(inputData){

    ## Support Data frame, tbl(from dplyr) and data.table
    if(any(class(inputData) == "data.frame")){
        inputData = as.data.frame(inputData)
    }else{
        stop("inputData is not a data frame!")
    }

    if(dim(inputData)[1] < 2){
        warning("Ther is only one row in the data frame! ")
    }

    return(inputData)
}

## Test
# dataFrameCheck(iris)



## Check the vector

vectorCheck = function(vector,
                       class = "numeric",
                       lengthVector,
                       name = "It"){

    if(!is.vector(vector)){
        stop(sprintf("%s is not a vector!",name))
    }

    if(missing(lengthVector)){
        lengthVector = length(vector)
    }else if(length(vector) != lengthVector){
        warning(sprintf("The length of %s is not correct!",name))
    }

    if(class(vector) != class){
        stop(sprintf("%s is not a %s object!",name,class))
    }

}

## Test
# vectorCheck(iris,name = "Range")
#
# vectorCheck(1:10/10,lengthVector = 3,name = "Range")

# Getname

returnName = function(a){
  cc = as.character(substitute(a))
  cc
}
# all.equal("iris", returnName(iris))

naNum = function(vec){
    sum(is.na(vec))
}
# naNum(rep(c(NA,1),5))

tableProb = function(vec){
    Count = table(vec, useNA="always")
    Prob = round(Count / length(vec) * 100)
    Prob = paste0(Prob, "%")
    output = as.data.frame(rbind(Count, Prob))
    output
}

CountLines = function(){
    a = paste0("R/",dir("R"))
    n = 0
    for (file in a){
        n = n + length(readLines(file))
    }
    n
}


copy_dir <- function(from, to){
  if (!(file.exists(to))){
    dir.create(to, recursive = TRUE)
    message('Copying files to ', to, '...')
    file.copy(list.files(from, full.names = T), to, recursive = TRUE)
  }
}

endWithID = function(vecName){
  n = nchar(vecName)
  judge = substr(vecName,n-1,n)
  "id" == tolower(judge)
}
