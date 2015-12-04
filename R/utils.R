
## Check the class of inputData
dataFrameCheck = function(inputData){

    ## Support Data frame, tbl(from dplyr) and data.table
    if(any(class(inputData) == "data.frame")){
        inputData = as.data.frame(inputData)
    }else{
        stop("inputData is not a data frame!")
    }

    if(dim(inputData) < 2){
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
