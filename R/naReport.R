



naReport = function(inputData,
                    # input data
                    range = c(0,0.2,0.5)
                    ){
    if(missing(inputData)){
        stop("You should input a data.fame.")
    }
    ## Check data class, convert to data.frame
    inputData = dataFrameCheck(inputData)

    ## Check range
    range = vectorCheck(range)




}