##' Create a uniqueReport of a data.frame
##'
##' naReport will detect the NAs i around the variables in the
##' dataframe and show the codes users may need for omit.
##'
##' @usage
##' uniqueReport (inputData,range = c(0, 0.4))
##'
##'
##' @param inputData   The data frame to create report before modeling.
##' @param range   A vector of length 2 for the range of NA proportion.
##'  Variables with NA proportion less than range[1] will do nothing,
##'  Variables with NA proportion more than range[2] will be suggested to delete.
##'  Others will be suggested to remove the observations.
##' @examples
##' naReport(testData)




uniqueReport =  function(inputData, naRange = c(0, 0.4)){
    if(missing(inputData)){
        stop("You should input a data.frame.")
    }
    ## Check data class, convert to data.frame
    datName = as.character(substitute(inputData))
    inputData = dataFrameCheck(inputData)

    n = dim(inputData)[1]
    uniqN = dim(unique(inputData))[1]

    output = list()
    inputData_new = naProcess(inputData, naRange)

    n1 = dim(inputData_new)[1]
    uniqN1 = dim(unique(inputData_new))[1]

    if(n == uniqN){
        # unique[[1]]
        # "There's no repeat data in original data frame."
      output$unique = list("There's no repeat data in original data frame.")
    }else{
        output$unique = list(
        #  unique[[2]]
        #  "There are %s%%(%s/%s) repeat observations in the original data frame"
                sprintf("There are %s%%(%s/%s) repeat observations in the original data frame",
                    round((1 -  uniqN / n) * 100),
                    n - uniqN,
                    n),
        # unique[[3]]
        # "Use the following codes to replace them:"
                "Use the following codes to replace them:",
                sprintf("%s_uniq = unique(%s)", datName, datName)
                )
    }

    if(n1 == uniqN1){
        # unique[[4]]
        # "There's no repeat data in the data frame after removing NA variables."
      output$unique2 = list("There's no repeat data in the data frame after removing NA variables.")
    }else{
        output$unique2 = list(
        # unique[[5]]
        # "There are %s%%(%s/%s) repeat observations in the data frame after removing NA variables."
            sprintf("There are %s%%(%s/%s) repeat observations in the data frame after removing NA variables.",
                round((1 - uniqN1 / n1) * 100),
                n1 - uniqN1,
                n1),
        # unique[[6]]
        # "Use the following codes to replace them:"
            "Use the following codes to replace them:",
            sprintf("%s_new = naProcess(%s)/n%s_new = unique(%s_new)",
             datName,
             datName,
             datName,
             datName)
            )
    }

    return(output)

}
