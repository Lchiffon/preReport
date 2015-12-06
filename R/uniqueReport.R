##' Create a naReport of a data.frame
##'
##' naReport will detect the NAs i around the variables in the
##' dataframe and show the codes users may need for omit.
##'
##' @usage
##' naReport(inputData,range = c(0, 0.4))
##'
##'
##' @param inputData   The data frame to create report before modeling.
##' @param range   A vector of length 2 for the range of NA proportion.
##'  Variables with NA proportion less than range[1] will do nothing,
##'  Variables with NA proportion more than range[2] will be suggested to delete.
##'  Others will be suggested to remove the observations.
##' @author Chiffon <\url{http://chiffon.gitcafe.io}>
##' @examples
##' naReport(testData)




uniqueReport =  function(inputData){
    if(missing(inputData)){
        stop("You should input a data.frame.")
    }
    ## Check data class, convert to data.frame
    datName = as.character(substitute(inputData))
    inputData = dataFrameCheck(inputData)

    n = dim(inputData)[1]
    uniqN = dim(unique(inputData))[1]

    output = list()
    inputData_new = naProcess(inputData)

    n1 = dim(inputData)[1]
    uniqN1 = dim(unique(inputData))[1]

    if(n == uniqN){
      output$unique = "There's no repeat data in original data frame."
    }

    if(n1 == uniqN1){
      output$unique2 = "There's no repeat data in the data frame after removing NAs."
    }


}
