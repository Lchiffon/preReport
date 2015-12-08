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
##' @examples
##' naReport(testData)

featureReport = function(vector, vecName){

    if(!is.vector(vector)){
        stop("Input should be a vector!")
    }
    feaClass = class(vector)
    feaLength = length(vector)

    if(missing(vecName)){
        vecName = "Current Variable"
    }

    if(is.numeric(vector)){
        output = featureReportNum(vector)
    }

    if(is.character(vector)){
        output = featureReportChar(vector)
    }

    if(is.factor(vector)){
        output = featureReportChar(vector)
    }

}
