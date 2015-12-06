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



naReport = function(inputData,
                    range = c(0, 0.4)
                    ){
    if(missing(inputData)){
        stop("You should input a data.frame.")
    }
    ## Check data class, convert to data.frame
    datName = as.character(substitute(inputData))
    inputData = dataFrameCheck(inputData)

    ## Check range
    vectorCheck(range,"numeric",2,"Range")

    if(max(range) > 1 | min(range) < 0){
      warning("Wrong range input!")
      range = c(0, 0.4)
    }

    nas = sapply(inputData, function(x) sum(is.na(x)))
    naPor = nas / dim(inputData)[1]
    output = list()


    if(all(naPor <= range[1])){
      output$noNA = ("There's no NA in any variables!")
    }

    if(any(naPor >= range[2])){
      output$Delete = list(
        sprintf("There are more than %s%% NAs in these variables:",
          round(range[2]*100)),
        paste0(names(inputData)[which(naPor >= range[2])],
          collapse=", "),
        sprintf("Use the following codes to delete them:
nas = sapply(%s, function(x) sum(is.na(x)))
naPor = nas / dim(%s)[1]
%s_new = %s[,-which(naPor >= %s)]",
                              datName,
                              datName,
                              datName,
                              datName,
                              range[2]))
    }

    if(any(naPor > range[1] & naPor<range[2])){
      output$Replace = list(
          sprintf("These variables including  %s%% to %s%% NAs:",
              round(range[1]*100),round(range[2]*100)),
           paste0(names(inputData)[which(naPor > range[1] & naPor<range[2])],
              collapse=", "),
           "Use the following codes to remove the observations:",
          sprintf("%s_new = na.omit(%s_new)", datName, datName)
#            sprintf("
# nas = sapply(%s, function(x) sum(is.na(x)))
# naPor = nas / dim(%s)[1]
# %s_new[,-which(naPor > %s & naPor<%s)] = -1",
#               datName,
#               datName,
#               datName,
#               datName,
#               range[1],
#               range[2])
          )
    }

    return(output)

}


naProcess = function(inputData,
                      range = c(0,0.4)){



  nas = sapply(inputData, function(x) sum(is.na(x)))
  naPor = nas / dim(inputData)[1]



  if(all(naPor <= range[1])){
    return(inputData)
  }

  if(any(naPor >= range[2])){
    inputData_new = inputData[,-which(naPor >= range[2])]
  }else{
    inputData_new = inputData
  }

  if(any(naPor > range[1] & naPor<range[2])){
    inputData_new = na.omit(inputData_new)
  }

  return(inputData_new)
}
