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

featureReport = function(vector, vecName, ...){

    if(!is.vector(vector)){
        stop("Input should be a vector!")
    }

    if(missing(vecName)){
        vecName = "Current Variable"
    }

    UseMethod(vector, vecName, ...)

}


# numeric
featureReport.numeric = function(vector,vecName){
    output = list()

    if(length(unique(vector))<10){
        output$table = list(
            table(vector)
            )
    }else{
        output$table = list(
            table(cut(vector,quantile(vector,c(0,0.2,0.4,0.6,0.8,1))))
            )
    }

    summary(vector)


    output$
}


new1 = function(a) UseMethod(a)

setMethod("new1","numeric", featureReport.numeric )
setMethod("new1","character", featureReport.character)
setMethod("new1","factor", featureReport.factor)
setMethod("new1","Date", featureReport.Date)



library(ggplot2)
a = rnorm(1000)
qplot(a,fill = factor(sample(1:5,1000,replace=T)),geom="histogram",y=..density..) + scale_fill_grey()+
    geom_density()



m <- ggplot2::ggplot(data.frame(a),ggplot2::aes(x=a)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..),color="white",fill="#1F78B4") +
    ggplot2::geom_density(fill=NA, colour="black",size=1)
