##' Create a Features Report of a Feature in the Data.frame
##'
##' featureReport will create a list of information of the feature
##'
##'
##' @usage
##' featureReport(vector, vecName, ...)
##'
##'
##' @param vector  A vector.
##' @param vecName The Name of the vector.
##' @examples
##' featureReport(iris[,1])

featureReport = function(vector, vecName, ...){

    if(!is.vector(vector)){
        stop("Input should be a vector!")
    }

    if(missing(vecName)){
        vecName = "Current Variable"
    }

    UseMethod("featureReport")

}


# numeric
featureReport.numeric = function(vector, vecName){
    if(missing(vecName)){
        vecName = "Current Variable"
    }

    # Initialize the output object
    output = list()

    if(endWithID(vecName)){
        output = featureReport.character(as.character(vector), vecName)
        output$Info = list("This variable may be a character variable!")
        return(output)
    }

    if(length(unique(vector))<10){
        output$table = list(
            tableProb(vector)
            )
    }else{
        output$table = list(
            tableProb(cut(vector, unique(quantile(vector, c(0,0.25,0.5,0.75,1), na.rm = T)),
                                include.lowest = T))
            )

        # names(output$table[[1]]) = c( "Min_1stQu",
        #                                                  "Qu_Median",
        #                                                  "Median_3rdQu",
        #                                                  "Qu_Max",
        #                                                  "Na")
    }

    output$plot =  ggplot2::ggplot(data.frame(vector),ggplot2::aes(x=vector)) +
        ggplot2::geom_histogram(ggplot2::aes(y=..density..),color="white",fill="#1F78B4") +
        ggplot2::geom_density(fill=NA, colour="black",size=1) +
        ggplot2::ggtitle(sprintf("Histogram of %s",vecName)) +
        ggplot2::xlab("Value")

    output$summary = cbind(names(summary(vector)),
                                                as.vector(summary(vector)))
    output$summary  = as.data.frame(output$summary)
    names(output$summary) = c("Name","Value")

    output$class = "numeric"

    output
}

## featureReport.numeric(iris[,1], names(iris)[1])

# character
featureReport.character = function(vector, vecName){
    if(missing(vecName)){
        vecName = "Current Variable"
    }

    # Initialize the output object
    output = list()

     ## try numeric
     if(naNum(vector) == naNum(as.numeric(vector)) & !endWithID(vecName)){
         output = featureReport.numeric(as.numeric(vector), vecName)
         output$Info = list("This variable may be a numeric variable")
         return(output)
     }

     ## TODO: Try Date

     ## character
     if(length(unique(vector))<10){
         output$table = tableProb(vector)
     }else{
         tab = tableProb(vector)
         otherCount = sum(vector %in% names(sort(tab[1,], decreasing = T)[1:10]))
         otherProb = paste0(round(otherCount / length(vector) * 100,1),"%")
         tab = tab[,order(tab[1,], decreasing = T)[1:10]]
         tab$others = c(otherCount, otherProb)
         output$tableInfo = sprintf("%s has over 10 different values.",
                                    vecName)
         output$table = tab
     }

     output$plot =  ggplot2::ggplot(data.frame(vector),ggplot2::aes(x=vector)) +
                                ggplot2::geom_bar(color="white",fill="#1F78B4") +
                                ggplot2::ggtitle(sprintf("Bar plot of %s",vecName)) +
                                ggplot2::xlab("Value")


    #  output$summary = summary(vector)
    output$class = "character"

     output
}

# factor
featureReport.factor = function(vector, vecName){
    if(missing(vecName)){
        vecName = "Current Variable"
    }

    # Initialize the output object
    output = list()

    output = featureReport.character(as.character(vector), vecName)

    output
}

setMethod("featureReport","numeric", featureReport.numeric )
setMethod("featureReport","character", featureReport.character)
setMethod("featureReport","factor", featureReport.factor)
# setMethod("featureReport","Date", featureReport.Date)
