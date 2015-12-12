##' Function for Knitring NA Report Function
##'
##' @usage
##' knitrNa(naR)
##'
##' @param naR  naReport created by naReport function.
##'
##' @examples
##' report = naReport(testData)
##' knitrNa(report)

knitrNa = function(naR){

    if(is.null(naR$noNA)){
        div1 = tagList(p(" "))
    }else{
        div1 = tagList( p(strong(naR$noNA)),
                                br()
                                )
    }

    if(is.null(naR$Delete)){
        div2 = tagList(p(" "))
    }else{
        div2 = tagList( p(naR$Delete[[1]]),
                                br(),
                                p(naR$Delete[[2]]),
                                br(),
                                p(naR$Delete[[3]]),
                                br(),
                                pre(class='r', code(class = 'r', naR$Delete[[4]]),
                                                      code(class = 'r', naR$Delete[[5]]),
                                                      code(class = 'r', naR$Delete[[6]]))
                                )
    }

    if(is.null(naR$Replace)){
        div3 = tagList(p(" "))
    }else{
        div3 = tagList( br(),
                                p(naR$Replace[[1]]),
                                br(),
                                p(naR$Replace[[2]]),
                                br(),
                                p(naR$Replace[[3]]),
                                br(),
                                pre(class='r', code(class = 'r', naR$Replace[[4]]))
                                )
    }
    doc <-   tagList(div1, div2, div3)
        doc
}
