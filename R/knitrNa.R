knitrNa = function(naR){
    doc <-   tagList(
                        p(strong(naR$noNA)),
                        br(),
                        p(naR$Delete[[1]]),
                        br(),
                        p(naR$Delete[[2]]),
                        br(),
                        p(naR$Delete[[3]]),
                        br(),
                        pre(class='r', code(class = 'r',naR$Delete[[4]]),
                                              code(class = 'r',naR$Delete[[5]]),
                                              code(class = 'r',naR$Delete[[6]])),
                        br(),
                        p(naR$Replace[[1]]),
                        br(),
                        p(naR$Replace[[2]]),
                        br(),
                        p(naR$Replace[[3]]),
                        br(),
                        pre(class='r', code(class = 'r',naR$Replace[[4]]))
                        )
        doc
}
