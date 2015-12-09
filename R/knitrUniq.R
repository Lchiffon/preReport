knitrUniq = function(uniqR){
    if(is.null(uniqR$unique2)){
        div1 = tagList(p(strong(uniqR$unique1[[1]]))
            )
    }else{
        div1 = tagList(p(uniqR$unique2[[1]]),
                            p(uniqR$unique2[[2]]),
                            pre(class = 'r', code(class = 'r', uniqR$unique2[[3]]))
                            )
    }

    if(is.null(uniqR$unique4)){
        div2 = tagList(p(strong(uniqR$unique3[[1]]))
            )
    }else{
        div2 = tagList(p(uniqR$unique4[[1]]),
                            p(uniqR$unique4[[2]]),
                            pre(class = 'r', code(class = 'r', uniqR$unique4[[3]]),
                                                    code(class = 'r', uniqR$unique4[[4]]))
                            )
    }
    doc <-   tagList(div1, div2)
        doc
}
