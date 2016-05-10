knitrFeature = function(feaReport){
    if(feaReport$class == "numeric"){
        print(p(feaReport$Info))
        print(feaReport$plot)
        opts_current$set(results = "asis")

        print(knitr::kable(as.data.frame(feaReport$table), row.names = T))

        print(knitr::kable(as.data.frame(feaReport$summary), row.names = T))
    }

    if(feaReport$class == "character"){
        print(feaReport$plot)

        print(p(feaReport$tableInfo))

        print(knitr::kable(as.data.frame(feaReport$table), row.names = T))
    }

}
