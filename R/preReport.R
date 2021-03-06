preReport = function(inputData, path,
    scaffold = system.file("skeleton", package = "preReport"),
    open_rmd = T, useBytes = T, target = NULL, ...){

    if(missing(inputData)){
        stop("You should input a data.frame.")
    }
    ## Check data class, convert to data.frame
    datName = as.character(substitute(inputData))
    inputData = dataFrameCheck(inputData)

    if(missing(path)){
        setwd(tempdir())
        path = "Current"
    }

    if(!is.null(target)){
        if(!target %in% names(inputData))
            target = NULL
    }

    message("Creating report directory at ", path, "...")
    copy_dir(scaffold, path)
    message("Finished creating report directory...")
    message("Switching to report directory...")
    setwd(path)

    data = inputData
    save(data, file = "data/data.Rdata")
    body = readLines("assets/body.Rmd", ...)
    body[2] = sprintf(body[2], datName)
    body[3] = sprintf(body[3], Sys.Date())

    if(is.null(target)){
        featureLayout = readLines("assets/features.Rmd", ...)
    }else{
        featureLayout = readLines("assets/featuresWithTarget.Rmd", ...)
        j = which(names(inputData)==target)
    }

    featureIndex = which(grepl("%s", featureLayout))
    featureOutput = c()


    ## Create dirction
    if(is.null(target)){
        for( i in 1:(dim(data)[2])){
            feature = featureLayout
            feature[[featureIndex[1]]] = sprintf(feature[[featureIndex[1]]], names(data)[i])
            feature[[featureIndex[2]]] = sprintf(feature[[featureIndex[2]]], i, names(data)[i])
            featureOutput = c(featureOutput, feature)
        }
    }else{
        for( i in 1:(dim(data)[2])){
            feature = featureLayout
            feature[[featureIndex[1]]] = sprintf(feature[[featureIndex[1]]], names(data)[i])
            feature[[featureIndex[2]]] = sprintf(feature[[featureIndex[2]]], i, names(data)[i], j)
            featureOutput = c(featureOutput, feature)
        }
    }


    out = c(body, featureOutput)
    ## Write Files
    writeLines(out, "index.Rmd", useBytes = useBytes)

    ## knitrtohtml
    rmarkdown::render("index.Rmd","html_document")
    #
    #
    # knitr::knit('index.Rmd', 'index.md') # creates md file
    # markdown::markdownToHTML('index.md', 'index2.html',
    #    options=c('use_xhtml', 'base64_images','fragment_only')) # creates html file

    ## openRmd
    if (open_rmd) {
        message("Opening slide deck...")
        file.edit("index.Rmd")
    }

    browseURL("index.html")



}
