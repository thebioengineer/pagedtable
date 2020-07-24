.rs.addFunction("rnb.outputSourceRdf", function(fileName,
                                                fileContents,
                                                metadata,
                                                rnbData,
                                                chunkId,
                                                ...)
{
  rdfName <- .rs.withChangedExtension(fileName, ".rdf")
  rdfPath <- file.path(rnbData$cache_path, chunkId, rdfName)

  content <- .rs.rnb.pagedTableHtml(rdfPath)


  annotated <- rmarkdown:::html_notebook_annotated_output(
    content,
    "frame"
  )

  saveRDS(annotated, file = "c://Users/ehhughes/Documents/Projects/pagedtable_junk/annotated.rds")

  knitr::asis_output(annotated)
})

.rs.addFunction("rnb.pagedTableHtml", function(rdfPath)
{
  data <- try(.rs.readDataCapture(rdfPath))

  saveRDS(data, file = "c://Users/ehhughes/Documents/Projects/pagedtable_junk/data.rds")

  pagedtable:::pagedtable_container(data)
})

.rs.addFunction("readDataCapture", function(path){
  e <- new.env(parent = emptyenv())
  load(file = path, envir = e)

  saveRDS(e, file = "c://Users/ehhughes/Documents/Projects/pagedtable_junk/e.rds")

  data <- e$x

  options <- e$options

  json <- pagedtable:::pagedtable_json(
    data,
    ruse_rownames = isTRUE(options[["rownames.print"]]),
    pagerows = if (is.null(options[["rows.print"]])) 10 else options[["rows.print"]],
    shadowDOM = if (is.null(options[["shadowDOM"]])) 10 else options[["shadowDOM"]]
  )

  saveRDS(json, file = "c://Users/ehhughes/Documents/Projects/pagedtable_junk/json.rds")

  return(json)

})

