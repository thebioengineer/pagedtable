pagedtable_container <- function(name,
                                 x,
                                 ...,
                                 width = "100%",
                                 height = "auto") {


  x$data <- jsonlite::fromJSON(x$data)


  deps <- htmlwidgets::getDependency("pagedtable", "pagedtable")

  js_src <-
    list.files(deps[[which(sapply(deps, `[[`, "name") == "pagedtable")]][["src"]]$file,
               "[.]js$", full.names = TRUE)

  pt_js <- paste0(
    "<script type=\"text/javascript\">\n",
    paste(readLines(js_src), collapse = "\n"),
    "\n</script>\n"
  )

  pt_div <- paste0(
    "<div data-pagedtable>",
    "<script data-pagedtable-source type='application/json'>",
    jsonlite::toJSON(x),
    "</script>",
    "</div>"
  )

  # htmltools::HTML(paste(pt_js, pt_div))

  paste("<div>",pt_js, pt_div,"</div>")

}
