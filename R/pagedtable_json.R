pagedtable_json <- function(x,
                            ...,
                            use_rownames = getOption("pagedtable.rownames.print"),
                            pagerows = 10,
                            shadowDOM = TRUE) {


  # read the data into json format
  formatted_contents <- lapply(colnames(x),
                               function(name, x){
                                 pagedtable_col(
                                   x = x[[name]],
                                   name = name,
                                   ...
                                 )
                               },x)

  names(formatted_contents) <- colnames(x)


  if(use_rownames){
    formatted_contents <- c(list("_rn_" = list(
      content = rownames(x),
      columns = list(
        label = "",
        name = "_rn_",
        type = "",
        align = "left"
      ),
      dependencies = NULL
    )),
    formatted_contents)
  }

  data <- jsonlite::toJSON(
    data.frame(lapply(formatted_contents, `[[`, 'content'), check.names = FALSE)
  )

  columns <- lapply(formatted_contents, `[[`, 'columns')
  names(columns) <- NULL

  dependencies <- unique(
    do.call('c',lapply(formatted_contents, `[[`, 'dependencies')))

  options = list(
    "rows" =  list(min = pagerows),
    "shadowDOM" = shadowDOM
  )

  # pass the data and settings using 'pagedtable_list'
  list(
    data = data,
    columns = columns,
    options = options,
    dependencies = dependencies
  )
}
