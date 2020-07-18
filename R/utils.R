type_sum <- function(x) {
  format_sum <- switch (class(x)[[1]],
                        ordered = "ord",
                        factor = "fctr",
                        POSIXt = "dttm",
                        difftime = "time",
                        Date = "date",
                        data.frame = class(x)[[1]],
                        tbl_df = "tibble",
                        NULL
  )
  if (!is.null(format_sum)) {
    format_sum
  } else if (!is.object(x)) {
    switch(typeof(x),
           logical = "lgl",
           integer = "int",
           double = "dbl",
           character = "chr",
           complex = "cplx",
           closure = "fun",
           environment = "env",
           typeof(x)
    )
  } else if (!isS4(x)) {
    paste0("S3: ", class(x)[[1]])
  } else {
    paste0("S4: ", methods::is(x)[[1]])
  }
}

"%||%" <- function(x, y) {
  if(is.null(x)) y else x
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

dim_desc <- function(x) {
  dim <- dim(x) %||% length(x)
  format_dim <- vapply(dim, big_mark, character(1))
  format_dim[is.na(dim)] <- "??"
  paste0(format_dim, collapse = " \u00d7 ")
}

is_atomic <- function(x) {
  is.atomic(x) && !is.null(x)
}

is_vector <- function(x) {
  is_atomic(x) || is.list(x)
}

paged_table_is_vector_s3 <- function(x) {
  switch(class(x)[[1]],
         ordered = TRUE,
         factor = TRUE,
         Date = TRUE,
         POSIXct = TRUE,
         difftime = TRUE,
         data.frame = TRUE,
         tbl_df = TRUE,
         !is.object(x) && is_vector(x))
}

size_sum <- function(x) {
  if (!paged_table_is_vector_s3(x)) return("")

  paste0(" [", dim_desc(x), "]" )
}

table_tooltip <- function(x){

  n_col <- ncol(x)
  n_row <- nrow(x)

  ncol_disp <- min(n_col,3)
  nrow_disp <- min(n_row,3)

  x_sub <- x[1:nrow_disp, 1:ncol_disp]

  thead <- paste0("<thead><tr>",
                  paste0("<td>",colnames(x_sub),"</td>", collapse = ""),
                  "</tr></thead>")

  tbody <- do.call(paste,c("<tbody>",lapply(1:nrow_disp,function(r){
    paste0("<tr>",
           paste(sapply(1:ncol_disp, function(col_){
                  paste0("<td>",format(x_sub[[col_]][[r]], method = "html"),"</td>")
           }), collapse = ""),
      "</tr>")
  }),"</tbody>",collapse = ""))

  tfooter <- NULL

  if(n_row > nrow_disp){
    tfooter <- paste("...with",n_row - nrow_disp,"more rows")
  }

  if(n_col > ncol_disp){

    remaining_col_types <- sapply(as.list(x)[(ncol_disp + 1):n_col],type_sum)

    tfooter <- paste0(
      tfooter,
      ifelse(is.null(tfooter),"...with ",", and "),
      n_col - ncol_disp," more variables: ",
      paste0(names(remaining_col_types),
            " &lt",remaining_col_types,"&gt", collapse = ", ")
    )
  }

  paste("<div style = 'width:300px'><div style = 'background-color: lightgrey;overflow:auto;width:fit-content;margin:auto;'><table>",thead,tbody,"</table></div>","<p style='text-align:left;margin:4px'>",tfooter,"</p></div>")

}

