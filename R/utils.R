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
         !is.object(x) && is_vector(x))
}

size_sum <- function(x) {
  if (!paged_table_is_vector_s3(x)) return("")

  paste0(" [", dim_desc(x), "]" )
}

obj_sum.default <- function(x) {
  paste0(type_sum(x), size_sum(x))
}

obj_sum <- function(x) {
  switch(class(x)[[1]],
         POSIXlt = rep("POSIXlt", length(x)),
         list = vapply(x, obj_sum.default, character(1L)),
         paste0(type_sum(x), size_sum(x))
  )
}
