#' @export
`%private%` <- function(x, y) {
  environment(x)[[as.character(substitute(y))]]
}

#' @export
`%private%<-` <- function(parent, child, value) {
  ns <- parent.frame()
  if (identical(environment(parent), ns)) {
    environment(parent) <- new.env(parent = ns)
    oldClass(parent) <- union("private_parent", oldClass(parent))
  }
  e <- environment(parent)
  oldClass(value) <- union("private_child", oldClass(value))
  e[[as.character(substitute(child))]] <- value
  parent
}

#' @export
print.private_parent <- function(x, ...) {
  header <- sprintf(
    "<private_parent> (%s) with children: %s",
    environmentName(topenv(environment(x))),
    toString(paste0(ls(environment(x), all.names = TRUE), "()"))
  )
  writeLines(cli::col_grey(header))
  xbkp <- x
  if (!length(setdiff(oldClass(x), c("private_parent", "private_child")))) {
    oldClass(x) <- setdiff(oldClass(x), "private_parent")
  }
  NextMethod()
  invisible(xbkp)
}

#' @export
print.private_child <- function(x, ...) {
  header <- sprintf(
    "<private_parent> (%s)",
    environmentName(topenv(environment(x)))
  )
  writeLines(cli::col_grey("<private_child>"))
  xbkp <- x
  if (!length(setdiff(oldClass(x), c("private_parent", "private_child")))) {
    oldClass(x) <- setdiff(oldClass(x), "private_child")
  }
  NextMethod()
  invisible(xbkp)
}

if ("devtools_shims" %in% search()) {
  #registerS3method("print", "private_parent",
  registerS3method("$", "private_parent", function(x, y) {
    environment(x)[[y]]
  })
  registerS3method(".DollarNames", "private_parent", function(x, pattern="") {
    ls(environment(x), all.names = TRUE)
  })
}
