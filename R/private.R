#' Define and access private functions
#'
#' These functions provide a convenient way to create namespaces associated with
#' a given function. This avoids cluttering the main namespace of a package,
#' prevents name collision and makes the scope of helper functions explicit in the code.
#'
#' The first time `%private%<-` is called on a function, its environment (usually
#' the package's namespace) is changed to be a new namespace, child of its original environment.
#'
#' Anytime `%private%<-` is called on a function its environment is populated with
#' the object defined in the right hand side, which will be bound there and have its environment set there as well
#' if it's a function.
#'
#' `%private%` is called to access the child function from outside, to test it
#' for instance.
#'
#' @param parent The `"private_parent"` function, or the target to be converted to a `"private_parent"`
#' @param child The name of the object to access or to define in `parent`'s private namespace
#' @param value An object, often a function.
#'
#' @export
#' @name private
`%private%` <- function(parent, child) {
  environment(parent)[[as.character(substitute(child))]]
}

#' @export
#' @rdname private
`%private%<-` <- function(parent, child, value) {
  # we work on environments by reference, if `value` is already a private child
  # it means we have a `a %private% b %private% c <- function(...) {...}` call
  # and we can return already since the job is done
  if (inherits(value, "private_child")) return(parent)

  e <- environment(parent)
  if (!inherits(parent, "private_parent")) {
    ns_fun_names <- ls(e, all.names = TRUE)
    ns_funs <- mget(ns_fun_names, e, inherits = FALSE, mode = "function", ifnotfound = list(NULL))
    fun_name <- setdiff(names(Filter(function(x) identical(x, parent), ns_funs)), "*tmp*")
    # environment(parent) <- new.env(parent = ns)
    #new_ns_name <- paste0(environmentName(ns), "/", fun_name)
    environment(parent) <- new_namespace(e, fun_name)
    oldClass(parent) <- union("private_parent", oldClass(parent))
    e <- structure(
      environment(parent),
      class = c("private_env", "environment")
    )
    class(e) <- "private_env"
    # need to update binding of parent
    parent.env(e)[[fun_name]] <- parent
  }

  if (is.function(value)) {
    value <- structure(value, class = union("private_child", oldClass(value))) #, private_parent = fun_name)
    # so helper functions can call each other
    environment(value) <- e
  }
  e[[as.character(substitute(child))]] <- value

  parent
}

#' @export
print.private_parent <- function(x, ...) {
  e <- environment(x)
  children <- setdiff(ls(e, all.names = TRUE), c(".__NAMESPACE__.", ".__S3MethodsTable__."))
  funs_lgl<- sapply(mget(children, e), is.function)
  children[funs_lgl] <- paste0(children[funs_lgl], "()")
  header <- sprintf("<private_parent> with children: %s", toString(children))
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
  writeLines(cli::col_grey("<private_child>"))
  xbkp <- x
  if (!length(setdiff(oldClass(x), c("private_parent", "private_child")))) {
    oldClass(x) <- setdiff(oldClass(x), "private_child")
  }
  NextMethod()
  invisible(xbkp)
}

# if ("devtools_shims" %in% search()) {
#   registerS3method("$", "private_parent", function(x, y) {
#     environment(x)[[y]]
#   })
#   registerS3method(".DollarNames", "private_parent", function(x, pattern="") {
#     ls(environment(x), all.names = TRUE)
#   })
# }
