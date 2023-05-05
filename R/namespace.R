# build actual namespace, we might not need all these empty elements but the requirements for
# isNamespace() are a bit weak (just have `.__NAMESPACE__.$spec` <- NA_character_` basically )
# so we do a little extra to have a proper namespace
new_namespace <- function(parent, name) {
  # to be safe we shape our private env like a regular namespace
  # its name is illegal but for our purpose it might work
  name <- sprintf("%s/%s", environmentName(parent), name)
  ns <- new.env(parent = parent)
  ns$.__NAMESPACE__. <- new.env(baseenv())
  ns$.__NAMESPACE__.$exports <- new.env(baseenv())
  ns$.__NAMESPACE__.$imports <- list(base = TRUE)
  ns$.__NAMESPACE__.$lazydata <- structure(new.env(baseenv()), name = paste0("lazydata:", name))
  ns$.__NAMESPACE__.$nativeRoutines <- list()
  # FIXME: probably not needed ?
  #ns$.packageName <- "FIXME"
  ns$.__NAMESPACE__.$path <- NA_character_
  # FIXME: not sure how it looks when it's empty
  #ns$.__NAMESPACE__.$S3methods <- "FIXME"
  ns$.__NAMESPACE__.$spec <- c(name = name, version = "0.0.0.9000")
  ns$.__S3MethodsTable__. <- new.env(baseenv())
  ns
}
