.onLoad <- function(libname, pkgname) {
  .fetch_apis_impl <<- memoise::memoise(.fetch_apis_impl) # nocov
}
