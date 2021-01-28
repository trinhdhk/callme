.onLoad <- function(libname, pkgname){
  is_rstudio <- rstudioapi::isAvailable()
  if (is_rstudio){
    assign(".rs.getNames",
           function(object){
             tryCatch({
               if (is.environment(object))
                 ls(object, all.names = TRUE)
               else if (inherits(object, "tbl") && "dplyr" %in%
                        loadedNamespaces())
                 dplyr::tbl_vars(object)
               else if (inherits(object, "jobjRef"))
                 gsub("[\\(\\)]", "", names(object))
               else names(object)
             }, error = function(e) NULL)

           }, envir = environment(.rs.getNames))
  }
  cat(crayon::yellow("This is an experimental package. Use at your own risk.\n"))
}
