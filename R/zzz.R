.onLoad <- function(libname, pkgname){
  is_rstudio <- rstudioapi::isAvailable()

  if (is_rstudio){
    # This is exactly identical to the default .rs.getNames
    # I don't know why that one does not work
    # So I re-define it (by copy and paste the code)
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
  writeLines(crayon::yellow("This is an experimental package. Use at your own risk."))
}
