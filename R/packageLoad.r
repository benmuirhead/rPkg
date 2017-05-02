#' Package Load Function
#'
#' This function accepts a string (or vector of strings) representing a package name. It checks to see if any of the packages are not installed and installs them. Then, it loads all the packages.
#' @param package_list String or vector of strings. Defaults to "tictoc".
#' @keywords packages
#' @export
#' @examples
#' packageLoad("beepr")
#'
#' list_of_packages = c("chron","corrplot","data.table")
#' packageLoad(list_of_packages)



packageLoad = function(package_list = "tictoc") {

  new_packages <-
    package_list[!(package_list %in% installed.packages()[, "Package"])]
  if (length(new_packages))
    install.packages(new_packages)

  for (i in package_list)
  {
    library(i, character.only = TRUE)
  }

  if (length(new_packages)) {
    print(paste("New Packages:",length(new_packages)))
    print(new_packages)
  }
  print(paste("Loaded Packages:",length(package_list)))
  print(package_list)

}
