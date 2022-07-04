installOrLoadPackage <- function(listOfPackages) {
  repository <- 'https://cloud.r-project.org/'
  for (package in listOfPackages) {
    if (package %in% row.names(installed.packages()) == TRUE) {
      update.packages(package, dependecies = TRUE, repos=repository)
    } else {
      install.packages(package, repos=repository)
    }
    require(package, character.only = TRUE)
  }
}
