#' Run Shiny apps
#'
#' @description function for run the shiny apps from sapiens package
#' @family sapiens
#' @importFrom shiny runApp
#' @export

elisios <- function() {
  appDir <- system.file("elisios", package = "sapiens")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sapiens`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


wanuy <- function() {
  appDir <- system.file("wanuy", package = "sapiens")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sapiens`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}




