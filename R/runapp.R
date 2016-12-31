#' elisios
#'
#' @description Irrigation management app.
#' @family sapiens
#' @importFrom shiny runApp
#' @export

elisios <- function() {
    appDir <- system.file("elisios", package = "sapiens")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `sapiens`.",
            call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


#' wanuy
#'
#' @description Fertilization management app.
#' @family sapiens
#' @importFrom shiny runApp
#' @export

wanuy <- function() {
    appDir <- system.file("wanuy", package = "sapiens")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `sapiens`.",
            call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}




