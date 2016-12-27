#' update google spreadsheet for shiny app
#'
#' @description function use for automatic o manual update of data frame contend in a google spreadsheet
#' @param data data frame with want update
#' @param type  type of update: automatic o manually
#' @param time time to automatic reload of the information
#' @return google spreadsheet file update
#' @importFrom gsheet gsheet2tbl
#' @importFrom shiny invalidateLater
#' @export


getData <- function(url, type = "automatic", time = 60000) {


  if (type == "automatic") {

    shiny::invalidateLater(60000) # 1 min = 60000 msec
    data <- gsheet::gsheet2tbl(url)


  } else if (type == "manually") {

    data <- gsheet::gsheet2tbl(url)

  }

}

