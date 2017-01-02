#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`


#' Mean Comparison Table Summary
#'
#' @description Function using resulting output from mean comparison test from agricolae package optimized for graphs.
#' @param meanComp Object list with the result from mean comparison test
#' @return Table with complete data for graphics
#' @importFrom dplyr mutate select rename group_by_ summarise full_join
#' @importFrom tidyr separate
#' @export

dtsm <- function(meanComp){

  #to avoid no bisible global variable function
  std <- r <- trt <- means <- Min <- Max <- ste <- M <- NULL

  #fct <- as.character(mc$parameters$name.t)
  fct <- as.character(meanComp$parameters$name.t)
  fct <- as.expression(strsplit(fct, split = ":"))

  #dtmn <- mc$means #flavio
  dtmn <- meanComp$means #omar
  #dtgr <- mc$groups #flavio
  dtgr <- meanComp$groups #omar

  dtgr$trt <- gsub("\\s", "", as.character(dtgr$trt))

  dta <- dtmn %>%
    dplyr::mutate(ste = std/sqrt(r), trt = as.character(row.names(dtmn)))

  sm <- dplyr::full_join(dta[2:7], dtgr, by = "trt") %>%
    dplyr::select(trt, means, Min, Max, r, std, ste, M) %>%
    tidyr::separate("trt", sep = ":", into = eval(fct)) %>%
    dplyr::rename(mean = means, min = Min, max = Max, sg = M)


}









