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


#' Multiple comparison test
#'
#' @description Function analisis of variance for summary data.
#' @param aov lm o aov result function.
#' @param comp treatments will be compared.
#' @param type method for made comparision analysis: c("snk", "tukey", "duncan").
#' @param sig significance level. Default 0.05
#' @return Table with complete data for graphics
#' @importFrom agricolae SNK.test HSD.test duncan.test
#' @export


test_comparison <- function( aov, comp, type = "snk", sig = 0.05){

  if( type == "snk"){

    mc <- agricolae::SNK.test(y = aov, trt = comp, alpha = sig)

  } else if (type == "tukey"){

    mc <- agricolae::HSD.test(y = aov, trt = comp, alpha = sig)

  } else if (type == "duncan"){

    mc <- agricolae::duncan.test(y = aov, trt = comp, alpha = sig)

  }

  sapiens::dtsm(mc)

}

