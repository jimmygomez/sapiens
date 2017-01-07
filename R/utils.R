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


#' Field book design for data collection
#'
#' @description Function to construct your field book for your experiment
#' @param treat1 strign with the name of the level factor
#' @param treat2 strign with the name of the level factor
#' @param rep number of repetition
#' @param design experimental design c("rcbd","crd","lsd")
#' @param lbl_treat1 col label for treat 1
#' @param lbl_treat2 col label for treat 2
#' @return Table with the experimental design
#' @importFrom agricolae design.ab
#' @export


design_fieldbook <- function( treat1 = NULL, treat2 = NULL, rep = NULL, design = "crd", lbl_treat1 = NULL, lbl_treat2 = NULL){


  if( is.null(treat1) && is.null(treat2) && is.null(rep) ){

    stop("You need to insert level for your treatments")

  }


  tr1 <- treat1
  tr2 <- treat2
  dsg <- design

  if(is.null(lbl_treat1)){

    lbt1 <- "treat1"

  } else {

    lbt1 <- lbl_treat1

  }

  if(is.null(lbl_treat2)){

    lbt2 <- "treat2"

  } else {

    lbt2 <- lbl_treat2

  }


  if(is.null(tr2)){

    tr2 = as.character("1")

  }

  vc1 <- unlist(strsplit(tr1, split = " "))
  vc2 <- unlist(strsplit(tr2, split = " "))

  trt1 <- factor(unique( vc1[ vc1 != ""]))
  trt2 <- factor(unique( vc2[ vc2 != ""]))

  lt1 <- length(trt1)
  lt2 <- length(trt2)

  fact <-c( lt1, lt2)

  table <- agricolae::design.ab(
    trt = fact,
    r = rep,
    serie = 0,
    design = dsg
  )

  book <- table$book

  lv1 <- factor(1:lt1)
  lv2 <- factor(1:lt2)

  book[,"A"] <- factor(book[,"A"], levels = lv1, labels = trt1)
  book[,"B"] <- factor(book[,"B"], levels = lv2, labels = trt2)


  fb <- plyr::rename(x = book, replace = c("plots" = "ID", "A" = lbt1, "B" = lbt2))

  if( tr2 == "1"){

    fb[, lbt2] <- NULL
    fb

  } else {

    fb

  }



}















