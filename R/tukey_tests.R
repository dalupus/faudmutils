


#' Function to automagically create tukey test tables from an anova anlysis
#'
#' @param aov Anova analysis of from \code{\link[stats]{aov}}
#' @param which This is what you factors you want to test and can either be a single string or array of strings c('F1','F2')
#' @param metric The metric you want to use for evaluation
#' @param type Type of output you would like.  valid values are 'html' and 'latex'
#' @param caption String containing the caption of the table
#' @param label Latex label to give the table
#' @param digits Number of significant digits to include in the table
#' @param file The file in which to output the results ("" means it simply prints to the screen)
HSDTable <- function(aov, which=NULL, metric="AUC",type="html",caption="Tukey HSD Test",label=NULL,digits=3,file=""){
  if(is.null(which)){
    which = ls(df.aov$xlevels)
  }
  hsd <- agricolae::HSD.test(aov,which)
  hsd.groups <- hsd$groups
  hsd.means <- hsd$means
  hsd.means$trt<-trimws(as.character(rownames(hsd.means)))
  hsd.groups$trt <- trimws(as.character(hsd.groups$trt))
  hsd <- dplyr::left_join(hsd.groups,hsd.means,by=c('trt'))
  hsd <- dplyr::select(hsd,trt,M,means,std)
  if(length(which) > 1) {
    colnames(hsd) <- c(paste(which, collapse = ':'), 'Group', metric, 'stdev')
  } else {
    colnames(hsd) <- c(which, 'Group', metric, 'stdev')
  }
  print(xtable::xtable(hsd,caption=caption,label=label,digits=digits),type=type,file=file,comment=FALSE, include.rownames=FALSE)
}
