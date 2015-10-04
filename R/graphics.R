

#' Function to create the fake Tukey plot
#'
#' @param data The dataframe to be processed
#' @param metric The evaluation metric column to use (default is AUC)
#' @param groupVars The columns which are used to group the rows (default is Learner)
#' @param conf The confidence interval to use
tukeyPlot <- function(data, metric="AUC", groupVars="Learner", conf=0.95){
  data.stats <- faudmutils::summarySE(data,measurevar = metric, groupvars = groupVars, conf.interval = conf)
  if(length(groupVars)>1){
    data.stats$plotVars <- apply(data.stats[,groupVars],1,paste,collapse=":")
    xlabel <- paste(groupVars,collapse=":")
  }else{
    data.stats$plotVars <- data.stats[,groupVars]
    xlabel <- groupVars
  }
  data.stats$ymin <- data.stats[,metric] - data.stats$ci
  data.stats$ymax <- data.stats[,metric] + data.stats$ci
  ggplot2::ggplot(data.stats, aes_string(x="plotVars", y=metric, colour="plotVars")) +
    ggplot2::geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.2) +
    ggplot2::geom_point(size=4) +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(metric) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::theme(legend.position="none")
}
