#functions for writing out model fits and predictions

write.intraday.results.fxn <- function(intraday.list, write=TRUE) {
  ################################################################################
  #overview: function to write out each geography's hour-of-day offsets from the corresponding daily average prediction. writes separate arrays for weekdays/weekends. rownames of the output array are a concatenation of the month and the hour-of-day offset, e.g. "03-01" for the first HOD offset for march (i.e. the offset for 12:00:00a-01:00:00a).
  #args:
  #--"intraday.list": list with as many elements as geographies being modeled. each geography is represented by a single data.frame of size (24 hours x (slices * months) "levels"). 
  #--"write": boolean indicating whether to write the results to CSV files.
  #returns:
  #--a list with the formatted intraday model objects (optional).
  ################################################################################
  weekend.df <- weekday.df <- data.frame(matrix(0, nrow=24*12, ncol=length(intraday.list)))
  colnames(weekend.df) <- colnames(weekday.df) <- names(intraday.list)
  rownames(weekend.df) <- rownames(weekday.df) <- paste(rep(formatC(1:12, digits=1, flag="0"), each=24), rep(formatC(1:24, digits=1, flag="0"), times=12), sep="-")
  weekday.month.idxs <- grep("Weekday", names(intraday.list[[1]]), fixed=TRUE) #determine which columns are for weekdays
  weekend.month.idxs <- grep("Weekend", names(intraday.list[[1]]), fixed=TRUE) #determine which columns are for weekends
  for (i in 1:length(intraday.list)) {
    weekday.series <- weekend.series <- c()
    for (weekday.month.idx in weekday.month.idxs) {
      weekday.series <- c(weekday.series, intraday.list[[i]][,weekday.month.idx])
    }
    weekday.df[,i] <- weekday.series
    for (weekend.month.idx in weekend.month.idxs) {
      weekend.series <- c(weekend.series, intraday.list[[i]][,weekend.month.idx])
    }
    weekend.df[,i] <- weekend.series
  }
  if (write) {
    write.csv(weekend.df, "fits/intraday.weekend.csv")
    write.csv(weekday.df, "fits/intraday.weekday.csv")
  }
  else {return(list(weekend=weekend.df, weekday=weekday.df))}
}


write.daily.avg.results.fxn <- function(daily.avg.list, write=TRUE) {
  ################################################################################
  #overview: function to write out each geography's average daily average "y" vs daily average "x". writes separate arrays for weekdays/weekends. for now it independently determines the min/max "x" for weekdays vs weekends for sizing the arrays, but eventually upstream functions might force the same "support" for all slices during model fitting. 
  #args:
  #--"daily.avg.list": list with as many elements as geographies being modeled. each geography is a list of two data.frames, weekdays and weekends.
  #--"write": boolean indicating whether to write the results to CSV files.
  #returns: 
  #--a list with the formatted daily average model objects (optional).
  ################################################################################
  #determine the full temperature range experienced by all geographies
  max.max.weekday <- max.max.weekend <- min.min.weekday <- min.min.weekend <- 0
  for (i in 1:length(daily.avg.list)) {
    if (daily.avg.list[[i]]$Weekday[1,1] < min.min.weekday) {min.min.weekday <- daily.avg.list[[i]]$Weekday[1,1]}
    if (daily.avg.list[[i]]$Weekday[nrow(daily.avg.list[[i]]$Weekday),1] > max.max.weekday) {max.max.weekday <- daily.avg.list[[i]]$Weekday[nrow(daily.avg.list[[i]]$Weekday),1]}
    if (daily.avg.list[[i]]$Weekend[1,1] < min.min.weekend) {min.min.weekend <- daily.avg.list[[i]]$Weekend[1,1]}
    if (daily.avg.list[[i]]$Weekend[nrow(daily.avg.list[[i]]$Weekend),1] > max.max.weekend) {max.max.weekend <- daily.avg.list[[i]]$Weekend[nrow(daily.avg.list[[i]]$Weekend),1]}
  }
  #initialize appropriately sized arrays to store results
  weekend.df <- data.frame(matrix(NA, nrow=length(seq(min.min.weekend, max.max.weekend, by=1)), ncol=length(daily.avg.list)))
  weekday.df <- data.frame(matrix(NA, nrow=length(seq(min.min.weekday, max.max.weekday, by=1)), ncol=length(daily.avg.list)))
  colnames(weekend.df) <- colnames(weekday.df) <- names(daily.avg.list)
  rownames(weekend.df) <- seq(min.min.weekend, max.max.weekend, by=1)
  rownames(weekday.df) <- seq(min.min.weekday, max.max.weekday, by=1)
  #push each geography's observations into the right chunk of its column
  for (i in 1:length(daily.avg.list)) {
    #weekends
    first.weekend <- which(rownames(weekend.df) == daily.avg.list[[i]]$Weekend[1,1])
    last.weekend <- which(rownames(weekend.df) == daily.avg.list[[i]]$Weekend[nrow(daily.avg.list[[i]]$Weekend),1])
    weekend.df[first.weekend:last.weekend, i] <- daily.avg.list[[i]]$Weekend$SplinePred
    #weekdays
    first.weekday <- which(rownames(weekday.df) == daily.avg.list[[i]]$Weekday[1,1])
    last.weekday <- which(rownames(weekday.df) == daily.avg.list[[i]]$Weekday[nrow(daily.avg.list[[i]]$Weekday),1])
    weekday.df[first.weekday:last.weekday, i] <- daily.avg.list[[i]]$Weekday$SplinePred
  }
  if (write) {
    write.csv(weekend.df, "fits/daily.avg.weekend.csv")
    write.csv(weekday.df, "fits/daily.avg.weekday.csv")
  }
  else {return(list(weekend=weekend.df, weekday=weekday.df))}
}


#' Load and ensure consistency of data
#' 
#' This function reads in the `X` and `Y` data and ensures consistency. It calls `load.data.fxn` twice (once to build the `x.df` and once to build the `y.df`) and then calls `ensure.compatible.arrays.fxn` to make sure `x.df` and `y.df` are consistent.
#' 
#' @param results.list list of lists containing fitted models for each geography.
#' @param write.daily.avg boolean indicating whether to write out the fitted "daily average" models.
#' @param write.intra.day boolean indicating whether to write out the fitted "intra-day" models.
#' @export
write.results.fxn <- function(results.list, write.daily.avg=TRUE, write.intra.day=TRUE) {
  ################################################################################
  #overview: top-level function to write out the list of results as arrays that are more convenient for both storage and subsequent analysis. 
  #args:
  #--"results.list": list of lists containing fitted models for each geography.
  #--"write.daily.avg": boolean indicating whether to write out the fitted "daily average" models.
  #--"write.intra.day": boolean indicating whether to write out the fitted "intra-day" models.
  #returns:
  #--none
  ################################################################################
  if (write.daily.avg) {write.daily.avg.results.fxn(results.list$daily.avg)}
  if (write.intra.day) {write.intraday.results.fxn(results.list$intra.day)}
}


#' Write model predictions
#' 
#' This function writes out hourly predictions from fitted models using new "x.df" data.
#' 
#' @param preds.df a data.frame (n.hours x n.geogs) of predicted output variable values.
#' @param first.date string formatted as "YYYYMMDD" indicating the first date for which predictions were generated.
#' 
#' @export write.preds.fxn
write.preds.fxn <- function(preds.df, first.date) {
  ################################################################################
  #overview: function to write out a data.frame of predictions. output array will have the same dimensions as "preds.df". the other arguments are used for generating more descriptive filenames.
  #args:
  #--"preds.df": data.frame (n.hours x n.geogs) of predicted output variable values.
  #--"first.date": string formatted as "YYYYMMDD" indicating the first date for which predictions were generated.
  #returns:
  #--none
  ################################################################################
  if (!dir.exists("preds")) {dir.create("preds")}
  write.csv(preds.df, paste0("preds/preds_", first.date, "_", nrow(preds.df)/24, "days.csv"))
}
