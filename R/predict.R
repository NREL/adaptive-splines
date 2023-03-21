#functions for generating predictions from fitted models

get.daily.avg.fcast.fxn <- function(x.df) {
  ################################################################################
  #overview: helper function to translate hourly forecast values from "x.df" into corresponding daily average values so that predictions can be made.
  #args:
  #--"x.df": data.frame (n.hours x n.geogs) containing forecast independent variable values for desired hours and geographies for prediction.
  #returns: 
  #--"daily.avg.df": data.frame (n.days x n.geogs) containing corresponding daily average forecast independent variable values for prediction.
  #TODO: any consistency checking on the inputs? e.g., if (length(pred.days) != nrow(x.df)/24)) {...}
  ################################################################################
  daily.avg.df <- t(sapply(1:(nrow(x.df)/24), FUN=function(x) {colMeans(x.df[((x-1)*24+1):(x*24),])}))
  pred.days <- unique(substr(rownames(x.df), 1, 10))
  rownames(daily.avg.df) <- pred.days
  return(daily.avg.df)
}


#' Predict using adaptive spline models
#' 
#' This function makes hourly predictions from fitted models using new "x.df" data. 
#' 
#' @param x.df a data.frame (n.hours x n.geogs) of forecast independent variable values to serve as inputs for prediction.
#' @param first.date the date of the first day to be predicted, in "YYYY-MM-DD" structure.
#' @param from.csv boolean indicating whether to read the fitted models from CSV files (TRUE) or whether they are in working memory already (FALSE).
#' @param fitted.models optional arg (defaulting to NULL) containing the fitted models; required when "from.csv" is FALSE.
#' @return a data.frame of hourly predictions for the intersection of geogs included in "x.df" and the fitted models.
#' @export predict.fxn
predict.fxn <- function(x.df, first.date, from.csv=TRUE, fitted.models=NULL) {
  ################################################################################
  #overview: top-level function to make hourly predictions from fitted models given forecast observations in "x.df". will only make predictions for the intersection of geogs in "x.df" and those contained in the fitted models (but warns if there are mismatches in either direction).
  #args:
    #--x.df: data.frame (n.hours x n.geogs) of forecast independent variable values to serve as inputs for prediction.
    #--first.date: string indicating the date of the first day to be predicted, in "YYYYMMDD" structure.
    #--from.csv: optional boolean arg (defaulting to TRUE) indicating whether to read the fitted models from CSV files (TRUE) or whether they are in working memory already (FALSE).
    #--fitted.models: optional arg (defaulting to NULL) containing the fitted models; required when "from.csv" is FALSE.
  #returns:
    #--a data.frame of hourly predictions for the intersection of geogs included in "x.df" and the fitted models.
  ################################################################################
  #load/prep data
  if (from.csv) { #load fitted models from CSV files
    daily.avg.weekday <- load.model.csv.fxn("daily.avg", "weekday")
    daily.avg.weekend <- load.model.csv.fxn("daily.avg", "weekend")
    daily.avg.list <- list(weekday=daily.avg.weekday, weekend=daily.avg.weekend)
    intraday.weekday <- load.model.csv.fxn("intraday", "weekday")
    intraday.weekend <- load.model.csv.fxn("intraday", "weekend")
    intraday.list <- list(weekday=intraday.weekday, weekend=intraday.weekend)
  }
  else { #format in-memory fitted models to take same structure as CSV files
    if (is.null(fitted.models)) {stop("'fitted.models' arg not provided")}
    intraday.list <- fitted.models$intra.day #unpack
    intraday.list <- write.intraday.results.fxn(intraday.list, FALSE) #format
    daily.avg.list <- fitted.models$daily.avg #unpack
    daily.avg.list <- write.daily.avg.results.fxn(daily.avg.list, FALSE) #format
  }
  #ensure not trying to make predictions for geogs that don't have fitted models
  geogs.without.models <- setdiff(colnames(x.df), colnames(daily.avg.list$weekday))
  if (length(geogs.without.models) > 0) {
    print(paste0("the following geogs with forecast observations do not have fitted models loaded: ", geogs.without.models))
    overlapping.geogs <- intersect(colnames(x.df), colnames(daily.avg.list$weekday))
    x.df <- x.df[,overlapping.geogs] #chop down to just the geogs that have models
  }
  #provide info if some fitted geogs don't have forecast observations
  geogs.without.fcasts <- setdiff(colnames(daily.avg.list$weekday), colnames(x.df))
  if (length(geogs.without.fcasts) > 0) {print(paste0("the following geogs with fitted models do not have forecast observations loaded: ", geogs.without.fcasts))}
  #create datetime features for desired prediction dates
  predict.dates <- as.character(seq(from=as.Date(first.date, format="%Y%m%d"), by=1, length.out=nrow(x.df)/24)) #determine the full range of days
  predict.years <- unique(substr(predict.dates, 1, 4)) #determine if these days span multiple calendar years
  datetime.features.years <- build.datetime.features.fxn(predict.years, FALSE) #build daily features for full year for all relevant years
  datetime.features.dates <- datetime.features.years[substr(rownames(datetime.features.years), 1, 10) %in% predict.dates,] #subset to fcast dates
  #forecast hour-of-day offsets
  preds.hod.df <- data.frame(matrix(0, nrow=nrow(x.df), ncol=ncol(x.df)))
  rownames(preds.hod.df) <- rownames(x.df)
  colnames(preds.hod.df) <- colnames(x.df)
  pred.datetime.levels <- unique(datetime.features.dates$Level)
  for (datetime.level in pred.datetime.levels) {
    day.idxs <- which(datetime.features.dates$Level == datetime.level) #day.idxs will be consistent across "geogs" since all are in local time
    hour.idxs <- c(sapply(day.idxs, FUN=function(x) {(24*(x-1)+1):(24*x)})) #obtain seq of 24 hours corresponding to each day.idx
    level.month <- unlist(strsplit(datetime.level, fixed=T, split="-"))[1] #"01:12"
    level.daytype <- unlist(strsplit(datetime.level, fixed=T, split="-"))[2] #"Weekday"/"Weekend"
    intraday.fits <- ifelse(level.daytype == "Weekday", list(intraday.list$weekday), list(intraday.list$weekend))[[1]]
    start.idx <- (as.numeric(level.month)-1)*24+1 #determine first idx for HOD offsets corresponding to level.month
    hod.offsets <- intraday.fits[start.idx:(start.idx+23),] #dim: (24 x n.geogs)
    hod.offsets <- hod.offsets[,colnames(x.df)] #order columns to match (and subset as necessary)
    hod.offsets.rep <- hod.offsets[rep(seq_len(nrow(hod.offsets)), times=length(day.idxs)),] #repeat rows to match size of level
    preds.hod.df[hour.idxs,] <- hod.offsets.rep #push into HOD array at correct locations
  }
  #forecast daily average values
  preds.base.df <- data.frame(matrix(0, nrow=nrow(x.df), ncol=ncol(x.df)))
  rownames(preds.base.df) <- rownames(x.df)
  colnames(preds.base.df) <- colnames(x.df)
  daytypes <- tolower(as.character(unique(datetime.features.dates$DayType)))
  daily.avg.x.preds <- get.daily.avg.fcast.fxn(x.df) #calculate daily average fcast values from hourly fcast values
  for (daytype in daytypes) { #as much as one pass for weekdays, one pass for weekends
    day.idxs <- which(tolower(datetime.features.dates$DayType) == daytype)
    hour.idxs <- c(sapply(day.idxs, FUN=function(x) {(24*(x-1)+1):(24*x)})) #obtain seq of 24 hours corresponding to each day.idx
    daytype.fits <- ifelse(daytype == "weekday", list(daily.avg.list[["weekday"]]), list(daily.avg.list[["weekend"]]))[[1]]
    daytype.da.ints <- rownames(daytype.fits)
    for (geog in colnames(x.df)) {
      geog.fits <- daytype.fits[,geog]
      names(geog.fits) <- daytype.da.ints
      geog.fcast.ints <- round(as.numeric(daily.avg.x.preds[day.idxs, geog]), 0)
      geog.preds <- as.numeric(sapply(geog.fcast.ints, FUN=function(x) {geog.fits[as.numeric(names(geog.fits)) == x]})) #get daily avg preds
      geog.preds <- rep(geog.preds, each=24) #expand daily avg preds from daily to hourly
      preds.base.df[hour.idxs, geog] <- geog.preds
    }
  }
  #sum forecast components to obtain final hourly predictions
  preds.df <- preds.base.df + preds.hod.df
  return(preds.df)
}
