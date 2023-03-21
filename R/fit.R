#functions for fitting the models

# build.hourly.datetime.features.fxn <- function(data.years) {
#   ################################################################################
#   #overview: function to determine the datetime "level" each hour belongs to. takes in a vector of calendar years spanning the input data and uses it to create a daily sequence of values indicating which month and day of the week each day belongs to. the day of week is then mapped to coarser weekday/weekend categories. that sequence is then expanded to an hourly resolution (all 24 hours of the day have the same feature values as the corresponding day), and the values are then concatenated to define the unique "level" that each hourly observation belongs to, which subsequently determines which models the observation is used to fit. the function assumes that all years of the underlying data are comprised of 8760 contiguous hours (regardless of whether the actual years are leap years) that have uninterrupted 5/2/5/2 weekday/weekend patterns that correspond to the true weekday/weekend cycle of that input year, rather than that of the target year (NREL standard practice for load modeling). called by "get.levels.hourly.idxs.fxn". 
#   #args:
#   #--"data.years": vector (length: variable) of integers indicating the calendar years of the data.
#   #returns:
#   #--"hourly.levels.df.multi": data.frame (shape: (n.hours x (n.features+1)) with the final column containing the "level" to which each hour belongs.
#   #TODO: treat holidays as weekends
#   ################################################################################
#   datetime.features <- c("Month", "DayType")
#   hourly.levels.df.multi <- data.frame(matrix(nrow=0, ncol=(length(datetime.features)+1)))
#   colnames(hourly.levels.df.multi) <- c(datetime.features, "Level")
#   data.years <- sort(data.years)
#   my.rownames <- character(length=0)
#   for (year in data.years) {
#     #steps to assign each hour to weekday/weekend
#     day.seq <- seq.Date(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")), by=1)[1:365] #"[1:365]" handles leap years
#     dow.seq <- weekdays(day.seq)
#     dow.seq[dow.seq %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] <- "Weekday"
#     dow.seq[dow.seq %in% c("Saturday", "Sunday")] <- "Weekend"
#     hourly.dow.seq <- rep(dow.seq, each=24)
#     #steps to assign each hour to a month (built using a fixed non-leap year to ensure february always has 28 days)
#     month.seq <- substr(seq.Date(as.Date("2022-01-01"), as.Date("2022-12-31"), by=1), 6,7)
#     hourly.month.seq <- rep(month.seq, each=24)
#     #combine fields to define each hour's "level"
#     hourly.levels.df <- data.frame(cbind(hourly.month.seq, hourly.dow.seq))
#     colnames(hourly.levels.df) <- datetime.features
#     hourly.levels.df$Level <- paste(hourly.levels.df$Month, hourly.levels.df$DayType, sep="-")
#     #append to prior years
#     hourly.levels.df.multi <- rbind(hourly.levels.df.multi, hourly.levels.df)
#     #get repeated date rownames for this year and append to prior years
#     year.dates <- seq(from=as.Date(paste0(year, "-01-01")), by=1, length.out=365)
#     year.dates.rep <- paste(rep(year.dates, each=24), formatC(rep(1:24, times=365), digits=1, flag="0"), sep="-")
#     my.rownames <- c(my.rownames, year.dates.rep)
#   }
#   rownames(hourly.levels.df.multi) <- my.rownames
#   return(hourly.levels.df.multi)
# }

build.datetime.features.fxn <- function(data.years, expand=TRUE) {
  ################################################################################
  #overview: function to determine the datetime "level" each time period belongs to. takes in a vector of calendar years spanning the input data and uses it to create a daily sequence of values indicating which month and day of the week each day belongs to. the day of week is then mapped to coarser weekday/weekend categories. if "expand" is TRUE, that sequence is then expanded to an hourly resolution (all 24 hours of the day have the same feature values as the corresponding day), and the values are then concatenated to define the unique "level" that each hourly observation belongs to, which subsequently determines which models the observation is used to fit. the function assumes that all years of the underlying data are comprised of 8760 contiguous hours (regardless of whether the actual years are leap years) that have uninterrupted 5/2/5/2 weekday/weekend patterns that correspond to the true weekday/weekend cycle of that input year, rather than that of the target year (NREL standard practice for load modeling). called by "get.levels.hourly.idxs.fxn" for model fitting and by "predict.fxn" for prediction. 
  #args:
  #--"data.years": vector (length: variable) of integers indicating the calendar years of the data.
  #--"expand": boolean indicating whether to expand from daily resolution to hourly resolution
  #returns:
  #--"hourly.levels.df.multi": data.frame (shape: (n.hours x (n.features+1)) with the final column containing the "level" to which each hour belongs.
  #TODO: treat holidays as weekends
  ################################################################################
  datetime.features <- c("Month", "DayType")
  levels.df.multi <- data.frame(matrix(nrow=0, ncol=(length(datetime.features)+1)))
  colnames(levels.df.multi) <- c(datetime.features, "Level")
  data.years <- sort(data.years)
  my.rownames <- character(length=0)
  for (year in data.years) {
    #steps to assign each hour to weekday/weekend
    day.seq <- seq.Date(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")), by=1)[1:365] #"[1:365]" handles leap years
    dow.seq <- weekdays(day.seq)
    dow.seq[dow.seq %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] <- "Weekday"
    dow.seq[dow.seq %in% c("Saturday", "Sunday")] <- "Weekend"
    if (expand) {dow.seq <- rep(dow.seq, each=24)} #expand from daily to hourly
    #steps to assign each hour to a month (built using a fixed non-leap year to ensure february always has 28 days)
    month.seq <- substr(seq.Date(as.Date("2022-01-01"), as.Date("2022-12-31"), by=1), 6,7)
    if (expand) {month.seq <- rep(month.seq, each=24)} #expand from daily to hourly
    #combine fields to define each hour's "level"
    levels.df <- data.frame(cbind(month.seq, dow.seq))
    colnames(levels.df) <- datetime.features
    levels.df$Level <- paste(levels.df$Month, levels.df$DayType, sep="-")
    #append to prior years
    levels.df.multi <- rbind(levels.df.multi, levels.df)
    #get repeated date rownames for this year and append to prior years
    year.dates <- as.character(seq(from=as.Date(paste0(year, "-01-01")), by=1, length.out=365))
    if (expand) {year.dates <- paste(rep(year.dates, each=24), formatC(rep(1:24, times=365), digits=1, flag="0"), sep="-")}
    my.rownames <- c(my.rownames, year.dates)
  }
  rownames(levels.df.multi) <- my.rownames
  return(levels.df.multi)
}


get.levels.hourly.idxs.fxn <- function(data.years) {
  ################################################################################
  #overview: function to determine the idxs of all hours belonging to each unique datetime "level". calls "build.hourly.datetime.features.fxn" to get a data.frame that indicates which "level" each year belongs to and then iterates over unique "levels" to determine all associated hour idxs.
  #args:
  #--"data.years": vector (length: variable) of integers indicating the calendar years of the data.
  #returns:
  #--"levels.idxs.list": list (length: n.levels) with number of elements equal to the number of unique datetime "levels" (i.e., 2 * 12 = 24 for weekday/weekend x month) and each named as the "level" it represents (i.e., "01-Weekday", "01-Weekend", "02-Weekday", ...). each element of the list is a vector containing all of the hour idxs corresponding to that level (e.g., "01-Weekday" contains the idxs of all hours corresponding to weekdays in each january of the data period).
  ################################################################################
  hourly.levels.df <- build.datetime.features.fxn(data.years)
  levels.idxs.list <- list()
  for (level in unique(hourly.levels.df$Level)) {
    levels.idxs.list[[level]] <- which(hourly.levels.df$Level == level)
  }
  return(levels.idxs.list)
}


get.all.day.type.hourly.idxs.fxn <- function(hourly.levels.idxs.list, day.type) {
  ################################################################################
  #overview: function to get the idxs of all hours corresponding to a particular "day.type" (weekday or weekend). takes in the output of "get.levels.hourly.idxs.fxn" and aggregates over months to obtain the desired result. this is needed to pool observations so as to fit a single daily average "x" vs daily average "y" model for each "day.type" level, rather than learning a different such model for each month of the year.
  #args:
  #--"hourly.levels.idxs.list": list (length: n.levels) of vectors containing integer idxs for hours belonging to each datetime "level".
  #--"day.type": string indicating which day type to pool hour idxs for (currently "Weekday" or "Weekend").
  #returns:
  #--"hourly.day.type.idxs": vector (length: variable) of integers corresponding to hour idxs of the specified day type.
  ################################################################################
  stopifnot(day.type %in% c("Weekday", "Weekend"))
  day.type.list.idxs <- grep(day.type, names(hourly.levels.idxs.list), fixed=T) #determine which elements of the list belong to this "day.type"
  hourly.day.type.idxs <- c()
  for (i in 1:length(day.type.list.idxs)) {
    list.idx <- day.type.list.idxs[i]
    hourly.day.type.idxs <- c(hourly.day.type.idxs, hourly.levels.idxs.list[[list.idx]])
  }
  return(hourly.day.type.idxs)
}


hourly.to.daily.idxs.fxn <- function(hourly.levels.idxs.list) {
  ################################################################################
  #overview: function determines which days in the overall series of days (i.e., across all data years) belong to each datetime "level" (currently month x weekday/weekend) so that separate intra-day patterns can be learned for each. it does this by mapping the hour idxs from each level of the output of "get.levels.hourly.idxs.fxn" to the corresponding day idxs (e.g., hours 1:24 are all mapped to day 1). the output of this function is used by "learn.intraday.y.fxn".
  #args:
  #--"hourly.levels.idxs.list": list (length: n.levels) of vectors, where each contains the idxs indicating which hours of the series belong to each datetime "level".
  #returns:
  #--"daily.levels.idxs.list": list (length: n.levels) of vectors, where each contains the unique day idxs indicating which days of the series belong to each datetime "level".
  ################################################################################
  daily.levels.idxs.list <- list()
  for (i in 1:length(hourly.levels.idxs.list)) {
    daily.levels.idxs.list[[i]] <- unique((hourly.levels.idxs.list[[i]] - 1) %/% 24 + 1) #map this level's hour idxs to unique corresponding day idxs
  }
  names(daily.levels.idxs.list) <- names(hourly.levels.idxs.list)
  stopifnot(sum(unlist(lapply(daily.levels.idxs.list, length))) == sum(unlist(lapply(hourly.levels.idxs.list, length)))/24) #integrity check
  return(daily.levels.idxs.list)
}


learn.intraday.y.fxn <- function(y.df, days.series, level.idxs.list, geog) {
  ################################################################################
  #overview: function to learn the average intra-day load pattern (i.e., the average offset from the daily average for each hour of the day) for each datetime "level" (currently month x weekday/weekend) for one geography. function gets called iteratively by the top-level function ("fit.geogs.fxn") to build this result for each geography.
  #args:
  #--"y.df": data.frame (size: (n.hours x n.geographies)) containing the outcome time series to model.
  #--"days.series": vector (length: n.hours) indicating the day idx that each hour corresponds to.
  #--"level.idxs.list": list (length: n.levels) output by hourly.to.daily.idxs.fxn indicating which hours belong to each datetime "level".
  #--"geog": string indicating the current geography. must be a column name of "y.df". 
  #returns:
  #--"hourly.load.offsets.df": data.frame (size: (24 x n.levels)) where each length-24 column contains the hourly y-value offsets to the corresponding level's daily average y-value. rows correspond to hour of day (e.g., row 1 is 12:00:00a-1:00:00a).
  ################################################################################
  geog.vec <- y.df[,colnames(y.df) == geog] #get this geog's hourly series
  geog.df <- data.frame(cbind(geog.vec, days.series)) #associate each observation with its day integer
  geog.daily.avg <- aggregate(geog.df, by=list(geog.df$days.series), mean) #calculate daily averages
  geog.daily.avg.rep <- rep(geog.daily.avg$geog.vec, each=24) #expand each daily average to 24 (hourly) observations
  geog.demean <- geog.vec - geog.daily.avg.rep #de-mean the hourly time series
  geog.demean.mat <- matrix(geog.demean, ncol=24, byrow=T) #reshape to matrix (n_days x 24), such that each row is one day
  hourly.offsets.df <- data.frame(matrix(0, nrow=24, ncol=length(level.idxs.list))) #object to hold geog's final results
  colnames(hourly.offsets.df) <- names(level.idxs.list)
  for (i in 1:length(level.idxs.list)) { #loop over "levels" and calculate hourly average offsets for each
    level.idxs <- level.idxs.list[[i]] #the days that belong to the current "level"
    hourly.offsets.df[,i] <- apply(t(geog.demean.mat)[,level.idxs], 1, mean) #subset matrix to current level and calculate hourly average values
  }
  return(hourly.offsets.df)
}


learn.avg.daily.x.vs.y.fxn <- function(y.df, x.df, days.series, geog, lam.mult=2, lam.mult.base=2, level.idxs, day.type) {
  ################################################################################
  #overview: function to fit a spline relating daily average temperature (from "x.df") to *average* daily average load (from "y.df") for a particular geography. this relationship is assumed to not depend on month. default behavior is for the fitting process to increase the spline stiffness (default: 2x) whenever there is more than one critical point in the spline predictions over the empirical temperature range; the goal of this requirement is to ensure that predictions are at least non-decreasing in extreme temperatures in both directions. this function is called by the top-level function ("fit.geogs.fxn") once for weekdays and once for weekends for each geography. default settings also double the initial spline stiffness as determined by LOOCV. this function will call "interpolate.fxn" if any x integers between the min and max are unobserved. 
  #args:
  #--"y.df": data.frame (size: n.hours x n.geogs) of dependent variable values (e.g. loads).
  #--"x.df": data.frame (size: n.hours x n.geogs) of independent variable values (e.g. temperatures).
  #--"days.series": vector (length: n.hours/24) mapping hours to days.
  #--"geog": string corresponding to a geography name; must be present in both "x.df" and "y.df".
  #--"lam.mult": real number used as multiplier to stiffen the spline fit as needed; must be >1 to ensure "convergence"; defaults to 2.
  #--"lam.mult.base": real number used as multiplier on the LOOCV-determined initial value of lambda; required to be ≥1; defaults to 2.
  #--"level.idxs": vector of integers (length depends on length of data period and value of slicer) representing all of the hour idxs that correspond to either weekdays or weekends.
  #--"day.type": character string indicating which model is being fit (used only for printing info to the console).
  #returns:
  #--"avg.daily.avg.df": data.frame (size: n.temperatures x 4) containing the average daily average "y" (e.g. load) as a function of daily average "x" (e.g. temperature) for one geography.
  ################################################################################
  ##input validation checks
  stopifnot(lam.mult.base >= 1) #no reason to loosen the spline relative to the LOOCV default
  stopifnot(lam.mult > 1) #otherwise will get stuck in a loop if not satisfying the conditional immediately
  ##data prep process
  #get geog vectors
  y.vec <- y.df[level.idxs, colnames(y.df) == geog]
  x.vec <- x.df[level.idxs, colnames(x.df) == geog]
  #associate each observation with corresponding day idx
  y.and.day.df <- data.frame(cbind(y.vec, days.series[level.idxs]))
  x.and.day.df <- data.frame(cbind(x.vec, days.series[level.idxs]))
  #calculate daily averages for each series
  daily.avg.y <- aggregate(y.and.day.df, by=list(y.and.day.df$V2), mean) #"V2" because variable name gets messed up because of the subset
  daily.avg.x <- aggregate(x.and.day.df, by=list(x.and.day.df$V2), mean) #"V2" because variable name gets messed up because of the subset
  #put together to get (rounded) average daily x vs average daily y
  daily.avg.x.y.df <- data.frame(cbind(round(daily.avg.x$x.vec), daily.avg.y$y.vec))
  colnames(daily.avg.x.y.df) <- c("TempInt", "Load")
  temp.counts <- table(daily.avg.x.y.df$TempInt) #count the incidence of each unique daily average temperature
  avg.avg.y.by.x.df <- aggregate(daily.avg.x.y.df$Load, by=list(daily.avg.x.y.df$TempInt), mean)
  colnames(avg.avg.y.by.x.df) <- c("TempInt", "AvgLoad")
  avg.avg.y.by.x.df$TempCounts <- temp.counts
  ##spline fitting process
  my.lam <- smooth.spline(x=avg.avg.y.by.x.df$TempInt, y=avg.avg.y.by.x.df$AvgLoad, w=avg.avg.y.by.x.df$TempCounts, cv=T)$lambda #get initial lambda via LOOCV
  fitted.spline <- smooth.spline(x=avg.avg.y.by.x.df$TempInt, y=avg.avg.y.by.x.df$AvgLoad, w=avg.avg.y.by.x.df$TempCounts, lambda=lam.mult.base*my.lam)
  if (length(rle(sign(diff(fitted.spline$y)))$lengths) > 2) { #if there is more than one critical point in the spline predictions over the empirical support
    mult.count <- 0
    while (length(rle(sign(diff(fitted.spline$y)))$lengths) > 2) {
      my.lam <- my.lam * lam.mult
      fitted.spline <- smooth.spline(x=avg.avg.y.by.x.df$TempInt, y=avg.avg.y.by.x.df$AvgLoad, w=avg.avg.y.by.x.df$TempCounts, lambda=my.lam)
      mult.count <- mult.count + 1
    }
    print(paste0(geog, " final mult.count for ", day.type, " model: ", mult.count))
  }
  avg.avg.y.by.x.df$SplinePred <- fitted.spline$y
  #interpolate as needed
  if (length(seq(min(avg.avg.y.by.x.df$TempInt), max(avg.avg.y.by.x.df$TempInt), 1)) > nrow(avg.avg.y.by.x.df)) {
    avg.avg.y.by.x.df <- interpolate.fxn(avg.avg.y.by.x.df, fitted.spline)
  }
  return(avg.avg.y.by.x.df)
}


interpolate.fxn <- function(preds.df, fitted.spline) {
  ################################################################################
  #overview: function generates spline predictions for any unobserved x-values between the min and max observed x-value for that geography. it is called by "learn.avg.daily.x.vs.y.fxn" as needed (i.e. as much as once per geography for each slice (e.g. weekdays/weekends)).
  #args:
  #--"preds.df": data.frame (size: as many rows as degrees empirically observed x 4 columns) with each row containing an x-value (integer), the empirical "average daily average" y-value, the observation counts of each x-value, and the spline prediction for that y-value. 
  #--"fitted.spline": the final fitted smoothing.spline for the given geography used to generate y-value predictions for unobserved x-values.
  #returns:
  #--"preds.df": data.frame (size: as many rows as integer degrees between the minimum and maximum observed temperature x 4 columns) representing the interpolated input data.frame (i.e., containing additional rows representing interpolated predictions for each unobserved integer x).
  ################################################################################
  temps.seq <- seq(preds.df[1,1], preds.df[nrow(preds.df),1], 1)
  missing.temps <- setdiff(temps.seq, preds.df$TempInt)
  interp.df <- data.frame(cbind(missing.temps), NA, 1, NA) #"1" instead of "0" here for the observation count (vs extrapolation)
  colnames(interp.df) <- colnames(preds.df)
  interp.df$SplinePred <- predict(fitted.spline, interp.df$TempInt)$y #make predictions for the missing temperatures
  preds.df <- rbind(preds.df, interp.df) #append interpolated rows
  preds.df <- preds.df[with(preds.df, order(TempInt, decreasing=F)),] #sort observations
  rownames(preds.df) <- 1:nrow(preds.df)
  return(preds.df)
}


extrapolate.fxn <- function(preds.df, degrees.cold, degrees.hot, degrees.to.fit) {
  ################################################################################
  #overview: function takes in the predictions.df and uses simple linear regression on each x-value "tail" to extend the temperature "support" by a desired number of degrees in either direction. this is used to help ensure the models are robust to temperatures "outside" of those historically experienced.
  #args:
  #--"preds.df": data.frame (size: as many rows as integer degrees between the minimum and maximum observed temperature x 4 columns) representing the input data.frame, previously interpolated as needed.
  #--"degrees.cold": integer indicating how many degrees to extrapolate the empirical support for cold temperatures for each geography.
  #--"degrees.hot" integer indicating how many degrees to extrapolate the empirical support for hot temperatures for each geography.
  #--"degrees.to.fit": integer indicating how many degrees to use when fitting the linear regression used to extrapolate the empirical support.
  #returns: 
  #--"preds.df": data.frame with extrapolated observations appended.
  #TODO: since for now i've decided to fit separate weekday/weekend splines for each month, in order to end up with the same "support" for both results (desirable though perhaps not crucial), this function could be modified to extrapolate out a different number of degrees hot or cold for either result. 
  #TODO: make it work with weights again?
  #TODO: re: calculation of "min.load.temp" what should happen when trough is an extreme? could restrict to a window around the midpoint x-value
  ################################################################################
  ##input robustness
  degrees.cold <- floor(degrees.cold)
  degrees.hot <- floor(degrees.hot)
  degrees.to.fit <- floor(degrees.to.fit)
  ##input validation
  stopifnot(degrees.cold >= 0) #negative values are meaningless
  stopifnot(degrees.hot >= 0) #negative values are meaningless
  stopifnot(degrees.to.fit >= 2) #two observations needed to fit a line (but more is probably better)
  #determine the geography-specific empirical range
  min.temp <- preds.df[1,"TempInt"]
  max.temp <- preds.df[nrow(preds.df),"TempInt"]
  #extend in both directions
  cold.extend.seq <- seq(min.temp-degrees.cold, min.temp-1, 1)
  cold.extend.df <- data.frame(cbind(cold.extend.seq, NA, 0, NA)) #"0" instead of "1" here for the observation count (vs interpolation)
  colnames(cold.extend.df) <- colnames(preds.df)
  hot.extend.seq <- seq(max.temp+1, max.temp+degrees.hot, 1)
  hot.extend.df <- data.frame(cbind(hot.extend.seq, NA, 0, NA)) #"0" instead of "1" here for the observation count (vs interpolation)
  colnames(hot.extend.df) <- colnames(preds.df)
  #combine up with preds.df
  preds.df <- rbind(cold.extend.df, preds.df, hot.extend.df) #add the placeholder extrapolation rows to preds.df
  #determine the lowest point of the "trough"
  min.load.temp <- preds.df[which(preds.df$SplinePred == min(preds.df$SplinePred, na.rm=T)),"TempInt"]
  #fit linear model to extrapolate cold temperatures
  cold.lm.df <- head(preds.df[which(!is.na(preds.df$SplinePred)),], degrees.to.fit) #grab the first "degrees.to.fit" non-NA observations
  cold.lm <- lm("SplinePred ~ TempInt", data=cold.lm.df)#, weights=cold.lm.df$TempCounts) #TODO: make weights work again (optionally)
  preds.df[1:degrees.cold,"SplinePred"] <- predict(cold.lm, data.frame(TempInt=cold.extend.seq)) #push extrapolated cold predictions
  #fit linear model to extrapolate hot temperatures
  hot.lm.df <- tail(preds.df[which(!is.na(preds.df$SplinePred)),], degrees.to.fit) #grab the last "degrees.to.fit" non-NA observations
  hot.lm <- lm("SplinePred ~ TempInt", data=hot.lm.df)#, weights=hot.lm.df$TempCounts) #TODO: make weights work again (optionally)
  preds.df[(nrow(preds.df)-degrees.hot+1):nrow(preds.df),"SplinePred"] <- predict(hot.lm, data.frame(TempInt=hot.extend.seq)) #push extrapolated hot predictions
  return(preds.df)
}


#' Fit adaptive spline models
#' 
#' This function loops over geographies and calls helper functions once for each geography to learn "daily average" and "intra-day" models. It saves all results in a list.
#' 
#' @param df.list list containing y.df (data.frame containing the dependent variable values) and x.df (data.frame containing the independent variable values).
#' @param data.years vector (length: variable) of integers indicating the calendar years of the data.
#' @param lam.mult a multiplier indicating how much to stiffen the smooth.spline in each iteration (unless the performance criterion is achieved).
#' @param lam.mult.base a multiplier indicating how much to stiffen the smooth.spline on the initial fit (with 1 translating to using the lambda found via leave-one-out cross-validation).
#' @param degrees.cold how many degrees to extrapolate the empirical support for cold temperatures for each geography.
#' @param degrees.hot how many degrees to extrapolate the empirical support for hot temperatures for each geography.
#' @param degrees.to.fit how many degrees to use when fitting the linear regression used to extrapolate the empirical support.
#' @return a list (length: 2) containing sublists holding each geography's "daily average" and "intra-day" models.
#' @export
fit.geogs.fxn <- function(df.list, data.years, lam.mult=2, lam.mult.base=2, degrees.cold=5, degrees.hot=5, degrees.to.fit=5) {
  ################################################################################
  #overview: top-level function that loops over geographies and calls helper functions once for each geography and saves their results in a multi-geography struct
  #args: 
  #--"df.list": list containing y.df (data.frame containing the dependent variable values) and x.df (data.frame containing the independent variable values).
  #--"data.years": sequence of integers indicating the span of the data. used to map observations to weekday/weekend for fitting separate models for those "levels". 
  #--"lam.mult": a multiplier indicating how much to stiffen the smooth.spline in each iteration (unless the performance criterion is achieved).
  #--"lam.mult.base": a multiplier indicating how much to stiffen the smooth.spline on the initial fit (with 1 translating to using the lambda found via LOOCV).
  #--"degrees.cold": how many degrees to extrapolate the empirical support for cold temperatures for each geography.
  #--"degrees.hot": how many degrees to extrapolate the empirical support for hot temperatures for each geography.
  #--"degrees.to.fit": how many degrees to use when fitting the linear regression used to extrapolate the empirical support (same for cold and hot extrapolations).
  #returns:
  #--a list of two lists: the first ("intra.day") contains the intra-day patterns for each geography -- each geography will have a df with shape (24 x n.levels) with each column representing the hour-of-day offsets from that level's daily average load. the second ("daily.avg") contains the overall relationship between x and y for each geography -- each geography will have a list containing as many data.frames as there are "slicers" (e.g. weekday/weekend); each data.frame will have shape (n.x-values x 4) with columns containing the empirical average y-value, the spline predicted y-value, and the number of observations at that x-value.
  ################################################################################
  #unpack df.list
  y.df <- df.list$y.df
  x.df <- df.list$x.df
  #hours to days mapper
  days.series <- rep(seq(1, nrow(y.df)/24, by=1), each=24) #assumes all series are in local time
  #create objects to store results
  intra.day.patterns.list <- list()
  daily.load.vs.temp.list <- list()
  #get the multi-year month idxs object
  hourly.level.idxs.list <- get.levels.hourly.idxs.fxn(data.years)
  daily.level.idxs.list <- hourly.to.daily.idxs.fxn(hourly.level.idxs.list) #map hours to days (e.g. hours 1:24 -> day 1)
  weekday.idxs <- get.all.day.type.hourly.idxs.fxn(hourly.level.idxs.list, "Weekday")
  weekend.idxs <- get.all.day.type.hourly.idxs.fxn(hourly.level.idxs.list, "Weekend")
  stopifnot(length(intersect(weekday.idxs, weekend.idxs)) == 0) #check integrity
  stopifnot((sum(sort(union(weekday.idxs, weekend.idxs)) == seq(1, nrow(x.df), 1)) == nrow(x.df)) == TRUE) #check integrity
  #loop over geographies and fit the models
  for (i in 1:length(colnames(y.df))) {
    geog <- colnames(y.df)[i]
    ##build intra-day patterns for all levels (currently month x weekday/weekend)
    intra.day.patterns.list[[geog]] <- learn.intraday.y.fxn(y.df, days.series, daily.level.idxs.list, geog) #fxn loops over all "levels" already
    ##build spline fits of daily average temperature vs daily average load
    #empirical fits
    preds.df.weekday <- learn.avg.daily.x.vs.y.fxn(y.df, x.df, days.series, geog, lam.mult, lam.mult.base, weekday.idxs, "weekday")
    preds.df.weekend <- learn.avg.daily.x.vs.y.fxn(y.df, x.df, days.series, geog, lam.mult, lam.mult.base, weekend.idxs, "weekend")
    #determine how much the supports need to be extended to make them consistent
    cold.gap.weekday <- max(0, preds.df.weekday[1,"TempInt"] - preds.df.weekend[1,"TempInt"])
    cold.gap.weekend <- max(0, preds.df.weekend[1,"TempInt"] - preds.df.weekday[1,"TempInt"])
    hot.gap.weekday <- max(0, preds.df.weekend[nrow(preds.df.weekend),"TempInt"] - preds.df.weekday[nrow(preds.df.weekday),"TempInt"])
    hot.gap.weekend <- max(0, preds.df.weekday[nrow(preds.df.weekday),"TempInt"] - preds.df.weekend[nrow(preds.df.weekend),"TempInt"])
    #extrapolate (incorporating the "gaps" calculated above)
    daily.load.vs.temp.list[[geog]] <- list()
    daily.load.vs.temp.list[[geog]]$Weekday <- extrapolate.fxn(preds.df.weekday, (degrees.cold+cold.gap.weekday), (degrees.hot+hot.gap.weekday), degrees.to.fit)
    daily.load.vs.temp.list[[geog]]$Weekend <- extrapolate.fxn(preds.df.weekend, (degrees.cold+cold.gap.weekend), (degrees.hot+hot.gap.weekend), degrees.to.fit)
  }
  models.list <- list(intra.day=intra.day.patterns.list, daily.avg=daily.load.vs.temp.list)
  return(models.list)
}
