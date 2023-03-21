#functions for visualizing results

size.plots.fxn <- function(n.geogs) {
  ################################################################################
  #overview: function to size the plot. it figures out dimensions that are sufficient to plot all geographies with the smallest "perimeter" (i.e., shape as close to that of a square as possible) and breaks ties with the option that results in the fewest extra plot boxes.
  #args:
  #--"n.geogs": the number of geographies.
  #returns: 
  #--list with the selected number of rows and columns to be used in plot creation.
  ################################################################################
  n.rowss <- n.colss <- c() #initialize objects
  ceil.root <- ceiling(sqrt(n.geogs)) #define maximum size (e.g. if n.geogs is 8, 3x3 is the max plot dimension that needs to be considered)
  for (n.rows in 1:ceil.root) {
    n.colss <- c(n.colss, ceiling(n.geogs / n.rows))
    n.rowss <- c(n.rowss, n.rows)
  }
  sums <- n.rowss + n.colss
  keep.idx <- which(sums == min(sums)) #find the dimensions with the smallest "perimeter"
  if (length(keep.idx) > 1) { #if a tie
    prods <- sapply(1:length(n.rowss), FUN=function(x) {n.rowss[x] * n.colss[x]})
    keep.idx <- keep.idx[which.min(prods[keep.idx])] #find the dimensions resulting with the smallest number of empty plot boxes
  }
  return(list(n.rows=n.rowss[keep.idx], n.cols=n.colss[keep.idx]))
}


get.consistent.xlim.fxn <- function(daily.avg.list) {
  ################################################################################
  #overview: function to get consistent x-axis limits for all of the plots (recycles the first part of "write.daily.avg.results.fxn"). includes extrapolated temperatures.
  #args:
  #--"daily.avg.list": list containing each geog's weekday and weekend "daily average" fitted models.
  #returns:
  #--a list containing the min and max x-value for both weekday and weekend "daily average" fitted models.
  ################################################################################
  max.max.weekday <- max.max.weekend <- min.min.weekday <- min.min.weekend <- 0 #initialize values
  for (i in 1:length(daily.avg.list)) {
    if (daily.avg.list[[i]]$Weekday[1,1] < min.min.weekday) {min.min.weekday <- daily.avg.list[[i]]$Weekday[1,1]}
    if (daily.avg.list[[i]]$Weekday[nrow(daily.avg.list[[i]]$Weekday),1] > max.max.weekday) {max.max.weekday <- daily.avg.list[[i]]$Weekday[nrow(daily.avg.list[[i]]$Weekday),1]}
    if (daily.avg.list[[i]]$Weekend[1,1] < min.min.weekend) {min.min.weekend <- daily.avg.list[[i]]$Weekend[1,1]}
    if (daily.avg.list[[i]]$Weekend[nrow(daily.avg.list[[i]]$Weekend),1] > max.max.weekend) {max.max.weekend <- daily.avg.list[[i]]$Weekend[nrow(daily.avg.list[[i]]$Weekend),1]}
  }
  weekday.xlim.list <- list(x.lower=min.min.weekday, x.upper=max.max.weekday)
  weekend.xlim.list <- list(x.lower=min.min.weekend, x.upper=max.max.weekend)
  return(list(Weekday=weekday.xlim.list, Weekend=weekend.xlim.list))
}


plot.spline.fits.fxn <- function(daily.average.list, plot.dims, xlims.list, data.years, day.type) {
  ################################################################################
  #overview: function to generate the plots.
  #args:
  #--"daily.average.list": list containing each geog's weekday and weekend "daily average" fitted models.
  #--"plot.dims": list containing plot dimensions (nrows and ncols).
  #--"xlims.list": list containing the min and max x-axis limits for consistent plotting.
  #--"data.years": numeric vector indicating the years of the data.
  #--"day.type": string ("Weekday" or "Weekend") used to determine which x-axis limits and fitted models to use when plotting.
  #returns:
  #--none
  ################################################################################
  ##process args
  #obtain plot dims
  n.rows <- plot.dims[["n.rows"]]
  n.cols <- plot.dims[["n.cols"]]
  #obtain relevant x-axis dimensions for day.type
  x.lower <- xlims.list[[day.type]][["x.lower"]]
  x.upper <- xlims.list[[day.type]][["x.upper"]]
  #obtain data span
  first.year <- sort(data.years)[1]
  last.year <- sort(data.years)[length(data.years)]
  ##make plot
  png(filename=paste0("plots/daily.average.", tolower(day.type), ".png"), units="in", res=300, width=2*n.cols, height=2*n.rows)
  par(oma=c(.2,1,2.5,.2))
  par(mar=c(1,1,1,1))
  par(mfrow=c(n.rows,n.cols))
  for (i in 1:length(daily.average.list)) {
    geog.name <- names(daily.average.list)[i]
    plot(daily.average.list[[i]][[day.type]]$TempInt, daily.average.list[[i]][[day.type]]$SplinePred, xlab="", ylab="", type="l", main=geog.name, xaxt="n", xlim=c(x.lower, x.upper))
    points(daily.average.list[[i]][[day.type]]$TempInt, daily.average.list[[i]][[day.type]]$AvgLoad, col="red", pch=20)
  }
  title(paste0("Empirical (red dots) vs modeled (black lines) relationships by geography; modeled includes extrapolated values (", first.year, "-", last.year, ")"), outer=T, line=0.5, font.lab=2, cex.main=2.2)
  dev.off()
}


#' Plot "daily average" model fits for each geography
#' 
#' This function plots the "weekday" and "weekend" "daily average" spline predictions against the empirical values to allow the user to assess goodness of fit. Automatically determines reasonable plot dimensions and plots each geography on consistent x-axis limits.
#' 
#' @param daily.average.list list of lists containing fitted "daily average" models for each geography.
#' @param data.years numeric vector indicating the years of the data.
#' @param plot.weekdays boolean indicating whether to plot the "weekday" models.
#' @param plot.weekends boolean indicating whether to plot the "weekend" models.
#' @export plot.results.fxn
plot.results.fxn <- function(daily.average.list, data.years, plot.weekdays=TRUE, plot.weekends=TRUE) {
  ################################################################################
  #overview: top-level function to plot results to evaluate goodness of fit.
  #args:
  #--"daily.average.list": list of lists containing fitted "daily average" models (weekday and weekend) for each geography.
  #--"data.years": numeric vector indicating the years of the data.
  #--"plot.weekdays": boolean indicating whether to plot the "weekday" models.
  #--"plot.weekends": boolean indicating whether to plot the "weekend" models.
  #returns:
  #--none
  ################################################################################
  dims.list <- size.plots.fxn(length(daily.average.list))
  xlims.list <- get.consistent.xlim.fxn(daily.average.list)
  if (plot.weekdays) {plot.spline.fits.fxn(daily.average.list, dims.list, xlims.list, data.years, "Weekday")}
  if (plot.weekends) {plot.spline.fits.fxn(daily.average.list, dims.list, xlims.list, data.years, "Weekend")}
}

