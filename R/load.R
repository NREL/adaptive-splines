#functions for loading and preparing data and previously fitted models

#' Load data
#' 
#' This function reads in `X` or `Y` data, one type at a time.
#' @param type string (either "x" or "y"); used to determine the full read path of input data files.
#' @param root string indicating the structure of the filenames, including the extension (".csv" is expected), but with the characters indicating the date (e.g., 4-digit year) replaced by a compatible pattern as provided by "patt" (e.g., "yyyy"). filename structure is assumed consistent across files.
#' @param file.dates vector (length: variable) of integers indicating the dates of the data.
#' @return a single data.frame (size: n.hours x n.geographies) that combines all data specified by the input args.
#' @export
load.data.fxn <- function(type, root, file.dates, patt="yyyy") {
  ################################################################################
  #overview: function to read in desired periods of "x" (e.g., temperature) and "y" (e.g., load) data, one type at a time. function expects colnames and rownames to be present on all arrays. rownames are ignored during data loading (i.e., each array is expected to be correctly ordered temporally) but column names are checked to ensure that data files are combined correctly. called by "load.and.check.data.fxn" and also called directly to load data for predictions.
  #args:
  #--"type": string (either "x" or "y"); used to determine the full read path of input data files.
  #--"root": string indicating the structure of the filenames, including the extension (".csv" is expected), but with the characters indicating the date (e.g., 4-digit year) replaced by a compatible pattern as provided by "patt" (e.g., "yyyy"). filename structure is assumed consistent across files.
  #--"file.dates": vector (length: variable) of integers indicating the dates of the data.
  #--"patt": string indicating the date pattern of the files -- e.g., "yyyy" for annual files (typical case for model fitting) or "yyyymmdd" for sub-annual files containing one or more days (typical case for prediction).
  #returns:
  #--"df": a single data.frame (size: n.hours x n.geographies) that combines all data specified by the input args.
  ################################################################################
  file.dates <- sort(file.dates)
  for (i in 1:length(file.dates)) {
    file.date <- file.dates[i]
    filename <- gsub(pattern=patt, fixed=TRUE, x=root, replacement=file.date)
    if (file.date == file.dates[1]) {
      df <- read.csv(paste0("data/", type, "/", filename), check.names=F)
      rownames(df) <- df[,1]
      df <- df[,-1]
      df.colnames <- colnames(df)
    }
    else {
      temp.df <- read.csv(paste0("data/", type, "/", filename), check.names=F)
      rownames(temp.df) <- temp.df[,1]
      temp.df <- temp.df[,-1]
      stopifnot(length(intersect(colnames(temp.df), df.colnames)) == length(df.colnames)) #geography sets must match across files
      if (sum(colnames(temp.df) == df.colnames) < length(df.colnames)) {temp.df <- temp.df[,df.colnames]} #reorder columns as necessary
      df <- rbind(df, temp.df)
    }
    if ((nrow(df) / 8760 != i) & (patt == "yyyy")) {warning("non-8760 data year present")}
  }
  return(df)
}


ensure.compatible.arrays.fxn <- function(x.df, y.df) {
  ################################################################################
  #overview: function to ensure the "x.df" and "y.df" represent the same geographies and time periods. it takes in both data.frames and keeps only the intersection of column names (geographies). it errors out if the row names are not consistent across the two arrays. it warns if the number of rows is not a multiple of 24 (indicating partial days). called by "load.and.check.data.fxn".
  #args:
  #--"x.df": data.frame (size: n.hours x n.geographies) containing observations for the independent variable (e.g., temperature).
  #--"y.df": data.frame (size: n.hours x n.geographies) containing observations for the dependent variable (e.g., load). 
  #returns:
  #--"x.y.list": list (length: 2) containing possibly updated "x.df" and "y.df" arrays.
  ################################################################################
  ##ensure geographies are consistent
  mutual.geogs <- intersect(colnames(x.df), colnames(y.df))
  x.df <- x.df[,mutual.geogs]
  y.df <- y.df[,mutual.geogs]
  ##ensure consistent rows (~timestamps)
  stopifnot(all(rownames(x.df) == rownames(y.df)))
  if (nrow(x.df) %% 24 != 0) {warning("partial days identified (nrow(x.df) not a multiple of 24)")}
  x.y.list <- list(x.df=x.df, y.df=y.df)
  return(x.y.list)
}


#' Load and ensure consistency of data
#' 
#' This function reads in the `X` and `Y` data and ensures consistency. It calls `load.data.fxn` twice (once to build the `x.df` and once to build the `y.df`) and then calls `ensure.compatible.arrays.fxn` to make sure `x.df` and `y.df` are consistent.
#' 
#' @param x.root string indicating the structure of the filenames for the independent variable. 
#' @param y.root string indicating the structure of the filenames for the dependent variable.
#' @param file.dates vector (length: variable) of integers indicating the dates of the data.
#' @return a list (length: 2) containing final `x.df` and `y.df` arrays.
#' @export
load.and.check.data.fxn <- function(x.root, y.root, file.dates) {
  ################################################################################
  #overview: top-level function to read in the data and ensure consistency. calls "load.data.fxn" twice (once to build the "x.df" and once to build the "y.df") and then calls "ensure.compatible.arrays.fxn" to make sure "x.df" and "y.df" are consistent.
  #args:
  #--"x.root": string indicating the structure of the filenames for the independent variable. see description of "root" in "load.data.fxn" for more detail.
  #--"y.root": string indicating the structure of the filenames for the dependent variable. see description of "root" in "load.data.fxn" for more detail.
  #--"file.dates": vector (length: variable) of integers indicating the dates of the data.
  #returns: 
  #--"x.y.list": list (length: 2) containing final "x.df" and "y.df" arrays.
  ################################################################################
  #build x.df and y.df
  x.df <- load.data.fxn("x", x.root, file.dates)
  y.df <- load.data.fxn("y", y.root, file.dates)
  #ensure consistency
  x.y.list <- ensure.compatible.arrays.fxn(x.df, y.df)
  return(x.y.list)
}


load.model.csv.fxn <- function(model.type, day.type) {
  ################################################################################
  #overview: function to load the fitted models from CSV files back into working memory for making predictions.
  #args:
  #--"model.type": string indicating which model type should be loaded; options are "daily.avg" or "intraday".
  #--"day.type": string indicating which model datetime type should be loaded; options are "weekday" or "weekend".
  #returns:
  #--"model.df": data.frame containing selected fitted model component; if model.type is "daily.avg", dims will be (n.unique.x.vals [for the given day.type, including extrapolated values] x n.geogs); if model.type is "intraday", dims will be (24*n.months x n.geogs).
  ################################################################################
  stopifnot(model.type %in% c("daily.avg", "intraday"))
  stopifnot(day.type %in% c("weekday", "weekend"))
  model.df <- read.csv(paste0("fits/", model.type, ".", day.type, ".csv"), check.names=F)
  rownames(model.df) <- model.df[,1]
  model.df <- model.df[,-1]
  return(model.df)
}
