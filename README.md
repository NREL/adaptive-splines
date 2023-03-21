
# Adaptive Splines for Prediction (ASP)
A package for easy learning of univariate relationships in R using splines.

![Example model fits](example.png?raw=true "Example model fits")

## 1. Overview
This package provides functionality to fit a piecewise cubic spline to model the relationship between a univariate `X` time series and a univariate `Y` time series. The output is akin to linear regression but has higher "capacity" in that it can learn more complicated relationships despite leveraging only a single predictor (independent variable) time series. It thus can learn very reasonable relationships between `X` and `Y` with minimal effort. The package also provides functionality for using the fitted spline models to make predictions based on new `X` data. Examples of model fitting and prediction workflows are provided in the [usage](#4-usage) section.

The canonical intended use case is for predicting electric demand (`Y`) from temperature (`X`). There can be complicated, non-linear relationships between these two variables, but these can be reasonably approximated by the sum of: 1) an "aggregate" relationship between daily average temperature and daily average demand, and 2) intra-day patterns represented as hourly offsets from the daily average load. The spline functionality is used for learning a robust "aggregate" relationship without the inconvenience of manual feature engineering. More details on the modeling approach are provided in the [modeling details](#7-modeling-details) section. 

The model fitting workhorse is R's built-in `smooth.spline`. This package provides a suite of options for controlling how `smooth.spline` fits a spline to the data. In particular, it is designed to return a model with a single critical point (i.e., a single location along the support where the first derivative is zero). This is to ensure that changes in predicted electric demand are always positive as temperatures become more extreme. This helps to prevent overfitting, particularly when the input data has few observations or has significant uncertainty from other sources (e.g., small geographies).


## 2. Installation
* [Install R](https://www.r-project.org) (and, optionally, [RStudio](https://www.rstudio.com/products/rstudio/download/)) on your machine.
* In your preferred R environment, install `devtools` via: `install.packages("devtools")`.
* In your preferred R environment, install this package via: `devtools::install_github(repo="NREL/adaptive-splines")`.


## 3. Prepare your project directory
* Create a project directory where you will perform your analysis.
* In your project directory, create a `data` directory with `x` and `y` sub-directories, a `fits` directory, and a `plots` directory.
* Place your input `X` and `Y` data into the relevant `data` sub-directories (i.e., `data/x` and `data/y`). See the [input data requirements](#5-conventions-and-requirements-on-input-files) section for details.


## 4. Usage
Perform your desired analysis by loading the package, setting R's working directory to your project directory, defining your input data parameters, and then running the top-level functions exposed to the user as shown below. Separate code snippets are provided to demonstrate model fitting functionality and prediction functionality. See the [input data requirements](#5-conventions-and-requirements-on-input-files) and [explanation of user-accessible settings](#6-explanation-of-user-accessible-settings) sections for further details. 

### Model fitting
Model fitting should be performed using one or (ideally) multiple full years of `X` and `Y` data. 
```{r}
#load the package
library(adaptivesplines)

#set working directory
setwd("/path/to/project")

#define data loading args for model fitting (modify to fit your data, but note that 'yyyy' is required)
x.root <- "hourly_temperature_by_geog_yyyy.csv"
y.root <- "hourly_load_by_geog_yyyy.csv"
data.years <- 2007:2013

#load data
df.list <- load.and.check.data.fxn(x.root, y.root, data.years)

#fit models
results.list <- fit.geogs.fxn(df.list, data.years) #optional args: c(lam.mult, lam.mult.base, degrees.cold, degrees.hot, degrees.to.fit)

#plot modeled "aggregate" relationships
plot.results.fxn(results.list$daily.avg, data.years) #optional args: c(plot.weekdays, plot.weekends)

#write out models
write.results.fxn(results.list) #optional args: c(write.daily.avg, write.intra.day)
```

### Prediction
The prediction workflow can be run immediately after fitting the models (in which case the in-memory `results.list` can be used directly) or in a separate R session leveraging models previously written to CSV. In contrast to the model fitting workflow, the expected use case for prediction is to load one or more days of forecast `X` observations and use the fitted models to predict each corresponding `Y` value. Note that `first.date` is used both to read in the correct array of forecast `X` observations and to determine the appropriate models to employ in generating each hour's predictions. 
```{r}
#load the package
library(adaptivesplines)

#set working directory
setwd("/path/to/project")

#define data loading args for forecasting (modify to fit your data, but note that 'yyyymmdd' is required)
x.preds.root <- "hourly_temperature_by_geog_yyyymmdd.csv"
first.date <- "20500101" #format as 'yyyymmdd'

#load forecast data
x.preds.df <- load.data.fxn("x", x.preds.root, first.date, patt="yyyymmdd")

#make predictions (two approaches available, choose one)
y.preds.df <- predict.fxn(x.preds.df, first.date) #from CSV
y.preds.df <- predict.fxn(x.preds.df, first.date, from.csv=F, fitted.models=results.list) #from memory

#write out predictions
write.preds.fxn(y.preds.df, first.date)
```

The prediction workflow can be readily looped. Each iteration of the loop will result in a set of predictions written to CSV. 
```{r}
#loop example
first.dates <- c("20500101", "20500102", "20500103")

for (first.date in first.dates) {
  print(first.date)
  x.preds.df <- load.data.fxn("x", x.preds.root, first.date, patt="yyyymmdd")
  y.preds.df <- predict.fxn(x.preds.df, first.date)
  write.preds.fxn(y.preds.df, first.date)
}
```


## 5. Requirements on input files
All `Y` arrays should retain their true weekday/weekend/holiday schedule rather than being adjusted to that of a "target year" (if any). This is true regardless of whether the values in the arrays have been normalized (e.g., to account for changes in infrastructure over time), which is the typical load modeling use case at NREL.

In addition, input files are required to satisfy the following:
* All `X` and `Y` files used in model fitting represent full calendar years of hourly data with size (8760 x `n.geographies`), excluding row names and column names; `X` files used for prediction can contain any number of complete days of hourly data.
* All arrays have row names (ideally datetimes) and column names (identifying geographies). 
* All time series are in the local time zone of each respective geography.
* Row names must be consistent across the `X` and `Y` data used for model fitting.
* Only the intersection of the `X` and `Y` column names will be retained for model fitting; similarly, only the intersection of column names in the forecast `X` data and the fitted models will be retained for prediction.
* The three sets of input data files are each named in an internally consistent manner, differing only in their dates (with the date placeholder taking the form of "yyyy" for model fitting and "yyyymmdd" for prediction).


## 6. Explanation of function arguments
### Model fitting workflow
#### Data loading (`load.and.check.data.fxn`)
* `x.root`: string indicating the structure of the filenames for the independent variable `X` (e.g., "hourly_temperature_by_geog_yyyy.csv"). Note the "yyyy" which is a placeholder for the four-digit year in each filename.
* `y.root`: string indicating the structure of the filenames for the dependent variable `Y` (e.g., "hourly_load_by_geog_yyyy.csv"). Note the "yyyy" which is a placeholder for the four-digit year in each filename.
* `data.years`: vector of integers indicating the calendar years of the data. 

#### Model fitting (`fit.geogs.fxn`)
* `df.list`: list containing the arrays generated by `load.and.check.data.fxn`.
* `data.years`: described above.
* `lam.mult` (optional): a multiplier (`>1`) indicating how much to stiffen the `smooth.spline` in each fitting iteration (default: `2`).
* `lam.mult.base` (optional): a multiplier (`≥1`) indicating how much to stiffen the `smooth.spline` on the initial fit, with `1` corresponding to the lambda obtained via leave-one-out cross-validation (default: `2`).
* `degrees.cold` (optional): how many degrees to extrapolate the empirical support for cold temperatures (default: `5`).
* `degrees.hot` (optional): how many degrees to extrapolate the empirical support for hot temperatures (default: `5`).
* `degrees.to.fit` (optional): how many degrees to use when fitting the linear regressions used to extrapolate the empirical support (default: `5`).

#### Plotting (`plot.results.fxn`)
* `daily.average.list`: list containing the "daily average" model fits; a sub-list of what is generated by `fit.geogs.fxn`.
* `data.years`: described above.
* `plot.weekdays` (optional): whether to plot the "weekday" model fits (default: `TRUE`).
* `plot.weekends` (optional): whether to plot the "weekend" model fits (default: `TRUE`).

#### Model writing (`write.results.fxn`)
* `results.list`: the output generated by `fit.geogs.fxn`. 
* `write.daily.avg` (optional): whether to write out the "daily average" model fits (default: `TRUE`).
* `write.intra.day` (optional): whether to write out the "intra-day" model fits (default: `TRUE`).

### Prediction workflow
#### Data loading (`load.data.fxn`)
* `x.preds.root`: string indicating the structure of the filenames for forecast values of the independent variable `X` (e.g., "hourly_temperature_by_geog_yyyymmdd.csv"). Note the "yyyymmdd" which is a placeholder for the first prediction date contained in the file.
* `first.date`: the first day to forecast, specified as "yyyymmdd".
* `patt`: string indicating the structure of the date in the input filepath; must match the structure of `first.date` (i.e., "yyyymmdd"). 

#### Making predictions (`predict.fxn`)
* `x.preds.df`: data frame of forecast `X` values generated by `load.data.fxn`.
* `first.date`: described above.
* `from.csv` (optional): whether to read the fitted models from CSV files (default: `TRUE`).
* `fitted.models` (optional): list containing the fitted models; required when `from.csv` is `FALSE` (default: `NULL`).

#### Writing predictions (`write.preds.fxn`)
* `y.preds.df`: data frame of `Y` predictions generated by `predict.fxn`.
* `first.date`: described above.


## 7. Modeling details
### Daily average model
This component learns the "aggregate" relationship between `X` (e.g., temperature) and `Y` (e.g., electric demand) via R's built-in `smooth.spline` using all provided years of input data as defined by `x.root`, `y.root`, and `data.years`. This relationship is learned separately for weekdays and weekends since electric demand is typically higher on weekdays. If a given geography's spline has to be stiffened in order to satisfy the stopping criterion, its final "multiplier count" will be printed to the console. Each geography ends up with a daily average `Y` prediction for each integer of corresponding daily average `X` value spanning its historical range (plus predictions for extrapolated daily average `X` values beyond the historical range). Weekdays and weekends are written out as separate CSV files, each with as many rows as unique temperatures (including extrapolated temperatures, plus a row for the column names indicating the geography) and as many columns as geographies (plus a column for the row names, indicating the temperature). `NA` is used to populate cells where a given geography did not experience a given temperature. 

### Intra-day model
This component learns separate models of the typical hour-of-day offset for `Y` for each calendar month x weekday/weekend "level" (i.e., 24 different models) via simple averaging. Weekdays and weekends are written out as separate CSV files, each with 288 (=12*24) rows (plus a row for column names indicating the geography), where the first 24 rows represent the hourly offsets for January, and as many columns as geographies (plus a column for the row names, indicating the hour-of-day x month offset).


## 8. Troubleshooting
Guidance for warnings and errors users may encounter:
* `spar-finding: non-finite value inf; using BIG value`: Indicates that `smooth.spline` is bounding non-finite values to large finite values for safety when fitting the spline model. This is not an error and a fitted model should still be obtained for the affected geography. Best practice is to view the plot for the affected geography to confirm that a reasonable model has been learned. Note that additional years of data may improve model fit.


## 9. References
Shalizi, C. (n.d.). Splines. Chapter 7 in [_Advanced Data Analysis from an Elementary Point of View_](https://www.stat.cmu.edu/~cshalizi/ADAfaEPoV/ADAfaEPoV.pdf).


## 10. Acknowledgements
Funding for initial capability development provided by the U.S. Department of Energy (DOE) Office of Electricity. Funding for package creation provided by DOE Solar Energy Technology Office. 

Comments are welcome; please [email me](mailto:sinnott.murphy@nrel.gov?subject=Adaptive%20Splines%20package). 


## To do
* Treat federal holidays as weekends
* Handle leap years
