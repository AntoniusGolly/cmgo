#' @section Time series analyses:
#' The package cmgo can handle time series of channel geometries and offers the opportunity
#' to compare them. To do this, simply put the an input file for each data set in the input directory
#' (see \code{\link[=CM.ini]{CM.ini()}}). The function will create a data object for each file in the
#' global data object under, \code{cmgo.obj$data$set1}, \code{cmgo.obj$data$set2}, \code{cmgo.obj$data$set3}, etc.
#' All functions will iterate over the data sets automatically.
#'
#' If you want to address via a variable use \code{cmgo.obj$data[[set]]}, where is a string of the data set,
#' e.g. \code{"set1"}. The order of data sets will be determined by the filenames. So, make sure to name the files
#' accordingly, e.g. \code{"channelsurvey_a.csv"},\code{"channelsurvey_b.csv"}. The mapping of the filenames to
#' data sets will be printed to the console. Also, you can view the file names belonging to a data set with
#' \code{print(cmgo.obj$data[[set]]$filename)}.
#'
#' \strong{Reference centerline}\cr
#' The channel metrics are calculated based on a centerline. Normally, for a river plan geometry one centerline exists
#' and this is the basis for the metrics. However, when there are multiple time lines two options exist.
#' Metrics are either calculated for each channel geometry individually. This way you have the most accurate
#' representation of the channel metrics for that channel observation. For example, channel width is most
#' accurately measured. However, different time series of observations are difficult to compare since the basis
#' for the calculations -- the centerlines -- differ. Thus, when comparing time series, a second approach exists
#' where you can determine a reference centerline for all metrics calculations. To do this set: \preformatted{
#' cmgo.obj$par$centerline.use.reference = TRUE
#' cmgo.obj$par$centerline.reference = "set1"
#' }
#' Now, all metrics for the different bank surveys will be calculated based on the centerline
#' of the data set "set1". Use this option only if your bank surveys differ only slightly. Otherwise, the
#' calculated channel metrics might not be representative (see Fig. 10).
#'
#' \figure{10_documentation.png}{options: width="800px" alt="Figure: reference centerline"}
#'
#' \emph{Figure 10: For channel geometries that differ drastically, the usage of a reference centerline
#' is not advised. The centerline of a data set (blue line) is not useful for calculating the metrics
#' of the dashed channel geometry.}
#'
#'
