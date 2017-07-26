#' Run the full stack of the main cmgo functions
#'
#' This function is a wrapper function for the main functions of the package \code{cmgo}. With no parameters
#' passed, it initializes the global data object containing the demo data set and the default parameters.
#' It returns the global data object - as all main function do - which can be used for further execution
#' of the program. If your global data object already exists, you can pass this to \code{CM.run()} to execute
#' all main functions at once. Alternatively, you can call \code{CM.run()} with a parameter object or a file
#' name of a parameter configuration (see \code{\link[=CM.par]{CM.par()}} for further information.)
#'
#' CM.run represents a wrapper function of the main \code{cmgo} functions with the following code:
#' \preformatted{
#' CM.ini()
#' CM.generatePolygon()
#' CM.calculateCenterline()
#' CM.processCenterline()
#' CM.writeData()
#' CM.plotPlanView()
#' }
#'
#' You can use \code{CM.run()} either for demo purposes (just call CM.run() without parameters) or to start
#' a new project from scratch. To do this, make sure you have read about the input data preparation in the
#' documentation of \code{\link[=CM.ini]{CM.ini()}}. If you are familiar with the input data preparation
#' just call CM.run() while you place your valid input files to the specified input directory (defaults to \code{"./input"}).
#'
#' @param object Possible values include \code{NULL} and the global data object.
#' @param par Possible values include \code{NULL}, filename or list of parameters. If \code{par} is not specified or
#' NULL the default parameters are used (see documentation of CM.par() to learn about the default parameters). If \code{par}
#' is a filename CM.ini() will try to open that file and look for a list \code{par} in that file. In case of success the parameters
#' from that file are loaded and merged with the default parameters. Merging means, not all parameters have to be defined in the parameter
#' file. Parameters that are not defined are taken from the default. In case \code{par} is a list, the default parameters are merged with this
#' list. That means, if the actual parameters are handed over to CM.ini() - e.g. \code{global_data_object$par} - the parameters will be passed
#' through.
#' @return The global data object containing data ($data) and parameters ($par). The global data object must be passed to all main functions of \code{cmgo}.
#' @author Antonius Golly
#' @examples
#'
#' # example 1: open with demo data and default parameters
#' cmgo.obj = CM.run()
#'
#' # example 2: re-create global data object with updated parameters
#' #parameter_file = "par/new_config.r" # specify an existing parameter file
#' parameter_file = NULL
#' cmgo.obj = CM.run(cmgo.obj, parameter_file)
#'
#' @export CM.run
CM.run <- function(object = NULL, par = NULL){

  #par  = if(is.null(par)) object$par else CM.par(par)
  #data = object$data


  ### define parameters
  par  = CM.par(par)


  ### get data
  cmgo.obj = CM.ini(object, par)


  ### generate polygon
  cmgo.obj = CM.generatePolygon(cmgo.obj)


  ### get centerline (calculate or load)
  cmgo.obj = CM.calculateCenterline(cmgo.obj)


  ### process centerline (calculate width)
  cmgo.obj = CM.processCenterline(cmgo.obj)


  ### write data to workspace and export data to csv-files (see par)
  CM.writeData(cmgo.obj)


  ### plot plan view
  plot.par = CM.plotPlanView(cmgo.obj, "set2", cl="cl1")

  cat("\n--> ChannelMetrics ended successfully!\n")

  return(cmgo.obj)

}

