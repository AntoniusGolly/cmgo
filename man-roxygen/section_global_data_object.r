#' @section General information on the global data object:
#' All the data and parameters used in \code{cmgo} are stored in one variable of
#' \href{http://www.r-tutor.com/r-introduction/list}{type list}: the global data object,
#' in the following examples named \code{cmgo.obj}. Its structure is:
#' \preformatted{cmgo.obj = list(
#'   data = list()  # the data set(s), different surveys of the channel
#'     set1 = list(), # survey 1
#'     set2 = list()  # survey 2
#'     # ...
#'   ),
#'   par  = list() # all plotting and model parameters
#' )}
#'
#' The global data object then has to be \strong{passed to} and is \strong{returned from}
#' all main functions of the package as in
#'
#' \code{cmgo.obj = \link[=CM.generatePolygon]{CM.generatePolygon(cmgo.obj)}}\cr
#' \code{cmgo.obj = \link[=CM.calculateCenterline]{CM.calculateCenterline(cmgo.obj)}}\cr
#' \code{cmgo.obj = \link[=CM.processCenterline]{CM.processCenterline(cmgo.obj)}}\cr
#' \code{\link[=CM.writeData]{CM.writeData(cmgo.obj)}}\cr
#' \code{\link[=CM.plotPlanView]{CM.plotPlanView(cmgo.obj)}}\cr
#'
