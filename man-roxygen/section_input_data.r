#' @section Input data:
#' The package requires as \strong{input data} the bank points of a river in 2D- or
#' 3D-coordinates along with the information of the side of the bank points
#' (right or left), as for example:
#'
#' \code{Name  POINT_X  POINT_Y}\cr
#' \code{right  401601.0819  3106437.335}\cr
#' \code{right  401586.5327  3106406.896}\cr
#' \code{right  401568.3238  3106383.586}\cr
#' \code{right  401558.4961  3106364.129}\cr
#' \code{...}\cr
#' \code{left  401621.4337  3106431.134}\cr
#' \code{left  401602.9913  3106405.991}\cr
#' \code{left  401574.6073  3106352.232}\cr
#' \code{left  401582.2671  3106323.134}\cr
#' \code{...}\cr
#'
#' The data can be either collected during field
#' surveys with GPS or total stations or through remote sensing techniques
#' with further digitizing for example in a GIS. The input can be given in any ASCII
#' table format. By default, the program expects tab-delimited columns of a table with one header
#' line with the header names \strong{Names} (for the side) and \strong{POINT_X}/_Y/Z (the coordinates
#' of the bank points) where the z component is optional. The expected column names and tab delimiters
#' are set in the parameters (see documentation of \code{\link[=CM.par]{CM.par()}} for details).
#' The order of the points can be either all right bank points first or all left bank points first but not mixed.
#'
#' The input file(s) have to be placed in the input directory specified in the parameters (defaults to "./input")
#' and can have any file extension (.txt, .csv, etc.). The function \code{\link[=CM.ini]{CM.ini()}} will iterate
#' over all files in that directory and create a data set for each file in the global data object of cmgo.
