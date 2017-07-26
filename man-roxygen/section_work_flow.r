#' @section Work flow:
#' The program can be divided into three main parts which you will go through if you start a project:
#' \strong{1. initialization} (loading data and parameters),
#' \strong{2. data processing} (calculating channel metrics) and
#' \strong{3. review results} (plotting or writing results to file).
#' The initialization covers the loading of the parameters in \code{\link[=CM.par]{CM.par()}} and loading of
#' the data in \code{\link[=CM.ini]{CM.ini()}}. See their documentation for details. The work flow of
#' the data processing is shown in the plan view plots below.
#'
#'  \if{html}{\figure{01-processing.png}{options: width="800px" alt="Figure: processing"}}
#'  \if{latex}{\figure{01-processing.pdf}{options: width=9cm}}
#'  \emph{Figure 1: A visualization of the work flow of the package, a) data input, b) polygon generation,
#' c-e) centerline generation, f) transect generation, g) channel width calculation.}
#'
#' Channel bank points (Fig. 1a) represent the required input data for the package. The algorithm
#' then creates a polygon from these points (Fig. 1b) where the points are linearly interpolated
#' to increase their spatial resolution. The maximum distance the points have is defined by the parameter
#' \code{bank.interpolate.max.dist}. From these points Voronoi polygons are calculated (Fig. 1c).
#' Voronoi polygons around points denote the areas within which all points are closest to that point.
#' In Fig. 1c you can already notice a centerline evolving in the middle of the channel polygon. Fig. 1d shows
#' the segments that represent the centerline filtered by the algorithm. These centerline segments will be
#' connected to one consistent line and get smoothed (Fig. 1e). The degree of smoothing can be adjusted
#' through the parameter \code{centerline.smoothing.width} (defaults to the same value as
#' \code{bank.interpolate.max.dist}). This centerline represents the reference of the river, for which
#' length, local width and slope are calculated next. Note, that the length of the centerline depends on
#' the smoothing in 1e). The pros and cons of the smoothing are explained in the documentation of the function
#' \code{CM.calculateCenterline()}. To derive the local channel width, transects are calculated perpendicular
#' to portions of the centerline (Fig. 1f). The transects are lines perpendicular to a group of centerline points
#' where the size of that group is defined by the parameter \code{transects.span}. See
#' \code{\link[=CM.processCenterline]{CM.processCenterline()}} for detailed information on how the transects
#' are generated. In the final step the intersections of the
#' transects with the banks are calculated (Fig. 1g). The distance of the
#' centerline point to the bank is stored sepearately for the left and the right bank. When the transects
#' cross the banks multiple times, the minimum distance is taken.
#'
#' The described algorithm is hosted in the following functions:
#' \itemize{
#'   \item load data points in \code{\link[=CM.ini]{CM.ini()}}, step \strong{a}
#'   \item generate a polygon from bank points in \code{\link[=CM.generatePolygon]{CM.generatePolygon()}}, step \strong{b}
#'   \item calculate centerlin from the polygon in \code{\link[=CM.calculateCenterline]{CM.calculateCenterline()}}, steps \strong{c-e}
#'   \item process the centerline in \code{\link[=CM.processCenterline]{CM.processCenterline()}}, steps \strong{f-g}
#' }
