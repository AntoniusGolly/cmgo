#' Deriving basic channel metrics from bank and long-profile geometry
#'
#' Principle channel metrics, as for example \strong{channel width or gradient},
#' convey immanent information that can be exploited for geomorphic research.
#' For example, a snap-shot of the current local channel geometry can provide
#' an integrated picture of the processes leading to its formation, if examined
#' in a statistically sound manner. Repeated surveys,
#' as time-series of channel gradients, can reveal local erosional characteristics
#' that sharpen our understanding of the underlying processes and facilitate,
#' inspire and motivate further research. However, these geometrical metrics
#' are not directly available. Typically, the measurable quantities are limited
#' to the position of features, such as the channel banks (or water surface) or
#' the water flow path (thalweg) in two- or three-dimensional coordinates. This package
#' derives with a scale-free approach principle
#' channel metrics, as channel width and slope. It does that by first generating a
#' reference line in the middle of the channel (centerline) based on which then the
#' channel width is calculated. It also allows for analyzing the evolution of channel
#' metrics over time if multiple surveys are provided. Furthermore, secondary spatial information (as for example the
#' occurrence of knickpoints, the abundance of certain species, etc.) can be projected
#' to the reference line allowing for a spatial correlation of different variables.
#'
#' @template section_getting_started
#'
#' @template section_global_data_object
#' @section General information on the global data object:
#' The global data object is \strong{initialized} with \code{cmgo.obj = CM.ini()} where
#' \code{\link[=CM.ini]{CM.ini()}} will either create the object from input files, from a previously
#' saved user workspace or from a demo data set. See the documentation of \code{\link[=CM.ini]{CM.ini()}}
#' for detailed information on how to create the object.
#'
#' @template section_parameters
#' @section Parameters:
#' See the documentation of \code{\link[=CM.par]{CM.par()}} for detailed information on how to load parameters.
#'
#' @template section_work_flow
#'
#' @template section_run_cmgo
#'
#' @template section_time_series
#'
#' @template section_technical_fails
#'
#' @docType package
#' @name cmgo
#' @importFrom grDevices colors dev.copy dev.copy2pdf dev.off graphics.off png
#' @importFrom graphics abline legend lines plot points segments text
#' @importFrom stats lm median princomp
#' @importFrom utils data head modifyList read.table tail write.table str packageVersion
#' @importFrom stringr str_pad
#' @importFrom spatstat dirichlet ppp psp owin nncross
#' @importFrom sp point.in.polygon
#' @importFrom zoo rollapply
#' @importFrom rgl clear3d plot3d abclines3d segments3d points3d
#' @importFrom shapefiles convert.to.shapefile write.shapefile
NULL
