#' @section Technical fails and how to prevent them:
#' All parameters are stored in the global data object (see the section 'Global data object') under the sub-list
#' $par. For example, if your global data object is named \code{obj} the parameter \code{$plot.planview} is
#' accessible by \code{obj$par$transects.span}. The available parameters of the model are described in
#' the documentation of the function \code{\link[=CM.par]{CM.par()}}. Here we only describe a few cases why editing the
#' default parameters can be desired.
#'
#' There are certain geometrical cases in which the algorithm can fail with the default parametrization.
#' To prevent this a wise parametrization of the model is required. The program will inform you during runtime
#' if the generation of the centerline fails and will offer you ways to elaborate the issue. The main reason for
#' failure occurs if the resolution of channel bank points (controlled via \code{$bank.interpolate.max.dist})
#' is relatively low compared to the channel width. The following image illustrates the problem:
#'
#' \figure{02_gap.png}{options: width="500px" alt="Figure: gap"}
#'
#' \emph{Figure 2: A gap in the centerline occurs since the spacing of the bank points was too high.}
#'
#' Apparently, the centerline segments are too scraggy and do not lie entirely within the bank polygons. Since
#' the filter mechanism (step \strong{c-d}) checks for segments within the polygon first, this will create a gap
#' in the centerline. The program will not be able to fill this gap automatically. Thus, if you experience problems
#' with the calculation of the centerline consider to increase the spatial resolution of bank points. The following
#' example illustrates how this fixes the problem.
#'
#' \figure{03_sep_spacing.png}{options: width="500px" alt="Figure: fix"}
#' \emph{Figure 3: The same location of the channel with two different bank point spacings.}
#'
#' Another problem can arise from an unsuitable settings of the span for the calculation of the
#' transects (step \strong{f}). The transects are perpendicular to a line that goes through the outer points
#' of a group of \code{n} points of the centerline. This \code{n} equals 3 by default (parameter
#' \code{$transects.span}). The following plan view plot illustrates to what misinterpretation of the channel width
#' can lead:
#'
#' \figure{04_transect_span.png}{options: width="500px" alt="span_issues"}
#' \emph{Figure 4, left: the transects (perpendiculars to the centerline) do not intersect
#' with banks properly, thus channel width is overrepresented. Right: an increased transect span fixes the problem
#' and channel width is now identified correctly.}
#'
#' It can be seen that one of the red transects does not touch the left bank of the channel, thus leading to
#' an overestimated channel width at this location. To prevent this, you can increase the span of the transect
#' calculation.
