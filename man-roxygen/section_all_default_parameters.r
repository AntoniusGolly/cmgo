#' @section All parameters and their defaults:
#' The parameter list contains more than 50 parameters specifying the model and the plotting.
#' All parameters can directly accessed via the global data object, for example
#' \code{cmgo.obj$par$input.dir = "/my_folder"}.
#'
#' This is the full list with explanations: \preformatted{
#'
#' # name of the parameter set
#' name                        = "default",
#'
#' # workspace
#' workspace.read              = TRUE,        # if [TRUE] it is tried to load the global data object from a workspace file in CM.ini()
#' workspace.write             = FALSE,       # if [TRUE] a workspace with the global data object will be written in CM.writeData()
#' workspace.replace           = FALSE,       # if [TRUE] a workspace will be replaced when existing in CM.writeData()
#' workspace.filename          = "user_workspace.RData", # the filename used in CM.ini() and CM.writeData()
#'
#' # input settings
#' input.dir                   = "input",     # the directory from which all input files will be read in by CM.ini()
#' input.sep                   = "\t",        # the column separator sign, e.g. ",", ";", "\t" (tab) passed to read.table (?read.table for more information)
#' input.col.easting           = "POINT_X",   # the column name for the x-value
#' input.col.northing          = "POINT_Y",   # s.a.
#' input.col.elevation         = "POINT_Z",   # s.a.
#' input.units                 = "m",         # units of input coordinates (will be used for axis labels in plotting functions)
#' input.col.bank              = "Name",      # the column name of the side (left/right bank)
#' bank.code.left              = "left",      # the string code used for the left bank
#' bank.code.right             = "right",     # the string code used for the right bank
#'
#' # output settings
#' output.write                = FALSE,       # if [TRUE] output ASCII files will be written
#' output.replace              = FALSE,       # if [TRUE] the output files are replaced when existing in CM.writeFiles()
#' output.write.centerline     = FALSE,       # if [TRUE] the geometry of the centerline will be written in CM.writeFiles()
#' output.write.metrics        = TRUE,        # if [TRUE] the calculated channel metrics will be written in CM.writeFiles()
#' output.write.metrics.d      = TRUE,        # switch on/off the variable d.r and d.l (distances from centerline to banks)
#' output.write.metrics.w      = TRUE,        # switch on/off the variable w (channel width)
#' output.write.metrics.r      = TRUE,        # switch on/off the variable r.r and r.l (direction factor of d.r and d.l)
#' output.write.metrics.diff   = TRUE,        # switch on/off the variable diff.r and diff.l (distances between two banks)
#'
#' output.dir                  = "output",
#' output.sep                  = "\t",
#'
#' # enable/disable plots
#' plot.polygoncheck           = TRUE,        # if [TRUE], a three-column plot is generated showing the entire river and both ends to rouhgly check the polygon consitency (see also CM.generatePolygon())
#'
#' plot.planview               = TRUE,        # create a plan view overview plot
#' plot.planview.secondary     = TRUE,        # in the plan view plot, add a secodary data set for comparison (will be displayed in dashed lines)
#' plot.planview.bankpoints    = FALSE,       # in the plan view plot, add the bank points of a data set
#' plot.planview.polygons      = TRUE,        # in the plan view plot, add the channel borders
#' plot.planview.voronoi       = FALSE,       # in the plan view plot, add voronoi polygons in plan view plot
#' plot.planview.cl.original   = FALSE,       # in the plan view plot, add the rough centerline (before smoothing)
#' plot.planview.cl.smoothed   = TRUE,        # in the plan view plot, add the smoothed centerline
#' plot.planview.cl.tx         = FALSE,       # in the plan view plot, add a label with the number next to the centerline points
#' plot.planview.transects     = FALSE,       # in the plan view plot, add transects (perpendiculars to centerline)
#' plot.planview.transects.len = 20,          # give the length of transects in the unit of the input coordinates
#' plot.planview.dist2banks    = TRUE,        # in the plan view plot, add transect segments from centerline to the banks (left and right)
#' plot.planview.grid          = TRUE,        # in the plan view plot, add a grid in the background
#' plot.planview.grid.dist     = 20,          # the distance of the grid lines in the unit of the input coordinates
#' plot.planview.legend        = TRUE,        # in the plan view plot, add a legend
#' plot.planview.scalebar      = TRUE,        # in the plan view plot, add a scalebar (width of one plot.planview.grid.dist)
#'
#' # plot options
#' plot.zoom                   = TRUE,        # if [TRUE] the plan view plot is zoomed in (see also CM.plotPlanView())
#' plot.zoom.extent.length     = 140,         # zoom window extent for the plan view plot in the unit of the input coordinates
#' plot.zoom.extent            = "e1",        # applied zoom window name (see also CM.plotPlanView())
#' plot.zoom.extents           = list(        # presets (customizable list) of zoom windows
#'   e1  = c(400480,  3103130),
#'   e2  = c(399445,  3096220),
#'   e3  = c(401623,  3105925)
#' ),
#' plot.cl.range               = "cl1",       # applied zoom cl range (see also CM.plotPlanView)
#' plot.cl.ranges              = list(        # presets (customizable list) of cl ranges
#'   cl1 = c(1235,    1260)
#' ),
#' plot.cl.range.use.reference = TRUE,        # determines whether to look for reference centerline [TRUE] or current centerline when centering around cl.range
#' plot.to.file                = FALSE,       # if [TRUE] all plots will be copied to file devices
#' plot.to.pdf                 = TRUE,        # if [TRUE] the plot will be saved as pdf
#' plot.to.png                 = TRUE,        # if [TRUE] the plot will be saved as png
#' plot.index                  = 0,           # numbering for filenames (see also CM.plotPlanView())
#' plot.directory              = "plots/",    # directory for saving plots if plot.to.file = TRUE
#' plot.filename               = "documentation", # plot file name
#'
#' # model parameters
#' force.calc.voronoi          = FALSE,       # if [TRUE] the voronoi polygons are always re-calculated and never taken from cache
#' force.calc.cl               = FALSE,       # if [TRUE] the centerline is always re-calculated and never taken from cache
#' bank.interpolate            = TRUE,        # if [TRUE] the provided bank points are linearly interpolated to generate a denser polygon (see CM.generatePolygon())
#' bank.interpolate.max.dist   = 6,           # if bank.interpolate is [TRUE] this is the minimum distance all bank points will have
#' bank.filter3.max.it         = 12,          # number of the maximum iterations for filter 3 to prevent the program to run infinitely
#' centerline.smoothing.width  = 7,           # smoothing window width of mean filter in number of observations (see CM.calculateCenterline())
#' transects.span              = 3,           # span of centerline points used for calculating the transects (see CM.processCenterline())
#' centerline.bin.length       = 5,           # for simplifying the centerline give the spacing in the unit of the input coordinates (see CM.reduceCenterline())
#' centerline.use.reference    = FALSE,       # sets method for calculating distance centerline to banks, if [FALSE] (default) each river profile will be compared to its own centerline, if [TRUE] the centerline of centerline.reference will be taken (see CM.processCenterline())
#' centerline.reference        = "set1",      # sets the reference data set if centerline.use.reference is [TRUE]
#' calculate.metrics           = TRUE,        # if [TRUE] all centerline metrics are calculated (see CM.processCenterline())
#' force.calc.metrics          = FALSE,       # if [TRUE] the metrics  are always re-calculated and never taken from cache
#' }
#'
