#' Get the parameter object
#'
#' Create a parameter list containing all model and plotting parameters. Depending on the arguments passed to
#' \code{CM.par()}, the parameter list is either created from the defaults or from a specified parameter file.
#'
#' \code{CM.par()} creates or renews a parameter list based on the arguments passed to it. The list contains all
#' model and plot parameters and is stored within the global data object, for example \code{cmgo.obj$par}
#' (see \link[=cmgo]{package documentation}). Thus, the returned parameter object  must be assigned to the global data
#' object as in \code{cmgo.obj$par = CM.par()}.
#'
#' If you call \code{CM.par()} without arguments (example #1), the default parameter list
#' is returned (see section "All default parameters and their defaults"). For larger projects, it can be desired to easily switch between
#' parameter sets, for example to reproduce different plots. You can save these customized parameter sets in files
#' (see section "Make a custom parameter file"). These files are loaded with \code{CM.par("path_to_parameter_file")} (example #2). The file is
#' \strong{not required} to host \strong{all} parameters, since the list will be merged with the defaults on loading. That means, a parameter file
#' has to host only those parameters differing from the defaults. As a third option, you can pass directly a list to
#' \code{CM.par()} (example #3). The directly passed parameters will be merged with defaults, as well.
#'
#' @template section_all_default_parameters
#'
#' @section Make a custom parameter file:
#' Create an empty .r-file with the following code:\preformatted{
#' par = list(
#'   plot.to.file = TRUE,
#'   other_par    = "value"
#'   # other_par  = "other value"
#' )
#' }
#' A full list of available parameters can be found in the paragraph "All parameters and their defaults" above. In a parameter file you
#' don't have to specify \strong{all} parameters. Just list parameters that you like to differ from the defaults. The parameter object that
#' will be created by CM.par() if you specify a file name will be merged with the defaults, where specified parameters overwrite the defaults.
#'
#' @param par.set either NULL (default parameters are returned), of type string to specify a filename or of type list to specify parameters
#' @return The resulting parameters list.
#' @author Antonius Golly
#' @examples
#' # instantiate your global data object first, for example with CM.ini()
#' cmgo.obj = list()
#'
#' # example #1: get the default parameters
#' cmgo.obj$par = CM.par()
#'
#' # example #2: get parameters from a configuration file (see also "Make a custom parameter file")
#' #cmgo.obj$par = CM.par("par/custom_parameters.r")#'
#'
#' # example 3: get modified default parameters
#' cmgo.obj$par = CM.par(list(
#'   plot.to.file   = TRUE,
#'   plot.directory = "/my_figures"
#' ))
#'
#' @export CM.par

CM.par <- function(par.set=NULL){

  notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}
  error     = function(x){stop(x, call.=FALSE)}

  par.default = list(

    # name of the parameter set
    name                        = "default",

    # workspace
    workspace.read              = TRUE,        # if [TRUE] it is tried to load the global data object from a workspace file in CM.ini()
    workspace.write             = FALSE,       # if [TRUE] a workspace with the global data object will be written in CM.writeData()
    workspace.replace           = FALSE,       # if [TRUE] a workspace will be replaced when existing in CM.writeData()
    workspace.filename          = "user_workspace.RData", # the filename used in CM.ini() and CM.writeData()

    # input settings
    input.dir                   = "input",     # the directory from which all input files will be read in by CM.ini()
    input.sep                   = "\t",        # the column separator sign, e.g. ",", ";", "\t" (tab) passed to read.table (?read.table for more information)
    input.col.easting           = "POINT_X",   # the column name for the x-value
    input.col.northing          = "POINT_Y",   # s.a.
    input.col.elevation         = "POINT_Z",   # s.a.
    input.units                 = "m",         # units of input coordinates (will be used for axis labels in plotting functions)
    input.col.bank              = "Name",      # the column name of the side (left/right bank)
    bank.code.left              = "left",      # the string code used for the left bank
    bank.code.right             = "right",     # the string code used for the right bank

    # output settings
    output.write                = FALSE,       # if [TRUE] output ASCII files will be written
    output.replace              = FALSE,       # if [TRUE] the output files are replaced when existing in CM.writeFiles()
    output.write.centerline     = FALSE,       # if [TRUE] the geometry of the centerline will be written in CM.writeFiles()
    output.write.metrics        = TRUE,        # if [TRUE] the calculated channel metrics will be written in CM.writeFiles()
    output.write.metrics.d      = TRUE,        # switch on/off the variable d.r and d.l (distances from centerline to banks)
    output.write.metrics.w      = TRUE,        # switch on/off the variable w (channel width)
    output.write.metrics.r      = TRUE,        # switch on/off the variable r.r and r.l (direction factor of d.r and d.l)
    output.write.metrics.diff   = TRUE,        # switch on/off the variable diff.r and diff.l (distances between two banks)

    output.dir                  = "output",
    output.sep                  = "\t",

    # enable/disable plots
    plot.polygoncheck           = TRUE,        # if [TRUE], a three-column plot is generated showing the entire river and both ends to rouhgly check the polygon consitency (see also CM.generatePolygon())

    plot.planview               = TRUE,        # create a plan view overview plot
    plot.planview.secondary     = TRUE,        # in the plan view plot, add a secodary data set for comparison (will be displayed in dashed lines)
    plot.planview.bankpoints    = FALSE,       # in the plan view plot, add the bank points of a data set
    plot.planview.polygons      = TRUE,        # in the plan view plot, add the channel borders
    plot.planview.voronoi       = FALSE,       # in the plan view plot, add voronoi polygons in plan view plot
    plot.planview.cl.original   = FALSE,       # in the plan view plot, add the rough centerline (before smoothing)
    plot.planview.cl.smoothed   = TRUE,        # in the plan view plot, add the smoothed centerline
    plot.planview.cl.tx         = FALSE,       # in the plan view plot, add a label with the number next to the centerline points
    plot.planview.transects     = FALSE,       # in the plan view plot, add transects (perpendiculars to centerline)
    plot.planview.transects.len = 20,          # give the length of transects in the unit of the input coordinates
    plot.planview.dist2banks    = TRUE,        # in the plan view plot, add transect segments from centerline to the banks (left and right)
    plot.planview.grid          = TRUE,        # in the plan view plot, add a grid in the background
    plot.planview.grid.dist     = 20,          # the distance of the grid lines in the unit of the input coordinates
    plot.planview.legend        = TRUE,        # in the plan view plot, add a legend
    plot.planview.scalebar      = TRUE,        # in the plan view plot, add a scalebar (width of one plot.planview.grid.dist)

    # plot options
    plot.zoom                   = TRUE,        # if [TRUE] the plan view plot is zoomed in (see also CM.plotPlanView())
    plot.zoom.extent.length     = 140,         # zoom window extent for the plan view plot in the unit of the input coordinates
    plot.zoom.extent            = "e1",        # applied zoom window name (see also CM.plotPlanView())
    plot.zoom.extents           = list(        # presets (customizable list) of zoom windows
      e1  = c(400480,  3103130),
      e2  = c(399445,  3096220),
      e3  = c(401623,  3105925)
    ),
    plot.cl.range               = "cl1",       # applied zoom cl range (see also CM.plotPlanView)
    plot.cl.ranges              = list(        # presets (customizable list) of cl ranges
      cl1 = c(1235,    1260)
    ),
    plot.cl.range.use.reference = TRUE,        # determines whether to look for reference centerline [TRUE] or current centerline when centering around cl.range
    plot.to.file                = FALSE,       # if [TRUE] all plots will be copied to file devices
    plot.to.pdf                 = TRUE,        # if [TRUE] the plot will be saved as pdf
    plot.to.png                 = TRUE,        # if [TRUE] the plot will be saved as png
    plot.index                  = 0,           # numbering for filenames (see also CM.plotPlanView())
    plot.directory              = "plots/",    # directory for saving plots if plot.to.file = TRUE
    plot.filename               = "documentation", # plot file name

    # model parameters
    force.calc.voronoi          = FALSE,       # if [TRUE] the voronoi polygons are always re-calculated and never taken from cache
    force.calc.cl               = FALSE,       # if [TRUE] the centerline is always re-calculated and never taken from cache
    bank.interpolate            = TRUE,        # if [TRUE] the provided bank points are linearly interpolated to generate a denser polygon (see CM.generatePolygon())
    bank.interpolate.max.dist   = 6,           # if bank.interpolate is [TRUE] this is the maximum distance all bank points will have
    bank.reduce                 = FALSE,       # if [TRUE] the provided bank points are reduced by points that are closer to each other than bank.reduce.min.dist
    bank.reduce.min.dist        = 0.5,         # if bank.reduce is [TRUE] this is the minimum distance all bank point will have
    bank.filter3.max.it         = 12,          # number of the maximum iterations for filter 3 to prevent the program to run infinitely
    centerline.smoothing.width  = 7,           # smoothing window width of mean filter in number of observations (see CM.calculateCenterline())
    transects.span              = 3,           # span of centerline points used for calculating the transects (see CM.processCenterline())
    centerline.bin.length       = 5,           # for simplifying the centerline give the spacing in the unit of the input coordinates (see CM.reduceCenterline())
    centerline.use.reference    = FALSE,       # sets method for calculating distance centerline to banks, if [FALSE] (default) each river profile will be compared to its own centerline, if [TRUE] the centerline of centerline.reference will be taken (see CM.processCenterline())
    centerline.reference        = "set1",      # sets the reference data set if centerline.use.reference is [TRUE]
    calculate.metrics           = TRUE,        # if [TRUE] all centerline metrics are calculated (see CM.processCenterline())
    force.calc.metrics          = FALSE,       # if [TRUE] the metrics  are always re-calculated and never taken from cache

    # ignore
    dummy = TRUE

  )

  notice("load parameters", TRUE)

  # par.set is not specified: return default or existing parameter set
  if(is.null(par.set)){

    # set to default
    par = par.default
    tx  = "default parameters"

  }

  # par.set is a filename: open file and merge with defaults
  if(typeof(par.set) == "character"){

    # check if parameter file exists
    if(!file.exists(par.set)) error(paste("parameter file ", par.set, " does not exist!", sep=""))

    # source parameter file
    source(par.set, local=TRUE)

    # check for a valid par list
    if(!exists("par"))        error(paste("parameter file", par.set, "does not contain a parameter list!"))
    if(typeof(par) != "list") error(paste("parameter file", par.set, "does not contain a parameter list!"))

    # merge with defaults (specified parameters have higher priorities
    par = modifyList(par.default, par)
    tx  = paste("parameters from file '", par.set, "' merged with defaults", sep="")

  }

  if(typeof(par.set) == "list"){

    par = modifyList(par.default, par.set)
    tx  = "passed parameters merged with defaults"

  }

  # manipulate defaults
  if(is.null(par$centerline.smoothing.width)) par$centerline.smoothing.width = ceiling(par$bank.interpolate.max.dist)

  notice(paste("parameter set:", tx))

  return(par)

}
