#' Get the global data object
#'
#' Create the global data object with data and parameters (information on the global data object in section 
#' "General information on the global data object). Depending on the arguments passed to 
#' \code{CM.ini()}, the data are either created from input files, a work space file or from a demo data set. 
#' The parameters are either created from defaults, from a parameter file
#' or merged with a passed list of parameters.
#'
#' \code{CM.ini()} returns a global data object based on the arguments passed to it. Initially,
#' the function builds a parameter object. If you leave the \code{par} argument empty, the default configuration
#' is loaded. You can also pass everything to \code{par} that is accepted by \code{\link[=CM.par]{CM.par()}} as
#' this argument will be directly passed to this function (see the documentation of CM.par() for further information).
#' Once the parameter object is built, the function will create the data object by the following rules (if one rule
#' was successfull, the routine stops and returns the global data object):
#'
#' \enumerate{
#'
#'   \item If \code{$workspace.read} is \code{TRUE} (default) the function looks for an .RData user workspace
#'   named \code{$workspace.filename} (defaults to "./user_workspace.RData"). Note: there will be no such workspace file
#'   once you start a project, since it needs to be saved by the user with \code{\link[=CM.writeData]{CM.writeData()}}.
#'   If such a workspace file exists the global data object will be created from this source, otherwise the next source
#'   is tested.
#'   \item If data input files are available in the directory \code{$input.dir} (defaults to "./input") the function
#'   interates over all files in this directory and create the data object from this source (see section "Input data" below
#'   for further information on the data format). If this rule applies you will start with a clean data set, that
#'   contains nothing than the bank geometry and you will need to process the data from scratch. Otherwise the next source is tested.
#'   \item If \code{object} is a string, the function will check for a demo data set with the same name. Available demo data
#'   sets are \code{"demo"}, \code{"demo1"} and \code{"demo2"}. See section "Demo data sets" below.
#' }
#'
#' @template section_global_data_object
#'
#' @template section_input_data
#'
#' @section Demo data sets:
#' Four demo data sets are available, which contain a few kilometer long section of a channel. The data sets 
#' differ in the degree of data processing. The data set \strong{demo} only contains the channel bank points, thus 
#' contains a global data object how it looks directly after reading the input files (check 
#' structure with \code{str(cmgo.obj$data[[set]]$channel)}). The data set \strong{demo1} has passed the first steps 
#' of the processing: CM.generatePolygon() and CM.calculateCenterlin(). Thus, it contains successfully created
#' centerlines (check structure with \code{str(cmgo.obj$data[[set]]$cl)}). The data set \strong{demo2} has gone through
#' the full stack of processing of cmgo, e.g. CM.processCenterline() , and all metrics are calculated (check 
#' structure with \code{str(cmgo.obj$data[[set]]$metrics)}). Thus you can use this data set for testing the 
#' plotting functions CM.plotPlanView() and CM.plotWidth(). Note, that demo data set demo3 uses the standard 
#' mode for the calculation of the metrics (default)
#' and not the reference centerline mode. To see the reference centerline mode in action use data set \strong{demo3}. In addition
#' to the previously mentioned plotting functions, here also CM.plotBankShift() is available.  
#'
#' @param object either a global data object of type list or a string to specify a demo data set (see Details)
#' @param par either a parameter list, a string to specify a parameter file or NULL to load default parameters (see also \code{\link[=CM.par]{CM.par()}})
#' @return returns the global data object containing data and parameters that will be passed to all main functions of \code{cmgo}.
#' @author Antonius Golly
#' @examples
#' # example 1: get data set from demo data
#' cmgo.obj = CM.ini("demo")
#'
#' # example 2: get data set from input files
#' par = CM.par()
#' print(cmgo.obj$par$input.dir)
#' par$input.dir = "custom_input_folder"
#' #cmgo.obj = CM.ini(NULL, par) # works if 'custom_input_folder' contains file(s)
#'
#' # example 3: load existing workspace
#' # (see CM.writeData() to learn how to write your workspace)
#' cmgo.obj$par$workspace.filename = "custom_workspace.RData"
#' #cmgo.obj = CM.ini(NULL, par) # works if 'custom_workspace.RData' exists
#'
#' @export CM.ini

CM.ini <- function(object = "demo", par = NULL){

  notice    = function(x,prim=FALSE){ cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n") }
  error     = function(x){stop(x, call.=FALSE)}

  # user notice
  notice("load data", TRUE)

  # get parameters
  par = CM.par(par)

  # object is a valid data object
  if(typeof(object) == "list"){

    notice("data is already existing, no data loaded!", TRUE)
    return(object)

  }

  # try to load from workspace
  if(par$workspace.read){

    if(file.exists(par$workspace.filename)){

      cmgo.obj = NULL

      load(par$workspace.filename)

      if(typeof(cmgo.obj) == "list"){

        notice(paste("data set: workspace '", par$workspace.filename, "' loaded!", sep=""), TRUE)

        return(cmgo.obj)

      } else {

        error("tried to load workspace but workspace does not contain a valid global data object!")

      }

    }

  }

  files = list.files(par$input.dir)
  if(length(files) > 0 ){

    data = list()

    # read all files in $input.dir
    for(file in files){

      table = read.table(paste(par$input.dir, "/", file, sep=""), sep=par$input.sep, header=TRUE, stringsAsFactors=FALSE, na.strings="-")
      set   = paste("set", which(files == file), sep="")

      data[[set]] = list(

        filename = file,
        channel  = list(
          x    = table[[par$input.col.easting]],
          y    = table[[par$input.col.northing]],
          z    = table[[par$input.col.elevation]],
          bank = table[[par$input.col.bank]]
        )

      )

      notice(paste("file '", file, "' assigned to ", set, sep=""))

    }

    notice(paste("data set: ", length(files), " file(s) read from directory '", par$input.dir, "'!", sep=""), TRUE)

    return(list(data = data, par = par))

  } else {

    notice(paste("no input files in folder '", par$input.dir, "' available, load demo data!", sep=""))

    if(is.null(object)) object = "demo"

  }


  # data specifies a demo workspace
  if(typeof(object) == "character"){

    if(exists(object)){

      notice(paste("data set '", object, "' loaded!", sep=""))
      object = get(object)
      return(object)

    } else error(paste("data set '", object, "' does not exist!", sep=""))

  }

}
