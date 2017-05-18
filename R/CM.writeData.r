#' Write data
#'
#' Write output files with channel metrics and the current workspace.
#'
#' \code{CM.writeData()} allows you to write the results to output files and to an R workspace file. 
#' The outputs written depend on the settings in the parameter object. If \code{cmgo.obj$par$workspace.write = TRUE} 
#' (default is FALSE) a workspace file is written containing the global data object. The 
#' filename is defined in \code{cmgo.obj$par$workspace.filename}. Further, ASCII tables can be written 
#' containing the centerline geometry and the calculated metrics. If \code{cmgo.obj$par$output.write = TRUE} 
#' (default is FALSE) an output file for each data set is written to the output folder specified 
#' in \code{cmgo.obj$par$output.dir}. The file names are the same as the input filenames with the 
#' prefixes cl_* and metrics_*. All parameters regarding the output generation are listed in \code{\link[=CM.par]{CM.par()}}. 
#'
#' @template param_global_data_object
#' @return This file function does not return any value.
#' @author Antonius Golly
#' @examples
#' # get demo data (find instructions on how to use own data in the documentation of CM.ini())
#' cmgo.obj = CM.ini("demo2")
#'
#' # example 1: write workspace
#' cmgo.obj$par$workspace.write = TRUE
#' CM.writeData(cmgo.obj)
#'
#' # example 2: write output files
#' cmgo.obj$par$output.write = TRUE
#' cmgo.obj$par$output.write.centerline = TRUE
#' cmgo.obj$par$output.write.metrics = TRUE
#' cmgo.obj$par$output.dir   = "custom_output_folder"
#' cmgo.obj$par$workspace.write = FALSE
#'
#' CM.writeData(cmgo.obj)
#'
#' @export CM.writeData

CM.writeData <- function(object){

  par  = object$par
  data = object$data

  notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}
  error     = function(x){stop(x, call.=FALSE)}
  alert     = function(x, y=""){if(y!=""){message(paste("--> ",x, y, sep=""))}else{message(paste("--> ", x, sep=""))}}
  warn      = function(x){warning(x, call.=FALSE)}
  plot.file = function(par){if(!par$plot.to.file) return(NULL); file.no   = 0 + par$plot.index; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep=""); while(file.exists(paste(file.name, ".png", sep="")) || file.exists(paste(file.name, ".pdf", sep=""))){  file.no   = file.no + 1; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep="") }; dev.copy(png, filename=paste(file.name, ".png", sep=""), width=800, height=600); dev.off(); dev.copy2pdf(file=paste(file.name, ".pdf", sep=""));}

  notice("write output files", TRUE)

  if(par$workspace.write){

    if(!file.exists(par$workspace.filename) || par$workspace.replace){

      cmgo.obj = object

      save("cmgo.obj", file = par$workspace.filename)
      notice(paste("workspace saved to",par$workspace.filename))

    } else {

      notice("workspace not written since it exists")

    }

  }

  if(par$output.write){

    for(set in names(data)){

      ## write centerline data ###########################################
      if(par$output.write.centerline){

        if(!file.exists(par$output.dir)) dir.create(par$output.dir)

        output.filename = paste(par$output.dir, "/cl_", data[[set]]$filename, sep="")

        notice(paste("write centerline of", set, "to", output.filename))

        output = data.frame(
          centerline_x = data[[set]]$cl$smoothed$x,
          centerline_y = data[[set]]$cl$smoothed$y,
          centerline_z = if(is.null(data[[set]]$cl$smoothed$z)) rep(NA, length(data[[set]]$cl$smoothed$x)) else data[[set]]$cl$smoothed$z,
          length       = data[[set]]$cl$smoothed$length,
          seg_length   = data[[set]]$cl$smoothed$segments
        )

        write.table(
          output,
          file = output.filename, sep = par$output.sep,
          row.names = FALSE
        )

      } # if(par$output.write.centerline)

      ## write channel metrics ###########################################
      if(par$output.write.metrics){

        if(!file.exists(par$output.dir)) dir.create(par$output.dir)

        output.filename = paste(par$output.dir, "/metrics_", data[[set]]$filename, sep="")

        if(!file.exists(output.filename) || par$output.replace){

          notice(paste("write metrics of ", set, " to ", output.filename, "...", sep=""))

          output = data[[set]]$metrics

          # remove standard fields
          output[[which(names(output) == "cl.ref")]]  = NULL
          output[[which(names(output) == "cl.type")]] = NULL
          output[[which(names(output) == "tr")]]      = NULL
          output[[which(names(output) == "cp.r")]]    = NULL
          output[[which(names(output) == "cp.l")]]    = NULL

          write.table(
            output,
            file = output.filename, sep = par$output.sep,
            row.names = FALSE
          )

          cat(" done!\n")

        } else {

          notice("metrics not written since it exists")

        }

      } # if(par$output.write.centerline)

    } # for(set in names(data))

  } # if(par$output.write)

}
