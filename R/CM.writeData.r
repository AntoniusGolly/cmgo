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

CM.writeData <- function(cmgo.obj){

  par  = cmgo.obj$par
  data = cmgo.obj$data

  notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}
  error     = function(x){stop(x, call.=FALSE)}
  alert     = function(x, y=""){if(y!=""){message(paste("--> ",x, y, sep=""))}else{message(paste("--> ", x, sep=""))}}
  warn      = function(x){warning(x, call.=FALSE)}
  plot.file = function(par){if(!par$plot.to.file) return(NULL); file.no   = 0 + par$plot.index; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep=""); while(file.exists(paste(file.name, ".png", sep="")) || file.exists(paste(file.name, ".pdf", sep=""))){  file.no   = file.no + 1; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep="") }; dev.copy(png, filename=paste(file.name, ".png", sep=""), width=800, height=600); dev.off(); dev.copy2pdf(file=paste(file.name, ".pdf", sep=""));}

  notice("write output files...", TRUE)



  ## write workspace data ############################################

  if(par$workspace.write){

    if(!file.exists(par$workspace.filename) || par$workspace.replace){

      notice("write workspace now...")

      save("cmgo.obj", file = par$workspace.filename)
      notice(paste("workspace saved to",par$workspace.filename))

    } else {

      notice("workspace not written since it exists")

    }

  } else { notice("write workspace: disabled") }

  ####################################################################


  ## write centerline data ###########################################

  if(par$output.write.centerline){

    if(!file.exists(par$output.dir)) dir.create(par$output.dir)

    for(set in names(data)){

      output.filename = paste(par$output.dir, "/cl_", data[[set]]$filename, sep="")

      notice(paste("write centerline of", set, "to", output.filename))

      output = data.frame(
        centerline_x = data[[set]]$cl$smoothed$x,
        centerline_y = data[[set]]$cl$smoothed$y,
        centerline_z = if(is.null(data[[set]]$cl$smoothed$z)) rep(NA, length(data[[set]]$cl$smoothed$x)) else data[[set]]$cl$smoothed$z,
        length       = data[[set]]$cl$smoothed$cum_dist_2d,
        seg_length   = data[[set]]$cl$smoothed$seg_dist_2d
      )

      write.table(
        output,
        file = output.filename, sep = par$output.sep,
        row.names = FALSE
      )

    }

  } else { # if(par$output.write.centerline)

    notice("write centerline geometry: disabled")

  } # if(par$output.write.centerline) {} else

  ####################################################################


  ## write channel metrics ###########################################

  if(par$output.write.metrics){

    if(!file.exists(par$output.dir)) dir.create(par$output.dir)

    for(set in names(data)){

      output.filename = paste(par$output.dir, "/metrics_", data[[set]]$filename, sep="")

      if(!file.exists(output.filename) || par$output.replace){

        notice(paste("write metrics of ", set, " to ", output.filename, "...", sep=""))

        output = data[[set]]$metrics

        # keep standard fields
        output = output[which(names(output) %in% c(
          "w", "d.r", "d.l", "r.r", "r.l", "diff.r", "diff.l"
        ))]

        # add centerline information: location and cumulative length of centerline points
        output = cbind(data.frame(
            x           = data[[set]]$cl$smoothed$x,
            y           = data[[set]]$cl$smoothed$y,
            cum_dist_2d = data[[set]]$cl$smoothed$cum_dist_2d
        ), output, stringsAsFactors = FALSE)


        # write output
        write.table(
          output,
          file = output.filename, sep = par$output.sep,
          row.names = FALSE
        )

      } else {

        notice("metrics not written since it exists")

      }

    } # for(set in names(data))

  } else { # if(par$output.write.metrics)

    notice("write metric files: disabled")

  } # if(par$output.write.metrics) {} else

  ####################################################################


  ## write steps data ################################################

  if(par$output.write.steps){

    if(!file.exists(par$output.dir)) dir.create(par$output.dir)

    for(set in names(data)){

      output.filename = paste(par$output.dir, "/steps_", data[[set]]$filename, sep="")

      if(!file.exists(output.filename) || par$output.replace){

        notice(paste("write steps table of ", set, " to ", output.filename, "...", sep=""))

        output = data[[set]]$steps

        # keep standard fields
        output = output[which(names(output) %in% c(
          "id", "ix",
          "start.of.step.x", "start.of.step.y", "start.of.step.z", "start.of.step.d",
          "pool.start.x",    "pool.start.y",    "pool.start.z",    "pool.start.d",
          "pool.end.x",      "pool.end.y",      "pool.end.z",      "pool.end.d",
          "pool.depth.x",    "pool.depth.y",    "pool.depth.z",    "pool.depth.d",
          "pool.length",     "step.height",     "pool.depth.r"
        ))]

        write.table(
          output,
          file = output.filename, sep = par$output.sep,
          row.names = FALSE
        )

      } else {

        notice("step data not written since it exists")

      }

    } # for(set in names(data))

  } else { # if(par$output.write.metrics)

    notice("write step data files: disabled")

  } # if(par$output.write.metrics) {} else

  ####################################################################




  ####################################################################

  notice("CM.writeData() has ended successfully!", TRUE)

}
