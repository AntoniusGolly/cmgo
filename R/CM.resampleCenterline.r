#' Reduce centerline points
#'
#' Create an equally spaced centerline with a given interval length.
#'
#' The spatial resolution of the centerline depends on the spatial resolution of the bank points which is
#' directly dependent on the parameter par$polygon.bank.interpolate.max.dist. A small interpolation distance is necessary for complex
#' bed shapes. However, for further metric analyes a large number of centerline points is not necessary. Since they can introduce
#' high computational costs during further calculation you can resample the centerline intervals with this function CM.resampleCenterline().
#' decreasing the resolution to a given value. This will have slight impact on the length of the centerline. Since you are losing
#' detail a coarser interval will decrease the length of the centerline.
#'
#' @template param_global_data_object
#' @template param_set
#' @return the global data object
#' @author Antonius Golly
#' @examples
#'
#' # get the demo data set
#' cmgo.obj = CM.ini("demo2")
#'
#' # reduce centerline resolution
#' cmgo.obj = CM.resampleCenterline(cmgo.obj)
#'
#' @export CM.resampleCenterline

CM.resampleCenterline <- function(object, set = NULL){

  par  = object$par
  data = object$data
  sets = if(is.null(set)) names(data) else set

  notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}
  error     = function(x){stop(x, call.=FALSE)}
  alert     = function(x, y=""){if(y!=""){message(paste("--> ",x, y, sep=""))}else{message(paste("--> ", x, sep=""))}}
  warn      = function(x){warning(x, call.=FALSE)}
  plot.file = function(par){if(!par$plot.to.file) return(NULL); file.no   = 0 + par$plot.index; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep=""); while(file.exists(paste(file.name, ".png", sep="")) || file.exists(paste(file.name, ".pdf", sep=""))){  file.no   = file.no + 1; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep="") }; dev.copy(png, filename=paste(file.name, ".png", sep=""), width=800, height=600); dev.off(); dev.copy2pdf(file=paste(file.name, ".pdf", sep=""));}
  resample  = function(polyline, interval_length = 20, add_original_points = FALSE, add_final_point = FALSE) {

    # The function splits a polyline into segments of a given length.
    # polyline: a spatial polyline data frame
    # interval_length: the length of the segments to split the lines into, in units of the polyline coordinates
    # add_original_points: whether or not the original points of the polyline should be added to the resulting line
    #                      if set FALSE, the resulting line will be shorter
    # add_final_point: whether or not the final point of the polyline should be added to the resulting line

    # transform input polyline
    linedf = data.frame(
      x  = polyline$x[1:nrow(polyline)-1],
      y  = polyline$y[1:nrow(polyline)-1],
      x2 = polyline$x[2:nrow(polyline)],
      y2 = polyline$y[2:nrow(polyline)]
    )

    # prepare output
    df = data.frame(
      x  = numeric(),
      y  = numeric()
    )

    residual_seg_length = 0
    for (i in 1:nrow(linedf)) {

      # for each line of the dataframe calculate segment length
      v_seg      = linedf[i, ]
      seg_length = sqrt((v_seg$x - v_seg$x2) ^ 2 + (v_seg$y - v_seg$y2) ^ 2)

      # create a vector of direction for the segment
      v = c(v_seg$x2 - v_seg$x, v_seg$y2 - v_seg$y)

      # unit length
      u = c(v[1]  /  sqrt(v[1]  ^  2 + v[2]  ^  2), v[2]  /  sqrt(v[1]  ^  2 + v[2]  ^ 2))

      # calculate number of segment the segment is split into
      num_seg = floor((seg_length - residual_seg_length)  /  interval_length)

      # skip if next vertex is before interval_length
      if(num_seg >= 0) {

        # add interpolated segments
        for (i in 0:(num_seg)) {
          df[nrow(df) + 1,] = c(
            v_seg$x  +  u[1] * residual_seg_length  +  u[1]  *  interval_length  *  i ,
            v_seg$y  +  u[2] * residual_seg_length  +  u[2]  *  interval_length  *  i
          )
        }

        # add original point (optional)
        if(add_original_points){
          df[nrow(df) + 1,] = c(
            v_seg$x2,
            v_seg$y2
          )
        }

      } else {

        # add original point (optional)
        if(add_original_points){
          df[nrow(df) + 1,] = c(
            v_seg$x2,
            v_seg$y2
          )
        }

        residual_seg_length = residual_seg_length - seg_length
        next()

      }

      # calculate residual segment length
      residual_seg_length = interval_length - ((seg_length - residual_seg_length) - (num_seg  *  interval_length))

    }

    # add final point (optional)
    if(add_final_point){
      df = rbind(df, data.frame(
          x = tail(polyline$x, n=1),
          y = tail(polyline$y, n=1)
        ))
    }

    return(df)

  }

  cl.type = "smoothed"
  set     = "set2"

  for(set in sets){

    notice(paste("resample centerline of", set))

    ixs = length(data[[set]]$cl$smoothed$cum_dist_2d)
    l   = data[[set]]$cl$smoothed$cum_dist_2d[ixs]

    notice(paste("current average spacing: ", round(l/ixs, digits=3),    par$input.unit))
    notice(paste("intended spacing:", par$centerline.bin.length, par$input.unit))

    if( par$centerline.bin.length < (round(l/ixs, digits=3))) error("intended spacing of resampled centerline must be larger than current average spacing")

    # re-bin centerline
    cl.rebinned = resample(data[[set]]$cl[[cl.type]][c("x","y")], par$centerline.bin.length)
    cum_dist    = seq(from = 0, to = floor(max(data[[set]]$cl[[cl.type]]$cum_dist_2d)), by = par$centerline.bin.length)
    bin_metrics = do.call(rbind, apply(as.array(cum_dist), 1, function(x){

      # determine closest index and index ranges
      ix        = which.min(abs(data[[set]]$cl[[cl.type]]$cum_dist_2d - x))
      bin_ix    = which((data[[set]]$cl[[cl.type]]$cum_dist_2d > (x - par$centerline.bin.length/2)) & (data[[set]]$cl[[cl.type]]$cum_dist_2d < (x + par$centerline.bin.length/2)))

      if(length(bin_ix) == 0){
        bin_ix  = which.min(abs(data[[set]]$cl$smoothed$cum_dist_2d - x))
        notice(paste("no points in bin ", x, "! take nearest point...", sep=""))
      }

      return(data.frame(

        bin_x = mean(data[[set]]$cl[[cl.type]]$x[bin_ix]),
        bin_y = mean(data[[set]]$cl[[cl.type]]$y[bin_ix]),
        bin_z = if(!is.null(data[[set]]$channel$z)) mean(data[[set]]$cl$original$z[bin_ix]) else NA,

        # widths
        width_median = median(data[[set]]$metrics$w[bin_ix]),
        width_mean   = mean(data[[set]]$metrics$w[bin_ix]),
        width_actual = data[[set]]$metrics$w[ix]

        #slope_median = if(!is.null(data[[set]]$channel$z)) median(data[[set]]$metrics$slope[bin_ix]) else NA,
        #slope_mean   = if(!is.null(data[[set]]$channel$z)) mean(data[[set]]$metrics$slope[bin_ix])   else NA

      ))

    }))

    # store
    data[[set]]$cl$binned             = cbind(cl.rebinned, bin_metrics) # x and y coordinates
    data[[set]]$cl$binned$cum_dist_2d = cum_dist
    data[[set]]$cl$binned$bin.length  = par$centerline.bin.length

    notice(paste("re-binning centerline of", set, "done!"), TRUE)

  }

  notice("CM.resampleCenterline() has ended successfully!", TRUE)

  # return
  return(list(
    data = data,
    par  = par
  ))

}
