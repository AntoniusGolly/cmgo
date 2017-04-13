#' Reduce centerline points
#'
#' Create an equally spaced centerline by decreasing its spatial resolution while preserving its length.
#'
#' The spatial resolution of the centerline depends on the spatial resolution of the polygon generated from the bank points. Thus,
#' it is directly dependent from the parameter par$polygon.bank.interpolate.max.dist. If you have chosen a value smaller
#' than the channel width, the resulting resolution of the centerline can be very high. This results in a large number of
#' centerline points, which are not necessary and introduce high computational costs during further calculation. With
#' the function CM.reduceCenterline you can again decrease the resolution to a given value.
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
#' cmgo.obj = CM.reduceCenterline(cmgo.obj)
#'
#' @export CM.reduceCenterline

CM.reduceCenterline <- function(object, set = NULL){

  par  = object$par
  data = object$data
  sets = if(is.null(set)) names(data) else set

  notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}
  error     = function(x){stop(x, call.=FALSE)}
  alert     = function(x, y=""){if(y!=""){message(paste("--> ",x, y, sep=""))}else{message(paste("--> ", x, sep=""))}}
  warn      = function(x){warning(x, call.=FALSE)}
  plot.file = function(par){if(!par$plot.to.file) return(NULL); file.no   = 0 + par$plot.index; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep=""); while(file.exists(paste(file.name, ".png", sep="")) || file.exists(paste(file.name, ".pdf", sep=""))){  file.no   = file.no + 1; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep="") }; dev.copy(png, filename=paste(file.name, ".png", sep=""), width=800, height=600); dev.off(); dev.copy2pdf(file=paste(file.name, ".pdf", sep=""));}

  notice("reduce centerline resolution...", TRUE)

  cl.type = "smoothed"
  set     = "set1"

  for(set in sets){

    notice(paste("reduce centerline of", set))

    ixs = length(data[[set]]$cl$smoothed$length)
    l   = data[[set]]$cl$smoothed$length[ixs]

    notice(paste("current spacing: ", round(l/ixs, digits=3), par$input.unit, "average spacing"))
    notice(paste("intended spacing:", par$centerline.bin.length, par$input.unit, "spacing"))

    if( (round(l/ixs, digits=3) * 2) > par$centerline.bin.length) error("intended spacing must be at least two times the current spacing")

    bin_vec     = seq(from=1, to=round(max(data[[set]]$cl[[cl.type]]$length)), by = par$centerline.bin.length)
    bin_metrics = apply(as.array(bin_vec), 1, function(x){

      bin_ix    = which((data[[set]]$cl[[cl.type]]$length > (x - par$centerline.bin.length/2)) & (data[[set]]$cl[[cl.type]]$length < (x + par$centerline.bin.length/2)))
      bin_ix_z  = which.min(abs(data[[set]]$cl$original$length - x))

      if(length(bin_ix) == 0){
        bin_ix  = which.min(abs(data[[set]]$cl$smoothed$length - x))
        notice(paste("no points in bin ", x, "! take nearest point...", sep=""))
      }

      return (data.frame(

        x = mean(data[[set]]$cl[[cl.type]]$x[bin_ix]),
        y = mean(data[[set]]$cl[[cl.type]]$y[bin_ix]),
        z = if(!is.null(data[[set]]$channel$z)) mean(data[[set]]$cl$original$z[bin_ix]) else NA,

        width_median = median(data[[set]]$metrics$w[bin_ix]),
        width_mean   = mean(data[[set]]$metrics$w[bin_ix])

        #slope_median = if(!is.null(data[[set]]$channel$z)) median(data[[set]]$metrics$slope[bin_ix]) else NA,
        #slope_mean   = if(!is.null(data[[set]]$channel$z)) mean(data[[set]]$metrics$slope[bin_ix])   else NA

      ))

    })

    # store
    data[[set]]$cl$binned             = as.list(do.call(rbind, bin_metrics))
    data[[set]]$cl$binned$length      = bin_vec
    data[[set]]$cl$binned$lengthInter = bin_vec[-length(bin_vec)] + diff(bin_vec) / 2
    data[[set]]$cl$binned$bin.length  = par$centerline.bin.length

    notice(paste("re-binning centerline of", set, "done!"), TRUE)

  }



  # return
  return(list(
    data = data,
    par  = par
  ))

}
