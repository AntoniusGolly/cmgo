#' Plot channel width
#'
#' Plot the channel width of the whole channel (default) or for a portion (use \code{cl} argument). The function can
#' also compare two data sets if they have the same reference centerline.
#'
#'
#' If more than one data set is defined in the global data object, of
#'
#' Details
#'
#' @template param_global_data_object
#' @param set the primary data set to plot ("set1" by default)
#' @param set.compare the secondary data set to plot (optional)
#' @param cl the range of centerline points to be plotted, if NULL (default) the full channel length will be plotted, if
#' a vector of two elements is provided (e.g. c(200, 500)) this cl range is plotted, if a string is provided (e.g. "cl1"),
#' the range defined in \code{par$plot.cl.ranges$cl1} will be plotted
#' @param d the distance range of the centerline downstream to be plotted, NULL (default) cl defintions are taken, if a
#' single value (e.g. d=500) is given 50m around this distance is plotted, if a vector with two elements is given (e.g.
#' c(280, 620)) this distance range will be plotted
#' @return desc
#' @author Antonius Golly
#' @examples
#'
#' # open demo
#' cmgo.obj = CM.ini("demo2")
#'
#' # example 1: plot channel width of whole channel
#' CM.plotWidth(cmgo.obj)
#'
#' # example 2: plot channel width of a defined range (centerline points)
#' CM.plotWidth(cmgo.obj, cl=c(800, 1000))
#'
#' # example 2: plot channel width of a defined range (distance downstream)
#' CM.plotWidth(cmgo.obj, d=c(200, 600))
#'
#' @export CM.plotWidth

CM.plotWidth <- function(object, set="set1", set.compare=NULL, cl=NULL, d=NULL){

  par  = object$par
  data = object$data

  notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}
  error     = function(x){stop(x, call.=FALSE)}
  alert     = function(x, y=""){if(y!=""){message(paste("--> ",x, y, sep=""))}else{message(paste("--> ", x, sep=""))}}
  warn      = function(x){warning(x, call.=FALSE)}
  plot.file = function(par){if(!par$plot.to.file) return(NULL); file.no   = 0 + par$plot.index; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep=""); while(file.exists(paste(file.name, ".png", sep="")) || file.exists(paste(file.name, ".pdf", sep=""))){  file.no   = file.no + 1; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep="") }; dev.copy(png, filename=paste(file.name, ".png", sep=""), width=800, height=600); dev.off(); dev.copy2pdf(file=paste(file.name, ".pdf", sep=""));}

  plot_secondary = FALSE

  leg.add = function(leg, tx, item=NULL, lwd=1, lty=NA, pch=NA, cex=1, col="black"){

    item = if(is.null(item)) paste("item_", length(leg)+1, sep="") else item

    leg[[item]] = list(
      tx   = tx,
      lwd  = lwd,
      lty  = lty,
      pch  = pch,
      cex  = cex,
      col  = col
    )

    return(leg)

  }

  leg.make = function(leg){

    tx = type = lwd = lty = pch = cex = col = c()


    for(item in c(1:length(leg))){
      tx   = append(tx,  leg[[item]]$tx)
      lwd  = append(lwd, leg[[item]]$lwd)
      lty  = append(lty, leg[[item]]$lty)
      pch  = append(pch, leg[[item]]$pch)
      cex  = append(cex, leg[[item]]$cex)
      col  = append(col, leg[[item]]$col)
    }

    leg.vec = function(x) return(if(length(unique(x))==1) x[1] else x)

    legend("topleft", inset=0.05,
      legend = leg.vec(tx),
      lwd    = leg.vec(lwd),
      lty    = leg.vec(lty),
      pch    = leg.vec(pch),
      pt.cex = leg.vec(cex),
      col    = leg.vec(col)
    )
  }

  #dev.new(width=12, height=10)
  notice("create channel width plot...", TRUE)

  plots = c(1)
  par(mfcol=c(1,length(plots)))

  for(plot in plots){

    ### get reference centerline
    #set.ref       = data[[set]]$metrics$cl.ref
    #set.secondary = if(is.null(set.compare)) set.ref else set.compare

    ### define cl range
    if(is.null(cl)) cl = seq(along=data[[set]]$cl$smoothed$x)
    if(typeof(cl) == "character"){ if(cl %in% names(par$plot.cl.ranges)) cl = par$plot.cl.ranges[[cl]] else stop(paste("given cl range", cl, "unknown!"))}
    if(length(cl) == 1) cl = c(cl, cl + 50)
    if(length(cl) == 2) cl = seq(from = cl[1], to = cl[2]);

    if(!is.null(d)){
      if(length(d) == 1) d = c(d-25, d+25)
      if(d[1] < 0 ) d[1] = 0; if(d[2] >  tail(data[[set]]$cl$smoothed$cum_dist_2d, n=1)) d[2] = tail(data[[set]]$cl$smoothed$cum_dist_2d, n=1)
      if(length(d) == 2) d = seq(from = d[1], to = d[2])
      cl = seq(
        from = which.min(abs(data[[set]]$cl$smoothed$cum_dist_2d - d[1])),
        to   = which.min(abs(data[[set]]$cl$smoothed$cum_dist_2d - d[length(d)]))
      )
      notice(paste("d range: distance", d[1], "to", d[length(d)]))
    }

    notice(paste("cl range: index", cl[1], "to", cl[length(cl)]))
    notice(paste("plot ", set, " (reference ", data[[set]]$metrics$cl.ref, ")", sep=""))


    leg = list()

    y.lim = range(data[[set]]$metrics$w[cl], na.rm=TRUE)
    if(!is.null(set.compare)) y.lim = range(data[[set]]$metrics$w[cl],data[[set.compare]]$metrics$w[cl])

    plot(
      0,
      main = paste("Channel width of", set, if(!is.null(set.compare)) paste("(solid) and", set.compare, " (dashed)") else ""),
      ylim = y.lim,
      xlim = c(data[[set]]$cl$smoothed$cum_dist_2d[cl[1]],data[[set]]$cl$smoothed$cum_dist_2d[cl[length(cl)]]),
      xlab = paste("Distance upstream [", par$input.unit, "]", sep=""),
      ylab = paste("Width [", par$input.unit, "]", sep=""),
      type = "n"
    )

    ## channel width main set
    lines(data[[set]]$metrics$w ~ data[[set]]$cl$smoothed$cum_dist_2d,                    col = "blue", lty=1, lwd=2)
    leg = leg.add(leg, paste("channel width of", set),                                    col = "blue", lty=1, lwd=2)

    ## channel width compare set
    if(!is.null(set.compare)){
      lines(data[[set.compare]]$metrics$w ~ data[[set.compare]]$cl$smoothed$cum_dist_2d,  col = "green", lty=1, lwd=2)
      leg = leg.add(leg, paste("channel width of", set.compare),                          col = "green", lty=1, lwd=2)

      notice(paste("added ", set.compare, " (reference ", data[[set.compare]]$metrics$cl.ref, ") for comparison", sep=""))

      if(data[[set.compare]]$metrics$cl.ref == data[[set]]$metrics$cl.ref){

      }


    }


    # legend and scale bar ####################################################
    if(par$plot.planview.legend) leg.make(leg)

  } # for(plot in plots)

  plot.file(par)

  notice("plotting done!", TRUE)

}
