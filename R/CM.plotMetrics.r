#' Plot changes of the banks (erosion and aggradation)
#'
#' If multiple channel surveys are present (time series analyses) and the reference centerline mode is
#' used, this function allows to plot the shift of the banks (bank erosion and aggradation) along the channel.
#'
#' \code{CM.plotMetrics()} allows to plot the position of banks with regard to the centerline in downstream
#' direction. If only one survey is present, the plot will only show the distance of the right and left bank
#' to the centerline over the centerline distance (downstream length of the channel). If multiple surveys of a
#' channel exists (time series analyses) the differences of the channel banks can be calculated if a reference
#' centerline is used (reference centerline mode). The differences (bank ersosion and aggradation) are plotted
#' with this function.
#'
#' Without passing something to the function, the banks of the whole channel are plotted. To specify a region
#' use the \code{cl} and \code{d} parameters. See the parameter definition and the example section for details.
#'
#' @template param_global_data_object
#' @param set the reference data set
#' @param cl the range of centerline points to be plotted, if NULL (default) the full channel length will be plotted, if
#' a vector of two elements is provided (e.g. c(200, 500)) this cl range is plotted, if a string is provided (e.g. "cl1"),
#' the range defined in \code{par$plot.cl.ranges$cl1} will be plotted
#' @param d the distance range of the centerline downstream to be plotted, NULL (default) cl defintions are taken, if a
#' single value (e.g. d=500) is given 50 m around this distance is plotted, if a vector with two elements is given (e.g.
#' c(280, 620)) this distance range will be plotted
#' @return desc
#' @author Antonius Golly
#' @examples
#'
#' # open demo
#' cmgo.obj = CM.ini("demo3")
#'
#' # example 1: plot the distance of channel banks to centerline
#' CM.plotMetrics(cmgo.obj)
#'
#' # example 2: plot the change of the channel banks (aggradation/erosion)
#' CM.plotMetrics(cmgo.obj, set="set2")
#'
#' # example 3: plot only a range
#' CM.plotMetrics(cmgo.obj, set="set2", cl=c(800, 850))
#' CM.plotPlanView(cmgo.obj,  set="set2", cl=c(800, 850)) # compare with plan view map
#'
#' @export CM.plotMetrics

CM.plotMetrics <- function(object, set="set1", cl=NULL, d=NULL){

  par  = object$par
  data = object$data

  notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}
  error     = function(x){stop(x, call.=FALSE)}
  alert     = function(x, y=""){if(y!=""){message(paste("--> ",x, y, sep=""))}else{message(paste("--> ", x, sep=""))}}
  warn      = function(x){warning(x, call.=FALSE)}
  plot.file = function(par){if(!par$plot.to.file) return(NULL); file.no   = 0 + par$plot.index; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep=""); while(file.exists(paste(file.name, ".png", sep="")) || file.exists(paste(file.name, ".pdf", sep=""))){  file.no   = file.no + 1; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep="") }; dev.copy(png, filename=paste(file.name, ".png", sep=""), width=800, height=600); dev.off(); dev.copy2pdf(file=paste(file.name, ".pdf", sep=""));}

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
  notice("create bank retreat plot...", TRUE)

  plots = c(1)
  par(mfcol=c(1,length(plots)))

  for(plot in plots){

    ### get reference centerline
    set.ref       = data[[set]]$metrics$cl.ref
    #set.secondary = if(is.null(set.compare)) set.ref else set.compare

    ## build names
    name.set     = if(par$plot.metrics.use.names) data[[set]]$survey else set
    name.set.ref = if(par$plot.metrics.use.names) {if(!is.null(set.ref)) data[[set.ref]]$survey else "not set" } else set.ref

    ### define cl range
    if(is.null(cl)) cl = seq(along=data[[set.ref]]$cl$smoothed$x)
    if(typeof(cl) == "character"){ if(cl %in% names(par$plot.cl.ranges)) cl = par$plot.cl.ranges[[cl]] else stop(paste("given cl range", cl, "unknown!"))}
    if(length(cl) == 1) cl = c(cl, cl+1)
    if(length(cl) == 2) cl = seq(from = cl[1], to = cl[2]);

    if(!is.null(d)){
      if(length(d) == 1) d = c(d-25, d+25)
      if(d[1] < 0 ) d[1] = 0; if(d[2] >  tail(data[[set.ref]]$cl$smoothed$cum_dist_2d, n=1)) d[2] = tail(data[[set.ref]]$cl$smoothed$cum_dist_2d, n=1)
      if(length(d) == 2) d = seq(from = d[1], to = d[2])
      cl = seq(
        from = which.min(abs(data[[set.ref]]$cl$smoothed$cum_dist_2d - d[1])),
        to   = which.min(abs(data[[set.ref]]$cl$smoothed$cum_dist_2d - d[length(d)]))
      )
      notice(paste("d range: distance", d[1], "to", d[length(d)]))
    }

    notice(paste("cl range: index", cl[1], "to", cl[length(cl)]))

    leg = list()

    y.lim = max(data[[set.ref]]$metrics$d.r[cl],data[[set.ref]]$metrics$d.l[cl], data[[set]]$metrics$d.r[cl], data[[set]]$metrics$d.l[cl], na.rm=TRUE)
    if(set != set.ref) y.lim = max(y.lim , data[[set]]$metrics$diff.r[cl], data[[set]]$metrics$diff.l[cl], na.rm=TRUE)

    plot(
      0,
      main = if(set == set.ref) paste("Bank position of", name.set) else paste("Bank shift of", name.set, "(solid) over", name.set.ref, "(dashed)"),
      ylim = c(-y.lim, y.lim),
      xlim = c(data[[set.ref]]$cl$smoothed$cum_dist_2d[cl[1]],data[[set.ref]]$cl$smoothed$cum_dist_2d[cl[length(cl)]]),
      xlab = paste("Distance upstream [", par$input.unit, "]", sep=""),
      ylab = "Distance [m]",
      type = "n"
    )
    abline(h=0, col="gray")

    ############ right bank #############

    length     = data[[set.ref]]$cl$smoothed$cum_dist_2d

    ## bank distance of set
    lines(data[[set]]$metrics$d.r * data[[set]]$metrics$r.r ~ length,                    col = "green", lty=1)
    leg = leg.add(leg, paste("right bank of", name.set, "(reference cl:", name.set.ref, ")"),      col = "green", lty=1, lwd=1)

    ## bank distance of set.ref
    if(set != set.ref){
      lines(data[[set.ref]]$metrics$d.r * data[[set.ref]]$metrics$r.r ~ length,          col = "green", lty=2)
      leg = leg.add(leg, paste("right bank of", name.set.ref, "(reference cl:", name.set.ref,")"), col = "green", lty=2, lwd=1)
    }

    ## bank retreat
    if(set != set.ref){
      lines(data[[set]]$metrics$diff.r ~ length,                                         col = colors()[240], lty=1, lwd=8)
      lines(data[[set]]$metrics$diff.r ~ length,                                         col = "green", lty=6, lwd=2)
      leg = leg.add(leg, paste("right bank shift", name.set, "over", name.set.ref),                col = "green", lty=6, lwd=2)
    }


    ##### left bank #####################
    ## bank distance of set
    lines(data[[set]]$metrics$d.l     * data[[set]]$metrics$r.l ~ length,                col = "red",   lty=1)
    leg = leg.add(leg, paste("left bank of", name.set, "(reference cl:", name.set.ref, ")"),       col = "red",   lty=1, lwd=1)

    ## bank distance of set.ref
    if(set != set.ref){
      lines(data[[set.ref]]$metrics$d.l * data[[set.ref]]$metrics$r.l ~ length    ,      col = "red",   lty=2)
      leg = leg.add(leg, paste("left bank of", name.set.ref, "(reference cl:", name.set.ref,")"),  col = "red",   lty=2, lwd=1)
    }

    ## bank retreat
    if(set != set.ref){
      lines(data[[set]]$metrics$diff.l ~ length,                                         col = colors()[240], lty=1, lwd=8)
      lines(data[[set]]$metrics$diff.l ~ length,                                         col = "red",   lty=6, lwd=2)
      leg = leg.add(leg, paste("left bank shift", name.set, "over", name.set.ref),                 col = "red",   lty=6, lwd=2)
    }

    # legend and scale bar ####################################################
    if(par$plot.planview.legend) leg.make(leg)

  } # for(plot in plots)

  plot.file(par)

  notice("plotting done!")

}