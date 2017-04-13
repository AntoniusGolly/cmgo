#' Plot changes of the banks (erosion and aggradation)
#'
#' If more than one data set is defined in the global data object, of
#'
#' Details
#'
#' @template param_global_data_object
#' @param set the reference data set
#' @param cl The range of centerline points to be plotted, e.g. cl=c("120:130"). If NULL the full range will be plotted.
#' @return desc
#' @author Antonius Golly
#' @examples
#'
#' # open demo
#' cmgo.obj = CM.ini("demo2")
#'
#' # example 1: plot the change of the channel banks (aggradation/erosion)
#' CM.plotBankRetreat(cmgo.obj)
#'
#' # example 2: plot
#' CM.plotBankRetreat(cmgo.obj, set="set2")
#'
#' @export CM.plotBankRetreat

CM.plotBankRetreat <- function(object, set="set1", cl=NULL){

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
  notice("create bank retreat plot...")

  plots = c(1)
  par(mfcol=c(1,length(plots)))

  for(plot in plots){

    ### get reference centerline
    set.ref       = data[[set]]$metrics$cl.ref
    #set.secondary = if(is.null(set.compare)) set.ref else set.compare

    ### define cl range
    if(is.null(cl)) cl = par$plot.cl.range
    if(typeof(cl) == "character"){ if(cl %in% names(par$plot.cl.ranges)) cl = par$plot.cl.ranges[[cl]] else stop(paste("given cl range", cl, "unknown!"))}
    if(length(cl) == 1) cl = c(cl, cl+1)
    if(length(cl) == 2) cl = seq(from = cl[1], to = cl[2]);

    notice(paste("cl range set from", cl[1], "to", cl[length(cl)]))

    y.lim         = max(data[[set]]$metrics$d.r[cl],data[[set]]$metrics$d.l[cl] )
    leg = list()

    plot(
      0,
      main = paste("Bank retreat of", set, "(solid) and", set.ref, "reference (dashed)"),
      ylim = c(-y.lim, y.lim),
      xlim = c(cl[1],cl[1]+length(cl)),
      xlab = "Index",
      ylab = "Distance [m]",
      type = "n"
    )
    abline(h=0, col="gray")

    ############ right bank #############

    ## bank distance of set
    lines(data[[set]]$metrics$d.r     * data[[set]]$metrics$r.r,                     col = "green", lty=1)
    leg = leg.add(leg, paste("right bank of", set, "to", set.ref, "(reference)"),   col = "green", lty=1, lwd=1)

    ## bank distance of set.ref
    lines(data[[set.ref]]$metrics$d.r * data[[set.ref]]$metrics$r.r,                 col = "green", lty=2)
    leg = leg.add(leg, paste("right bank of", set.ref, "to", set.ref,"(reference)"),col = "green", lty=2, lwd=1)

    ## bank retreat
    lines(data[[set]]$metrics$diff.r,                                               col = colors()[240], lty=1, lwd=8)
    lines(data[[set]]$metrics$diff.r,                                               col = "green", lty=6, lwd=2)

    leg = leg.add(leg, paste("right bank retreat", set, "over", set.ref),           col = "green", lty=6, lwd=2)
    # 0 # lines(data[[set.ref]]$metrics$diff.r,                                     col = "green", lty=6, lwd=2)

    ##### left bank #####################
    ## bank distance of set
    lines(data[[set]]$metrics$d.l     * data[[set]]$metrics$r.l,                    col = "red",   lty=1)
    leg = leg.add(leg, paste("left bank of", set, "to", set.ref, "(reference)"),    col = "red",   lty=1, lwd=1)

    ## bank distance of set.ref
    lines(data[[set.ref]]$metrics$d.l * data[[set.ref]]$metrics$r.l,                col = "red",   lty=2)
    leg = leg.add(leg, paste("left bank of", set.ref, "to", set.ref,"(reference)"), col = "red",   lty=2, lwd=1)

    ## bank retreat
    lines(data[[set]]$metrics$diff.l,                                               col = colors()[240], lty=1, lwd=8)
    lines(data[[set]]$metrics$diff.l,                                               col = "red",   lty=6, lwd=2)
    leg = leg.add(leg, paste("left bank retreat", set, "over", set.ref),            col = "red",   lty=6, lwd=2)

    # 0 # lines(data[[set.ref]]$metrics$diff.l, col="red", lty=2, lwd=2)

    # legend and scale bar ####################################################
    if(par$plot.planview.legend) leg.make(leg)

  } # for(plot in plots)

  plot.file(par)

  notice("plotting done!")

}

