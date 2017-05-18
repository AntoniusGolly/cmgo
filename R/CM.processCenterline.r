#' Process the centerline
#'
#' Derive width, slope and further principle channel metrics for the channel centerline previously created.
#'
#' \code{CM.processCenterline()} calculate the channel metrics (Fig. 9) based on the centerline previously calculated.
#' It does that by first deriving channel transects. The transects are lines perpendicular to a group of centerline points
#' where the size of that group is defined by the parameter \code{transects.span}. By default this span
#' equals three which means for each group of three centerline points a line is created through the outer points of
#' that group to which the perpendicular -- the transect -- is calculated (see Fig. 8b). In the final step the intersections of the
#' transects with the banks are calculated (Fig. 8c).
#'
#' \if{html}{\figure{08_processing.png}{options: width="800px" alt="Figure: processing"}}
#' \if{latex}{\figure{08_processing.pdf}{options: width=12cm}}
#'
#' \emph{Figure 8: A visualization of the transect calculation. a) the channel centerline, b) for each
#' group of \code{n} points (in the example n=3) a line is fitted through the outer points (black line).
#' The perpendicular to this line is the transect. c) the vectors from each centerline point to the left
#' and to the right bank are calculated and stored separately.}
#'
#' When the transects cross the banks multiple times, the minimum distance is taken. In addition to the width,
#' the distances of the centerline points to the banks is stored sepearately for the left and the right bank. This
#' is particularly of importance when using multiple time series of channel profiles. See paragraph in
#' the (see \link[=cmgo]{paragraph "Time series analyses" of package documentation}.
#'
#' The function returns the global data object extended by the following variables (length equals number
#' of points of the reference centerline): \preformatted{
#' $metrics$tr       # linear equations of the transects
#' $metrics$cp.r     # coordinates of crossing points transects / right bank
#' $metrics$cp.l     # coordinates of crossing points transects / left bank
#' $metrics$d.r      # distance of reference centerline point / right bank
#' $metrics$d.l      # distance of reference centerline point / left bank
#' $metrics$w        # channel width
#' $metrics$r.r      # direction value: -1 for right, +1 for left to the centerline
#' $metrics$r.l      # direction value: -1 for right, +1 for left to the centerline
#' $metrics$diff.r   # difference between right bank point of actual time series and right bank point of reference series
#' $metrics$diff.l   # difference between left bank point of actual time series and left bank point of reference series
#'}
#' If you calculate channel metrics for one channel survey, the right bank is always to the right side of the centerline and
#' the left bank always left to the centerline. Thus, the values of \code{r.r} are all -1 and the values of \code{r.l} all +1. However,
#' when using a reference centerline to compare different channel surveys, these values can be required. If for example
#' you examine the bank metrics for a reference centerline that is very different to the actual survey, the centerline
#' is not necessarily between the two banks. This can happen when a massive shift of the channel bed occurred (see Fig. 9).
#' For further calculations consider then the sign of the \code{r.r} and \code{r.l} values!
#'
#'
#'
#' @template param_global_data_object
#' @param set an optional argument for processing a specific data set, if \code{NULL} all available data sets are used
#' @return returns the global data object extended by the centerline data \code{$metrics} for the respective data set
#' @author Antonius Golly
#' @examples
#' # get demo data (find instructions on how to use own data in the documentation of CM.ini())
#' cmgo.obj = CM.ini("demo1")
#'
#' # calculate channel metrics from centerline
#' cmgo.obj = CM.processCenterline(cmgo.obj)
#'
#' @export CM.processCenterline

CM.processCenterline <- function(object, set=NULL){

  par  = object$par
  data = object$data
  sets = if(is.null(set)) names(data) else set

  notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}
  error     = function(x){stop(x, call.=FALSE)}
  warn      = function(x){warning(x, call.=FALSE)}

  notice("process centerline (calculate channel metrics)", TRUE)

  CM.calculateIntersections = function(tr, cb){

    # create all crossing points
    cp.Px  = (cb[,"n"] - tr["n"]) / (tr["m"] - cb[,"m"])
    cp.Py  = tr["m"] * cp.Px + tr["n"]

    # get scalar factors to filter to those which cross bank segments
    scalar = (cp.Py - cb[,"Py"]) / (cb[,"Ny"]  - cb[,"Py"])
    X      = cp.Px[scalar >= 0 & scalar <= 1]
    Y      = cp.Py[scalar >= 0 & scalar <= 1]

    # check length of coordinate vectors
    if(length(X) != length(Y)) stop("inconsistent number of crossing points (X,Y)")

    # find shortest section when more than one available
    d.ix   = which.min(((X - tr["Px"])^2 + (Y - tr["Py"])^2 )^(1/2))

    if(length(d.ix) == 0){
      warn(paste("no intersection found for the transect at x=", tr[["Px"]], ", y=", tr[["Py"]]))
      return(c(NA, NA, NA))
    }

    X      = X[d.ix]
    Y      = Y[d.ix]

    # get direction
    r      = X - tr["Px"]
    r      = if(r < 0) -1 else if(r > 0) 1 else 0

    # return
    return(c(X, Y, r))

  }

  CM.linearEquationsBanks     = function(x){

    if(!is.matrix(x)) return(F)

    # first and last elements represent averages over moving window
    x1 = x[1,1]
    x2 = x[2,1]
    y1 = x[1,2]
    y2 = x[2,2]

    # calculate slope (m) and center point (P)
    m  = (( y2 - y1 ) / ( x2 - x1 ))
    Px = x1
    Py = y1
    n  = Py - ( m * Px )
    Nx = x2
    Ny = y2

    # return
    return(data.frame(m=m,n=n,Px=Px,Py=Py,Nx=Nx,Ny=Ny))

  }

  CM.linearEquationsTransects = function(x){

    if(!is.matrix(x)) return(F)

    # first and last elements represent averages over moving window
    x1 = head(as.matrix(x)[,1], n=1)
    x2 = tail(as.matrix(x)[,1], n=1)
    y1 = head(as.matrix(x)[,2], n=1)
    y2 = tail(as.matrix(x)[,2], n=1)

    # calculate slope (m) and center point (P)
    m  = -1 / ((( y2 - y1 ) / ( x2 - x1 )))
    Px = (( x2 - x1 ) / 2 ) + x1
    Py = (( y2 - y1 ) / 2 ) + y1
    n  = Py - ( m * Px )

    # return
    return( data.frame(m=m,n=n,Px=Px,Py=Py) )

  }

  set = "set2" # ignore, only for debuggin

  for(set in sets){

    if(par$calculate.metrics){

      if(is.null(data[[set]]$metrics) || par$force.calc.metrics){

        cl.type = "smoothed" # it is highly recommended to do the processing on the "smoothed" version of the centerline
        set.ref  = if(par$centerline.use.reference) par$centerline.reference else set

        notice(paste("process data set '", set, "' with centerline reference '", set.ref,"' now...", sep=""))
        reasons = c()
        if(is.null(data[[set]]$metrics))     reasons = append(reasons, "data does not exis")
        if(par$force.calc.metrics)           reasons = append(reasons, "calculation is forced")
        notice(paste("reason for calculation:", paste(reasons, collapse = " + ")))


        ### calculate linear equations of transsects ##############
        tr = rollapply(as.data.frame(data[[set.ref]]$cl[[cl.type]][c("x","y")]), par$transects.span, CM.linearEquationsTransects, by.column=FALSE, partial=TRUE)
        if(par$transects.span==2) tr[nrow(tr),] = tr[nrow(tr)-1,] # when span==2 and partial=TRUE the last element will be 0,0,0,0 which will be substituted
        notice(paste("linear equations for transects calculated with span = ", par$transects.span,"!", sep=""))

        ### calculate linear equations of bank segments ###########
        cb.r     = rollapply(data.frame(x = data[[set]]$channel$x[data[[set]]$ix.r], y = data[[set]]$channel$y[data[[set]]$ix.r]), 2, CM.linearEquationsBanks, by.column=FALSE)
        cb.l     = rollapply(data.frame(x = data[[set]]$channel$x[data[[set]]$ix.l], y = data[[set]]$channel$y[data[[set]]$ix.l]), 2, CM.linearEquationsBanks, by.column=FALSE)
        notice("linear equations for bank segments calculated!")

        ### calculate intersections of transsects and bank segments
        cp.r = t(apply(tr, 1, function(x){ CM.calculateIntersections(x, cb.r) }))
        cp.l = t(apply(tr, 1, function(x){ CM.calculateIntersections(x, cb.l) }))
        notice("intersections calculated!")

        ### calculate distances ###################################
        d.r  = apply(cbind(cp.r[,c(1,2)], tr[,c(3,4)]),   1, function(x){ ( (x[1]-x[3])^2 + (x[2]-x[4])^2 )^(1/2)    })
        d.l  = apply(cbind(cp.l[,c(1,2)], tr[,c(3,4)]),   1, function(x){ ( (x[1]-x[3])^2 + (x[2]-x[4])^2 )^(1/2)    })

        ### calculate width #######################################
        w    = apply(cbind(cp.r[,c(1,2)], cp.l[,c(1,2)]), 1, function(x){ ( (x[1]-x[3])^2 + (x[2]-x[4])^2 )^(1/2)    })

        notice("width calculated!")

        # check
        #if(length(data[[set]]$cl[[cl.type]]$x) != length(cw)) error("length of CL and CW differ!")
        if(length(data[[set.ref]]$cl[[cl.type]]$x) != length(d.r)) error("length of CL and CW differ!")
        if(length(data[[set.ref]]$cl[[cl.type]]$x) != length(d.l)) error("length of CL and CW differ!")

        # store
        data[[set]]$metrics$cl.ref   = set.ref
        data[[set]]$metrics$cl.type  = cl.type

        data[[set]]$metrics$tr       = tr
        data[[set]]$metrics$cp.r     = cp.r[,c(1,2)]
        data[[set]]$metrics$cp.l     = cp.l[,c(1,2)]
        data[[set]]$metrics$d.r      = d.r
        data[[set]]$metrics$d.l      = d.l
        data[[set]]$metrics$w        = w
        data[[set]]$metrics$r.r      = cp.r[,3]
        data[[set]]$metrics$r.l      = cp.l[,3]
        data[[set]]$metrics$diff.r   = if(set == set.ref) rep(0, length(w)) else -(data[[set.ref]]$metrics$d.r * data[[set.ref]]$metrics$r.r - d.r * cp.r[,3])
        data[[set]]$metrics$diff.l   = if(set == set.ref) rep(0, length(w)) else   data[[set.ref]]$metrics$d.l * data[[set.ref]]$metrics$r.l - d.l * cp.l[,3]

        notice("centerline processed successfully!")

      } else { # if(is.null(data[[set]]$metrics) || par$force.calc.metrics)

        notice("(metrics taken from cache)")

      }

    } # if(par$calculate.width)

  } # for(set in names(data))

  # return
  return(list(
    data = data,
    par  = par
  ))

}
