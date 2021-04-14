#' Generate polygon from bank points
#'
#' Generates an object of type polygon from the bank points. The bank points must be
#' ordered and must have an attribute "left" or "right".
#'
#' \code{CM.generatePolygon()} creates a polygon from the bank points which were previously
#'  loaded with \code{\link[=CM.ini]{CM.ini()}}. The polygon generation is mainly
#' a type conversion but involves also the chaining of the points in the correct
#' order. That is, taking all bank points from the left bank and
#' chain them with all reversed points of the right bank. The bank points from the right side
#' have to be reversed so that a valid, circular polygon results (see figure). Since this process
#' is prone to error when the points are not provided in the expected order, there is a visual feedback
#' of this process. An overview of the generated polygon will be plotted to check the polygon's consistency. This
#' plot is by default enabled but can be suppressed by setting \code{par$plot.polygoncheck} to \code{FALSE}.
#'
#' \if{html}{\figure{plotCheckPolygon.png}{options: width="600px" alt="Figure: check polygon"}}
#' \if{latex}{\figure{plotCheckPolygon.pdf}{options: width=8cm}}
#' \emph{Figure 5: The plot shows the polygon check plot for two data sets (two rows). The columns are: overview of the full channel (left),
#' start of the polygon (middle) and end of the polygon (right). For the first data set everything looks fine. The
#' polygon is circular and the ends look consistent (note, the starting end (middle) is open). The second data
#' set is corrupt. You can see a line crossing the entire channel (left). Also, at the start and end
#' of the channel there are discrepancies. This means that the order of bank points was not given properly. In this
#' case, run \code{\link[=CM.ini]{CM.ini()}} again after re-arranging the channel bank points.}
#'
#' \code{CM.generatePolygon()}  also applies a linear interpolation to the bank points if desired. The spacing of the
#' bank points directly affects the resolution of the centerline (see \link[=cmgo]{paragraph "Work flow"
#' of package documentation}). Generally, a high resolution of the centerline represents the channel course
#' better but leads to more data points and higher computational costs. The linear interpolation will
#' insert equidistant points in between existing bank points where the added points have a maximum distance defined by
#' the parameter \code{bank.interpolate.max.dist}. This value is by default 6 and needs to be adjusted
#' to the individual geometry. As a rule of thumb it is advised to \strong{change bank.interpolate.max.dist to
#' the value of the expected channel width}! Note, that the units of this value is always the same unit
#' as your input coordinates. The \emph{best} resolution of the centerline, however, depends also on the shape of
#' the channel. If you experience problems with the chosen interpolation distance read the \link[=cmgo]{paragraph
#' "Technical fails and how to prevent them" of the package documentation}).
#'
#' \code{CM.generatePolygon()} requires the global data object as argument and -- within this -- a list of bank points.
#' The list of bank points must be previously created with \code{\link[=CM.ini]{CM.ini()}}. See
#' the example section for the structure of the expected input. If multiple data sets (time series)
#' are available, the polygon generation will be performed on each data set.
#'
#' @template param_global_data_object
#' @return returns the global data object extended by the polygon data \code{$polygon} for the respective data set(s)
#' @author Antonius Golly
#' @examples
#' # get demo data (find instructions on how to use own data in the documentation of CM.ini())
#' cmgo.obj = CM.ini("demo")
#'
#' # check structure of the required input for CM.generatePolygon()
#' set = "set1"
#' print(str(cmgo.obj$data[[set]]$channel))
#'
#' #List of 4
#' # $ x   : num [1:396] 401601 401587 401568 401558 401552 ...
#' # $ y   : num [1:396] 3106437 3106407 3106384 3106364 3106329 ...
#' # $ z   : NULL
#' # $ bank: chr [1:396] "right" "right" "right" "right" ...
#'
#' # generate the polygon from the bank points
#' cmgo.obj = CM.generatePolygon(cmgo.obj)
#'
#' @export CM.generatePolygon

CM.generatePolygon <- function(cmgo.obj){

  plotCheckPolygon <- function(cmgo.obj){

    par  = cmgo.obj$par
    data = cmgo.obj$data

    notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}

    notice("check topology of generated channel polygon in the plan view plot...", TRUE)

    #par(mfrow=c(length(names(data)),3))
    par(mfrow=c(1,1))
    for(set in names(data)){

      plot(data[[set]]$polygon$y ~ data[[set]]$polygon$x, asp=1, type="l", xlab="X", ylab="Y", main=paste(paste("Channel overview ", set, " (", data[[set]]$survey, ")", sep="")))
      if(par$plot.polygoncheck.colors == TRUE) points(data[[set]]$polygon$y ~ data[[set]]$polygon$x, asp=1, cex=0.3, col=colorRampPalette(c("blue", "red"))(length(data[[set]]$polygon$x)))
      n = 0

      ## produce an overview plot on both ends of the river polygon
      if(FALSE){
        for(i in c(min(which(data[[set]]$channel$bank=="left")), max(which(data[[set]]$channel$bank=="right")))){

          n = n+1
          title = "end"; if(n == 1) title ="start";
          title = paste(title, "of polygon", set)

          x.lim = c(data[[set]]$channel$x[i] - par$plot.zoom.extent.length, data[[set]]$channel$x[i] + par$plot.zoom.extent.length)
          y.lim = c(data[[set]]$channel$y[i] - par$plot.zoom.extent.length, data[[set]]$channel$y[i] + par$plot.zoom.extent.length)


          plot(data[[set]]$polygon$y ~ data[[set]]$polygon$x,
            asp=1, type="l", xlab="X", ylab="Y", xlim = x.lim, ylim = y.lim,  main = title
          )

          points(data[[set]]$polygon$y ~ data[[set]]$polygon$x, pch=4, cex=0.5, col="red" )
          points(data[[set]]$channel$y ~ data[[set]]$channel$x, pch=4, cex=1,   col="blue")

        }
      }

      if(length(names(data)) > 1) {cat ("Press [Enter] to continue"); line <- readline()}

    }

    notice("plotting done!")

  } # plotCheckPolygon

  par  = cmgo.obj$par
  data = cmgo.obj$data

  notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}
  error     = function(x){stop(x, call.=FALSE)}
  alert     = function(x, y=""){if(y!=""){message(paste("--> ",x, y, sep=""))}else{message(paste("--> ", x, sep=""))}}
  warn      = function(x){warning(x, call.=FALSE)}
  plot.file = function(par){if(!par$plot.to.file) return(NULL); file.no   = 0 + par$plot.index; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep=""); while(file.exists(paste(file.name, ".png", sep="")) || file.exists(paste(file.name, ".pdf", sep=""))){  file.no   = file.no + 1; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep="") }; dev.copy(png, filename=paste(file.name, ".png", sep=""), width=800, height=600); dev.off(); dev.copy2pdf(file=paste(file.name, ".pdf", sep=""));}

  notice(paste("generate polygon from bank points for", length(names(data)), "set(s)"), TRUE)

  notice(paste("> bank.interpolate          =", par$bank.interpolate))
  notice(paste("> bank.interpolate.max.dist =", par$bank.interpolate.max.dist))
  notice(paste("> bank.reduce               =", par$bank.reduce))
  notice(paste("> bank.reduce.min.dist      =", par$bank.reduce.min.dist))

  for(set in names(data)){

    if(is.null(data[[set]]$polygon)

      || isTRUE(data[[set]]$polygon.bank.interpolate != par$bank.interpolate) || isTRUE(data[[set]]$polygon.bank.interpolate.max.dist != par$bank.interpolate.max.dist)
      || isTRUE(data[[set]]$polygon.bank.reduce      != par$bank.reduce)      || isTRUE(data[[set]]$polygon.bank.reduce.min.dist      != par$bank.reduce.min.dist)

      ){

      notice(paste("\nprocessing ", set, " (", data[[set]]$survey, ")...", sep=""))

      data[[set]]$polygon.bank.interpolate          = par$bank.interpolate
      data[[set]]$polygon.bank.interpolate.max.dist = NULL

      data[[set]]$polygon.bank.reduce               = par$bank.reduce
      data[[set]]$polygon.bank.reduce.min.dist      = NULL


      ix.l = which(data[[set]]$channel$bank == "left")
      ix.r = which(data[[set]]$channel$bank == "right")

      # check for existing bank codes
      if(length(ix.l) == 0) error(paste("no bank points found with the given code \"", par$bank.code.left,  "\". Check parameters!", sep=""))
      if(length(ix.r) == 0) error(paste("no bank points found with the given code \"", par$bank.code.right, "\". Check parameters!", sep=""))

      data[[set]]$ix.l = ix.l
      data[[set]]$ix.r = ix.r

      # create bank object from original bank points
      banks = list(
        x = list(left = data[[set]]$channel$x[ix.l], right = data[[set]]$channel$x[ix.r]),
        y = list(left = data[[set]]$channel$y[ix.l], right = data[[set]]$channel$y[ix.r])
      )

      banks_original = banks




      # interpolate bank points (optional)
      if(par$bank.interpolate){

        notice("interpolate bank points to generate a denser polygon...")

        # create (empty) bank point object
        banks = list(
          x = list(left = c(), right = c()),
          y = list(left = c(), right = c())
        )
        ap = 0 # counter to sum the points added

        # linearly interpolate bank points
        for(bank in c("left", "right")){

          for(i in c(2:length(banks_original$x[[bank]]))){

            # add the actual point
            banks$x[[bank]] = append(banks$x[[bank]], banks_original$x[[bank]][i-1])
            banks$y[[bank]] = append(banks$y[[bank]], banks_original$y[[bank]][i-1])

            # calculate distance between consecutive bank points
            cb_dist = ( ( banks_original$x[[bank]][i] - banks_original$x[[bank]][i-1] )^2 + ( banks_original$y[[bank]][i] - banks_original$y[[bank]][i-1] )^2 ) ^(1/2)

            # if distance larger than threshold distance interpolate
            if(cb_dist > par$bank.interpolate.max.dist){

              # calculate number of artificial points
              nr_of_pts = ceiling(cb_dist / par$bank.interpolate.max.dist)
              ap = ap + nr_of_pts

              # add artificial points
              for(ido in c(1: (nr_of_pts-1))){

                banks$x[[bank]] = append(banks$x[[bank]], banks_original$x[[bank]][i-1] + ( ( (banks_original$x[[bank]][i] - banks_original$x[[bank]][i-1]) / nr_of_pts ) * ido ) )
                banks$y[[bank]] = append(banks$y[[bank]], banks_original$y[[bank]][i-1] + ( ( (banks_original$y[[bank]][i] - banks_original$y[[bank]][i-1]) / nr_of_pts ) * ido ) )

              }

            } # if(cb_dist > par$bank.man.dist)

          } # for(i in c(2:length(ix)))

          # add the final point
          banks$x[[bank]] = append(banks$x[[bank]], tail(banks_original$x[[bank]], n=1))
          banks$y[[bank]] = append(banks$y[[bank]], tail(banks_original$y[[bank]], n=1))

        } # for(bank in c("left", "right"))

        data[[set]]$polygon.bank.interpolate.max.dist = par$bank.interpolate.max.dist
        banks_original = banks

        notice(paste("banks of", set, "interpolated with", ap, "points added!"))

      } # if(par$bank.interpolate)




      # reduce bank points (optional)
      if(par$bank.reduce){

        notice("reduce bank points to generate a coarser polygon...")

        ap = 0 # counter to sum the points added

        banks = list(
          x = list(left = c(banks_original$x$left[1]), right = c(banks_original$x$right[1])),
          y = list(left = c(banks_original$y$left[1]), right = c(banks_original$y$right[1]))
        )

        # linearly interpolate bank points
        for(bank in c("left", "right")){

          for(i in c(2:length(banks_original$x[[bank]]))){

            # calculate distance between consecutive bank points
            cb_dist = ( ( banks_original$x[[bank]][i] - banks$x[[bank]][length(banks$x[[bank]])] )^2 + ( banks_original$y[[bank]][i] - banks$y[[bank]][length(banks$y[[bank]])] )^2 ) ^(1/2)

            # if distance larger than threshold distance interpolate
            if(cb_dist < par$bank.reduce.min.dist){

              ap = ap + 1

            } else { # if(cb_dist > par$bank.man.dist)

              # add the final point
              banks$x[[bank]] = append(banks$x[[bank]], banks_original$x[[bank]][i])
              banks$y[[bank]] = append(banks$y[[bank]], banks_original$y[[bank]][i])

            } # if(cb_dist > par$bank.man.dist) {} else

          } # for(i in c(2:length(ix)))

        } # for(bank in c("left", "right"))

        data[[set]]$polygon.bank.reduce.min.dist = par$bank.reduce.min.dist

        notice(paste("banks of", set, "reduced with", ap, "points removed!"))

      } # if(par$bank.interpolate)



      # store the final bank points as a polygon
      data[[set]]$polygon = ppp(
        c(banks$x$left, rev(banks$x$right)),
        c(banks$y$left, rev(banks$y$right)),
        window=owin(
          range(banks$x, na.rm=TRUE),
          range(banks$y, na.rm=TRUE)
        )
      )

    } else {

      notice(paste("(polygon for", set, "taken from cache)"))

    }

  } # for(set in names(data))

  notice("CM.generatePolygon() has ended successfully!", TRUE)

  object = list(
    data = data,
    par  = par
  )

  if(par$plot.polygoncheck) plotCheckPolygon(object) # create a plot to check polygon consistency (optional)

  # return
  return(object)

}
