#' Calculate channel centerline
#'
#' Calculate the centerline of the channel polygon in 5 steps:
#' \enumerate{
#'   \item creating Voronoi polygons of the bank points, convert to paths (line segments with two ends) and remove duplicates\cr
#'   \item filtering for path segments that lie within the banks\cr
#'   \item filtering for path segments that are dead ends (have less than 2 connected ends)\cr
#'   \item sorting of the centerline segments to generate centerline\cr
#'   \item smooth the centerline
#' }
#'
#' \code{CM.calculateCenterline()} calculates the centerline of the channel polygon (Fig. 7).
#'
#' \if{html}{\figure{06_processing.png}{options: width="800px" alt="Figure: processing"}}
#' \if{latex}{\figure{06_processing.pdf}{options: width=12cm}}
#'
#' \emph{Figure 6: A visualization of the calculation of the centerline a) the channel polygon, b) the Voronoi polygons,
#' c) extraction of the centerline segments, d) smoothing of the centerline path.}
#'
#' The function requires as input the channel polygon (Fig. 6a) which must be stored within the global data object
#' previously generated with \code{\link[=CM.generatePolygon]{CM.generatePolygon()}}.
#' The algorithm then creates Voronoi polygons around the bank points (Fig. 6b).
#' Voronoi polygons around points denote the areas within which all points are closest to that point. The polygons
#' are disassembled to single line segments. In Fig. 6b you can already notice a centerline evolving from the segments in
#' the middle of the channel polygon. To get only these segments a filtering (Fig. 7) is applied to the Voronoi segments.
#'
#' \if{html}{\figure{07_filtering.png}{options: width="600px" alt="Figure: processing"}}
#' \if{latex}{\figure{07_filtering.pdf}{options: width=6cm}}
#'
#' \emph{Figure 7: the filtering of the Voronoi segments: a) in blue all Voronoi segments, b) in red all segments fully within
#' the channel polygon, c) in green all segments without dead ends.}
#'
#' To retrieve only the segments that represent the centerline all segments that do not lie entirely
#' within the channel banks are removed (Fig. 7b). In a second step dead ends are removed (Fig. 7c). Dead ends are
#' segments that branch from the centerline but are not part of it.
#'
#' These centerline segments will be
#' chained to one consistent line and get smoothed (Fig. 7d). The degree of smoothing can be adjusted
#' through the parameter \code{centerline.smoothing.width} (defaults to the same value as
#' \code{bank.interpolate.max.dist}). This centerline represents the reference of the river, for which
#' length, local width and slope are calculated next. Note, that the length of the centerline has decreased
#' by the smoothing in d). It is important to understand, that the length of a river is not a well-defined measure.
#' The length of a river depends on the resolution of the bank points. Similar to
#' \href{https://en.wikipedia.org/wiki/Coast#Coastline_problem}{the coast line paradox}, the length depends on the
#' scale of the observations. Technically, a bended river is a fractal, which
#' means theoretically, the length diverges to infinity at an infinitely high resolution of the bank points.
#' However, practically there is an appropriate choice of a minimum feature size. Every user has to determine this
#' scale individually and should be aware of this choice. The decrease in length due to smoothing
#' is saved as value in the global data object under \code{cmgo.obj$data[[set]]$cl$length.factor}. A value of 0.95 means
#' that the length of the smoothed centerline is 95\% the length of the original centerline paths.
#'
#' @template param_global_data_object
#' @template param_set
#' @return returns the global data object extended by the centerline data \code{$cl} for the respective data set(s)
#' @author Antonius Golly
#' @examples
#' # get demo data
#' # (find instructions on how to use own data in the documentation of CM.ini())
#' cmgo.obj = CM.ini("demo")
#'
#' # generate the polygon from the bank points
#' cmgo.obj = CM.generatePolygon(cmgo.obj)
#'
#' # calculate the centerline from the polygon
#' cmgo.obj = CM.calculateCenterline(cmgo.obj)
#'
#' # check results
#' plot.par = CM.plotPlanView(cmgo.obj)
#'
#' # change degree of smoothing, re-calculate centerline and plot
#' cmgo.obj$par$centerline.smoothing.width = 12
#' cmgo.obj = CM.calculateCenterline(cmgo.obj)
#' plot.par = CM.plotPlanView(cmgo.obj)
#'
#' @export CM.calculateCenterline

CM.calculateCenterline <- function(object, set=NULL){

  par  = object$par
  data = object$data
  sets = if(is.null(set)) names(data) else set

  notice    = function(x,prim=FALSE){cat(paste((if(prim) "\n--> " else " "), x, sep=""), sep="\n")}
  error     = function(x){stop(x, call.=FALSE)}
  alert     = function(x, y=""){if(y!=""){message(paste("--> ",x, y, sep=""))}else{message(paste("--> ", x, sep=""))}}
  warn      = function(x){warning(x, call.=FALSE)}
  plot.file = function(par){if(!par$plot.to.file) return(NULL); file.no   = 0 + par$plot.index; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep=""); while(file.exists(paste(file.name, ".png", sep="")) || file.exists(paste(file.name, ".pdf", sep=""))){  file.no   = file.no + 1; file.name = paste(par$plot.directory, str_pad(file.no, 3, pad="0"), "_", par$plot.filename, sep="") }; dev.copy(png, filename=paste(file.name, ".png", sep=""), width=800, height=600); dev.off(); dev.copy2pdf(file=paste(file.name, ".pdf", sep=""));}

  notice("calculate centerline", TRUE)

  for(set in sets){

    if(is.null(data[[set]]$cl$smoothed) || par$force.calc.cl){

      # notice
      notice(paste("calculate centerline of", set, "now..."), TRUE)
      reasons = c()
      if(is.null(data[[set]]$cl$smoothed)) reasons = append(reasons, "data does not exis")
      if(par$force.calc.cl)                reasons = append(reasons, "calculation is forced")
      notice(paste("reason for calculation:", paste(reasons, collapse = " + ")))

      # check if parameter matches data base
      if(isTRUE(data[[set]]$cl$bank.interpolate.max.dist != par$bank.interpolate.max.dist)){ warn("there is a mismatch between the current parameter par$bank.interpolate.max.dist and the dense polygon: re-run CM.generatePolygon()."); return(data); }

      # create Voronoi/Dirichlet/Thiessen polygons
      notice("step 1 of 5: get voronoi polygons...", TRUE)
      if(is.null(data[[set]]$cl$paths) || par$force.calc.voronoi || isTRUE(data[[set]]$cl$bank.interpolate.max.dist != data[[set]]$polygon.bank.interpolate.max.dist)){

        # notice
        notice(paste("calculate based on dense polygon with a maximum bank point distance of ", par$bank.interpolate.max.dist, sep=""))
        reasons = c()
        if(is.null(data[[set]]$cl$paths))                                                      reasons = append(reasons, "data does not exist")
        if(par$force.calc.voronoi)                                                             reasons = append(reasons, "calculation is forced")
        if(isTRUE(data[[set]]$cl$bank.interpolate.max.dist != par$bank.interpolate.max.dist)) reasons = append(reasons, "max. dist. has changed")
        notice(paste("reason for calculation:", paste(reasons, collapse = " + ")))

        # set meta data: bank.interpolate.max.dist
        data[[set]]$cl$bank.interpolate.max.dist = par$bank.interpolate.max.dist

        # reset data
        data[[set]]$cl$cl.paths = NULL

        # calculate voronoi polygons => create paths from polygons => remove duplicated paths
        voronoi = dirichlet(data[[set]]$polygon)                    # create voronoi polygons
        tiles   = sapply(voronoi$tiles, "[[", "bdry")                # get individual tile coordinates
        paths   = do.call(rbind, lapply(tiles, function(tile){      # disassemble paths of tiles
          return(data.frame(
            x1 = tile$x,
            y1 = tile$y,
            x2 = c(tile$x[2:length(tile$x)], tile$x[1]),
            y2 = c(tile$y[2:length(tile$y)], tile$y[1])
          ))
        }))
        d.ixs = duplicated(t(apply(paths, 1, sort)))                # get duplicates
        data[[set]]$cl$paths = if(any(d.ixs)) paths[-which(d.ixs),] else paths     # remove duplicates
        rownames(data[[set]]$cl$paths) = NULL

      } else {

        notice("(paths taken from cache)")

      }



      # filter #1 (restrict to segments within banks)
      notice("step 2 of 5: filter #1 (clip)...", TRUE)
      if(is.null(data[[set]]$cl$cl.paths)){

        in.polygon.ixs = apply(data[[set]]$cl$paths, 1, function(path){
          all(point.in.polygon(path[c("x1", "x2")], path[c("y1", "y2")], data[[set]]$polygon$x, data[[set]]$polygon$y))
        })
        paths.in.polygon = data[[set]]$cl$paths[in.polygon.ixs,]

        # from segments create points (creates duplicates)
        points.in.polygon = rbind(as.matrix(paths.in.polygon[,c("x1","y1")]), as.matrix(paths.in.polygon[,c("x2","y2")]))
        rownames(points.in.polygon) = NULL
        colnames(points.in.polygon) = c("x", "y")


        ### find start/end segments
        cl.start.i = which.min((
          ((((data[[set]]$channel$x[data[[set]]$ix.l][1]                        + data[[set]]$channel$x[data[[set]]$ix.r][1])                        / 2) - points.in.polygon[,"x"])^2) +
          ((((data[[set]]$channel$y[data[[set]]$ix.l][1]                        + data[[set]]$channel$y[data[[set]]$ix.r][1])                        / 2) - points.in.polygon[,"y"])^2)
        ) ^(1/2))
        cl.end.i = which.min((
          ((((data[[set]]$channel$x[data[[set]]$ix.l][length(data[[set]]$ix.l)] + data[[set]]$channel$x[data[[set]]$ix.r][length(data[[set]]$ix.r)]) / 2) - points.in.polygon[,"x"])^2) +
          ((((data[[set]]$channel$y[data[[set]]$ix.l][length(data[[set]]$ix.l)] + data[[set]]$channel$y[data[[set]]$ix.r][length(data[[set]]$ix.r)]) / 2) - points.in.polygon[,"y"])^2)
        ) ^(1/2))


        # all paths in polygon
        data[[set]]$cl$paths.in.polygon = paths.in.polygon;

        # start and end point
        data[[set]]$cl$ends.x   = points.in.polygon[c(cl.start.i, cl.end.i),"x"]
        data[[set]]$cl$ends.y   = points.in.polygon[c(cl.start.i, cl.end.i),"y"]

      } else {

        notice("(paths.in.polygon taken from cache)")

        paths.in.polygon = data[[set]]$cl$paths.in.polygon

      }


      # filter #2: filter out segments that have less than two connections
      notice("step 3 of 5: filter #2 (dead ends)...", TRUE)
      if(is.null(data[[set]]$cl$cl.paths)){

        # notice
        notice(paste("start iterations with", par$bank.filter3.max.it, "iterations maximum"))

        cl.paths            = paths.in.polygon
        remove.continue     = TRUE
        remove.iteration    = 0
        remove.ixs.first.it = NULL
        while(remove.continue){

          # display interation and check for max
          remove.iteration = remove.iteration + 1
          if(remove.iteration > par$bank.filter3.max.it){ warn(paste("\n### exit due to maximum iterations (max. iterations = ", par$bank.filter3.max.it, ") ###", "\nNote: this may be caused by gaps that opened in the centerline due to\njagged centerline paths. First, check for gaps visually with CM.plotPlanView(data, par, error=1). \nYou can than either repair these gaps by editing the centerline paths manually or \nsimply increase the bank resolution via parameter par$bank.interpolation.max.dist!", sep=""));
            data[[set]]$cl$errors.filter2       = paths.in.polygon[remove.ixs,          c("x1", "y1")];
            data[[set]]$cl$errors.filter2.first = paths.in.polygon[remove.ixs.first.it, c("x1", "y1")];
            return(data);
          }
          cat(paste(" > iteration ", remove.iteration, ":", sep=""))

          ### merge all remaining cl.paths and the end points to cl.points
          cl.points = rbind(
            data.frame(x = c(cl.paths[,"x1"], cl.paths[,"x2"]), y = c(cl.paths[,"y1"], cl.paths[,"y2"])),
            data.frame(x = data[[set]]$cl$ends.x,               y = data[[set]]$cl$ends.y)
          )
          ### calculate number of connections of each path
          remove.ixs = which(apply(cl.paths, 1, function(path){
            con1 = nrow(merge(cl.points, path[c("x1", "y1")])) < 2 # time intensive
            con2 = nrow(merge(cl.points, path[c("x2", "y2")])) < 2 # time intensive
            return(any(con1 + con2))
          }))

          # get points to remove
          if(is.null(remove.ixs.first.it)) remove.ixs.first.it = remove.ixs

          if(length(remove.ixs)){

            cl.paths = cl.paths[-remove.ixs, ]
            notice(paste(length(remove.ixs), "elements removed!"))

          } else {

            notice("0 elements removed, filtering ended correctly!")
            remove.continue = FALSE

          }

        } # while(remove.continue)

        data[[set]]$cl$cl.paths = cl.paths

      } else {

        notice("(cl.paths taken from cache)")
        cl.paths = data[[set]]$cl$cl.paths

      }



      # sorting
      notice("step 4 of 5: sort points...", TRUE)
      if(is.null(data[[set]]$cl$original)){

        cl = data.frame(x = data[[set]]$cl$ends.x[1], y = data[[set]]$cl$ends.y[1])
        sort.continue = TRUE
        perc = nrow(cl.paths)
        perc.lev = 0
        if(perc > 1000) notice("0%")
        while(sort.continue){

          this = as.numeric(tail(cl, n=1))

          # create progress notice
          perc.ac = round((perc - nrow(cl.paths)) / perc * 100)
          if((perc > 1000) && (perc.ac > (perc.lev + 10))){perc.lev = perc.lev + 10; notice(paste(perc.lev,"%", sep=""))}

          # find connected segments
          match = which(apply(cl.paths, 1, function(row){
            any(identical(this, as.numeric(row[c(1,2)])), identical(this, as.numeric(row[c(3,4)])))
          }))

          if(length(match) != 1){

            if(all(this == c(data[[set]]$cl$ends.x[2], data[[set]]$cl$ends.y[2]))){

              notice("sorting done successfully!")

              sort.continue = FALSE
              next

            } else {

              warn(paste("number of connections should be 1 but is", length(match)))
              warn("call CM.plotPlanview(data, par, error=1, error.type=\"errors.sort\") to resolve")

              data[[set]]$cl$errors.sort = matrix(this, ncol=2)
              return(data)

            }

          }

          # store point
          point = cl.paths[match, this!=cl.paths[match,]]
          cl    = rbind(cl, data.frame(x = point[[1]], y = point[[2]]))

          # remove from paths
          cl.paths = cl.paths[-match, ]

        } # while(sort.continue)

        # store
        data[[set]]$cl$original = cl

      } else {

        notice("(cl points taken from cache)")
        cl = data[[set]]$cl$original

      }

      cl = list(original = cl)

      notice("step 5 of 5: smooth...", TRUE)

      if(par$centerline.smoothing.width %% 2 != 1){ par$centerline.smoothing.width = par$centerline.smoothing.width + 1; notice("par$centerline.smoothing.width must be odd thus has been increased by 1")}
      notice(paste("smoothing width:", par$centerline.smoothing.width))
      cl$smoothed = as.data.frame(rollapply(as.data.frame(cl$original), par$centerline.smoothing.width, mean, partial=TRUE))

      notice("measure centerline...", TRUE)

      ### calculate length and cumulative length ################

      cl.temp = data.frame(
        x  = c(cl$original$x[1], cl$original$x),
        y  = c(cl$original$y[1], cl$original$y),
        px = c(cl$original$x, NA),
        py = c(cl$original$y, NA)
      )

      cl$original$segments = apply(as.data.frame(cl.temp), 1, function(x) return ( ( ( ( x["px"] - x["x"] ) ^2) + (( x["py"] - x["py"] )^2 ) ) ^(1/2) ) )[1:length(cl$original$x)]
      cl$original$length   = cumsum(cl$original$segments)

      # for smoothed cl #########################################

      cl.temp = data.frame(
        x  = c(cl$smoothed$x[1], cl$smoothed$x),
        y  = c(cl$smoothed$y[1], cl$smoothed$y),
        px = c(cl$smoothed$x, NA),
        py = c(cl$smoothed$y, NA)
      )

      cl$smoothed$segments = apply(as.data.frame(cl.temp), 1, function(x) return (  ( ( ( x["px"] - x["x"] ) ^2) + (( x["py"] - x["py"] )^2 ) ) ^(1/2) ) )[1:length(cl$original$x)]
      cl$smoothed$length   = cumsum(cl$smoothed$segments)

      notice("length calculated!", TRUE)


      ###########################################################

      if(!is.null(data[[set]]$channel$z)){

        notice("elevation information found: project elevation to centerline", TRUE)

        ### project elevation #####################################

        lp_closest = apply(cbind(cl$original$x, cl$original$y), 1, function(x){
          return (which.min((  ((data[[set]]$channel$x - x[1])^2) + ((data[[set]]$channel$y - x[2])^2) ) ^(1/2) ) )
        })

        cl$original$z = data[[set]]$channel$z[lp_closest]

        lp_closest = apply(cbind(cl$smoothed$x, cl$smoothed$y), 1, function(x){
          return (which.min((  ((data[[set]]$channel$x - x[1])^2) + ((data[[set]]$channel$y - x[2])^2) ) ^(1/2) ) )
        })

        cl$smoothed$z = data[[set]]$channel$z[lp_closest]

        ### calculate slope #######################################

        slope_range = 15 # upslope distance in meters
        cl_slope = apply(cl$smoothed, 1, function(x){

          ind = which(cl$smoothed$length >= x[["length"]] & cl$smoothed$length < (x[["length"]] + slope_range))
          fit = lm(cl$smoothed$z[ind] ~ cl$smoothed$length[ind])
          return(fit$coefficients[[2]])

        })

        cl$smoothed$slope = cl_slope

      } else { notice("no elevation information provided in input data: skip elevation projection") }



      ### project features onto centerline
      if(typeof(cmgo.obj$data[[set]]$features) == "list"){

        notice("feature list found: project features to centerline", TRUE)
        cl$smoothed$projections = list()

        nrp = length(cl$smoothed$x)

        centerline_line = psp(
          cl$smoothed$x[1:(nrp-1)],
          cl$smoothed$y[1:(nrp-1)],
          cl$smoothed$x[2:(nrp)],
          cl$smoothed$y[2:(nrp)],
          window=owin(range(cl$smoothed$x), range(cl$smoothed$y))
        )

        for(feature in names(cmgo.obj$data[[set]]$features)){

          if(!is.null(cmgo.obj$data[[set]]$features[[feature]]$x) && !is.null(cmgo.obj$data[[set]]$features[[feature]]$y)){

            feature_points = ppp(cmgo.obj$data[[set]]$features[[feature]]$x, cmgo.obj$data[[set]]$features[[feature]]$y,
              window=owin(range(cmgo.obj$data[[set]]$features[[feature]]$x), range(cmgo.obj$data[[set]]$features[[feature]]$y))
            )

            cl$smoothed$projections[[feature]] = nncross(feature_points, centerline_line)

            notice(paste("feature", feature, "projected to centerline"))

          } else { warning("feature does not contain proper x,y coordinates list!") }

        }

      }

      notice(paste("centerline for", set, "calculated!"), TRUE)

      # store
      data[[set]]$cl$original = cl$original
      data[[set]]$cl$smoothed = cl$smoothed
      data[[set]]$cl$length.factor = cl$smoothed$length[length(cl$smoothed$length)] / cl$original$length[length(cl$original$length)]

    } else {

      notice(paste("(centerline of", set, "loaded from cache)"))

    }

  } # for(set in names(data))

  notice("centerline(s) returned sucessfully!", TRUE)

  # return
  return(list(
    data = data,
    par  = par
  ))

}
