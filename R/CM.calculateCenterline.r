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
#' \if{html}{\figure{06-processing.png}{options: width="800px" alt="Figure: processing"}}
#' \if{latex}{\figure{06-processing.pdf}{options: width=9cm}}
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
#' \if{html}{\figure{07-filtering.png}{options: width="600px" alt="Figure: processing"}}
#' \if{latex}{\figure{07-filtering.pdf}{options: width=6cm}}
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
      if(isTRUE(data[[set]]$polygon.bank.interpolate.max.dist != par$bank.interpolate.max.dist)){
        notice(paste("interpolate max. dist of data:", data[[set]]$polygon.bank.interpolate.max.dist))
        notice(paste("interpolate max. dist of par: ", par$bank.interpolate.max.dist))
        warn("there is a mismatch between the current parameter par$bank.interpolate.max.dist and the dense polygon: re-run CM.generatePolygon()."); return(list( data = data,   par  = par));
      }

      # check if parameter matches data base
      if(isTRUE(data[[set]]$polygon.bank.reduce.min.dist != par$bank.reduce.min.dist)){
        notice(paste("reduce min. dist of data:", data[[set]]$polygon.bank.reduce.min.dist))
        notice(paste("reduce min. dist of par: ", par$bank.reduce.min.dist))
        warn("there is a mismatch between the current parameter par$bank.reduce.min.dist and the dense polygon: re-run CM.generatePolygon()."); return(list( data = data,   par  = par));
      }

      ### find start/end of centerline (interpolated bank points)
      cl.start = list(
        x = (head(data[[set]]$channel$x[data[[set]]$ix.l], n=1) + head(data[[set]]$channel$x[data[[set]]$ix.r], n=1)) / 2,
        y = (head(data[[set]]$channel$y[data[[set]]$ix.l], n=1) + head(data[[set]]$channel$y[data[[set]]$ix.r], n=1)) / 2
      )
      cl.end = list(
        x = (tail(data[[set]]$channel$x[data[[set]]$ix.l], n=1) + tail(data[[set]]$channel$x[data[[set]]$ix.r], n=1)) / 2,
        y = (tail(data[[set]]$channel$y[data[[set]]$ix.l], n=1) + tail(data[[set]]$channel$y[data[[set]]$ix.r], n=1)) / 2
      )



      # create Voronoi/Dirichlet/Thiessen polygons
      notice("step 1 of 5: get voronoi polygons...", TRUE)
      if(is.null(data[[set]]$cl$paths) || par$force.calc.voronoi
        || isTRUE(data[[set]]$cl$bank.interpolate.max.dist != data[[set]]$polygon.bank.interpolate.max.dist)
        || isTRUE(data[[set]]$cl$bank.reduce.min.dist      != data[[set]]$polygon.bank.reduce.min.dist)
      ){

        # notice
        notice(paste("calculate based on dense polygon with a maximum bank point distance of ", par$bank.interpolate.max.dist, sep=""))
        notice(paste("calculate based on dense polygon with a minimum bank point distance of ", par$bank.reduce.min.dist, sep=""))
        reasons = c()
        if(is.null(data[[set]]$cl$paths))                                                      reasons = append(reasons, "data does not exist")
        if(par$force.calc.voronoi)                                                             reasons = append(reasons, "calculation is forced")
        if(isTRUE(data[[set]]$cl$bank.interpolate.max.dist != par$bank.interpolate.max.dist))  reasons = append(reasons, "max. dist. has changed")
        if(isTRUE(data[[set]]$cl$bank.reduce.min.dist      != par$bank.reduce.min.dist))       reasons = append(reasons, "min. dist. has changed")
        notice(paste("reason for calculation:", paste(reasons, collapse = " + ")))

        # set meta data: bank.interpolate.max.dist
        data[[set]]$cl$bank.interpolate.max.dist = par$bank.interpolate.max.dist
        data[[set]]$cl$bank.reduce.min.dist      = par$bank.reduce.min.dist

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


        ### find start/end segments from filter #1
        cl.start.i = which.min((
          (( cl.start$x - points.in.polygon[,"x"])^2) +
          (( cl.start$y - points.in.polygon[,"y"])^2)
        ) ^(1/2))
        cl.end.i = which.min((
          (( cl.end$x   - points.in.polygon[,"x"])^2) +
          (( cl.end$y   - points.in.polygon[,"y"])^2)
        ) ^(1/2))


        # save all paths in polygon
        data[[set]]$cl$paths.in.polygon = paths.in.polygon;

        # save coordinates of centerline ends (start and end points of centerline)
        data[[set]]$cl$ends.x   = points.in.polygon[c(cl.start.i, cl.end.i), "x"]
        data[[set]]$cl$ends.y   = points.in.polygon[c(cl.start.i, cl.end.i), "y"]

      } else {

        notice("(paths.in.polygon taken from cache)")

        paths.in.polygon = data[[set]]$cl$paths.in.polygon

      }


      # filter #2: filter out segments that have less than two connections
      notice("step 3 of 5: filter #2 (dead ends)...", TRUE)
      if(is.null(data[[set]]$cl$cl.paths)){

        # notice
        notice(paste("start iterations with", par$bank.filter2.max.it, "iterations maximum"))

        cl.paths            = paths.in.polygon
        remove.continue     = TRUE
        remove.iteration    = 0
        remove.ixs.first.it = NULL
        while(remove.continue){

          # display interation and check for max
          remove.iteration = remove.iteration + 1
          if(remove.iteration > par$bank.filter2.max.it){ warn(paste("\n### exit due to maximum iterations (max. iterations = ", par$bank.filter2.max.it, ") ###", "\nNote: this may be caused by gaps that opened in the centerline due to\njagged centerline paths. First, check for gaps visually with CM.plotPlanView(cmgo.obj, set=\"",set,"\", error=1). \nYou can than either repair these gaps by editing the centerline paths manually or \nsimply increase the bank resolution via parameter par$bank.interpolation.max.dist!", sep=""));
            data[[set]]$cl$errors.filter2       = paths.in.polygon[remove.ixs,          c("x1", "y1")];
            data[[set]]$cl$errors.filter2.first = paths.in.polygon[remove.ixs.first.it, c("x1", "y1")];
            data[[set]]$cl$cl.paths             = cl.paths
            return(list(
              data = data,
              par  = par
            ))
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
              warn("call CM.plotPlanView(cmgo.obj, set=\"",set,"\", error=1, error.type=\"errors.sort\") to resolve")

              data[[set]]$cl$errors.sort = matrix(this, ncol=2)

              return(list(
                data = data,
                par  = par
              ))

            }

          }

          if(all(this == cl.paths[match,1:2])) pos = c(3,4)
          if(all(this == cl.paths[match,3:4])) pos = c(1,2)

          # store point
          #point = cl.paths[match, this!=cl.paths[match,]]
          point = cl.paths[match, pos]
          cl    = rbind(cl, data.frame(x = point[[1]], y = point[[2]]))

          # remove from paths
          cl.paths = cl.paths[-match, ]

        } # while(sort.continue)

        # add start/end point from banks
        cl = rbind(cl.start, cl, cl.end)

        # store
        data[[set]]$cl$original = cl

      } else {

        notice("(cl points taken from cache)")
        cl = data[[set]]$cl$original

      }

      cl = list(original = cl)

      ###########################################################



      ### smooth centerline #####################################

      notice("step 5 of 5: smooth...", TRUE)

      if(par$centerline.smoothing.width %% 2 != 1){ par$centerline.smoothing.width = par$centerline.smoothing.width + 1; notice("par$centerline.smoothing.width must be odd thus has been increased by 1")}
      notice(paste("smoothing width:", par$centerline.smoothing.width))
      cl$smoothed = as.data.frame(rollapply(as.data.frame(cl$original), par$centerline.smoothing.width, mean, partial=TRUE))

      # add start/end point (has been moved by smoothing)
      cl$smoothed[1,]                     = cl.start
      cl$smoothed[length(cl$smoothed$x),] = cl.end

      ###########################################################



      ### calculate length and cumulative length ################

      notice("measure length of centerline...", TRUE)

      # calculate 2d length of original centerline
      diffs = diff(as.matrix(cl$original))
      cl$original$seg_dist_2d = c(0, sqrt(diffs[,"x"]^2 + diffs[,"y"]^2))
      cl$original$cum_dist_2d = cumsum(cl$original$seg_dist_2d)

      # calculate 2d length of smoothed centerline
      diffs = diff(as.matrix(cl$smoothed))
      cl$smoothed$seg_dist_2d = c(0, sqrt(diffs[,"x"]^2 + diffs[,"y"]^2))
      cl$smoothed$cum_dist_2d = cumsum(cl$smoothed$seg_dist_2d)

      ###########################################################



      ### project elevation #####################################

      if(!is.null(data[[set]]$channel$z)){

        notice("elevation information found: project elevation to centerline", TRUE)

        lp_closest = apply(cbind(cl$original$x, cl$original$y), 1, function(x){
          return (which.min((  ((data[[set]]$channel$x - x[1])^2) + ((data[[set]]$channel$y - x[2])^2) ) ^(1/2) ) )
        })

        cl$original$z = data[[set]]$channel$z[lp_closest]

        lp_closest = apply(cbind(cl$smoothed$x, cl$smoothed$y), 1, function(x){
          return (which.min((  ((data[[set]]$channel$x - x[1])^2) + ((data[[set]]$channel$y - x[2])^2) ) ^(1/2) ) )
        })

        cl$smoothed$z = data[[set]]$channel$z[lp_closest]

        ### calculate slope #######################################
        cl$smoothed$slope = apply(cl$smoothed, 1, function(x){

          ind = which(cl$smoothed$cum_dist_2d >= x[["cum_dist_2d"]] & cl$smoothed$cum_dist_2d < (x[["cum_dist_2d"]] + par$centerline.local.slope.range))
          fit = lm(cl$smoothed$z[ind] ~ cl$smoothed$cum_dist_2d[ind])
          return(fit$coefficients[[2]])

        })

        ### calculate 3d length of smoothed centerline ############
        diffs = diff(as.matrix(cl$smoothed))
        cl$smoothed$seg_dist_3d = c(0, sqrt(diffs[,"x"]^2 + diffs[,"y"]^2 + diffs[,"z"]^2))
        cl$smoothed$cum_dist_3d = cumsum(cl$smoothed$seg_dist_3d)

      } else { notice("no elevation information provided in input data: skip elevation projection") }

      #############################################################

      notice(paste("centerline for", set, "calculated!"), TRUE)

      # store
      data[[set]]$cl$original = cl$original
      data[[set]]$cl$smoothed = cl$smoothed
      data[[set]]$cl$length.factor = tail(cl$smoothed$cum_dist_2d, n=1) / tail(cl$original$cum_dist_2d, n=1)

    } else {

      notice(paste("(centerline of", set, "loaded from cache)"))

    }

  } # for(set in names(data))

  notice("CM.calculateCenterline() has ended successfully!", TRUE)

  # return
  return(list(
    data = data,
    par  = par
  ))

}