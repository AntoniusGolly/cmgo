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
#' \if{html}{\figure{08-processing.png}{options: width="800px" alt="Figure: processing"}}
#' \if{latex}{\figure{08-processing.pdf}{options: width=9cm}}
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

    # x has to be matrix representing x-y-pairs of centerline points in the number of transects.span
    if(!is.matrix(x)) return(F)

    # first and last elements represent averages over moving window
    x1 = head(as.matrix(x)[,1], n=1)
    x2 = tail(as.matrix(x)[,1], n=1)
    y1 = head(as.matrix(x)[,2], n=1)
    y2 = tail(as.matrix(x)[,2], n=1)

    # calculate orientation (m) and center point (P) of transect vectors
    m  = -1 / ((( y2 - y1 ) / ( x2 - x1 )))
    Px = (( x2 - x1 ) / 2 ) + x1
    Py = (( y2 - y1 ) / 2 ) + y1
    n  = Py - ( m * Px )

    # return
    return( data.frame(m=m,n=n,Px=Px,Py=Py) )

  }

  set = "set2" # ignored, only for debugging

  for(set in sets){

    if(par$calculate.metrics){

      if(is.null(data[[set]]$metrics) || par$force.calc.metrics || !identical(data[[set]]$metrics$tr.span, par$transects.span)){

        cl.type = "smoothed" # it is highly recommended to do the processing on the "smoothed" version of the centerline
        set.ref  = if(par$centerline.use.reference) par$centerline.reference else set

        notice(paste("process data set '", set, "' with centerline reference '", set.ref,"' now...", sep=""), TRUE)
        reasons = c()
        if(is.null(data[[set]]$metrics))     reasons = append(reasons, "data does not exis")
        if(par$force.calc.metrics)           reasons = append(reasons, "calculation is forced")
        if(!identical(data[[set]]$metrics$tr.span, par$transects.span)) reasons = append(reasons, "tr spans differ")
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
        data[[set]]$metrics$tr.span  = par$transects.span

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

        ### project features onto centerline
        if(typeof(data[[set]]$features) == "list"){

          notice("feature list found!")

          cl = data[[set]]$cl

          nrp = length(cl$smoothed$x)

          ## create ppp feature
          centerline_points = ppp(
            cl$smoothed$x,
            cl$smoothed$y,
            window=owin(range(cl$smoothed$x), range(cl$smoothed$y))
          )

          for(feature in names(data[[set]]$features)){

            notice(paste("processing feature '", feature, "'...", sep=""))

            if(!is.null(data[[set]]$features[[feature]]$x) && !is.null(data[[set]]$features[[feature]]$y)){

              ## create ppp feature
              feature_points = ppp(
                data[[set]]$features[[feature]]$x,
                data[[set]]$features[[feature]]$y,
                window=owin(range(data[[set]]$features[[feature]]$x), range(data[[set]]$features[[feature]]$y))
              )

              # project centerline to feature and store in $feature
              prj_to_feature = nncross(feature_points, centerline_points)
              data[[set]]$features[[feature]]$cl_index = prj_to_feature$which

              # project feature to centerline and store in $centerline
              prj_to_cl = nncross(centerline_points, feature_points)
              data[[set]]$metrics[[paste(feature, "_which", sep="")]] = prj_to_cl$which
              data[[set]]$metrics[[paste(feature, "_dist",  sep="")]] = prj_to_cl$dist
              for(variable in names(data[[set]]$features[[feature]][- which(names(data[[set]]$features[[feature]]) %in% c("x", "y", "cl_index"))])){
                data[[set]]$metrics[[paste(feature, "_", variable,  sep="")]] = data[[set]]$features[[feature]][[variable]][prj_to_cl$which]
              }

              # calculate 2d dist
              diffs = diff(as.matrix(data[[set]]$features[[feature]][which(names(data[[set]]$features[[feature]]) %in% c("x", "y"))]))
              data[[set]]$features[[feature]]$seg_dist_2d = c(0, sqrt(diffs[,"x"]^2 + diffs[,"y"]^2))
              data[[set]]$features[[feature]]$cum_dist_2d = cumsum(data[[set]]$features[[feature]]$seg_dist_2d)

              # calculate 3d dist
              if(!is.null(data[[set]]$features[[feature]]$z)){
                diffs = diff(as.matrix(data[[set]]$features[[feature]][which(names(data[[set]]$features[[feature]]) %in% c("x", "y", "z"))]))
                data[[set]]$features[[feature]]$seg_dist_3d = c(0, sqrt(diffs[,"x"]^2 + diffs[,"y"]^2 + diffs[,"z"]^2))
                data[[set]]$features[[feature]]$cum_dist_3d = cumsum(data[[set]]$features[[feature]]$seg_dist_3d)
              }

              notice(paste("feature '", feature, "' projected to centerline", sep=""))

            } else { warning("feature does not contain proper x,y coordinates lists!") }

          }

        }

        notice("features processed successfully!")



        # # #
        if(par$steps.identify){

          notice("identify steps in long profile...", TRUE)

          if(!is.null(data[[set]]$features$lp)){

            # set parameters
            sl_min  = par$steps.minimum.step.length    * par$steps.bank.full.width / 100
            sl_max  = par$steps.maximum.step.length    * par$steps.bank.full.width / 100
            pl_min  = par$steps.minimum.pool.length    * par$steps.bank.full.width / 100
            rd_min  = par$steps.minimum.residual.depth * par$steps.bank.full.width / 100
            dh_min  = par$steps.minimum.drop.height    * par$steps.bank.full.width / 100
            ssl_min = par$steps.minimum.step.slope     + par$steps.average.slope
            ssg_min = tan(ssl_min/360 * (2*pi)) * 100
            slope   = if(par$steps.average.slope.fix) par$steps.average.slope else {
              # calculate slope based on elevation and length of long profile
              slope = atan(diff(range(data[[set]]$features$lp$z)) / tail(data[[set]]$features$lp$cum_dist_2d, n=1)) * 360 / (2*pi)
            }

            notice("****** parameters **************************************************************")
            notice(paste("min step length (sl_min = ",    par$steps.minimum.step.length,    "% of bank full width): ", sl_min, "[m]", sep=""));
            notice(paste("max step length (sl_max = ",    par$steps.maximumg.step.length,   "% of bank full width): ", sl_max, "[m]", sep=""));
            notice(paste("min pool length (pl_min = ",    par$steps.minimum.pool.length,    "% of bank full width): ", pl_min, "[m]", sep=""));
            notice(paste("min residual depth (rd_min = ", par$steps.minimum.residual.depth, "% of bank full width): ", rd_min, "[m]", sep=""));
            notice(paste("min drop height (dh_min = ",    par$steps.minimum.drop.height,    "% of bank full width): ", dh_min, "[m]", sep=""));
            notice(paste("min step slope (ssl_min = ",    par$steps.minimum.step.slope,     "deg greater than mean slope): ", ssl_min, "[deg]  ", sep=""));
            notice("********************************************************************************")

            # prepare variables
            lp           = data[[set]]$features$lp
            steps        = data.frame()
            verbose      = par$steps.verbose
            step.i       = 0
            nr.of.checks = 4
            crash.i      = 0
            ix           = 1

            notice("identified steps:")
            checks = "start iteration:"

            while(ix < length(lp$id)){

              crash.i = crash.i + 1; if(crash.i > 50000){ stop("program crashed!"); break;}

              ix.add = 0

              #if(Parameters$step.max) if(nrow(steps) >= Parameters$step.max.count) break;

              if(verbose) notice(paste("index: ", ix, " / dist_cum: ", round(lp$cum_dist_2d[ix], digits=2), sep=""))

              # reset checks
              checks = rep(FALSE, nr.of.checks)

              # set indices
              ixl = ix + 1     # start looking from the next point on
              ixr = ix + 40    # end after 40 points
              if(ixr > length(lp$id)) ixr = length(lp$id)

              # actual point
              lpx  = lp$x[ix]
              lpy  = lp$y[ix]
              lpz  = lp$z[ix]
              lpd  = lp$cum_dist_2d[ix]

              # i-points to check
              lpxi = lp$x[c(ixl:ixr)]                               # all points upstream of ix (end of pool) excluding ix
              lpyi = lp$y[c(ixl:ixr)]                               # all points upstream of ix (end of pool) excluding ix
              lpzi = lp$z[c(ixl:ixr)]                               # all points upstream of ix (end of pool) excluding ix
              lpdi = lp$cum_dist_2d[c(ixl:ixr)]                     # cumulative distances
              lpfi = ( ( lpxi - lpx )^2  + ( lpyi - lpy )^2 )^(1/2) # real distances from end of pool upstream

              # get distances of points
              dist3d = ( ( lpxi - lpx )^2 + ( lpyi - lpy )^2 + ( lpzi - lpz )^2 )^(1/2)
              dist2d = ( ( lpxi - lpx )^2 + ( lpyi - lpy )^2  )^(1/2)

              ### check 1: higher than the next point?
              if(lpz > lpzi[1])          checks[1] = TRUE
              else{ ix = ix+1; next }

              ### check 2: highest point in elevation among all points upstream that fall within a minimum pool length
              if(all(lpzi < lpz)){ notice("end of lp reached"); break;}
              first_higher_point = min(which((lpz > lpzi) == FALSE))
              last_lower_point   = first_higher_point - 1                                              # pl_min.pts = which(dist2d > pl_min)

              if(dist2d[last_lower_point]>pl_min) checks[2] = TRUE
              else { ix = ix+1; next }

              # generate interpolated point
              lp_llp = c(lpxi[last_lower_point],   lpyi[last_lower_point],   lpzi[last_lower_point])   # LongProfile_LastLowerPoint
              lp_fhp = c(lpxi[first_higher_point], lpyi[first_higher_point], lpzi[first_higher_point]) # LongProfile_FirstHigherPoint
              lp_h   = lp_fhp - lp_llp                                                                 # LongProfile_Helpingpoint
              lambda = (lpz -lp_llp[3]) / lp_h[3]
              lp_sp  = c(lp_llp[1] + (lp_h[1] * lambda), lp_llp[2] + (lp_h[2] * lambda), lpz)          # LongProfile_StartPool (interpolated point)

              # calculate cum. dist of interpolated start of pool point (for plotting, cum. distances are required)
              lpd_sp = lpdi[last_lower_point] - lpd                                                    # LongProfileDistance_StartPool
              if(par$steps.thalweg.dist == "2d") lpd_sp = lpd_sp + ( ( lp_sp[1] - lpxi[last_lower_point] )^2 + ( lp_sp[2] - lpyi[last_lower_point] )^2 ) ^(1/2)
              if(par$steps.thalweg.dist == "3d") lpd_sp = lpd_sp + ( ( lp_sp[1] - lpxi[last_lower_point] )^2 + ( lp_sp[2] - lpyi[last_lower_point] )^2 + ( lp_sp[3] - lpzi[last_lower_point] )^2 ) ^(1/2)

              # calculate real pool length
              pool.length  = ( ( lp_sp[1] - lpx )^2 + ( lp_sp[2] - lpy )^2 ) ^(1/2)

              # calculate residual depth
              pool.depth.ix = which.min(lp$z[ix + seq(1, last_lower_point)])
              pool.depth.z  = min( lp$z[ix + seq(1, last_lower_point)] )
              pool.depth.r  = lpz - pool.depth.z

              ### check 3: min residual depth
              if(pool.depth.r >= rd_min) checks[3] = TRUE
              else { ix = ix + first_higher_point; next }

              # re-calculate real distances
              step.lengths = ( ( lpxi[first_higher_point:length(lpxi)] - lp_sp[1] )^2  + ( lpyi[first_higher_point:length(lpyi)] - lp_sp[2] )^2 )^(1/2) # real distances from end of pool upstream

              # find start of step (first point above lp_sp for which min. step drop height and min step length applies)
              if(!any( (lpzi[first_higher_point:length(lpzi)] - lpz) > dh_min)){ notice("end of lp reached"); break;}
              first_top_point = first_higher_point - 1 + max(
                min(which( (lpzi[first_higher_point:length(lpzi)] - lpz) > dh_min)), # minimum drop height
                min(which( step.lengths > sl_min ))                                  # minimum step length (not pool length), step length == length from start of pool (end of step) to first_top_point
              )



              # point cloud calculations ############################################################

              gr.ix      = 0
              r.squared  = 0;
              ssl        = 0
              group      = list()
              pointcloud = TRUE
              while(pointcloud){

                if(verbose) notice(paste("_________________ ix", ix, "_________________"))

                crash.i = crash.i + 1; if(crash.i > 5000){ error("program crashed!"); break;}

                if(first_top_point + gr.ix > length(lpxi)) {notice("end of point cloud!"); break;}

                # point cloud of points to fit
                slx  = c(lp_sp[1], lpxi[first_higher_point : ( first_top_point + gr.ix )])
                sly  = c(lp_sp[2], lpyi[first_higher_point : ( first_top_point + gr.ix )])
                slz  = c(lp_sp[3], lpzi[first_higher_point : ( first_top_point + gr.ix )])
                slz0 = rep(min(slz), length(slz))

                # re-calculate step length
                step.length = ( ( slx[1] - slx[length(slx)] )^2  + ( sly[1] - sly[length(sly)] )^2 )^(1/2) # real distances from end of pool upstream

                # check min and max step length
                if(step.length < sl_min){
                  if(verbose) notice("minimum step length is not reached!")
                  error("program aborted")
                  pointcloud = FALSE
                }

                if(step.length > sl_max){
                  if(verbose) notice("maximum step length reached!")
                  if(verbose) notice("No step detected. Discard and go to next...")
                  ix.add = first_higher_point - 1;
                  pointcloud = FALSE
                  break;
                }


                # gradient index
                gr.ix = gr.ix + 1

                # 3 dimensional fit
                fit.3d = TRUE
                if(fit.3d){

                  m = 0
                  r.squared = 0

                  if(length(slx) < 2){

                    notice("point cloud only contains 1 point!")
                    error("program aborted!")

                  } else if(length(slx) == 2){

                    if(verbose) notice("point cloud contains two points.");
                    if(verbose) notice("set r2 to 0.5")

                    r.squared = 0.5;
                    m = (slz[2]-slz[1]) / (( (slx[2]-slx[1])^2 + (sly[2]-sly[1]) ^2) ^(1/2))

                  } else {

                    if(verbose) notice(paste("point cloud size:", length(slx)))
                    if(verbose) notice(paste("step length [m]:",  round(step.length,1)))

                    ### PCA for bottom line ########################
                    xyz  <- data.frame(x = slx, y = sly, z = slz)
                    xyz0 <- data.frame(x = slx, y = sly, z = slz0)
                    N    <- nrow(xyz)

                    mean_xyz   <- apply(xyz, 2, mean)
                    mean_xyz0  <- apply(xyz0, 2, mean)

                    xyz_pca    <- princomp(xyz)
                    xyz_pca0   <- princomp(xyz0)

                    dirVector  <- xyz_pca$loadings[, 1]    # PC1
                    dirVector0 <- xyz_pca0$loadings[, 1]   # PC1

                    xyz_fit    <- matrix(rep(mean_xyz, each = N), ncol=3) + xyz_pca$score[, 1] %*% t(dirVector)
                    xyz_fit0   <- matrix(rep(mean_xyz0, each = N), ncol=3) + xyz_pca0$score[, 1] %*% t(dirVector0)

                    t_ends     <- c(min(xyz_pca$score[,1]) - 0.2, max(xyz_pca$score[,1]) + 0.2)  # for both ends of line
                    t_ends0    <- c(min(xyz_pca0$score[,1]) - 0.2, max(xyz_pca0$score[,1]) + 0.2)  # for both ends of line
                    endpts     <- rbind(mean_xyz0 + t_ends0[1]*dirVector0, mean_xyz0 + t_ends0[2]*dirVector0)

                    ### interactive plot ############################ #requires library(rgl)
                    if(verbose){
                      clear3d()
                      #Sys.sleep(2)
                      plot3d(xyz, type="p", size=10)
                      #rgl.bbox(xlen = 0, ylen = 0, zlen = 0, color = c('grey100'))

                      #Wait()
                      # plot fits
                      abclines3d(mean_xyz,  a = dirVector, col="orange", lwd=2)

                      abclines3d(mean_xyz0, a = dirVector0, col="blue", lwd=2)     # mean + t * direction_vector

                      # plot connections
                      for(i in 1:N) segments3d(rbind(xyz[i,], xyz_fit[i,]),  lty=2,col="green2")
                      for(i in 1:N) segments3d(rbind(xyz[i,], xyz_fit0[i,]), col="gray")

                      # plot secondary coordinate system
                      for(i in 1:N) points3d(rbind(c(xyz_fit0[i,c(1,2)], slz[i])),  size=8, col="red")
                      for(i in 1:N) points3d(rbind(xyz_fit0[i,]),                   size=4, col="red")
                    }

                    origin = xyz_fit0[1,]

                    # fit lm() in 2d sub-space
                    x2d = ( (xyz_fit0[,1] - origin[1])^2 + (xyz_fit0[,2] - origin[2])^2 ) ^ (1/2)
                    y2d = slz
                    final_fit = lm(y2d ~ x2d)

                    # plot this final fit in 2d space
                    if(verbose) plot(y2d ~ x2d)
                    if(verbose) abline(final_fit, col="red")

                    # extract fit parameters
                    n = final_fit$coefficients[1]
                    m = final_fit$coefficients[2]
                    r.squared = summary(final_fit)$r.squared

                    ### plot this final fit in 3d space
                    dirVectorFit = c(dirVector0[c(1,2)], m)
                    intercept = c(xyz_fit[1,c(1,2)], n)
                    if(verbose) abclines3d(intercept, a = dirVectorFit, col="red")

                    #stop()

                  } # if(length(slx) < 2) {} else if(length(slx) == 2) {} else

                  #if(gr.ix==2) stop("end")

                  # eval slope
                  ssl = atan( m ) / (2*pi) * 360
                  if(verbose) notice(paste("step slope [deg]", round(ssl,2)))

                  ### check 4: min step slope
                  if(ssl >= ssl_min){
                    group = rbind(group, data.frame(
                        "ix"  = gr.ix,
                        "n"   = length(slx),
                        "ssl" = ssl,
                        "r2"  = r.squared,
                        "sl"  = step.length,
                        "px"  = slx[length(slx)],
                        "py"  = sly[length(sly)],
                        "pz"  = slz[length(slz)],
                        "dummy" = TRUE
                      ))
                    checks[4] = TRUE
                  } else {
                    if(verbose) notice("ssl_min not reached!");
                  }

                } else {} # 2 dimensional fit to come

                #Wait();

              } # while(pointcloud)







              ### step identified ###################################################################

              if(all(checks)){

                ### find start of step

                stepgroup = group
                group = stepgroup
                bestfit = which.max(stepgroup$r2)

                start.of.step = first_top_point - 1 + group$ix[bestfit]
                step.slope    = group$ssl[bestfit]
                step.i        = step.i + 1

                cat(paste("#",step.i, " ", sep=""))
                if(step.i %% 20 == 0) cat("\n")

                ### store step data
                steps = rbind(steps, data.frame(
                    "id"              = step.i,
                    "ix"              = ix,

                    "pool.end.x"      = lpx,
                    "pool.end.y"      = lpy,
                    "pool.end.z"      = lpz,
                    "pool.end.d"      = lp$cum_dist_2d[ix],

                    "pool.start.x"    = lp_sp[1],
                    "pool.start.y"    = lp_sp[2],
                    "pool.start.z"    = lp_sp[3],
                    "pool.start.d"    = lp$cum_dist_2d[ix] + lpd_sp,

                    "pool.length"     = pool.length,
                    "pool.dist"       = lpd_sp,

                    "pool.depth.x"    = lp$x[ix + pool.depth.ix],
                    "pool.depth.y"    = lp$y[ix + pool.depth.ix],
                    "pool.depth.z"    = pool.depth.z,
                    "pool.depth.r"    = pool.depth.r,
                    "pool.depth.ix"   = ix + pool.depth.ix,
                    "pool.depth.d"    = lp$cum_dist_2d[ix + pool.depth.ix],

                    "step.height"     = lpzi[start.of.step] - pool.depth.z,

                    "start.of.step.x" = lpxi[start.of.step],
                    "start.of.step.y" = lpyi[start.of.step],
                    "start.of.step.z" = lpzi[start.of.step],
                    "start.of.step.d" = lpdi[start.of.step],

                    "top.first.x"     = lpxi[first_top_point],
                    "top.first.y"     = lpyi[first_top_point],
                    "top.first.z"     = lpzi[first_top_point],
                    "top.first.d"     = lpdi[first_top_point],

                    "tx.id"           = as.character(step.i),
                    "tx.rd"           = as.character(round(pool.depth.r, digits=2)),
                    "tx.pl"           = as.character(round(pool.length, digits=2)),

                    "dh_min"          = lpz + dh_min,

                    "dummy" = TRUE
                  ))

                ix = ix + first_higher_point

              } else {

                # proceed with next ix
                ix = ix + 1 + ix.add

              }

            } # while(ix < ixs[2])

            # store
            data[[set]]$steps = steps

          } else { # if(!is.null(data[[set]]$features$lp)

            warning("feature list does not contain long profile information to identify steps!")

          }

        } # if(par$steps.identify) # # #

      } else { # if(is.null(data[[set]]$metrics) || par$force.calc.metrics)

        notice("(metrics taken from cache)")

      }

    } # if(par$calculate.width)

  } # for(set in names(data))

  notice("CM.processCenterline() has ended successfully!", TRUE)

  # return
  return(list(
      data = data,
      par  = par
    ))

}
