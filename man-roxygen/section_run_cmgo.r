#' @section Run cmgo:
#' The main functions of cmgo (described in \strong{Work flow}) should be exectued in this order: \preformatted{
#' cmgo.obj = CM.ini(cmgo.obj, par="par/my_parameters.r")  # read data, optional: path to a parameter file, alternatively leave empty to use defaults
#' cmgo.obj = CM.generatePolygon(cmgo.obj)       # generate polygon
#' cmgo.obj = CM.calculateCenterline(cmgo.obj)   # get centerline (calculate or load)
#' cmgo.obj = CM.processCenterline(cmgo.obj)     # process centerline (calculate width)
#' CM.plotPlanView(cmgo.obj)          # plot results
#' CM.plotplotBankShift(cmgo.obj)     # plot channel width and bank retreat
#' CM.writeData(cmgo.obj)             # data to workspace and export data to csv-files (see par)#'
#'}
#'
#' If the generated polygon from CM.generatePolygon() has more than 10,000 vertices, the execution time can be extensive.
#' Thus, CM.calculateCenterline(), as the other main CM functions, has a chaching mechanism of the data. If you call
#' the function when the resulting data already exists, the data will not be processed. You will have to explicitly force the generation of the data.
#' When you change a parameter regarding the generation of the polygon (see CM.generatePolygon()) the program will
#' detect this change and will calculate the centerline without forcing it.
