#'@importFrom Rdpack reprompt
NULL

#' Computes the standard deviation of raster time series.
#'
#' \code{weatherSD} computes the standard deviation of raster
#' based time series (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) for the respective temporal
#' resolution.
#'
#' @param variable A \code{RasterBrick} or \code{RasterStack} object with
#' values on a daily resolution.
#' @param cores The number of cores to be used in parallel computing.
#' @param timedate A \code{POSIXct} vector containing the time
#' information for each band/layer of \code{variable}.
#' @param clcall A function passed to \code{\link[parallel]{clusterCall}}.
#' @return A list with the first element representing the aggregated
#' raster time series with mean values as specified by \code{resolution} as
#' a \code{RasterBrick} and the second element a \code{POSIXct} vector containing
#' information on the aggregated time intervals as specified by \code{resolution}.
#' For fixed ten day intervals, this is an integer vector with increasing values for
#' each ten-day interval.
#' @seealso
#' @examples #
#' @export
weatherSD <- function(variable, tstart = NULL, tend = NULL, resolution = "monthly", timedate, cores = 10, clcall = NULL){

  switch(resolution,
         monthly = {
           # extract months from timedate
           months <- strftime(timedate, "%m")
           # group layer indices according to months
           indices1 <- tapply(seq_along(months), months, function(x) x)
         },
         fixedtendays = {
           # extract days from timedate
           days <- strftime(timedate, "%m-%d")
           # group layer indices according to fixed tend-day intervals
           indices1 <- tapply(seq_along(days), days, function(x) x)
         },
         mwtendays = {
           # extract days from timedate
           days <- strftime(timedate, "%m-%d")
           # group layer indices according to moving window tend-day intervals
           indices1 <- tapply(seq_along(days), days, function(x) x)
           # search 02-28
           index1 <- which(days == "02-28")
           # search if 02-29 occurs
           index2 <- days[index1 + 1] == "02-29"
           # insert dummy days
           index3 <- as.numeric(index2)
           index3[index2] <- indices1[["02-29"]]
           index3[!index2] <- indices1[["02-28"]][!index2]
           # prune index3
           index3 <- index3[which(index3 <= length(days))]
           indices1[["02-29"]] <- index3
         })

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # compute mean values
  sdvariable <-
    foreach(i = seq_along(indices1), .export = c("variable", "indices1"), .combine = stack, .packages = "raster") %dopar% {

      calc(variable[[indices1[[i]]]], fun = sd)

    }

  # convert to RasterBrick
  sdvariable <- brick(sdvariable)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(meanvariable, indices[[1]]))

}
# monthly seems to work now
# fixedtendays seems to work now
# mvtendays seems to work now
