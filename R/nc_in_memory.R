
#'@importFrom Rdpack reprompt
#'@import rgdal
#'@import raster
#'@import ncdf4
NULL

#' 'Forces' raster to be loaded in memory through an array.
#'
#' \code{nc_in_memory} creates an \code{array} from a netcdf file, swaps x- and y-axis and creates a  \code{RasterBrick} from the array. The resulting \code{RasterBrick} is then completely in memory. Uses a dummy \code{RasterStack} created from the netcdf file for setting the extent, spatial reference and time information.
#' @param nc_path Character string indicating the path to the netcdf file to load.
#' @param varname Character string indicating the name of the variable to load from the netcdf file.
#' @return A \code{RasterBrick} object.
#' @seealso
#' @examples #
#' @export
nc_in_memory <- function(nc_path, varname=NULL){
  # open netcdf
  #if varname is not null, load specified variable as an array
  #if varname is null but the netcdf contains only one variable, load that variable, otherwise create warning containing available variables and stop
  nc <- nc_open(nc_path)
  if (!is.null(varname)){
    dummy <- stack(nc_path, varname = varname)
    raster_data <- ncvar_get(nc,varname)#create array from ncdf
  }else {
    if(length(nc$var)== 1){
      dummy <- stack(nc_path)
      raster_data <- ncvar_get(nc,names(nc$var))
    } else{
      stop(paste("ncdf contains more than one variable. Please set varname to one of:",paste(names(nc$var),collapse = ",")))
    }
  }

  #continue with loaded array
  raster_data <- aperm(raster_data,c(2,1,3)) #swap x and y axis
  raster_data <- brick(raster_data)#create brick from array


  ##set extent using dummy
  xmin(raster_data)<-xmin(dummy)
  xmax(raster_data)<-xmax(dummy)
  ymin(raster_data)<-ymin(dummy)
  ymax(raster_data)<-ymax(dummy)

  ##set projection using dummy
  proj4string(raster_data) <- proj4string(dummy)

  #set Z using dummy
  raster_data@z <- dummy@z

  return(raster_data)
}
