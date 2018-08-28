#'@importFrom Rdpack reprompt
NULL

#' Converts the time zone of a date and time vector.
#'
#' \code{timeZoneConvert} converts date and time information from a \code{character}
#' vector convertable to a \code{POSIXct} object to a user specified timezone.
#'
#' @param variable A \code{character} vector convertable to a \code{POSIXct} object.
#' @param timezone A character value representing a timezone
#' (\code{\link[base]{timezones}}).
#' @return A \code{POSIXct} vector with converted time and date information.
#' @seealso
#' @examples #
#' @export
timeZoneConvert <- function(variable, timezone = "Asia/Ulan_Bator"){

  as.character(format(as.POSIXct(variable, tz = "UTC"), tz = timezone, usetz = F))

}
