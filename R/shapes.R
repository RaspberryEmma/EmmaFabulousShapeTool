# SC1 - Personal Portfolio - Packages
# Example Package "Emma's Fabulous Shape Tool"
# Emma Tarmey
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Calculate the area of a circle
#'
#' @param radius A number
#' @return A number
#' @examples
#' circle.area(5)
#' circle.area(22.5)
circle.area <- function(radius = NULL) {
  return (pi * (radius * radius))
}

#' Calculate the perimeter of a circle
#'
#' @param radius A number
#' @return A number
#' @examples
#' circle.perimeter(5)
#' circle.perimeter(22.5)
circle.perimeter <- function(radius = NULL) {
  return (2 * pi * radius)
}

#' Calculate the area of a triangle where all 3 sides are known
#'
#' @param side.A A number
#' @param side.B A number
#' @param side.C A number
#' @return A number
#' @examples
#' triangle.area.heron.formula(5, 5, 5)
#' triangle.area.heron.formula(22.5, 14.6, 11.9)
triangle.area.heron.formula <- function(side.A = NULL, side.B = NULL, side.C = NULL) {
  s    <- (side.A + side.B + side.C)/2 # 1/2 perimeter of triangle
  area <- sqrt(s * (s - side.A) * (s - side.B) * (s - side.C) )
  return (area)
}

#' Calculate the area of a triangle where 2 sides and 1 angle are known
#'
#' @param side.A A number
#' @param side.B A number
#' @param angle.C A number, angle measured in radians
#' @return A number
#' @examples
#' triangle.area.SAS.formula(5, 5, 5)
#' triangle.area.SAS.formula(22.5, 14.6, )
triangle.area.SAS.formula <- function(side.A = NULL, side.B = NULL, angle.C = NULL) {
  return (0.5 * side.A * side.B * sin(angle.C))
}

#' Calculate the perimeter of a triangle where all 3 sides are known
#'
#' @param side.A A number
#' @param side.B A number
#' @param side.C A number
#' @return A number
#' @examples
#' triangle.perimeter(5, 5, 5)
#' triangle.perimeter(22.5, 14.6, 11.9)
triangle.perimeter <- function(side.A = NULL, side.B = NULL, side.C = NULL) {
  return (side.A + side.B + side.C)
}

#' Calculate the perimeter of a triangle where 2 sides and 1 angle are known
#'
#' @param side.A A number
#' @param side.B A number
#' @param angle.C A number, angle measured in radians
#' @return A number
#' @examples
#' triangle.perimeter.SAS.formula(5, 5, 5)
#' triangle.perimeter.SAS.formula(22.5, 14.6, )
triangle.perimeter.SAS.formula <- function(side.A = NULL, side.B = NULL, angle.C = NULL) {
  side.C <- sqrt( (side.A * side.A) + (side.B * side.B) - (2 * side.A * side.B * cos(angle.C)) )
  return ( triangle.perimeter(side.A, side.B, side.C) )
}

#' Calculate the area of a square
#'
#' @param side A number
#' @return A number
#' @examples
#' square.area(5)
#' square.area(22.5)
square.area <- function(side = NULL) {
  return ( quadrilateral.area(side, side) )
}

#' Calculate the perimeter of a square
#'
#' @param radius A number
#' @return A number
#' @examples
#' square.perimeter(5)
#' square.perimeter(22.5)
square.perimeter <- function(side = NULL) {
  return ( quadrilateral.perimeter(side, side) )
}

#' Calculate the area of a quadrilateral
#'
#' @param length A number
#' @param width A number
#' @return A number
#' @examples
#' quadrilateral.area(5, 10)
#' quadrilateral.area(22.5. 9.9)
quadrilateral.area <- function(length = NULL, width = NULL) {
  return (length * width)
}

#' Calculate the perimeter of a quadrilateral
#'
#' @param length A number
#' @param width A number
#' @return A number
#' @examples
#' quadrilateral.perimeter(5, 10)
#' quadrilateral.perimeter(22.5. 9.9)
quadrilateral.perimeter <- function(length = NULL, width = NULL) {
  return ((2 * length) + (2 * width))
}

#' Calculate the area of a regular pentagon
#'
#' @param side A number
#' @return A number
#' @examples
#' regular.pentagon.area(5)
#' regular.pentagon.area(22.5)
regular.pentagon.area <- function(side = NULL) {
  return ( 0.25 * sqrt(5 * (5 + (2 * sqrt(5)))) * (side * side) )
}

#' Calculate the perimeter of a regular pentagon
#'
#' @param side A number
#' @return A number
#' @examples
#' regular.pentagon.perimeter(5)
#' regular.pentagon.perimeter(22.5)
regular.pentagon.perimeter <- function(side = NULL) {
  return(5 * side)
}



