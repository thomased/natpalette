#' A 'natural' colour palette generator
#'
#' Some fairly impractical colour palettes inspired by nature.
#'
#' @param n Number of colours. If omitted, uses all colours.
#' @param name The desired palette. One of:
#' \itemize{
#'   \item \code{Acripeza}: The mountain katydid \emph{Acripeza reticulata}.
#'   \item \code{Aix}: The mandarin duck \emph{Aix galericulata}.
#'   \item \code{Chrysiridia}: The Madagascan sunset moth \emph{Chrysiridia rhipheus}.
#'   \item \code{Coracias}: The lilac-breasted roller \emph{Coracias caudatus}.
#'   \item \code{Furcifur}: The panther chameleon \emph{Furcifer pardalis}.
#'   \item \code{Delias}: The Phillippine jezabel \emph{Delias henningia}.
#'   \item \code{Maratus}: The peacock spider \emph{Maratus volans}.
#'   \item \code{Synchiropus}: The mandarinfish \emph{Synchiropus splendidus}.
#'   \item \code{Trichoglossus}: The rainbow lorikeet \emph{Trichoglossus haematodus}.
#'   \item \code{Tulipa}: Tulips \emph{Tulipa gesneriana}.
#'   }
#' @return A vector of colours.
#' @export
#' @keywords colours
#' @examples
#' nat_palette('Maratus')
#' nat_palette('Tulipa')
#'
nat_palette <- function(name, n) {

  pal <- palettes[[name]]
  if(is.null(pal))
    stop('No such palette.')

  if(missing(n)){
    n <- length(pal)
  }

  if(n > length(pal)){
    stop('Not enough colours in the requested palette.')
  }

  # Data attributes
  out <- pal[1:n]
  structure(out, class = 'palette', name = name)
}

#' Palette list
#'
#' Colour list. Use \code{\link{nat_palette}} to build palettes.
#'
#' @export
palettes <- list(
  Acripeza = c('#ca3703', '#0b0c07', '#2474c9', '#899273'),
  Aix = c('#c27040', '#dba368', '#6699c8', '#3253ee', '#12654b', '#6432a5', '#fd3853'),
  Chrysiridia = c('#5e8e87', '#e38e13', '#cacb39', '#3579f0', '#793af3', '#aa2132'),
  Coracias = c('#231fca', '#7ef7fb', '#627054', '#875b5d', '#e7e1cc'),
  Delias = c('#fb2d15', '#312c32', '#fdd633', '#e4e2f0'),
  Furcifer = c('#a2f5fb', '#7dcc7b', '#dbcc55', '#d8894e', '#93331d'),
  Maratus = c('#c2370e', '#c7621a', '#d8b54d', '#8ec9eb', '#d1d7ef'),
  Synchiropus = c('#00000a', '#fa7200', '#02c0fa', '#000199', '#7fd071', '#fffb02'),
  Trichoglossus = c('#517b39', '#b2ba31', '#8271b7', '#fdb81d', '#cd3f35'),
  Tulipa = c('#e61229', '#ff5e20', '#d46790', '#e8b413', '#e6d0a7')
  )


#' @export
#' @import graphics
print.palette <- function(x, ...){
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
    ylab = '', xaxt = 'n', yaxt = 'n', bty = 'n')
}
