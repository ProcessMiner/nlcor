#' Segmentation.
#'
#' Segment \code{1:l} in to almost equal set of indices.
#' @param l An integer.
#' @param s A float number between 0 and 1.
#' @return The list of segmented sequence \code{1:l}.
#' @export
#' @examples
#' Segment(l = 100, s = 0.2)
#'
Segment <- function(l, s) {

  windowLen <- floor(s * l)
  numSegments <- l %/% windowLen
  segments <- list()

  upp <- windowLen
  low <- 1
  for(i in 1:numSegments) {
    segments[[i]] <- low:upp
    low <- upp + 1
    upp <- upp + windowLen
  }

  if(l %% windowLen != 0) {
    segments[[length(segments) + 1]] <- low:l
  }

  return(segments)
}


#' Find the segmentation size.
#'
#' Identify a segment size that yields at least 5 record.
#' @param l An integer.
#' @return A floating number \code{s} between 0 and 1.
#' @export
#' @examples
#' FindSegmentSize(l = 100)
#'
FindSegmentSize <- function(l) {
  s <- 0.05
  seg.size <- floor(s * l)

  while(seg.size < 5) {
    s <- s + 0.05
    seg.size <- floor(s * l)
  }

  return(s)
}

