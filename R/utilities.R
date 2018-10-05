#' Segmentation.
#'
#' Segment \code{1:l} in to almost equal set of indices.
#' @param l An integer.
#' @param s A float number between 0 and 1.
#' @return The list of segmented sequence \code{1:l}.
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
    seg <- low:l
    if(length(seg) <= 5) {
      # Merge with the last segment
      segments[[length(segments)]] <- c(segments[[length(segments)]], seg)
    } else {
      segments[[length(segments) + 1]] <- seg
    }
  }

  return(segments)
}


#' Find the segmentation size.
#'
#' Identify a segment size that yields at least 5 record.
#' @param l An integer.
#' @return A floating number \code{s} between 0 and 1.
#' @examples
#' FindSegmentSize(l = 100, refine = 0.9)
#'
FindSegmentSize <- function(l, refine) {
  s <- 0.05 * (1 - refine)
  seg.size <- floor(s * l)

  segmentAdjusted <- F
  while(seg.size < 5) {
    segmentAdjusted <- T
    s <- s + 0.05 * (1 - refine)
    seg.size <- floor(s * l)
  }

  if(segmentAdjusted) {
    warning("Refinement too high or data is small. Adjusting computation.")
  }
  return(s)
}

