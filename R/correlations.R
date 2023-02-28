#' Compute Nonlinear Correlation
#'
#' Compute nonlinear correlation using adaptive spatial sampling.
#' @param x A numeric vector. NAs are not allowed. The length of the vector should be more than 10.
#' @param y A numeric vector. NAs are not allowed. Length should be same as `x`. The order of
#' `x` and `y` are relevant. `nlcor(x, y)`` is not equal to `nlcor(y, x)`. Therefore, `x`
#' should be a causal and `y` should be a dependent variable.
#' @param refine Optional. If manually set, a small value, e.g., 0.01, increases
#' the granularity of local correlation computation. The runtime is faster if `refine` is manually set.
#' Otherwise, the algorithm automatically finds the best refinement.
#' @param plt Optional. Default value FALSE. Set TRUE to return ggplot2 object
#' for the data correlation visualization.
#' @param line_thickness Optional. Default 1. Thickness of the correlation lines. It is a float argument > 0.
#' @param line_opacity Optional. Default 1, completely opaque. The opacity of the correlation lines. A float between 0-1. 0 is transparent.
#' @return The output is a list containing the nonlinear correlation \code{cor.estimate}, \code{adjusted.p.value}, and \code{cor.plot}. \code{cor.estimate}
#' is between 0 and 1 (a negative nonlinear correlation is undefined). The
#' \code{adjusted.p.value} shows the statistical significance of the
#' \code{cor.estimate}. If \code{adjusted.p.value > 0.05}, the nonlinear
#' correlation estimate can be considered as noise (statistically not
#' significant). If \code{plt = T}, \code{ggplot2} object is return for
#' plotting the identified local linear correlations in the data.
#' @keywords nonlinear correlation
#' @seealso \code{\link{cor}}
#' @export
#' @examples
#' library(nlcor)
#' library(ggplot2)
#' ncor <- nlcor(x1, y1)
#' ncor <- nlcor(x2, y2, plt = TRUE)
#' ncor <- nlcor(x3, y3, refine = 0.01, plt = TRUE)

nlcor <- function(x,
                  y,
                  refine = NA,
                  plt = T,
                  line_thickness = 1,
                  line_opacity = 1,
                  chart_title = NA) {

  # Initialization with linear correlation
  linearCor <- cor.test(x, y)  # linear correlation
  bestCor <- abs(linearCor$estimate[[1]])
  bestPvalue <- round(linearCor$p.value, 2)

  bestOptimalSegments <- list(1:length(x))  # The entire segment of data

  if(is.na(refine)) {
    refinements <- seq(0.01, 0.15, 0.01)

    for(refine in refinements) {
      greedyOutput <- NlcorGreedySearch(x = x,
                                        y = y,
                                        refine = refine)

      if(round(greedyOutput$netCor$cor.estimate, 2) > round(bestCor, 2) &
         round(greedyOutput$netCor$adjusted.p.value, 2) <= round(bestPvalue, 2)) {
        # This nonlinear segmentation is better
        # Store this output

        bestCor <- greedyOutput$netCor$cor.estimate
        bestPvalue <- greedyOutput$netCor$adjusted.p.value
        bestOptimalSegments <- greedyOutput$optimalSegments
        bestRefine <- refine
      } else {
        # Do nothing
      }
    }
  } else {
    greedyOutput <- NlcorGreedySearch(x = x,
                                      y = y,
                                      refine = refine)
    bestOptimalSegments <- greedyOutput$optimalSegments
  }

  if(plt) {
    # If plot output is required
    cor.plot <- PlotNlcor(df = data.frame(x, y),
                          segments = bestOptimalSegments,
                          pvalue = bestPvalue,
                          line_thickness = line_thickness,
                          line_opacity = line_opacity,
                          title = chart_title)
    print(cor.plot)


    return(list(cor.estimate = greedyOutput$netCor$cor.estimate,
                adjusted.p.value = greedyOutput$netCor$adjusted.p.value,
                cor.plot = cor.plot))
  } else {
    return(list(cor.estimate = greedyOutput$netCor$cor.estimate,
                adjusted.p.value = greedyOutput$netCor$adjusted.p.value
    )
    )
  }
}


NlcorGreedySearch <- function(x, y, refine = 0.05) {

  # Storing the x,y vectors in a dataframe
  df <- data.frame(x, y)

  # Sorting by x to spatially sample the df afterwards.
  # The nlcor output is different if the df(x,y) is sorted
  # by y. Therefore, nlcor(x, y) <> nlcor(y, x).
  df <- df[order(x), ]

  # Initialization
  l <- length(x)
  s <- ValidateRefine(l = l,
                      refine = refine)
    
  segments <- Segment(l = l,
                      s = s)

  df.fit <- data.frame()

  # Empty initialization
  segmentsCor <- list(cor = NULL,
                      pvalue = NULL)
  optimalSegments <- list()
  last.segment.merged <- FALSE

  seg <- segments[[1]]
  optimalSegments[[1]] <- seg

  if(length(segments) >= 2) {
    for(i in 2:length(segments)) {

      previous.optimal.segment.cor <- SegmentCorrelation(
        df$x[optimalSegments[[length(optimalSegments)]]],
        df$y[optimalSegments[[length(optimalSegments)]]])

      current.segment.cor <- SegmentCorrelation(df$x[segments[[i]]],
                                                df$y[segments[[i]]])

      combined.segment.cor <- SegmentCorrelation(
        df$x[c(optimalSegments[[length(optimalSegments)]],
               segments[[i]])],
        df$y[c(optimalSegments[[length(optimalSegments)]],
               segments[[i]])])

      merge.flag <- FALSE

      if((sign(previous.optimal.segment.cor$cor) ==
          sign(current.segment.cor$cor))) {
        # Same direction.
        if(current.segment.cor$p.value <= 0.05) {
          # Check for current segment's statistical significance.
          # Since the direction are same with statistical significance,
          # we merge the segments.
          merge.flag <- TRUE
        } else {
          # Check if combined correlation is higher and merge even if
          # the same direction is not statistically significant to
          # err in favor of fewer segments.
          if (abs(combined.segment.cor$cor) >= mean(c(abs(previous.optimal.segment.cor$cor), abs(current.segment.cor$cor)))) {
            merge.flag <- TRUE
          }
        }
      } else {
        # Direction is opposite
        if(current.segment.cor$p.value <= 0.05) {
          # Check for current segment's statistical significance, if
          # significant, do NOT merge.
          merge.flag <- FALSE
        } else {
          # If there is no statistical significance, the change
          # in direction could be due to noise. So check if combined
          # correlation is higher and merge.
          if (abs(combined.segment.cor$cor) >= mean(c(abs(previous.optimal.segment.cor$cor), abs(current.segment.cor$cor)))) {
            merge.flag <- TRUE
          }
        }
      }

      if(merge.flag) {
        # The correlation directions are the same in segments i-1 and i.

        # Merge the segments
        seg <- c(optimalSegments[[length(optimalSegments)]], segments[[i]])
        optimalSegments[[length(optimalSegments)]] <- seg

        if(i == length(segments)) {
          # Update the last segment is merged flag
          last.segment.merged <- TRUE
        }

      } else { # Direction changes.
        # Now work with the previously merged segments

        # Populate the segmentwise correlation
        segmentsCor <- UpdateSegmentsCor(x.seg = df$x[seg],
                                         y.seg = df$y[seg],
                                         segmentsCor = segmentsCor)

        # Reset the segment
        seg <- segments[[i]]
        optimalSegments[[length(optimalSegments) + 1]] <- seg
      }
    }

    # Look at the last segment if it still remains
    if(last.segment.merged) {

      # Populate the segmentwise correlation
      segmentsCor <- UpdateSegmentsCor(x.seg = df$x[seg],
                                       y.seg = df$y[seg],
                                       segmentsCor = segmentsCor)

    } else {
      lastSeg <- segments[[length(segments)]]
      optimalSegments[[length(optimalSegments) + 1]] <- lastSeg

      # Now work with the last unmerged segment

      # Populate the segmentwise correlation
      segmentsCor <- UpdateSegmentsCor(x.seg = df$x[lastSeg],
                                       y.seg = df$y[lastSeg],
                                       segmentsCor = segmentsCor)

    }

  } else if(length(segments) == 1) {
    # Populate the segmentwise correlation
    segmentsCor <- UpdateSegmentsCor(x.seg = df$x[seg],
                                     y.seg = df$y[seg],
                                     segmentsCor = segmentsCor)
  }

  netCor <- NetCor(cors = segmentsCor$cor,
                   pvalues = segmentsCor$pvalue,)

  return(list(netCor = netCor,
              optimalSegments = optimalSegments))
}



NetCor <- function(cors, pvalues, p.threshold = 0.05) {
  adjusted.p.threshold <- p.threshold / length(cors)

  for(i in 1:length(cors)) {
    if(is.na(pvalues[i])) {
      pvalues[i] <- NA
      cors[i] <- 0
    }
  }

  return(list(cor.estimate = mean(abs(cors)),
              adjusted.p.value = 1- prod((1 - pvalues), na.rm = T),  # Approximation of p-value assuming segment independence
              segment.cor = list(cor = cors,
                                 p.value = pvalues)))
}
