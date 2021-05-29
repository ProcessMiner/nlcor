MIN_SEGMENT_SIZE <- 3

UpdateSegmentsCor <- function(x.seg, y.seg, segmentsCor) {
  segcor <- stats::cor.test(x.seg, y.seg)
  segmentsCor$cor <- c(segmentsCor$cor, segcor$estimate[[1]])
  segmentsCor$pvalue <- c(segmentsCor$pvalue, segcor$p.value)
  return(segmentsCor)
}

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
    if(length(seg) <= MIN_SEGMENT_SIZE) {
      # Merge with the last segment
      segments[[length(segments)]] <- c(segments[[length(segments)]], seg)
    } else {
      segments[[length(segments) + 1]] <- seg
    }
  }

  return(segments)
}

SegmentCorrelation <- function(x.seg, y.seg) {
  temp <- stats::cor.test(x.seg, y.seg)

  segmentCorrelation <- list(cor = ifelse(is.na(temp$estimate[[1]]),
                                          0,
                                          temp$estimate[[1]]),
                             p.value = ifelse(is.na(temp$estimate[[1]]),
                                              1,
                                              temp$estimate[[1]])
  )
  return(segmentCorrelation)
}

ValidateRefine <- function(l, refine) {

  if(floor(refine * l) < MIN_SEGMENT_SIZE) {
    # Means too few data points in a segment.
    # Therefore, increase it.
    while(floor(refine * l) <= MIN_SEGMENT_SIZE) {
      refine <- refine + 0.01
    }
  }
  return(refine)
}

UpdateDfFit <- function(seg, df, df.fit) {
  fit <- stats::lm(y ~ x, data = df[seg, c("x", "y")])

  # print(summary(fit))

  if(length(summary(fit)$coefficients[, "Pr(>|t|)"]) == 2) {
    fit.significance <- summary(fit)$coefficients[2, "Pr(>|t|)"]
  } else {
    fit.significance <- NA
  }

  # print(fit.significance)

  if(is.na(fit.significance) | fit.significance > 0.01) {
    # TODO: Make the pvalue threshold of 0.01 adjusted as per Bonferroni
    # When the fit is not statistically significant.
    df.fit <- rbind(df.fit,
                    data.frame(x = df[seg, "x"],
                               fit = fit$fitted))  # Adding the fitted values as NA for plotting because no real correlation exist here.
    df.fit[seg[length(seg)], "fit"] <- NA  # Last point set to NA for plotting beautification. It results into disjoint lines. Otherwise, the plot is ugly with several cliffs.
  } else {
    # Fit is statistically significant.
    df.fit <- rbind(df.fit,
                    data.frame(x = df[seg, "x"],
                               fit = fit$fitted))  # Adding the fitted values for plotting.
    df.fit[seg[length(seg)], "fit"] <- NA  # Last point set to NA for plotting beautification. It results into disjoint lines. Otherwise, the plot is ugly with several cliffs.
  }

  return(df.fit)
}
