PlotNlcor <- function(df,
                      segments,
                      pvalue,
                      line_thickness = 1,
                      line_opacity = 1,
                      title = NA) {

  df.fit <- data.frame()

  for(i in 1:length(segments)) {

    # Populate the segmentwise linear fit for plotting
    df.fit <- UpdateDfFit(seg = segments[[i]],
                          df = df,
                          df.fit = df.fit)
  }

  if(all(is.na(df.fit$fit))) {
    # Exception: Only one segment AND the correlation
    # is statistically insignificant.
    df.fit$fit <- mean(df[, "y"])
  }

  # Configure line type based on its statistical significance
  if(pvalue < 0.05) {
    linetype = "solid"
  } else {
    linetype = "dashed"
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(data = df.fit, ggplot2::aes(x = x,
                                                   y = fit,
                                                   colour = "red"),
                       size = line_thickness,
                       alpha = line_opacity,
                       linetype = linetype) +
    ggplot2::xlab("x") +
    ggplot2::ylab("y") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "#000000"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 12),
                   legend.position = "none"
    )

  if(!is.na(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  return(p)
}
