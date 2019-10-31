rm(list = ls())
devtools::load_all()
library(nlcor)
library(ggplot2)
plot(x, y)

nonlinear.cor <- nlcor(x1, y1, plt = T)
print(nonlinear.cor$cor.plot)

nonlinear.cor <- nlcor(x2, y2, plt = T)
print(nonlinear.cor)

nonlinear.cor <- nlcor(x3, y3, plt = T)
print(nonlinear.cor)
nonlinear.cor <- nlcor(x3, y3, refine = 0.999, plt = T)
print(nonlinear.cor)

