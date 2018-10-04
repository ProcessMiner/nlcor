rm(list = ls())
devtools::load_all()
library(nlcor)
load_data(pkg = ".")
plot(x, y)

nonlinear.cor <- nlcor(x1, y1, plt = T)
print(nonlinear.cor$cor.plot)

nonlinear.cor <- nlcor(x2, y2, plt = T)
print(nonlinear.cor)

nonlinear.cor <- nlcor(x3, y3, plt = T)
print(nonlinear.cor)
nonlinear.cor <- nlcor(x3, y3, refine = 0.9, plt = T)
print(nonlinear.cor)

nonlinear.cor <- nlcor(x3, y3, refine = 0.1, plt = T)
print(nonlinear.cor)
cor(x3, y3)
