rm(list = ls())
devtools::load_all()
plot(x, y)

nonlinear.cor <- nlcor(x, y)
print(nonlinear.cor$cor.plot)
