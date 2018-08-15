#draws confidence interval
drawCI <- function(x, lower, upper, color ) polygon( c(rev(x), x), c(rev(upper), lower), col=adjustcolor(color, alpha.f=0.10) , border = NA)
