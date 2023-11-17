c <- as.character(NULL)
c
vc.coercion <- c(1,2L,'1')
vn.coercion <- c(1,2L,5,NULL,NA,TRUE)
vi.coercion <- c(1L,2L,5L)
vb.coercion <- c(TRUE, 'FALSE')

one.dim.space <- as.integer( c(1,4,7,9,3,2,0,3,4,4))
one.dim.space
is.vector(one.dim.space)
is.matrix(one.dim.space)
class(one.dim.space)
str(one.dim.space)
vi.uniq <- unique(one.dim.space)
length(unique(one.dim.space))
head(one.dim.space,3)
mode(one.dim.space)
max(one.dim.space)

library(psych)
describe(one.dim.space)
