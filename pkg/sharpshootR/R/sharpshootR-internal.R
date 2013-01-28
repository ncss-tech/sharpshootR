# compute pair-wise weightes for soil-relation graph
.pair.wise.wts <- function(d, wt) {
	x <- d[[wt]]
	x <- x / sum(x)
	w <- outer(x, x, FUN='*')
	return(w)
}
