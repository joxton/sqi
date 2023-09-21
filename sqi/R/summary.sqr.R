

"summary.sqr" <-
function (object, ...) {
        taus <- object$tau
        xsum <- as.list(taus)
	dots <- list(...)
	
	return(object$maxT)	
}



"plot.sqr" <-
function (object, xindx=1,...) {
        taus <- object$tau
        xsum <- as.list(taus)
	dots <- list(...)
	argg <- c(as.list(environment()), list(...))

	b = object$coefficients
	component_name = colnames(object$fit$x)[xindx]
	b = b[which(b$var == component_name),]
	
	clist=list(x=1,type="n")
	
	if(is.null(argg$ylim))
		clist$ylim = c(min(b$lower),max(b$upper))
	if(is.null(argg$xlim))
		clist$xlim = c(min(b$tau), max(b$tau))
	if(is.null(argg$xlab))
		clist$xlab = "tau"
	if(is.null(argg$ylab))
		clist$ylab = component_name
		
				
	# plot(1,type="n",ylim=ylim,xlim=c(min(b$tau),max(b$tau)),xlab="tau",ylab=component_name,...)
	do.call("plot",c(clist,dots))
	for(k in 1:nrow(b)) {
		points(rep(b$tau[k], 2), c(b$lower[k], b$upper[k]), type = "l", col = "grey")
	}
	points(b$tau,b$estimate,pch=16,...)

			
}




