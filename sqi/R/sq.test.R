

#sq.test <- function(x,plot.it=FALSE, ...) UseMethod("sq.test.formula")

#sq.test.default <-
#function(x, y = NULL,plot.it=FALSE,...)
#{
#}


"sq.test" <-
function(formula, data, subset, na.action,M=10000,plot.it=FALSE,...)
{
	dots <- list(...)
	names(dots)
	argg <- c(as.list(environment()), list(...))
	
	fit=sqr(formula,data=data)
	m=fit$maxT[2,]
	m$pvalue=m$p_value
	if(m$pvalue<1/M)
		m$pvalue=paste0("<",1/M)
	#if("plot.it" %in% names(dots)){	
	if(plot.it){
		object=fit
		xindx=2
		b=object$coefficients
		component_name=colnames(object$fit$x)[xindx]
		b=b[which(b$var==component_name),]
		clist=list(x=1,type="n")
	
		if(is.null(argg$ylim))
			clist$ylim = c(min(b$tau),max(b$tau))
		if(is.null(argg$xlim))
			clist$xlim = c(min(b$lower),max(b$upper))
		if(is.null(argg$ylab))
			clist$ylab = "tau"
		if(is.null(argg$xlab))
			clist$xlab = component_name
		
				
	# plot(1,type="n",ylim=ylim,xlim=c(min(b$tau),max(b$tau)),xlab="tau",ylab=component_name,...)
	do.call("plot",c(clist,dots))
		
	for(k in 1:nrow(b)){
		points(c(b$lower[k],b$upper[k]),rep(b$tau[k],2),type="l",col="grey")
	}
	points(b$estimate,b$tau,pch=16,...)
	abline(v=0,lty=2)
	}
	cat("Quantile difference test \n\n")
	cat(paste0("data: ",as.character(fit$fit$formula[[2]])," by ",colnames(fit$fit$x)[2],"\n"))
	if(substr(m$pvalue,1,1)!="<"){
		cat(paste0("maxT = ",round(m$maxT,2),", p.value = ",m$pvalue,"\n\n")) 
	}else{
		cat(paste0("maxT = ",round(m$maxT,2),", p.value < ",substr(m$pvalue,2,nchar(m$pvalue)),"\n\n")) 	
	}	
	invisible(m)
}

