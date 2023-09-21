

"sqr" <- function(formula, data, tau=seq(.1,.9,by=.025), method="boot-t", B = 100, M = 10000){


	fit = rq(formula, data = data, tau = tau)
	
#	 m = sqr(lbau~age + gender + group + vaccine + vaccine_sequence + time_between_blood_and_vaccine + time_between_vaccinations, data = x)
# 	fit = rq(lbau~age + gender + group + vaccine + vaccine_sequence + time_between_blood_and_vaccine + time_between_vaccinations, data = x, tau=seq(.1,.9,by=.025))

	p = ncol(fit$x) 
	
	if(! method %in% c("boot-t", "boot-p", "iid", "nid", "rank", "pwy-t", "pwy-p", "pivotal", "mcmb"))
		stop(" 'method' not matched ")
		
	# --- standard error based 		
	if(method %in%  c("boot-t", "iid", "nid", "pwy-t", "mcmb")) {
	
		if (method == "boot-t")  
			b = summary(fit, se = "boot", bsmethod = "xy", R = B)   #boot_aux(fit = fit, B = B, B_bridge = M)

		if (method == "pwy-t") 
			b = summary(fit, se = "boot", bsmethod = "pwy", R = B) 

		if (method == "mcmb") 
			b = summary(fit, se = "boot", bsmethod = "mcmb", R = B) 

		if (method == "iid") 
			b = summary(fit, se = "iid") 

		if (method == "nid") 
			b = summary(fit, se = "nid") 

		
		coeff=NULL
		for(k in 1:length(b)) {
			bk=b[[k]]$coefficients			
			coeff = rbind(coeff, data.frame(tau = rep(b[[k]]$tau, p), beta = paste0("beta", 1:p), estimate = bk[, 1], se = bk[, 2]))
		}

		coeff$tstat = coeff$estimate / coeff$se 
		maxT = aggregate(abs(coeff$tstat), by = coeff["beta"], max)
		names(maxT)[2] = "maxT"
		ii = order(as.numeric(gsub("beta","",maxT$beta)))
		maxT = maxT[ii,]		

		m = maxT_simulation(fit = fit, maxT[,2], B = M)

		coeff = merge(coeff, m[,c("beta", "bound")], by = "beta", all.x = TRUE)
		
		# upper/lower simultaneous confidence limits  
		coeff$lower = coeff$estimate - (coeff$bound * coeff$se)
		coeff$upper = coeff$estimate + (coeff$bound * coeff$se)

		
		da = data.frame(beta = paste0("beta", 1:p), var = colnames(fit$x))
		coeff = merge(coeff, da, by = "beta", all.x = TRUE)
		coeff = coeff[ ,c("var", "beta", "tau", "estimate", "se", "lower", "upper")]

		maxT = merge(maxT, m, by = "beta", all.x = TRUE)
		maxT = merge(maxT, da, by = "beta", all.x = TRUE)
		# Put in origional order
		ii = order(as.numeric(gsub("beta","",maxT$beta)))
		maxT = maxT[ii,]		


		maxT = maxT[ ,c("var", "maxT", "bound", "p_value")]


	}
	 

	

        return_list=list(maxT = maxT, coefficients = coeff, fit = fit)
	class(return_list)="sqr"
	return(return_list)

}











maxT_simulation=function(fit,maxT,B){

	## *************************************************************************************
	## Samples from asymptotic multi-variate normal distribution for 
	## the regression quantiles to deter 95-percentile of maxT statistic 
	## *************************************************************************************


	## ***** Helper function *************************************************************** 
	cquant = function(tau, y, x, coef){
		## Copied from quantreg-package 
		p = length(coef)
		n = length(y)
	        h <- bandwidth.rq(tau, n, hs = TRUE)
		while((tau - h < 0) || (tau + h > 1)) h <- h/2
	        uhat <- c(y - x %*% coef)
	        h <- (qnorm(tau + h) - qnorm(tau - h))*
			min(sqrt(var(uhat)), ( quantile(uhat,.75)- quantile(uhat, .25))/1.34 )
	        f <- dnorm(uhat/h)/h
	        fxxinv <- diag(p)
	        fxxinv <- backsolve(qr(sqrt(f) * x)$qr[1:p, 1:p,drop=FALSE], fxxinv)
	        fxxinv <- fxxinv %*% t(fxxinv)
	        cov <- tau * (1 - tau) * fxxinv %*% crossprod(x) %*%
	            fxxinv
	        scale <- mean(f)
	        serr <- sqrt(diag(cov))
	
		list( serr = serr, Jinv = (fxxinv * n), coeff = coef, tau = tau )
	}
	## ************************************************************************************ 


	x = fit$x
	y = fit$y
	n = nrow(x)
	tau = fit$tau
	ntau = length(tau)
	p = ncol(x)
	
	if(length(tau) == 1)
		fit$coefficients=matrix(fit$coefficients, ncol = 1)
	
	Jinv_stacked=NULL
	mlist=list()
	for(k in 1:length(tau)) {
		mlist[[k]] = cquant(tau = tau[k], y = y, x = x, coef = fit$coef[,k])
		Jinv_stacked = rbind( Jinv_stacked, mlist[[k]]$Jinv )
	}	
	

	Sigma = t(x) %*% x
	Sigma = Sigma / n
	
	e = eigen(Sigma)
	V = e$vectors
	Sigma_root = V %*% diag( sqrt(e$values) ) %*% t(V)


	ntau=length(tau)
	sigma=array(0,dim=c(ntau,ntau))
	for(k in 1 : ntau) 
		for(j in 1 : ntau)
			sigma[k,j] = min(tau[k], tau[j]) - tau[k] * tau[j]		
	
	
	draw_bridge=rmvnorm(B * p, sigma = sigma)
	for(b in 1 : B) 
		draw_bridge[(1 + (b - 1) * p ):(b * p),] = Sigma_root %*% draw_bridge[(1 + (b - 1) * p ):(b * p),]
	


	for(k in 1 :length(mlist))
			draw_bridge[, k]=(diag(1 / ( mlist[[k]]$serr * sqrt(n) )) %*% mlist[[k]]$Jinv) %*% matrix(draw_bridge[,k], nrow = p, byrow = FALSE)
	
	max_b=apply(abs(draw_bridge), 1, max)
			
	q_bound = p_value = NULL
	for(k in 1:p) {
		q_bound = c(q_bound, quantile(max_b[ seq(k, nrow(draw_bridge), by = p) ], prob = .95))
		p_value = c(p_value, mean(max_b[ seq(k, nrow(draw_bridge), by = p)] >= maxT[k]))
	
	}
	
	return_frame = data.frame( beta = paste0("beta", 1:p), bound = q_bound, p_value = p_value)
	return(return_frame) 
}



