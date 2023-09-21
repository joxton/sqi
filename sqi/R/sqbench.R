
sqbench.tarr1=function(n,df=2){

		beta = 10
		x = runif(n,0,5)
		mu = beta + x*beta
		y = mu + (rt(n,df=df))
		return(data.frame(y=y,x=x))

}


sqbench.tarr2=function(n,dist="chi1"){

		if(dist=="chi2"){
			x = rchisq(n,df=2)
		}
		if(dist=="chi1"){
			x = rchisq(n,df=1)
		}else{
			x = exp(rnorm(n))
		}
		beta = 10
		mu = 10 + x*beta
		y = mu + rnorm(n)
		
		return(data.frame(y=y,x=x))

}



sqbench.tarr3=function(n,rho=.5,alpha=.5){

		x = rmvnorm(n,sigma=(diag(2)*(1-rho)+array(rho,c(2,2))))
				
		beta = c(10,0)
		mu = 10 + x%*%beta
		y = mu + (rnorm(n))*(1 + alpha*x[,1])

		return(data.frame(y=y,x=x))

}




sqbench.khm1=function(n){

		x1 = rnorm(n)
		x3 = runif(n,0,1)
		x2 = x1 + x3 + rnorm(n)
		x = cbind(x1,x2)
		beta = c(1,1)
		mu = 1 + x%*%beta
		y = mu + rnorm(n)
	
		return(data.frame(y=y,x=x))



}



sqbench.khm2=function(n){


		x1 = rnorm(n)
		x3 = runif(n,0,1)
		x2 = x1 + x3 + rnorm(n)
		x = cbind(x1,x2,x3)
		beta = c(1,1,1)
		mu = 1 + x%*%beta
		y = mu + rnorm(n)*(1 + x3)

		return(data.frame(y=y,x1=x1,x2=x2,x3=x3))

}





sqbench.khm3=function(n){


		x1 = rbinom(n,size=1,prob=.4)
		x2 = rbinom(n,size=1,prob=.4)
		x3 = exp(rnorm(n))
		x4 = exp(rnorm(n))
		x56 = rmvnorm(n,mean=c(2,2),sigma=(diag(2)*.2+array(.8,c(2,2))))
		x7 = rchisq(n,df=1)
		x = cbind(x1,x2,x3,x4,x56,x7)
		
		beta = rep(1,7)
		mu = 1 + x%*%beta
		y = mu + rnorm(n)
		dat=data.frame(y=y,x)
		names(dat)[-1]=paste0("x",1:7)

		return(dat)

}




sqbench.khm4=function(n){

		x1 = rbinom(n,size=1,prob=.4)
		x2 = rbinom(n,size=1,prob=.4)
		x3 = exp(rnorm(n))
		x4 = exp(rnorm(n))
		x56 = rmvnorm(n,mean=c(2,2),sigma=(diag(2)*.2+array(.8,c(2,2))))
		x7 = rchisq(n,df=1)
		x = cbind(x1,x2,x3,x4,x56,x7)
		
		beta = rep(1,7)
		mu = 1 + x%*%beta
		y = mu + rnorm(n)*(1+x3+x56[,1]+x7)
		dat=data.frame(y=y,x)
		names(dat)[-1]=paste0("x",1:7)

		return(dat)

			
}	





sqbench.khm5=function(n,tau=.25){

		x1 = rbinom(n,size=1,prob=.4)
		x2 = rbinom(n,size=1,prob=.4)
		x3 = exp(rnorm(n))
		x4 = exp(rnorm(n))
		x56 = rmvnorm(n,mean=c(2,2),sigma=(diag(2)*.2+array(.8,c(2,2))))
		x7 = rchisq(n,df=1)
		x = cbind(x1,x2,x3,x4,x56,x7)
		
		beta = rep(1,7)
		mu = 1 + x%*%beta
		y = mu + (rnorm(n)-qnorm(tau))*(x56[,1]^2)
		dat=data.frame(y=y,x)
		names(dat)[-1]=paste0("x",1:7)

		return(dat)

		
}	




sqbench.khm6=function(n){

		x = runif(n,-1,1)
		mu = 1 + x
		y = mu + (rnorm(n))*(1.1+x)
		dat = data.frame(y=y,x)
		names(dat)[-1] = paste0("x",1:3)


		return(dat)

	
}	




sqbench.khm7=function(n){

		x1 = rnorm(n)
		x3 = rnorm(n)
		x2 = abs(rt(n,df=2))
		x = cbind(x1,x2,x3)
		beta=c(1,1,1)
		mu = 1 + x%*%beta 

		y = mu + (rnorm(n))
		dat = data.frame(y = y,x = x)

		return( dat )

	
}	



sqbench.khm=function(n,scenario){


	dat=NULL
	if(scenario==1)
		dat=sqbench.khm1(n=n)
	if(scenario==2)
		dat=sqbench.khm2(n=n)
	if(scenario==3)
		dat=sqbench.khm3(n=n)
	if(scenario==4)
		dat=sqbench.khm4(n=n)
	if(scenario==5)
		dat=sqbench.khm5(n=n)
	if(scenario==6)
		dat=sqbench.khm6(n=n)
	if(scenario==7)
		dat=sqbench.khm7(n=n)

	return(dat)

}

