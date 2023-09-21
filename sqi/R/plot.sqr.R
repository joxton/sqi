

if(0){
	da=data.frame(y=rnorm(100),x=rnorm(100))
	m=mqr(y~x,data=da)
	
	fit=rq(y~x,data=da,tau=seq(.1,.9,by=.1))
	b=boot_aux(fit=fit,B=B,B_bridge=M)
	
}
