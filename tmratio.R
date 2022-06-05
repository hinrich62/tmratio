#R-code used to estimate the power of an experiment
#to detect a significant difference between the
#smolt-to-adult SARs of transported juveniles
#and the smolt-to-adult survival rate of juveniles
#migrating in-river.
#inputs
#SART -- SAR of transported juveniles
#NT -- the number of tagged transported juveniles
#SARM -- SAR of in-river juvenile migrants
#NM -- number of tagged in-river juvenile migrants
tmpower<-function(SART=.01,NT=1000,SARM=.02,NM=1000,alpha=0.05){
if((SART>1)|(SARM<0)){
warning("The SARs are survival rates and so must be less than one")
return(NULL)}
if((SARM>1)|(SARM<0)){
warning("The SARs are survival rates and so must be less than one")
return(NULL)}
if(NM<=0){
warning("The number of juveniles released must exceed zero")
return(NULL)}
if(NT<=0){
warning("The number of juveniles released must exceed zero")
return(NULL)}
if((alpha>1)|(alpha<0)){
warning("The significance level (alpha) must be between zero and one")
return(NULL)}
if((NT*SART<5)|(NM*SARM<5)){
warning("An expected cell count is less than 5" )
return(NULL)}
delta<-log(SART/SARM)
q<-qnorm(1-alpha/2)
tmvar<-(1-SART)/(SART*NT)+(1-SARM)/(SARM*NM)
se<-sqrt(tmvar)
power<-(1-pnorm(q*se,mean=delta,sd=se))+pnorm(-q*se,mean=delta,sd=se)
return(list(SART=SART,NT=NT,SARM=SARM,NM=NM,alpha=alpha,delta=delta,se=se,
cv=se/delta,power=power))
}
#outputs
#delta is the true log(SART/SARM)
#se is the standard error of the estimate
#cv is the CV of the estimate
#power is the probability of rejecting the null hypothesis of delta=0.
