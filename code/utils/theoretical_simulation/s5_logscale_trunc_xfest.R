#===========================================================================================#
#------------------------------------  LIBRARY  --------------------------------------------#
#===========================================================================================#

library(saeSim)

#===========================================================================================#
#-------------------------------  SIM. OPTIONS  --------------------------------------------#
#===========================================================================================#
time.start.total=Sys.time()
options(set.memory=max)

calc_keepPop=function(dat)
{
  attr(dat, "pop")=dat
  dat
}

set.seed(100)
simruns=10
Domains=50
sample_size=c(12, 23, 28, 14, 10, 23, 19, 25, 29, 10, 14, 18, 15, 20, 13, 12, 16, 27, 20, 26, 27, 23, 12, 12, 11, 18, 17, 29, 11, 29, 17, 9, 14, 8, 8, 18, 21, 21, 16, 16, 25, 13, 26, 19, 28, 20, 24, 9, 25, 21)
pop_size=rep(200,Domains)
sum(pop_size)
sum(sample_size)

#==================================================================================================================================#
#-----------------------------------  Scenario ---------------------------------------------------------------#
#==================================================================================================================================#

gen_XNorm=function(dat,m=dat$muD, s=2){
  dat["x"]=rnorm(nrow(dat), mean = m, sd = s)
  return(dat)
}

gen_myE=function(dat,m=0, s=1.4) {
  dat["e"]=rnorm(nrow(dat), mean = 0, sd = s)
  dat
}

setup=sim_base(data = base_id(nDomains=Domains,nUnits=pop_size)) %>%
  sim_gen(gen_generic(runif, min = -1.5, max = 1.5, groupVars="idD", name = "muD")) %>%
  sim_gen(gen_XNorm)       %>%
  as.data.frame %>%
  sim_gen(generator=gen_myE)         %>% 
  sim_gen_v(mean=0,sd=0.5)         %>% 
  sim_resp_eq(y = exp( 3 + 0.5*x + v + e) )
Pop=sim(setup,R=simruns)
formel=formula(y~x)


sampler=function(DAT){
  smp=as.data.frame(matrix(nrow=sum(sample_size) , ncol=ncol(DAT)))
  brd=append(0,cumsum(sample_size))
  for(i in 1:Domains){
    smp[((brd[i]+1):brd[i+1]),]=(DAT[DAT$idD==i,])[sample(1:sum(DAT$idD==i),size=sample_size[i]),]
  }
  attr(smp,"pop")=DAT
  colnames(smp)=colnames(DAT)
  return(smp)
}

Pop=lapply(Pop,sampler)

save(Pop, file = "../Data/s5_logscale_trunc_xfest500.RData")


#r.squared=summary(lm(log(y)~x,data=Pop[[1]]))$r.squared
#r.squared

#quantplot(y~x,data=Pop[[1]])
#summary(Pop[[1]]$y)

#source("EBP_log.R")

#uuu=1
#test<-attr(Pop[[uuu]],"pop")
#daten_sample<-data.frame(y=Pop[[uuu]]$y,x=Pop[[uuu]]$x)
#daten_pop<-data.frame(x=test$x)
#Prove=FGTpovertyEB_log(formula = y ~ x, dataframe_sample = daten_sample,saind=Pop[[uuu]]$idD,threshold=NULL,
#                       dataframe_pop_aux = daten_pop,L=5,LB=2,x.total_saind = test$idD, B=10,MSE=FALSE)


#summary(Prove$Pov$est_hcr)
#Prove$Pov$est_hcr
#Prove$Pov$est_mean
#Prove$Pov$est_qsr



