#===========================================================================================#
#------------------------------------  LIBRARY  --------------------------------------------#
#===========================================================================================#

library(saeSim)
library(actuar)

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
#-----------------------------------  Scenario 7: Pareto Error Term ---------------------------------------------------------------#
#==================================================================================================================================#

gen_XNorm=function(dat,m=dat$muD, s=7.5){
  dat["x"]=rnorm(nrow(dat), mean = m, sd = s)
  return(dat)
}

gen_myE=function(dat,shape=3, scale=2000){
  tmp =  sqrt(2) * rpareto(nrow(dat), shape = shape, scale = scale)
  dat["e"]= tmp - mean(tmp)
  return(dat)
}

setup=
  sim_base(data = base_id(nDomains=Domains,nUnits=pop_size)) %>%
  sim_gen(gen_generic(runif, min = -3, max = 3, groupVars="idD", name = "muD")) %>%
  sim_gen(gen_XNorm)       %>%
  as.data.frame %>%
  sim_gen(generator=gen_myE)         %>% 
  sim_gen_v(mean=0,sd = 500)         %>% 
  sim_resp_eq(y = 12000 - 400 * x + v + e)  
Pop=sim(setup,R=simruns)
formel=formula(y~x)


trunc = function(DAT, th)
{
  fil = DAT$y < th 
  DAT$y[fil] = th  
  return(DAT)
}
lapply(Pop,function(X){sum(X$y <= 0)})
Pop=lapply(Pop,trunc,th=0)

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

Pop = lapply(Pop,sampler)

save(Pop, file="../Data/s7_pareto_500.RData")
