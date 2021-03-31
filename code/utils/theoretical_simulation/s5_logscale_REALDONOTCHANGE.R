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
simruns <- 500
Domains=50
sample_size=c(12, 23, 28, 14, 10, 23, 19, 25, 29, 10, 14, 18, 15, 20, 13, 12, 16, 
              27, 20, 26, 27, 23, 12, 12, 11, 18, 17, 29, 11, 29, 17, 9, 14, 8, 
              8, 18, 21, 21, 16, 16, 25, 13, 26, 19, 28, 20, 24, 9, 25, 21)
pop_size=rep(200, Domains)
sum(pop_size)
sum(sample_size)

#==============================================================================#
#-----------------------------------  Scenario --------------------------------#
#==============================================================================#

gen_XNorm=function(dat,m = dat$muD, s = .5){
  dat["x"]=rnorm(nrow(dat), mean = m, sd = s)
  return(dat)
}


gen_ZNorm=function(dat, m = dat$muD, s=1){
  dat["z"]=rnorm(nrow(dat), mean = 0, sd = s)
  return(dat)
}


gen_myE=function(dat, m = 0, s = sqrt(.8)){
  dat["e"] = rnorm(nrow(dat), mean = 0, sd = s)
  dat
}

setup=sim_base(data = base_id(nDomains=Domains,nUnits=pop_size)) %>%
  sim_gen(gen_generic(runif, min = 2, max = 3, groupVars="idD", name = "muD")) %>%
  sim_gen(gen_XNorm)       %>%
  sim_gen(gen_ZNorm)       %>% 
  sim_gen(generator=gen_myE)         %>% 
  sim_gen_v(mean = 0, sd = 0.4)         %>% 
  sim_resp_eq(y = exp(10 - x - 0.5 * z + v + e))
Pop=sim(setup,R=simruns)
formel=formula(y~x)


sampler=function(DAT){
  smp=as.data.frame(matrix(nrow=sum(sample_size) , ncol=ncol(DAT)))
  brd=append(0,cumsum(sample_size))
  for(i in 1:Domains){
    smp[((brd[i]+1):brd[i+1]),]=(DAT[DAT$idD==i,])[sample(1:sum(DAT$idD==i),
                                                          size=sample_size[i]),]
  }
  attr(smp,"pop")=DAT
  colnames(smp)=colnames(DAT)
  return(smp)
}


Pop=lapply(Pop,sampler)

save(Pop, file = "../Data/s5_logscale_500.RData")


