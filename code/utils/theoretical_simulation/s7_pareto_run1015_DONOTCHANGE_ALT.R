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
simruns=100
Domains=50
sample_size=c(12, 23, 28, 14, 10, 23, 19, 25, 29, 10, 14, 18, 15, 20, 13, 12, 16, 27, 20, 26, 27, 23, 12, 12, 11, 18, 17, 29, 11, 29, 17, 9, 14, 8, 8, 18, 21, 21, 16, 16, 25, 13, 26, 19, 28, 20, 24, 9, 25, 21)
pop_size=rep(200,Domains)
sum(pop_size)
sum(sample_size)

#==================================================================================================================================#
#-----------------------------------  Scenario 7: Pareto Error Term ---------------------------------------------------------------#
#==================================================================================================================================#

gen_XNorm=function(dat,m=dat$muD, s=7){
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
  sim_gen(gen_generic(runif, min = -1, max = 1, groupVars="idD", name = "muD")) %>%
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

#save(Pop, file="./Escenarios/s7_pareto_pimped.RData")

#===========================================================================================#
#-------------------------------  Var         ----------------------------------------------#
#===========================================================================================#

# tester = attr(Pop[[1]],"pop")
# 
# plot(tapply(tester$y, tester$idD, mean), type = "l")


#===========================================================================================#
#-------------------------------  Evaluation  ----------------------------------------------#
#===========================================================================================#

#summary(lm(y~x,data=Pop[[1]]))
#quantplot(y~idD,data=Pop[[1]])

#uuu=1
#test<-attr(Pop[[uuu]],"pop")
#daten_sample<-data.frame(y=Pop[[uuu]]$y,x=Pop[[uuu]]$x)
#daten_pop<-data.frame(x=test$x)


#fit7=FGTpovertyEB_non(formula = y ~ x, dataframe_sample = daten_sample,saind=Pop[[uuu]]$idD,threshold=NULL,
#                      dataframe_pop_aux = daten_pop,L=2,LB=2,x.total_saind = test$idD, B=2,MSE=FALSE)


#(fit7$Pov$est_mean)
#summary(fit7$Pov$est_hcr)
#fit7$Pov$est_mean
#fit7$Pov$est_qsr

#Prove=sims_ELL(dat=Pop[[1]],form=formel,L=2, B=2,EXP=F, LB=2, MSE_ind=T,thold=0.6*median(Pop[[1]]$y),randomseed=5)
#(Prove$Pov$est_mean)
#summary(Prove$Pov$est_hcr)
#Prove$Pov$est_mean
#Prove$Pov$est_qsr

