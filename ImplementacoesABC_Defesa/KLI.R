  ############### Kullback-Leibler ################

  ## my model is y = (p)1{y==1} + (1-p)Gamma(y>0|mu,phi)

  logit <- function(x){exp(x)/(1+exp(x))}
  
  marginalphi=res$marginals.hyperpar[[1]]
  phi.ger=(inla.rmarginal(1000, marginalphi)) #marginals.hyperpar[1]
  
  margin=function(k,yy,n){
    if(is.na(yy[k,1])==TRUE) finmargin=NA
    if(is.na(yy[k,1])==FALSE){
      if (yy[k,1]==1){
        marginalp=res$marginals.linear.predictor[[k]]
        p.ger= logit(inla.rmarginal(1000, marginalp))  
        finmargin=p.ger
      }
      if (yy[k,1]==0){
        marginalp=res$marginals.linear.predictor[[k]]
        marginalmu=res$marginals.linear.predictor[[n+k]]
        p.ger= logit(inla.rmarginal(1000, marginalp))
        mu.ger=exp(inla.rmarginal(1000, marginalmu)) #marginals.linear.predictor
        valuesmargin= dgamma(yy[n+k,2], shape=phi.ger, rate = phi.ger/(mu.ger), log = FALSE)
        finmargin=(1-p.ger)*valuesmargin
        
      }
    }
    return(finmargin)
  }
  
  DBfunction=function(k,yy,trim){
    n=(dim(yy)[1])/2
    marg=margin(k,yy,n)
    cpof=1/(mean(1/(marg),trim=trim))
    kl1=-log(cpof)
    kl2=mean(log(marg),trim=trim)
    KLF=kl1+kl2
    return(KLF)
  }
  
  
  KLi=NULL
  for (i in 1:n){
    DBi=DBfunction(i,yy,.028)
    KLi[i]=DBi[1]
    DBi=NULL
  }

print(KLi)

