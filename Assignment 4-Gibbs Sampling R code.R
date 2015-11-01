#the function for Gibbs sampling
Gibbs.sim <- function(x0,y0,B,Ite){#The inputs are initial values x0, y0, the bound B and the iteration number Ite.
  if (x0<0 || x0>B ||y0<0 || y0>B ) {#Check the validity of initial values
    stop("The initial value is not in the correct range.")
  }
  else {x_result=c()
        y_result=c()
        xt=x0
        yt=y0
        for(t in 1:Ite){
          u1=runif(1)
          u2=runif(1)
          xt=-log(1-u1*(1-exp(-B*yt)))/yt
          yt=-log(1-u2*(1-exp(-B*xt)))/xt #the xt here is the updated why, thus the algorithm constructs a Markov Chain
          x_result=c(x_result, xt)
          y_result=c(y_result, yt)
  }
  return(cbind(x_result,y_result)) #We will do burn in later
  }
}

set.seed(930817)
sample500=Gibbs.sim(1,2,5,700)[-(1:200),] #We burn-in the first 200 samples in order to get 500 samples
sample5000=Gibbs.sim(1,2,5,5200)[-(1:200),] #Also burn-in the first 200 samples 
sample50000=Gibbs.sim(1,2,5,50200)[-(1:200),] #50000 samples after burn-in
par(mfrow=c(1,3))
hist(sample500[,1],xlab="x", main="500 Samples by Gibbs Sampling")
hist(sample5000[,1],xlab="x", main="5000 Samples by Gibbs Sampling")
hist(sample50000[,1],xlab="x", main="50000 Samples by Gibbs Sampling")

mean(sample500[,1])
mean(sample5000[,1])
mean(sample50000[,1])