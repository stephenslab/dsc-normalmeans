#' @description Simulates data for the normal means problem
#' with true effects (beta) from the mixture model pi0 delta_0 + (1-pi0)g, where g is a normal mixture, and betahat \sim N(beta,sd)
#'
#' @param args A list of the remaining arguments, which in this case is
#' \item{nsamp}{The number of samples to create}
#' \item{scenario}{a string indicating the name of the scenario (effectively specifies g)}
#' \item{min_pi0}{The minimum value of pi0, the proportion of true nulls}
#' \item{max_pi0}{The maximum value of pi0, the proportion of true null}
#' \item{sd}{The standard deviation of betahat to use}
rnormmix = function(scenario=c("spiky","skew","bignormal","bimodal","flat_top","near_normal","vbignormal"),min_pi0,max_pi0,nsamp,sd){
  gdef = list(spiky=normalmix(c(.4,.2,.2,.2),c(0,0,0,0),c(.25,.5,1,2)),
            skew=normalmix(c(1/4,1/4,1/3,1/6),c(-2,-1,0,1),c(2,1.5,1,1)),
            bignormal=normalmix(c(1),c(0),c(4)),
            bimodal=normalmix(c(0.5,0.5),c(-2,2),c(1,1)),
            flat_top=normalmix(rep(1/7,7),c(-1.5,-1,-0.5,0,0.5,1,1.5),rep(0.5,7)),
            near_normal=normalmix(c(2/3,1/3),c(0,0),c(1,2)),
            vbignormal = normalmix(c(1),c(0),c(10))
            )
  g = gdef[[scenario]]

  pi0 = runif(1,min_pi0,max_pi0) #generate the proportion of true nulls randomly

  k = ncomp(g)
  comp = sample(1:k,nsamp,mixprop(g),replace=TRUE) #randomly draw a component
  isnull = (runif(nsamp,0,1) < pi0)
  beta = ifelse(isnull, 0,rnorm(nsamp,comp_mean(g)[comp],comp_sd(g)[comp]))
  sebetahat = sd
  betahat = beta + rnorm(nsamp,0,sebetahat)
  return(list(betahat, beta, pi0))
}

# example: rnormmix("spiky",0,1,100,1)

