horseshoe.wrapper=function(input,args=NULL){
  tau_hat = horseshoe::HS.MMLE(input$betahat/input$sebetahat, Sigma2=1)

  res = do.call(horseshoe::HS.post.mean,
                args= c(list(y=input$betahat/input$sebetahat,tau = tau_hat, Sigma2=1)))

  return(list(res=res, input=input))
}

horseshoe2beta_est = function(output){
  if(class(output)=="try-error"){return(list(beta_est = NA))} else {
  return (list(beta_est=output$res*output$input$sebetahat)) }
}

