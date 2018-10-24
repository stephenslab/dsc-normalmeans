ebnm.wrapper=function(input,args=NULL){
  args = list(x=input$betahat, s=input$sebetahat)
  res = do.call(ebnm::ebnm,
                args= args)

  return(res)
}
