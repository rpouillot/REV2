library(xtable)

dessin <- function(res,cf=1:2,ligne=1){
  ED <- ED50(res,x<-seq(0,1,length=100),cf)
  RT50 <- ED50(res,p=.5,cf)
  lines(ED[,1],x,lty=ligne)
  lines(ED[,2],x,lty=ligne)
  lines(ED[,3],x,lty=ligne)
  segments(RT50[1:3],0,RT50[1:3],.5,lty=ligne)
  segments(0,.5,RT50[3],.5,lty=ligne)
}

ED50 <- function(res,p=0.5,cf=1:2){
  r <- qnorm(p)
  a <- coef(res)[cf[1]]
  b <- coef(res)[cf[2]]
  V <- vcov(res)[cf,cf]
  trt <- (r-a)/b
  tinf <- (-a*b+b*r+z*(V[1,2]*z-sqrt(b^2*V[1,1]-2*b*(a-r)*V[1,2]+(a-r)^2*V[2,2]+(V[1,2]^2-V[1,1]*V[2,2])*z^2)))/(b^2-V[2,2]*z^2)
  tsup <- (-a*b+b*r+z*(V[1,2]*z+sqrt(b^2*V[1,1]-2*b*(a-r)*V[1,2]+(a-r)^2*V[2,2]+(V[1,2]^2-V[1,1]*V[2,2])*z^2)))/(b^2-V[2,2]*z^2)
  ED <- as.matrix(cbind(trt,tinf,tsup))
  dimnames(ED) <- list(paste('RT',p*100),c('Estimate','CIinf','CIsup')) 
  return(ED)
}

vcov <- function(res){
  so <- summary(res)
  vcov <- so$dispersion * so$cov.unscaled
  vcov
}

z <- qnorm(0.975)

shinyServer(function(input, output,session) {
 
  func <- reactive({
    if(input$soume == 0) return(NULL)
  
    isolate({
       donnee <- NULL
      for(i in 1:16){
        donnee = c(donnee, input[[paste("n",i,sep="")]])
      }
  
    qdet <- input$ouinon
    lesres <- NULL
    
    options(warn=-1)
    nom <- make.names(c(input$nom1,input$nom2))
    entre <- donnee
    detail <- ifelse(as.numeric(qdet)==0,FALSE,TRUE)
  
      strain <- factor(rep(nom,c(4,4)))
      base <- matrix(entre,ncol=2,byrow=TRUE)
      base <- rbind(base[seq(1,7,2),],base[seq(2,8,2),])
      out <- '<p><FONT color=#000080 size=+2>Results</font></p>'
      base <- cbind(strain,rep(c(3,6,9,12),2),base)
    dimnames(base) <- list(rep(nom,c(4,4)),c('Strain','Time','Treated','Cured'))
#    print(base)
#  out <- c(out,print(xtable(base),type="html"))
    Time <- base[,2]
    Treated <- base[,3]
    Cured <- base[,4]
    SF <- cbind(Cured,Treated-Cured)
    #FONCTIONS INTERNES
    #1 vaccin
    for (i in 1:2){
#    cat('\\nINDEPENDENT ANALYSIS for',nom[i],'\\n')
  out <- c(out,'<p><FONT color=#000080 size=+1>Independant Analysis for',nom[i],'</font></p>')
    quel <- (4*(i-1)+1):(4*(i-1)+4)
    SF1 <- SF[quel,]
    Time1 <- Time[quel]
    Treated1 <- Treated[quel]
    Cured1 <- Cured[quel]
    Infected1 <- Treated1-Cured1
    SF1 <- cbind(Cured1,Infected1)
    res <- glm(SF1 ~ Time1, family=binomial(link = 'probit'))
    SE <- sqrt(diag(V <- vcov(res)))
    coef <- rbind(res$coefficients,SE)
    dimnames(coef) <- list(c('Estimates','Standard Error'),c('a','b'))
    dimnames(V) <- list(c('a','b'),c('a','b'))
    Pi <- res$fitted.values
    prob <- round(rbind(Cured1/Treated1,Pi),4)
    dimnames(prob) <- list(c('Observed','Expected'),paste('Time',1:4))
    chi <- c(res$deviance,sum((Cured1-Treated1*Pi)**2 / (Treated*Pi*(1-Pi))))
    df <- c(res$df.residual,length(Time1)-2)
    theor <-  qchisq(.95,df)
    p <- 1-pchisq(chi,df)
    reschi <- round(cbind(chi,df,theor,p),4)
    dimnames(reschi) <- list(c('Deviance','Pearson'),c('Value','Df','Theor','p'))
    if (all(p >= 0.05)) com <- 'The model could fit' else com <- 'at least one Goodness-of-fit test is significant : the model may not be valid' 
    if (detail){
#      cat('\\nMODEL : Proba(Cured) = Probit(a + b Time)\\n')
  out <- c(out,'<p>Model : Proba(Cured) = Probit(<i>a</i> + <i>b</i> Time)</p>')
#      print(coef)
  out <- c(out,print(xtable(coef),type='html',file='tmp'),'<p></p>')
#    cat('\\nVariance - Covariance Matrix\\n')
  out <- c(out,'<p>Variance - Covariance Matrix</p>')
#    print(V)
  out <- c(out,print(xtable(V),type='html',file='tmp'),'<p></p>')
#    cat('\\nGoodness-of-fit tests\\n')
  out <- c(out,'<p>Goodness-of-fit tests</p>')
#    print(reschi)
  out <- c(out,print(xtable(reschi),type='html',file='tmp'),'<p></p>')
    }
#    cat('Validity of the model:',com,'\\n')
  out <- c(out,'<p>Validity of the model:<b>',com,'</b></p>')
    RT50 <- ED50(res,p=.5,cf=1:2)
#    print(RT50)
  out <- c(out,print(xtable(RT50),type='html',file='tmp'),'<p></p>')
#    plot(c(3,12),c(0,1),type='n',main=paste('Model',nom[i]),xlab='Time',ylab='prob')
#    dessin(res)
#    points(Time1,Cured1/Treated1)
    lesres[[i]] <- list(res=res,x=Time1,y=Cured1/Treated1)
    }
#    cat('\\nGLOBAL ANALYSIS\\n')
  out <- c(out,'<p><FONT color=#000080 size=+1>Global Analysis</font></p>')
  #Comparaison models 1,2,3,4,5
    assumpt <- glm(SF ~ Time * strain , family=binomial(link = 'probit'))
    resano <- anova(assumpt,test='Chisq')
    res2 <- glm(SF ~ strain + Time + factor(Time), family=binomial(link = 'probit'))
    resano2 <- anova(res2,test='Chisq')
    mod <- c(df <- resano['Time:strain','Resid. Df'],rd <- resano['Time:strain','Resid. Dev'],0,0,1-pchisq(rd,df))
    resano <- rbind(resano,resano2['factor(Time)',],mod)
    test <- Validity <- ((resano[,'Pr(>Chi)'] > 0.05) == c(NA,FALSE,TRUE,TRUE,TRUE,TRUE))
    Validity[test] <- 'OK'
    Validity[!test] <- 'The model is not valid'
    if (test[3]==F) Validity[3] <- 'The strains are not equal'
    if (all(Validity[-c(1,3)]=='OK')==TRUE) com <- 'The model could be valid' else com <- 'The model is not valid'
    resano <- cbind(resano,Validity) 
    row.names(resano) <- c('Model 1','Model 2','Model 3','Model 4','Model 4 prim','Model 5')
    resano[,c(2,4,5)] <- round(resano[,c(2,4,5)],3)
    #Evaluation relative potenty
    res <- glm(SF ~ strain + Time , family=binomial(link = 'probit'))
    b1 <- res$coefficients[2]
    b2 <- res$coefficients[3]
    V <- vcov(res)[2:3,2:3]
    rho <- b1/b2
    rhoinf <- (b1*b2-z*(V[1,2]*z+sqrt(b2^2*V[1,1]-2*b1*b2*V[1,2]+b1^2*V[2,2]+(V[1,2]^2-V[1,1]*V[2,2])*z^2)))/(b2^2-V[2,2]*z^2)
    rhosup <- (b1*b2+z*(-V[1,2]*z+sqrt(b2^2*V[1,1]-2*b1*b2*V[1,2]+b1^2*V[2,2]+(V[1,2]^2-V[1,1]*V[2,2])*z^2)))/(b2^2-V[2,2]*z^2)
    rho <- cbind(rho,rhoinf,rhosup)
    dimnames(rho) <- list('Relative potency',c('Estimate','Fid. limit inf','Fid. limit sup'))
    if (detail){
#    print(resano)
  out <- c(out,print(xtable(resano),type='html',file='tmp'),'<p></p>')  
    }
#    cat('\\nValidity of the model:',com,'\\n')
  out <- c(out,'<p>Validity of the model:',com,'</p>')
    if (com!='The model is not valid'){
      res <- glm(SF ~ strain + Time -1, family=binomial(link = 'probit'))
#      cat('\\nRT50 for',nom[1],'\\n')
  out <- c(out,'<p>RT50 for',nom[1],'</p>')
#    print(ED50(res,p=0.5,cf=c(1,3)))
  out <- c(out,print(xtable(ED50(res,p=0.5,cf=c(1,3))),type='html',file='tmp'),'<p></p>')
#    cat('\\nRT50 for',nom[2],'\\n')
  out <- c(out,'<p>RT50 for',nom[2],'</p>')
#    print(ED50(res,p=0.5,cf=c(2,3)))
  out <- c(out,print(xtable(ED50(res,p=0.5,cf=c(2,3))),type='html',file='tmp'),'<p></p>')
#    cat('\\nRelative potency for',nom[2],'\\n')
  out <- c(out,'<p>Relative potency for',nom[2],'</p>')
#  print(rho)
  out <- c(out,print(xtable(rho),type='html',file='tmp'),'<p></p>')
#    cat('\\nCOMPARISON OF STRAINS')
#    cat('\\nDeviance Test Results: ')
  out <- c(out,'<p>COMPARISON OF STRAINS</p><p>Deviance Test Results: ')
  if(test[3]==T) tmp <- '<b>Strains are not statistically different</b></p>' else tmp <- '<b>Strains are statistically different</b></p>'
#    cat(tmp)
#    cat('\\nRelative potency test: ')
  out <- c(out,tmp,'<p>Relative potency test: ')
    if(is.na(rhoinf) || is.na(rhosup)) tmp <- '<b>test non available</b></p>' else if(rhoinf>0 || rhosup<0) tmp <- '<b>Strains are statistically different</b></p>'  else tmp <- '<b>Strains are not statistically different</b></p>'
#    cat(tmp,"\\n")
  out <- c(out,tmp)
    }
  lesres[[3]] <- list(res=res,x=Time,y=Cured/Treated)
#  plot(c(3,12),c(0,1),type='n',main=paste('Global Model'),xlab='Time',ylab='prob')
#  dessin(res,cf=c(1,3),ligne=1)
#  dessin(res,cf=c(2,3),ligne=2)
#  points(Time,Cured/Treated,pch=19*(strain ==nom[1]))
#  cat('\\nREV-2 results, R.Pouillot, AFSSA, France, r.pouillot@afssa.fr\\n')
    return(list(out=paste(out),lesres=lesres))
  }) #end isolate
  }) #end reactive

output$Results <- renderText({
  if(is.null(func())) return(NULL)
     func()$out
  })

output$Graph1 <- renderPlot({
  if(is.null(func())) return(NULL)
  plot(c(3,12),c(0,1),type='n',main=paste('Model',input$nom1),xlab='Time',ylab='prob')
  dessin(func()$lesres[[1]]$res)
  points(func()$lesres[[1]]$x,func()$lesres[[1]]$y)
})

output$Graph2 <- renderPlot({
  if(is.null(func())) return(NULL)
  plot(c(3,12),c(0,1),type='n',main=paste('Model',input$nom2),xlab='Time',ylab='prob')
  dessin(func()$lesres[[2]]$res)
  points(func()$lesres[[2]]$x,func()$lesres[[2]]$y)
})

output$Graph3 <- renderPlot({
  if(is.null(func())) return(NULL)
  plot(c(3,12),c(0,1),type='n',main=paste('Global Model'),xlab='Time',ylab='prob')
  dessin(func()$lesres[[3]]$res,cf=c(1,3),ligne=1)
  dessin(func()$lesres[[3]]$res,cf=c(2,3),ligne=2)
  points(func()$lesres[[2]]$x,func()$lesres[[2]]$y,pch=c(rep(18,4),rep(19,4)))
})

}) # End shinyServer