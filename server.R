
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plotly)
library(markdown)

ref=read.csv("ref.csv")

shinyServer(function(input, output,session) {
    
    
    size.t.test=function(meandiff,sd=NULL,sdA=NULL,sdB=NULL,alpha,beta,delta,kappa,mode=1){
        if(mode==1){
            nB=(1+1/kappa)*(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(meandiff))^2
        } else if(mode==2){
            nA=(sdA^2+sdB^2/kappa)*((qnorm(1-alpha)+qnorm(1-beta))/(meandiff))^2
            nB=nA/kappa
        } else if(mode==3){
            nB=(1+1/kappa)*(sd*(qnorm(1-alpha)+qnorm(1-beta))/(meandiff-delta))^2
        } else {
            nB=(1+1/kappa)*(sd*(qnorm(1-alpha)+qnorm(1-beta/2))/(abs(meandiff)-delta))^2
        }
    }
    
    size.anova.test=function(meandiff,sd=NULL,sdA=NULL,sdB=NULL,alpha,beta,tau,kappa,mode=1){
        if(mode==1){
            n=2*(sd*(qnorm(1-alpha/(2/tau))+qnorm(1-beta))/(meandiff))^2
        } else{
            n=(sdA^2+sdB^2/kappa)*((qnorm(1-alpha/tau)+qnorm(1-beta))/(meandiff))^2
        }
        n
    }
    
    
    size.t1.prop=function(p,p0,delta,alpha=alpha,beta,mode){
        if(mode==1){
            n=p*(1-p)*((qnorm(1-alpha/2)+qnorm(1-beta))/(p-p0))^2
        } else if(mode==2){
            n=p0*(1-p0)*((qnorm(1-alpha)+qnorm(1-beta)*sqrt(p*(1-p)/p0/(1-p0)))/(p-p0))^2
        } else if(mode==3){
            n=p*(1-p)*((qnorm(1-alpha)+qnorm(1-beta))/(p-p0-delta))^2
        } else {
            n=p*(1-p)*((qnorm(1-alpha)+qnorm(1-beta/2))/(abs(p-p0)-delta))^2
        }
        n
    }
    
    size.compare.2prop=function(pA,pB,delta,kappa,alpha,beta,mode){
        if(mode==1){
            nB=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/2)+qnorm(1-beta))/(pA-pB))^2
        } else if(mode==2){
            nB=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha)+qnorm(1-beta))/(pA-pB))^2
        } else if(mode==3){
            nB=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha)+qnorm(1-beta))/(pA-pB-delta))^2
        } else{
            nB=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha)+qnorm(1-beta/2))/(abs(pA-pB)-delta))^2
        }
        nB
    }
    
    
    size.compare.kp=function(pA,pB,tau,alpha,beta,mode){
        n=(pA*(1-pA)+pB*(1-pB))*((qnorm(1-alpha/2/tau)+qnorm(1-beta))/(pA-pB))^2
        n
    }
    
    size.cox.ph=function(hr,hr0,pE,pA,alpha,beta,delta,mode){
        if(mode==1){
           n=((qnorm(1-alpha/2)+qnorm(1-beta))/(log(hr)-log(hr0)))^2/(pA*(1-pA)*pE)
        } else if(mode==2){
            n=((qnorm(1-alpha)+qnorm(1-beta))/(log(hr)-log(hr0)))^2/(pA*(1-pA)*pE)
        } else{
            n=((qnorm(1-alpha)+qnorm(1-beta/2))/(delta-abs(log(hr))))^2/(pA*(1-pA)*pE)
        }
        n
    }
    
    size.odds.ratio=function(pA,pB,alpha,beta,delta,kappa,mode){
        OR=pA*(1-pB)/pB/(1-pA) 
        if(mode==1){
            nB=(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha/2)+qnorm(1-beta))/log(OR))^2
        } else if(mode==2){
            nB=(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha)+qnorm(1-beta))/(log(OR)-delta))^2
        } else {
            nB=(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha)+qnorm(1-beta/2))/(abs(log(OR))-delta))^2
        }
        nB
    }
    
    size.oddsratio=reactive({
        pA<-input$pA9
        pB<-input$pB9
        alpha<-input$alpha9
        beta<-1-input$power9
        delta<-input$delta9
        kappa<-input$k9
        mode<-as.numeric(input$oddratio)
        
        n<-size.odds.ratio(pA,pB,alpha,beta,delta,kappa,mode)
        n
        
    })
    
    size.coxph=reactive({
        hr<-input$hr
        hr0<-input$hr0
        pE<-input$pE
        pA<-input$pA8
        alpha<-input$alpha8
        beta<-1-input$power8
        delta<-input$delta8
        mode<-as.numeric(input$coxph)
        
        n=size.cox.ph(hr,hr0,pE,pA,alpha,beta,delta,mode)
        n
        
        
    })
    
    size.ckp=reactive({
        pA=input$pA7
        pB=input$pB7
        tau<-input$tau7
        alpha<-input$alpha7
        beta<- 1-input$power7
        
        n=size.compare.kp(pA,pB,tau,alpha,beta,mode)
        n
    })
    
    size.c2p=reactive({
        pA=input$pA
        pB=input$pB
        delta=input$delta5
        kappa=input$k5
        alpha=input$alpha5
        beta=1-input$power5
        mode=as.numeric(input$c2p)
        
        nB=size.compare.2prop(pA,pB,delta,kappa,alpha,beta,mode)
        nB
    })
    
    size.compare.pp=function(proportions,alpha,beta,mode){
        proportions=proportions/sum(proportions)
        p01=proportions[3]
        p10=proportions[2]
        pdisc=p10+p01
        pdiff=p10-p01
        
        if(mode==1){
            n=((qnorm(1-alpha/2)*sqrt(pdisc)+qnorm(1-beta)*sqrt(pdisc-pdiff^2))/pdiff)^2
        } else{
            n=((qnorm(1-alpha)*sqrt(pdisc)+qnorm(1-beta)*sqrt(pdisc-pdiff^2))/pdiff)^2
        }
        n
    }
    
    size.cpp=reactive({
        n11<-input$n11
        n10<-input$n10
        n01<-input$n01
        n00<-input$n00
        
        proportions=c(n11,n10,n01,n00)
       
        alpha<-input$alpha6
        beta<-1-input$power6
        mode<-as.numeric(input$cpp)
        n=size.compare.pp(proportions,alpha,beta,mode=mode)
        n
        
    })
    
    size.t1p=reactive({
        
        p=input$prop
        p0=input$prop0
        delta=input$delta4
        alpha=input$alpha4
        beta=1-input$power4
        mode=as.numeric(input$t1p)
        
        n=size.t1.prop(p=p,p0=p0,delta=delta,alpha=alpha,beta=beta,mode=mode)
        n
    })
    
    size.anova=reactive({
        
        muA=input$mean31
        muB=input$mean32
        sd=input$sd3
        sdA=input$sd31
        sdB=input$sd32
        tau=input$tau
        alpha=input$alpha3
        beta=1-input$power3
        power=input$power3
        kappa=input$kappa3
        mode=as.numeric(input$anova)
        n=size.anova.test(meandiff=muA-muB,sd=sd,sdA=sdA,sdB=sdB,
                       alpha=alpha,beta=beta,tau=tau,kappa=kappa,mode=mode)
        n
    })
    
    size.ttest=reactive({
        
        muA=input$mean1
        muB=input$mean2
        sd=input$sd
        sdA=input$sd1
        sdB=input$sd2
        kappa=input$k
        alpha=input$alpha
        beta=1-input$power
        power=input$power
        delta=input$margin
        mode=as.numeric(input$ttest)
        nB=size.t.test(meandiff=muA-muB,sd=sd,sdA=sdA,sdB=sdB,
                       alpha=alpha,beta=beta,delta=delta,kappa=kappa,mode=mode)
        nB
    })
    
    
    size.t1.mean=function(meandiff,sd,alpha,beta,delta,mode){
        if(mode==1){
          n=(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(meandiff))^2
        } else if(mode==2){
            n=(sd*(qnorm(1-alpha)+qnorm(1-beta))/(meandiff))^2
        } else if(mode==3){
            n=(sd*(qnorm(1-alpha)+qnorm(1-beta))/(meandiff-delta))^2
        } else{
            n=(sd*(qnorm(1-alpha)+qnorm(1-beta/2))/(delta-abs(meandiff)))^2
        }
        n
    }
    size.t1mean=reactive({
        
        mu=input$mu
        mu0=input$mu0
        sd=input$sd0
        alpha=input$alpha1
        beta=1-input$power1
        power=input$power1
        delta=input$delta
        mode=as.numeric(input$t1mean)
        n=size.t1.mean(meandiff=mu-mu0,sd=sd,
                       alpha=alpha,beta=beta,delta=delta,mode=mode)
        n
    })
    
    output$sampleSize1=renderValueBox({
        
        n<-size.t1mean()
        valueBox(
            round(n,2),"Sample Size",icon=icon("user"),color="yellow"
        )
    })
    
    output$sampleSize12=renderValueBox({
        
        n<-size.t1mean()
        n1<-ceiling(n/(1-input$FUloss1)/input$compliance1)
        valueBox(
            n1,"Sample Size considering FU loss and compliance",
            icon=icon("users"),color="purple"
        )
    })

    
   output$sampleSize=renderValueBox({
        
        nB<-size.ttest()
        valueBox(
            round(nB,2),"Sample Size, nB",icon=icon("user-plus"),color="yellow"
        )
    })
    
    output$sampleSize2=renderValueBox({
        nB<-size.ttest()
        nA<-input$k*nB
        valueBox(
            round(nA,2),"Sample Size, nA",icon=icon("user"),color="yellow"
        )
    })
    
    output$sampleSize3=renderValueBox({
        nB<-size.ttest()
        nA<-input$k*nB
        temp=paste0(ceiling(nB/(1-input$FUloss)/input$compliance),"/",
                    ceiling(nA/(1-input$FUloss)/input$compliance))
        valueBox(
            temp,"nA/nB considering FU loss and compliance",icon=icon("users"),color="purple"
        )
    })
    
    output$sampleSize31=renderValueBox({
        nA<-size.anova()
        temp=ceiling(nA)
        temp2="Sample size"
        if(as.numeric(input$anova)==2){
            nB<-nA/input$k
            temp=paste0(ceiling(nA),"/",ceiling(nB))
            temp2=("nA/nB")
        } 
        #temp=paste0(ceiling(nB/(1-input$FUloss)/input$compliance),"/",
        #            ceiling(nA/(1-input$FUloss)/input$compliance))
        valueBox(
            temp,temp2,icon=icon("user"),color="yellow"
        )
    })
    
    output$sampleSize32=renderValueBox({
        nA<-size.anova()
        temp=ceiling(nA/(1-input$FUloss3)/input$compliance3)
        temp2="Sample size"
        if(as.numeric(input$anova)==2){
            nB<-nA/input$k
            temp=paste0(ceiling(nA/(1-input$FUloss3)/input$compliance3),"/",
                    ceiling(nB/(1-input$FUloss3)/input$compliance3))
            temp2="nA/nB"
        }
        valueBox(
            temp,paste(temp2,"considering FU loss and compliance"),icon=icon("users"),color="purple"
        )
    })
    
    output$sampleSize41=renderValueBox({
        n<-size.t1p()
        n<-ceiling(n)
        valueBox(
            n,paste("Sample size, n"),icon=icon("user"),color="yellow"
        )
    })
    
    output$sampleSize42=renderValueBox({
        n<-size.t1p()
        n=ceiling(n/(1-input$FUloss4)/input$compliance4)
        valueBox(
            n,"Sample size considering FU loss and compliance",icon=icon("users"),color="purple"
        )
    })
    
    output$sampleSize51=renderValueBox({
        nB<-size.c2p()
        nA=ceiling(nB*input$k5)
        valueBox(
            nA,paste("Sample size, nA"),icon=icon("user"),color="yellow"
        )
    })
    
    output$sampleSize52=renderValueBox({
        nB<-size.c2p()
        nB<-ceiling(nB)
        valueBox(
            nB,paste("Sample size, nB"),icon=icon("user-plus"),color="yellow"
        )
    })
    
    output$sampleSize53=renderValueBox({
        nB<-size.c2p()
        nA=nB*input$k5
        nB=ceiling(nB/(1-input$FUloss5)/input$compliance5)
        nA=ceiling(nA/(1-input$FUloss5)/input$compliance5)
        valueBox(
            paste0(nA,"/",nB),"nA/nB considering FU loss and compliance",
            icon=icon("users"),color="purple"
        )
    })
    
    output$sampleSize61=renderValueBox({
        n<-size.cpp()
        n=ceiling(n)
        valueBox(
            n,paste("Sample size, n"),icon=icon("user"),color="yellow"
        )
    })
    
    
    output$sampleSize63=renderValueBox({
        n<-size.cpp()
        n=ceiling(n/(1-input$FUloss6)/input$compliance6)
        
        valueBox(
            n,"Sample size considering FU loss and compliance",
            icon=icon("users"),color="purple"
        )
    })
    
    output$sampleSize71=renderValueBox({
        n<-size.ckp()
        n=ceiling(n)
        valueBox(
            n,paste("Sample size, n"),icon=icon("user"),color="yellow"
        )
    })
    
    
    output$sampleSize73=renderValueBox({
        n<-size.ckp()
        n=ceiling(n/(1-input$FUloss7)/input$compliance7)
        
        valueBox(
            n,"Sample size considering FU loss and compliance",
            icon=icon("users"),color="purple"
        )
    })
    
    output$sampleSize81=renderValueBox({
        n<-size.coxph()
        n=ceiling(n)
        valueBox(
            n,paste("Sample size, n"),icon=icon("user"),color="yellow"
        )
    })
    
    
    output$sampleSize83=renderValueBox({
        n<-size.coxph()
        n=ceiling(n/(1-input$FUloss8)/input$compliance8)
        
        valueBox(
            n,"Sample size considering FU loss and compliance",icon=icon("users"),color="purple"
        )
    })
    
    output$sampleSize91=renderValueBox({
        nB<-size.oddsratio()
        nA=ceiling(nB*input$k9)
        valueBox(
            nA,paste("Sample size, nA"),icon=icon("user"),color="yellow"
        )
    })
    
    output$sampleSize92=renderValueBox({
        nB<-size.oddsratio()
        nB<-ceiling(nB)
        valueBox(
            nB,paste("Sample size, nB"),icon=icon("user-plus"),color="yellow"
        )
    })
    
    output$sampleSize93=renderValueBox({
        nB<-size.oddsratio()
        nA=nB*input$k9
        nB=ceiling(nB/(1-input$FUloss9)/input$compliance9)
        nA=ceiling(nA/(1-input$FUloss9)/input$compliance9)
        valueBox(
            paste0(nA,"/",nB),"nA/nB considering FU loss and compliance",
            icon=icon("users"),color="purple"
        )
    })
    output$plot1=renderPlotly({
        
        muA=input$mean1
        muB=input$mean2
        sd=input$sd
        sdA=input$sd1
        sdB=input$sd2
        kappa=input$k
        alpha=input$alpha
        #power=input$power
        delta=input$margin
        mode=as.numeric(input$ttest)
        
        power=seq(0.5,0.95,by=0.01)
        beta=1-power
        
        nB=size.t.test(meandiff=muA-muB,sd=sd,sdA=sdA,sdB=sdB,
                       alpha=alpha,beta=beta,delta=delta,kappa=kappa,mode=mode)
        
        df=data.frame(power=power,n=nB)
       
        ggplotly(ggplot(df,aes(power,n))+geom_line()+labs(x="power",y="Sample Size"))
        
            
    })
    
    output$plot2=renderPlotly({
        
        mu=input$mu
        mu0=input$mu0
        sd=input$sd0
        alpha=input$alpha1
        # beta=1-input$power1
        # power=input$power1
        delta=input$delta
        mode=as.numeric(input$t1mean)
       
        power=seq(0.5,0.95,by=0.01)
        beta=1-power
        
        
         nB=size.t1.mean(meandiff=mu-mu0,sd=sd,
                       alpha=alpha,beta=beta,delta=delta,mode=mode)
        nB
        
        df=data.frame(power=power,n=nB)
        
        ggplotly(ggplot(df,aes(power,nB))+geom_line()+labs(x="power",y="Sample Size"))
        
        
    })
    
    output$plot3=renderPlotly({
        
        muA=input$mean31
        muB=input$mean32
        sd=input$sd3
        sdA=input$sd31
        sdB=input$sd32
        tau=input$tau
        alpha=input$alpha3
        # beta=1-input$power3
        # power=input$power3
        kappa=input$kappa3
        mode=as.numeric(input$anova)
        
        power=seq(0.5,0.95,by=0.01)
        beta=1-power
        
        n=size.anova.test(meandiff=muA-muB,sd=sd,sdA=sdA,sdB=sdB,
                          alpha=alpha,beta=beta,tau=tau,kappa=kappa,mode=mode)
        n
        
        df=data.frame(power=power,n=n)
        
        ggplotly(ggplot(df,aes(power,n))+geom_line()+labs(x="power",y="Sample Size"))
        
        
    })
    
    output$plot4=renderPlotly({
        
        p=input$prop
        p0=input$prop0
        delta=input$delta4
        alpha=input$alpha4
        #beta=1-input$power4
        mode=as.numeric(input$t1p)
        
        power=seq(0.5,0.95,by=0.01)
        beta=1-power
        
        n=size.t1.prop(p=p,p0=p0,delta=delta,alpha=alpha,beta=beta,mode=mode)
        n
        
        df=data.frame(power=power,n=n)
        
        ggplotly(ggplot(df,aes(power,n))+geom_line()+labs(x="power",y="Sample Size"))
        
        
    })
    
    output$plot5=renderPlotly({
        
        pA=input$pA
        pB=input$pB
        delta=input$delta5
        kappa=input$k5
        alpha=input$alpha5
        #beta=1-input$power5
        mode=as.numeric(input$c2p)
        
        power=seq(0.5,0.95,by=0.01)
        beta=1-power
        
        nB=size.compare.2prop(pA,pB,delta,kappa,alpha,beta,mode)
        
        
        df=data.frame(power=power,nB=nB)
        
        ggplotly(ggplot(df,aes(power,nB))+geom_line()+labs(x="power",y="Sample Size"))
        
        
    })
    
    output$plot6=renderPlotly({
        
        n11<-input$n11
        n10<-input$n10
        n01<-input$n01
        n00<-input$n00
        
        proportions=c(n11,n10,n01,n00)
        
        alpha<-input$alpha6
        #beta<-1-input$power6
        mode<-as.numeric(input$cpp)
         
        power=seq(0.5,0.95,by=0.01)
        beta=1-power
        
        n=size.compare.pp(proportions,alpha,beta,mode=mode)
        
        df=data.frame(power=power,n=n)
        
        ggplotly(ggplot(df,aes(power,n))+geom_line()+labs(x="power",y="Sample Size"))
        
        
    })
    
    output$plot7=renderPlotly({
        
        pA=input$pA7
        pB=input$pB7
        tau<-input$tau7
        alpha<-input$alpha7
        #beta<- 1-input$power7
        
        power=seq(0.5,0.95,by=0.01)
        beta=1-power
        
        n=size.compare.kp(pA,pB,tau,alpha,beta,mode)
        
        df=data.frame(power=power,n=n)
        
        ggplotly(ggplot(df,aes(power,n))+geom_line()+labs(x="power",y="Sample Size"))
        
        
    })
    output$plot8=renderPlotly({
        
        hr<-input$hr
        hr0<-input$hr0
        pE<-input$pE
        pA<-input$pA8
        alpha<-input$alpha8
        #beta<-1-input$power8
        delta<-input$delta8
        mode<-as.numeric(input$coxph)
        
        
        power=seq(0.5,0.95,by=0.01)
        beta=1-power
        
        n=size.cox.ph(hr,hr0,pE,pA,alpha,beta,delta,mode)
        
        df=data.frame(power=power,n=n)
        
        ggplotly(ggplot(df,aes(power,n))+geom_line()+labs(x="power",y="Sample Size"))
        
        
    })
    
    output$plot9=renderPlotly({
        
        pA<-input$pA9
        pB<-input$pB9
        alpha<-input$alpha9
        #beta<-1-input$power9
        delta<-input$delta9
        kappa<-input$k9
        mode<-as.numeric(input$oddratio)
        
       
        
        power=seq(0.5,0.95,by=0.01)
        beta=1-power
        
        nB<-size.odds.ratio(pA,pB,alpha,beta,delta,kappa,mode)
        
        df=data.frame(power=power,n=nB)
        
        ggplotly(ggplot(df,aes(power,nB))+geom_line()+labs(x="power",y="Sample Size"))
        
        
    })
    
    output$equation1=renderPrint({
        filename=paste0(input$t1mean,".png")
        img(src=filename)
        
    })
    
    output$ref1=renderPrint({
        mode=as.numeric(input$t1mean)
        temp=ref[ref$no==mode,]$ref
        p(temp)
    })
    output$equation2=renderPrint({
        filename=paste0("2",input$ttest,".png")
        img(src=filename)
        
    })
    
    output$ref2=renderPrint({
        mode=as.numeric(input$ttest)+20
        temp=ref[ref$no==mode,]$ref
        p(temp)
    })
    output$equation3=renderPrint({
        filename=paste0("3",input$anova,".png")
        img(src=filename)
        
    })
    
    output$ref3=renderPrint({
        mode=as.numeric(input$anova)+30
        temp=ref[ref$no==mode,]$ref
        p(temp)
    })
    
    output$equation4=renderPrint({
        filename=paste0("4",input$t1p,".png")
        img(src=filename)
        
    })
    
    output$ref4=renderPrint({
        mode=as.numeric(input$t1p)+40
        temp=ref[ref$no==mode,]$ref
        p(temp)
    })
    
    output$equation5=renderPrint({
        filename=paste0("5",input$c2p,".png")
        img(src=filename)
        
    })
    
    output$ref5=renderPrint({
        mode=as.numeric(input$c2p)+50
        temp=ref[ref$no==mode,]$ref
        p(temp)
    })
    output$equation6=renderPrint({
        filename=paste0("6",input$cpp,".png")
        img(src=filename)
        
    })
    
    output$ref6=renderPrint({
        mode=as.numeric(input$cpp)+60
        temp=ref[ref$no==mode,]$ref
        p(temp)
    })
    
    output$equation7=renderPrint({
        filename=paste0("71.png")
        img(src=filename)
        
    })
    
    output$ref7=renderPrint({
        mode=71
        temp=ref[ref$no==mode,]$ref
        p(temp)
    })
    
    output$equation8=renderPrint({
        filename=paste0("8",input$coxph,".png")
        img(src=filename)
        
    })
    
    output$ref8=renderPrint({
        mode=as.numeric(input$coxph)+80
        temp=ref[ref$no==mode,]$ref
        p(temp)
    })
    
    output$equation9=renderPrint({
        filename=paste0("9",input$oddratio,".png")
        img(src=filename)
        
    })
    
    output$ref9=renderPrint({
        mode=as.numeric(input$oddratio)+90
        temp=ref[ref$no==mode,]$ref
        p(temp)
    })
    
    output$about=renderPrint({
        includeMarkdown("aboute.md")
    })

})
