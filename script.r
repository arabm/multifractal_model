
# install.packages("tseries")
library(tseries)
library(fGarch)
library(FitARMA)
library(zoo)

library(stringr)

library(quantmod)

library(tools)
library(brew)

library(forecast)

library(MSwM)

# This is to concatate a list of list.
# Total depth should be 2.
paste0_rec<-function(text_l,sep1){
    res<-""
    for (i in text_l){
        tmp<-paste0(i,collapse=sep1)
        res<-paste0(res,tmp)
    }
    return(res)
}

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#  Path management for pdf output
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
# Store in a data.frame
# path to work.
# TODO : check if there is like in python a os.path.join
#   Because depending on the OS the "/" as a string may not be robust
conf_add_output_subdir<-function(conf,subdir){
    output_dir<-attr(conf,"output")
    basename<-paste0(output_dir,"/",subdir)
    attr(conf,subdir)<-basename
    if (! dir.exists(basename)){
        dir.create(basename)
    }
    return(basename)
}
conf_make<-function(working_dir="./"){
    output_dir<-paste0(working_dir,"/output",collapse="")
    if (! dir.exists(output_dir)){
        dir.create(output_dir)
    }
    conf<-structure(
        data.frame(),
        working_dir=working_dir,
        output=output_dir
        )
    return(conf)
}

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#  Download quotation
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
# Need an internet connection


# Currently used to retrieve data from the net.
download_quote<-function(name,output_file){
    start="1899-12-30"
    end="2017-01-01"
    quote<-get.hist.quote(instrument = name, start, end,
        quote = c("Open", "High", "Low", "Close"),
        provider = c("yahoo",""), method = NULL,
        origin = "1899-12-30", compression = "d",
        retclass = c("zoo", "its", "ts"), quiet = FALSE, drop = FALSE)
    write.zoo(quote,output_file,index.name="Date",sep=",")
}


quote_dl<-function(conf,symbol,sourcef){
    # Examples :
    # get_quote("DEXUSEU", "FRED")
    basename_dir<-conf_add_output_subdir(conf,symbol)
    #
    csvfile<-paste0(basename_dir,"/",symbol,".csv")
    #
    res<-getSymbols(symbol,src=sourcef)
    quote<-get(res)
    write.zoo(quote,csvfile,index.name="Date")
    #quote<-read.csv(basename)
    #quote<-read.zoo(basename)
    # retrieve Close, quote$quote.Close
    attr(quote,"symbol")<-symbol
    return(quote)
}

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#  Quotation
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
quote_close<-function(quote){
    src<-attr(quote,"src")
    v<-quote
    if(src=="yahoo"){
        v<-Cl(quote)
    }
    v<-na.omit(v)
    return(v)
}
quote_logret<-function(quote){
    src<-attr(quote,"src")
    v<-quote
    if(src=="yahoo"){
        v<-Cl(quote)
    }
    v<-na.omit(v)
    return(diff(log(v)))
}

quote_plot_value<-function(conf,seriets){
    serie<-na.omit(seriets)
    #
    pdf(paste0(basename,".pdf",collapse=""))
    plot(serie,type="l")
    dev.off()
    pdf(paste0(basename,"_acf.pdf",collapse=""))
    acf(serie)
    dev.off()
    pdf(paste0(basename,"_pacf.pdf",collapse=""))
    pacf(serie)
    dev.off()
}

quote_plot<-function(conf,quote){
    symbol<-attr(quote,"symbol")
    output_dir<-conf_add_output_subdir(conf,symbol)
    basename<-paste0(output_dir,"/",symbol,collapse="")
    #
    quote_plot_value(basename,quote_close(quote))
    quote_plot_value(paste0(basename,"_logret"),quote_logret(quote))
    return(basename)
}


# Save various charts (into pdf) on the Time Serie given
# Original value, ACF and PACF
retrieve_data_plot<-function(basename,serie){
    pdf(paste0(basename,".pdf",collapse=""))
    plot(serie,value,type="l")
    dev.off()
    pdf(paste0(basename,"_acf.pdf",collapse=""))
    acf(serie)
    dev.off()
    pdf(paste0(basename,"_pacf.pdf",collapse=""))
    pacf(serie)
    dev.off()
}

# Retrieve time serie from its symbol : "quote_name"
retrieve_data<-function(conf,quote_name,source_name){
    input_dir = attr(conf,"input_dir")
    output_dir = attr(conf,"output_dir")
    input_csv<-paste0(input_dir,"/",quote_name,".csv",collapse="",sep="")
    # Download quote if not yet on the disk
    if (! file.exists(input_csv)){
        #download_quote_quantmod(quote_name,source_name)        
        download_quote(quote_name,input_dir)
    }
    basename<-paste0(output_dir,"/",quote_name,collapse="")
    ###
    data<-read.csv(input_csv)
    #
    date<-as.Date(data$Date)
    quote<-data$Close #data[paste0(quote_name,".Close",collapse="")][,]
    retrieve_data_plot(paste0(basename,"_original"),date,quote)
    quote_log<-log(quote)
    retrieve_data_plot(paste0(basename,"_log"),date,quote_log)
    ret<-c(0,diff(log(quote)))
    retrieve_data_plot(paste0(basename,"_logret"),date,ret)
    ret2<-c(0,diff(log(quote)))^2
    retrieve_data_plot(paste0(basename,"_logret2"),date,ret2)
    #
    res<- structure(data.frame(date,quote,quote_log,ret,ret2,quote_name),basename=basename)
    return(res)
}




resolve_garch<-function(basename,quote){
    value<-na.omit(quote_logret(quote))

}

resolve_ar1<-function(val,model){
    r<-arima(val,model)
    # Look if residual are white noises
    plot(qqnorm(r$residuals))
}

# model should be a vector of size 3 :
# ARIMA(p,d,q).
# model <- 
#    c(1,0,0) ~ AR(1)
#    c(0,0,1) ~ MA(1)
plot_arima<-function(value,model,output_dir){
    # 
    # ARMA(p,q)
    # y(t) = Beta_1
    #      + \sum_i=1^p \alpha_i y(t-i) % AR
    #      + \sum_i=1^q \theta_i e(t-i) % MA
    #      + e(t)
    #  with e(t) ~ N(0,sigma^2)
    pdf(paste0(output_dir,"/fit_arima",model,sep=""))
    fit<-arima(value,order=model)
    fitt<-tsdiag(fit)
    plot(fitt,xlim=c(-1000,1000),ylim=c(-1000,1000))
    dev.off()
}

resolve_arima_manual<-function(conf,basename,value){
    p<-pacf(value)
    a<-acf(value)
}




plot_garch<-function(value,model,outputname){
    pdf(paste0("./output/",outputname,sep=""))
    # Note : remove the first element which was 0
    fit<-garch(value,order=model)
    tsdiag(fit)
    dev.off()
}

#------------------------------------------------------
# Solver
#------------------------------------------------------

solve_ar<-function(basename,value){
    res<-ar(value)

    sink(paste0(basename,"_ar_solving.txt"))
    print(res)
    sink()

    pdf(paste0(basename,"_ar_solving.pdf"))
    plot(res$aic,type="l")
    abline(h=0)
    grid()
    dev.off()
    pdf(paste0(basename,"_ar_param.pdf"))
    plot(res$ar,type="h")
    abline(h=0)
    grid()
    dev.off()

    test_normal_law(paste0(basename,"_ar_residual"),na.omit(res$resid))
    #pdf(paste0(basename,"_ar_residual.pdf"))
    #plot(res$resid,type="h")
    #grid()
    #dev.off()
    #pdf(paste0(basename,"_ar_residual_qqnorm.pdf"))
    #points(qqnorm(res$resid))
    #qqline(res$resid)
    #dev.off()
}

solve_ma<-function(basename,value){
    res<-auto.arima(value, max.p=0, stationary=TRUE, seasonal=FALSE)
    sink(paste0(basename,"_ma_solving.txt"))
    print(res)
    sink()
    pdf(paste0(basename,"_ma_param.pdf"))
    plot(res$ar,type="h")
    abline(h=0)
    grid()
    dev.off()
    test_normal_law(paste0(basename,"_ma_residual"),na.omit(res$residuals))
}


solve_arma<-function(basename,value){
    res<-auto.arima(value,  stationary=TRUE, seasonal=FALSE)
    sink(paste0(basename,"_arma_solving.txt"))
    print(res)
    sink()
    pdf(paste0(basename,"_arma_param.pdf"))
    plot(res$ar,type="h")
    abline(h=0)
    grid()
    dev.off()
    test_normal_law(paste0(basename,"_arma_residual"),na.omit(res$residuals))
}

solve_msm<-function(basename,value){
    #
    mod<-lm(value~1)
    mod.mswm<-msmFit(mod,k=2,p=1,sw=c(T,T,T), control=list(parallel=F)) 
    #
    sink(paste0(basename,"_msm_solving.txt"))
    print(mod.mswm)
    sink()
    #
    pdf(paste0(basename,"_msm_which1.pdf"))
    plotProb(mod.mswm,which=1)
    dev.off()

    pdf(paste0(basename,"_msm_which2.pdf"))
    plotProb(mod.mswm,which=2)
    dev.off()
}



solve_all<-function(basename,serie_input){
    pdf(paste0(basename,".pdf"))
    plot(serie_input,type="l")
    dev.off()
    serie<-na.omit(serie_input)
    solve_ar(basename,serie)
    solve_ma(basename,serie)
    solve_arma(basename,serie)
    solve_msm(basename,serie)
}
solve_quote<-function(conf,quote){
    #
    symbol<-attr(quote,"symbol")
    output_dir<-conf_add_output_subdir(conf,symbol)
    basename<-paste0(output_dir,"/",symbol)
    #
    close_level<-quote_close(quote)
    #solve_all(paste0(basename,"_original"),close_level)
    # Logret
    logret<-quote_logret(quote)
    #solve_all(paste0(basename,"_logret"),logret)

    return(basename)
}

#------------------------------------------------------
# Examples
#------------------------------------------------------
# In order to better understand
# what looks like AR(1) and MA(1)
# and its relative ACF and PACF
# or more generally ARMA(p,q)

#
ar_acf<-function(conf,beta){
    example_dir<-conf_add_output_subdir(conf,"example_ar")
    basename<- paste0(example_dir,"/example_ar1_",beta,".pdf",collapse="",sep="")
    pdf(basename)
    c<-numeric(30)
    gamma<-1
    for (i in index(c)){
        c[i]<-gamma
        gamma<-gamma*beta
    }
    plot(c,type="h")
    abline(h=0)
    grid()
    dev.off()
    return(basename)
}

# functions to retrieve a path or a string from the model
# It simplifies the brew template
arima_name<-function(model){
    n=model$n
    ar_l=model$ar
    ma_l=model$ma
    #
    if (is.null(ar_l)){
        name<-paste0_rec(list("MA(",ma_l,") (n=",n,")"),sep1=",")
    } else if (is.null(ma_l)){
        name<-paste0_rec(list("AR(",ar_l,") (n=",n,")"),sep1=",")
    } else {
        name<-paste0_rec(list("AR(",ar_l,"), MA(",ma_l,") (n=",n,")"),sep1=",")
    }
    return(name)
}
arima_file<-function(conf,model){
    example_dir<-conf_add_output_subdir(conf,"example_arima")
    index<-arima_name(model)
    #for (c in c("\\(","\\)","=","_ _","__","\\.","\\,","__","__")){
    #    index<-str_replace_all(index,c[1],"_")
    #}
    basename<- paste0(example_dir,"/example_",index,collapse="",sep="")
    return(basename)
}

arima_model_name<-function(model){
    ar_l=model$ar
    ma_l=model$ma
    #
    if (is.null(ar_l)){
        name<-paste0("MA(",length(ma_l),")",collapse="")
    } else if (is.null(ma_l)){
        name<-paste0("AR(",length(ar_l),")",collapse="")
    } else {
        name<-paste0("ARMA(",length(ar_l),",",length(ma_l),")",collapse="")
    }
    return(name)
}

# Simulate ARMA model, and save pdf into the example directory
example_arima<-function(conf,model){
    ar_l=model$ar
    ma_l=model$ma
    n=model$n
    # Looping for a new id
    basename<-arima_file(conf,model)
    #
    if (length(ar_l) < 1){
        s<-arima.sim(list(ma=ma_l),n)
    } else if (length(ma_l)<1) {
        s<-arima.sim(list(ar=ar_l),n)
    } else {
        s<-arima.sim(list(ar=ar_l,ma=ma_l),n)
    }
    #
    pdf(paste0(basename,"_sim.pdf",collapse=""))
    plot(s)
    dev.off()
    pdf(paste0(basename,"_acf.pdf",collapse=""))
    acf(s)
    dev.off()
    pdf(paste0(basename,"_pacf.pdf",collapse=""))
    pacf(s)
    dev.off()
    return(s)
}


file_from_normal<-function(conf,n,mean,std){
    example_dir<-conf_add_output_subdir(conf,"example_normal")
    basename<-paste0(example_dir,"/normal_",n,"_",mean,"_",std)
    return(basename)
}

test_normal_law<-function(basename,res){
    pdf(paste0(basename,".pdf",collapse=""))
    plot(res,type="l")
    dev.off()
    #
    pdf(paste0(basename,"_qqnorm.pdf",collapse=""))
    points(qqnorm(res))
    qqline(res)
    dev.off()
    #
    pdf(paste0(basename,"_density.pdf",collapse=""))
    #xseq<-seq(-std*10+mean,std*10+mean,.01)
    #densities<-dnorm(xseq, mean,std)
    #plot(x=xseq,y=densities,type="l",col="green")
    plot(density(res))
    dev.off()
    # Independance test
    sink(paste0(basename,"_box_test_ljung.txt"))
    print(Box.test(res,type='Ljung'))
    sink()
    sink(paste0(basename,"_shapiro.txt"))
    print(shapiro.test(res))
    sink()
}

example_normal<-function(conf,n,mean,std){
    basename<-file_from_normal(conf,n,mean,std)
    res<-rnorm(n,mean,std)
    test_normal_law(basename,res)
    return(basename)
}

file_from_uniform<-function(conf,n,a,b){
    example_dir<-conf_add_output_subdir(conf,"example_uniform")
    basename<-paste0(example_dir,"/uniform_",n,"_",a,"_",b)
    return(basename)
}
example_uniform<-function(conf,n,a,b){
    basename<-file_from_uniform(conf,n,a,b)
    res<-runif(n,a,b)
    test_normal_law(basename,res)
    return(basename)
}


#-------------------------------------------------------
#
# Main function
#

main_report<-function(working_dir="./",doLatex=TRUE)
{   
    conf<-conf_make(working_dir)
    report_dir<-conf_add_output_subdir(conf,"report")
    brewfile<-paste0(working_dir,"/template.brew",collapse="")
    reportfile<-paste0(report_dir,"/generated_report.tex",collapse="")
    brew(brewfile, reportfile)
    #texi2dvi(reportfile,pdf=TRUE)
    if(doLatex){
        texi2pdf(reportfile,clean=TRUE)
    }
}

if(!interactive()){
    working_dir<-"./"
    #
    args = commandArgs(trailingOnly=TRUE)
    if (length(args)>0) {
        # Not yet, because texi2pdf seems to be launch from the PWD (of the terminal)
        #working_dir<-paste0(args[1],"/",collapse="")
    }
    
    main_report(working_dir)
} else {
    working_dir<-"./"
    main_report(working_dir,doLatex=FALSE)

}

