
# install.packages("tseries")
library(tseries)
library(fGarch)
library(zoo)

library(stringr)

#library(quantmod)

library(tools)
library(brew)



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

# Store in a data.frame
# path to work.
# TODO : check if there is like in python a os.path.join
#   Because depending on the OS the "/" as a string may not be robust
make_conf<-function(working_dir){
    conf<-structure(
        data.frame(),
        working_dir=working_dir,
        input_dir=paste0(working_dir,"/input",collapse=""),
        output_dir=paste0(working_dir,"/output",collapse=""),
        example_dir=paste0(working_dir,"/example",collapse=""),
        report_dir=paste0(working_dir,"/report",collapse="")
        )
    for (attrname in c("input_dir","output_dir","example_dir","report_dir")){
        dirname<-attr(conf,attrname)
        if (! dir.exists(dirname)){
            dir.create(dirname)
        }
    }
    return(conf)
}

# Not used yet
# Example used with name="AAPL"
# Attributes of quote will be
# Date, AAPL.Close, ...
# The function which need these datas suppose that attribute are
# Date, Close, ... (without the "name")
#download_quote_quantmod<-function(name,sourcef,output_file){
#    res<-getSymbols(name)#,source=sourcef)
#    quote<-get(res)
#    write.zoo(quote,output_file,index.name="Date",sep=",")
#}

# Currently used to retrieve data from the net.
download_quote<-function(conf,name){
    output_dir<-attr(conf,"input_dir")
    output_file<-paste0(output_dir,"/",name,".csv",collapse="")
    start="1899-12-30"
    end="2017-01-01"
    quote<-get.hist.quote(instrument = name, start, end,
        quote = c("Open", "High", "Low", "Close"),
        provider = c("yahoo"), method = NULL,
        origin = "1899-12-30", compression = "d",
        retclass = c("zoo", "its", "ts"), quiet = FALSE, drop = FALSE)
    write.zoo(quote,output_file,index.name="Date",sep=",")
}


# Save various charts (into pdf) on the Time Serie given
# Original value, ACF and PACF
retrieve_data_plot<-function(basename,date,value){
    pdf(paste0(basename,".pdf",collapse=""))
    plot(date,value,type="l")
    dev.off()
    pdf(paste0(basename,"_acf.pdf",collapse=""))
    acf(value)
    dev.off()
    pdf(paste0(basename,"_pacf.pdf",collapse=""))
    pacf(value)
    dev.off()
}

# Retrieve time serie from its symbol : "quote_name"
retrieve_data<-function(conf,quote_name){
    input_dir = attr(conf,"input_dir")
    output_dir = attr(conf,"output_dir")
    input_csv<-paste0(input_dir,"/",quote_name,".csv",collapse="",sep="")
    # Download quote if not yet on the disk
    if (! file.exists(input_csv)){
        #download_quote_quantmod(quote_name,source_name,input_csv)        
        download_quote(conf,quote_name)
    }
    basename = paste0(output_dir,"/",quote_name,collapse="")
    ###
    data = read.csv(input_csv)
    #
    date = as.Date(data$Date)
    quote = data$Close #data[paste0(quote_name,".Close",collapse="")][,]
    retrieve_data_plot(paste0(basename,"_original"),date,quote)
    quote_log = log(quote)
    retrieve_data_plot(paste0(basename,"_log"),date,quote_log)
    ret = c(0,diff(log(quote)))
    retrieve_data_plot(paste0(basename,"_logret"),date,ret)
    ret2 = c(0,diff(log(quote)))^2
    retrieve_data_plot(paste0(basename,"_logret2"),date,ret2)
    #
    res<- structure(data.frame(date,quote,quote_log,ret,ret2),basename=basename)
    return(res)
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
    print(fit)
    fitt<-tsdiag(fit)
    plot(fitt,xlim=c(-1000,1000),ylim=c(-1000,1000))
    dev.off()
}

resolve_auto_arima<-function(conf,basename){
    res<-auto.arima(value)
    return(res)
}


plot_garch<-function(value,model,outputname){
    pdf(paste0("./output/",outputname,sep=""))
    # Note : remove the first element which was 0
    fit<-garch(value,order=model)
    tsdiag(fit)
    dev.off()
}

#------------------------------------------------------
# Examples

# In order to better understand
# what looks like AR(1) and MA(1)
# and its relative ACF and PACF
# or more generally ARMA(p,q)


# functions to retrieve a path or a string from the model
# It simplifies the brew template
example_arima_generate_basename<-function(conf,index){
    example_dir<-attr(conf,"example_dir")
    basename<- paste0(example_dir,"/example_",index,collapse="",sep="")
    return(basename)
}
example_arima_model_name<-function(ar_l,ma_l){
    name<-paste0("ARMA(",length(ar_l),",",length(ma_l),")",collapse="")
    return(name)
}
example_arima_param_name<-function(ar_l,ma_l){
    name<-paste0_rec(list("AR(",ar_l,"), MA(",ma_l,")"),sep1=",")
    return(name)
}

# Simulate ARMA model, and save pdf into the example directory
example_arima<-function(conf,ar_l,ma_l,n,mean){
    # Looping for a new id
    example_dir<-attr(conf,"example_dir")
    index<-0
    basename<-example_arima_generate_basename(conf,index)
    outputname<- paste0(basename,"_sim.pdf",collapse="",sep="")
    while (file.exists(outputname)){
        index<-index+1
        basename<-example_arima_generate_basename(conf,index)
        outputname<- paste0(basename,"_sim.pdf",collapse="",sep="")
    }
    if (length(ar_l) < 1){
        s<-arima.sim(list(ma=ma_l),n)
    } else if (length(ma_l)<1) {
        s<-arima.sim(list(ar=ar_l),n)
    } else {
        s<-arima.sim(list(ar=ar_l,ma=ma_l),n)
    }
    pdf(paste0(basename,"_sim.pdf",collapse=""))
    plot(s)
    dev.off()
    pdf(paste0(basename,"_acf.pdf",collapse=""))
    acf(s)
    dev.off()
    pdf(paste0(basename,"_pacf.pdf",collapse=""))
    pacf(s)
    dev.off()
    return(index)
}



#-------------------------------------------------------
#
# Main function
#

main_report<-function(working_dir)
{   
    conf<-make_conf(working_dir)
    report_dir<-attr(conf,"report_dir")
    brewfile<-paste0(working_dir,"/template.brew",collapse="")
    reportfile<-paste0(report_dir,"/generated_report.tex",collapse="")
    brew(brewfile, reportfile)
    #texi2dvi(reportfile,pdf=TRUE)
    texi2pdf(reportfile,clean=TRUE)
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
}

