#' codeIncase
#'
#' Find overlap in highlighted text between "codes" and "cases"
#' @param case vector of "cases"
#' @param code vector of "codes" to search within the "cases"
#' @param output output format df=data frame ; HTML = HTML file
#' @param files vector of files to restrict search to
#' @param connection .rqda project file
#' @keywords RQDA Shiny Network Cloud
#' @export

codeIncase <-function(case,code=NULL,output="df",files=NULL,connection=NULL){
  require(R2HTML)
  require(purrr)
  require(RQDA)
  require(dplyr)
  require(RSQLite)
  require(rstudioapi)


  #pick up link to SQL database
  if(!is.null(connection)){
    con <- dbConnect(RSQLite::SQLite(), connection)
  }
  else{
    con <- .rqda$qdacon
  }

  #get matching cases
  caseid<-dbGetQuery(con,paste("select name,id from freecode where name IN",
                          paste("('",paste(case,collapse="','"),"')",sep=""),sep=""))

  #give error if no cases found
  if(any(!case%in%caseid$name)){
    tmp<-case[!case%in%caseid$name]
    stop(paste("No code '",paste(tmp,collapse="','"),"' found in database",sep=""))
  }


  pos<-dbGetQuery(con,paste("select fid,cid,selfirst,selend from coding where cid IN",
                       paste("('",paste(caseid$id,collapse="','"),"')",sep="")
                       ))


  coding<-dbGetQuery(con,"select cid,fid,selfirst,selend from coding")
  freecode<-dbGetQuery(con,"select * from freecode")
  cases<-dbGetQuery(con,"select * from cases")
  caselinkage<-dbGetQuery(con,"select * from caselinkage")
  source<-dbGetQuery(con,"select name,id from source")
  colnames(source)<-c("filename","fid")

  pos$ID<-1:nrow(pos)
  coding$ID<-1:nrow(coding)
  coding<-inner_join(coding,source,by=c("fid"="fid"))

  if(is.null(code)==FALSE){
    codingid<-dbGetQuery(con,paste("select id,name from freecode where name IN",
                              paste("('",paste(code,collapse="','"),"')",sep=""),sep=""))

    if(any(!code%in%codingid$name)){
      tmp<-code[!code%in%codingid$name]
      stop(paste("No code '",paste(tmp,collapse="','"),"' found in database",sep=""))
    }


    coding<-subset(coding,cid%in%as.integer(codingid$id))
  }
  else{
  coding<-subset(coding,!cid%in%as.integer(caseid$id))
     }

  if(is.null(files)==FALSE){
    coding=subset(coding,filename%in%files)
    if(nrow(coding)==0){
      stop("No selected codes found in selected files")
    }
  }



  outputlist<-map_df(pos$ID,function(x)
    data.frame(subset(mutate(coding,Case=caseid$name[caseid$id==pos$cid[pos$ID==x]]),
                      fid==pos$fid[x]&selfirst>=pos$selfirst[x]&selend<=pos$selend[x])))

  if(nrow(outputlist)==0){
    return(data.frame(Case=(paste("No instances of '",code,"' found in '",case,"'",sep=""))))
  }
  outputlist$NewID<-1:nrow(outputlist)
    outputtext<-map_df(outputlist$NewID,
                     function(x)dbGetQuery(con,paste("select seltext from coding where cid=",
                                                outputlist$cid[outputlist$NewID==x],
                                                "AND fid=",outputlist$fid[outputlist$NewID==x],
                                                "AND selfirst=",outputlist$selfirst[outputlist$NewID==x],
                                                "AND selend=",outputlist$selend[outputlist$NewID==x])))

  list1<-outputlist %>% inner_join(freecode,by=c("cid"="id"))%>%
    select(selfirst,name,filename,Case) %>% cbind(outputtext)
  colnames(list1)<-c("position","code","filename","Case","text")

  if(output=="HTML"){
    filename=paste("Output",ceiling(round(runif(1,1,1000))+as.numeric(Sys.time())/1000),sep="_")
  HTMLStart(outdir=getwd(),filename =filename,
            extension="html", echo=FALSE, HTMLframe=FALSE)
  for(j in 1:length(case)){

    tmp<-subset(list1,Case==case[j])
    if(nrow(tmp)>0){
    HTML.title(paste(paste(code,collapse=","),"codes within",case[j]), HR=1)
  for(i in 1:nrow(tmp)){
    HTML.title(paste("file:",tmp$filename[i]," code:",tmp$code[i]), HR=2)
   HTMLhr()
    R2HTML::HTML(tmp$text[i])

  }
    }
  }

  HTMLStop()
  viewer(file.path(getwd(),paste(filename,".html",sep="")))
  }

  if(output=="df"){

    return(list1)
  }
  if(!is.null(connection)){
    dbDisconnect(con)
  }
}
