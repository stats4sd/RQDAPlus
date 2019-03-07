#' code_overlap
#'
#' Adjacency Matrix at text level
#' @param connection .rqda project file
#' @param files vector of files to restrict search to
#' @keywords RQDA Shiny Network Cloud
#' @export
code_overlap<-function(connection=NULL,files=NULL){
  require(DescTools)
  require(RSQLite)
  require(dplyr)
  require(RQDA)
  require(plyr)
  require(intervals)

  #pick up link to SQL database
  if(!is.null(connection)){
    con <- dbConnect(RSQLite::SQLite(), connection)
  }
  #if no link explicitly given then look for RQDA connection
  else{
    con <- .rqda$qdacon
  }


  #get coding dataframes
  coding<-dbGetQuery(con,"select cid,fid,selfirst,selend,source.name from coding left join source on (coding.fid=source.id)")
  colnames(coding)[5]<-"filename"
  if(is.null(files)==FALSE){
    coding=subset(coding,filename%in%files)
    if(nrow(coding)==0){
      stop("No codes found in selected files")
    }
  }


  freecode<-dbGetQuery(con,"select name,id from freecode")

  #merge coding dfs
  ctable<-inner_join(coding,freecode,by=c("cid"="id"))

  #make selfirst and selend unique across all files
  ctable$selfirst<-max(ctable$selend)*max(ctable$fid) + ctable$selfirst
  ctable$selend<-max(ctable$selend)*max(ctable$fid) + ctable$selend

  #add a new id variable into this (not sure why it wasnt in the original SQl DB)
  ctable$ID<-1:nrow(ctable)

  #define ranges as intervals
  Aint <- Intervals(ctable[,c("selfirst","selend")])

  #get overlap between intervals
 int<-interval_overlap(Aint, Aint)

 #add in names
names(int)<-ctable$name

#turn into DF of overlaps
 df <- ldply (int, data.frame) %>% inner_join(ctable[,c("name","ID")],by=c("X..i.."="ID"))%>%
   mutate(freq = 1) %>%
   acast(.id ~ name, sum)
 return(df)
}

