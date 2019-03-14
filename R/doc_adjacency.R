#' doc_adjacency
#'
#' Adjacency Matrix at document level
#' @param type "product" or "unit". Where overlaps are multiple should they be multiplied ("product") or summed ("unit")
#' @param connection .rqda project file
#' @param files vector of files to restrict search to
#' @keywords RQDA Shiny Network Cloud
#' @export

doc_adjacency<-function(connection=NULL,type="product",include="all",case=NULL,code=NULL,files=NULL){
require(igraph)
require(reshape2)
  require(RSQLite)
  require(plyr)

  #pick up link to SQL database
  if(!is.null(connection)){
    con <- dbConnect(RSQLite::SQLite(), connection)
  }
  else{
    con <- .rqda$qdacon
  }


  if(include=="all"){
  #copy-paste the getCodingTable() function to allow connection to SQL DB
  Codings <- dbGetQuery(con, "select coding.rowid as rowid, coding.cid,
                        coding.fid, freecode.name as codename, source.name as filename,\n
                        coding.selfirst as index1, coding.selend as index2,\n
                        coding.selend - coding.selfirst as CodingLength\n
                        from coding left join freecode on (coding.cid=freecode.id)\n
                        left join source on (coding.fid=source.id)\n
                        where coding.status==1 and source.status=1 and freecode.status=1")[,c(5, 4)]

  }
  if(include=="selection"&is.null(case)==FALSE){
    Codings <- codeIncase(case=case,code=code,output="df",files=files,connection=connection)
    if(ncol(Codings)==1){
      stop("No selected codes found in selected cases")
    }
    else{
      Codings<-Codings[,c(3,2)]
      colnames(Codings)[2]<-"codename"
    }
  }




  #turn into matrix
dtm <- Codings %>%
  mutate(freq = 1) %>%
  acast(filename ~ codename, sum)
if(type=="unit"){
dtm[dtm>0]<-1
}
adj <- crossprod(dtm)
return(adj)
}
