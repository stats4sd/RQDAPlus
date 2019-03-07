#' doc_adjacency
#'
#' Adjacency Matrix at document level
#' @param type "product" or "unit". Where overlaps are multiple should they be multiplied ("product") or summed ("unit")
#' @param connection .rqda project file
#' @param files vector of files to restrict search to
#' @keywords RQDA Shiny Network Cloud
#' @export

doc_adjacency<-function(connection=NULL,type="product",files=NULL){
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

  #copy-paste the getCodingTable() function to allow connection to SQL DB
  Codings <- dbGetQuery(con, "select coding.rowid as rowid, coding.cid,
                        coding.fid, freecode.name as codename, source.name as filename,\n
                        coding.selfirst as index1, coding.selend as index2,\n
                        coding.selend - coding.selfirst as CodingLength\n
                        from coding left join freecode on (coding.cid=freecode.id)\n
                        left join source on (coding.fid=source.id)\n
                        where coding.status==1 and source.status=1 and freecode.status=1")

  if(is.null(files)==FALSE){
    Codings=subset(Codings,filename%in%files)
    if(nrow(Codings)==0){
      stop("No codes found in selected files")
    }
  }

  if (nrow(Codings) != 0) {
    Encoding(Codings$codename) <- Encoding(Codings$filename) <- "UTF-8"
  }

  #turn into matrix
dtm <- Codings[,c(5, 4)] %>%
  mutate(freq = 1) %>%
  acast(filename ~ codename, sum)
if(type=="unit"){
dtm[dtm>0]<-1
}
adj <- crossprod(dtm)
return(adj)
}
