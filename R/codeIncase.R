#' codecase
#'
#' Find overlap in highlighted text between "codes" and "cases"
#' @param case vector of "cases" to include as search domain
#' @param code vector of "codes" to search within the "cases"
#' @param output output format df=data frame ; HTML = HTML file
#' @param files vector of files to include as search domain
#' @param connection .rqda project file
#' @keywords RQDA Shiny Network Cloud Code Case Overlaps
#' @export

codeIncase <-function(case,code=NULL,files=NULL,connection=NULL,output="df"){
  require(R2HTML)
  require(purrr)
  require(dplyr)
  require(RSQLite)
  require(rstudioapi)


  #pick up link to SQL database

  con <- dbConnect(RSQLite::SQLite(), connection)


  #get matching case id information from cases table
  caseid<-dbGetQuery(con,"select name,id from cases where status=1")
  #give error if no cases found
  if(any(!case%in%caseid$name)){
    tmp<-case[!case%in%caseid$name]
      stop(paste("No case '",paste(tmp,collapse="','"),"' found in database",sep=""))
  }



  #get coding table from freecode
  codingid<-dbGetQuery(con,"select id,name from freecode where status=1")

 #if no codes selected then conduct search based on all codes
  if(is.null(code)==FALSE){
  #return error if selected codes not found
    if(any(!code%in%codingid$name)){
      tmp<-code[!code%in%codingid$name]
       stop(paste("No code '",paste(tmp,collapse="','"),"' found in database",sep=""))
    }
  }
  else{
    code<-unique(codingid$name)
  }

  #get file information from source table
  source<-dbGetQuery(con,"select name,id from source where status=1")

  #if file restriction included then restrict files to only those in selected files
  if(is.null(files)==FALSE){
    if(any(!files%in%source$name)){
      tmp<-files[!files%in%source$name]
      stop(paste("No file '",paste(tmp,collapse="','"),"' found in database",sep=""))
    }
  }
else{
  files<-unique(source$name)
}

  list1<- dbGetQuery(con,paste(
"SELECT coding.selfirst AS 'position', freecode.name AS 'code', source.name 'filename',
cases.name AS 'Case', coding.seltext AS 'text' FROM cases
INNER JOIN coding, source, caselinkage, freecode ON freecode.id=coding.cid
AND caselinkage.selfirst<=coding.selfirst AND coding.fid=source.id AND caselinkage.selend>=coding.selend
AND caselinkage.caseid=cases.id AND caselinkage.fid = source.id
AND cases.name IN", paste("('",paste(case,collapse="','"),"')",sep=""),
"AND freecode.name IN", paste("('",paste(code,collapse="','"),"')",sep=""),
"AND source.name IN", paste("('",paste(files,collapse="','"),"')",sep=""),
"AND freecode.status=1 AND cases.status=1  AND caselinkage.status=1 AND source.status=1
AND coding.status=1 ORDER BY coding.selfirst"),sep="")


dbDisconnect(con)

return(list1)


}
