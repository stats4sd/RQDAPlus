#' RQDAPlus
#'
#' Shiny interface to additional features using an RQDA project file. Currently
#' implemented options for i) subsetting coded extracts to within certain codes
#' WordClouds of extracts, or overall text, or individual files Adjacency
#' matrices based on code occurence at within document and within case level
#' Network analysis based on code co-occurence at either within document or
#' within case level, with optional additional grouping algorithm
#' @param connection .rqda project file
#' @keywords RQDA Shiny Network Cloud
#' @export
RQDAPlus<-function(connection=NULL){
  require(shiny)
  require(DT)
  require(wordcloud2)
  require(tm)
  require(stringr)
  require(DBI)
  require(rio)
# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("RQDA Plus"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        selectizeInput("cases", "Cases", choices = "",multiple = TRUE),
        selectInput("codcat","Selection of Codes by:",choices=c("Code","Category")),
        selectizeInput("codes", "Codes", choices = "",multiple = TRUE),
        selectizeInput("files", "Files", choices = "",multiple = TRUE),
        downloadButton("downloadData", "Download Selected Text as CSV"),
        downloadButton("downloadData2", "Download Selected Text as HTML")
      ),

      # Show a plot of the generated distribution
      mainPanel(width = 6,
                tabsetPanel(type = "tabs",
    tabPanel("Extracts from Codes Within Cases",
             htmlOutput("Title1"),
             dataTableOutput("Output0"),
             dataTableOutput("Output1")),
    tabPanel("WordCloud",htmlOutput("Title4"),wordcloud2Output("cloud1")),
    tabPanel("Adjacency Matrix",htmlOutput("Title2"),
             selectInput("AdjType","Adjacency of Codes Within:",choices=c("Document","Case")),
             dataTableOutput("Output2")),
    tabPanel("Network Analysis",htmlOutput("Title3"),
             selectInput("NetType","Adjacency of Codes Within:",choices=c("Document","Case")),
             checkboxInput("group", "Group Nodes?", value=FALSE),
             numericInput("max", "Maximum Nodes to Include", value=75),
             plotOutput("Output3"))
   ,tags$style("#Title1{color: black;
                                 font-size: 22px;
                        font-style: bold;}"),
   tags$style("#Title2{color: black;
                                 font-size: 22px;
               font-style: bold;}"),
   tags$style("#Title3{color: black;
                                 font-size: 22px;
                        font-style: bold;}"),
   tags$style("#Title4{color: black;
                                 font-size: 22px;
                        font-style: bold;}")))
      )

)

# Define server logic required to draw a histogram
server <- function(input, output,session) {


    con <- dbConnect(RSQLite::SQLite(), connection)



  observe({

 fc<-dbGetQuery(con,"select name,id from freecode")

 fc3<-dbGetQuery(con,"select name,catid from codecat")
    avail_codes<-fc$name[fc$id%in%unique(dbGetQuery(con,"select cid from coding")$cid)]

      avail_cats<-fc3$name[fc3$catid%in%unique(dbGetQuery(con,"select catid from treecode")$catid)]

      avail_codes<-sort(avail_codes)
      avail_cats<-sort(avail_cats)


    if(input$codcat=="Code"){
    updateSelectInput(session, "codes", choices = avail_codes)



      }
      if(input$codcat=="Category"){
    updateSelectInput(session, "codes", choices = avail_cats)
     }


  })

  observe({
    fc<-dbGetQuery(con,"select name,id from freecode")
    fc2<-dbGetQuery(con,"select name,id from cases")


    avail_cases<-fc2$name[fc2$id%in%unique(dbGetQuery(con,"select caseid from caselinkage")$caseid)]

    avail_cases<-sort(avail_cases)

    avail_files<-dbGetQuery(con,"select name from source")

    updateSelectInput(session, "cases", choices = avail_cases)
    updateSelectInput(session, "files", choices = avail_files)
  })



  output$Title1<-renderText({
    if(input$codcat=="Code"){x=paste("Frequency of code(s)",paste(input$codes,collapse=", "),
                                     "found within case(s)",paste(input$cases,collapse=", "))}
    else{x=paste("Frequency of code category(s)",paste(input$codes,collapse=", "),
                                     "found within case(s)",paste(input$cases,collapse=", "))}
    x
  })

  output$Title2<-renderText({
   paste("Adjacency matrix showing code co-occurence within each",tolower(input$AdjType),sep=" ")
     })

  output$Title3<-renderText({
   if(input$group==FALSE){y=paste("Network analysis based on code co-occurence within each",tolower(input$NetType),sep=" ")}
    if(input$group==TRUE){y=paste("Network analysis based on code co-occurence within each",tolower(input$NetType),
                                "with clustering of related codes",sep=" ")}
    y
  })

  output$Title4<-renderText({
    if(input$codcat=="Code"){x=paste("Wordcloud of entries coded",paste(input$codes,collapse=", "),
                                     "found within case(s)",paste(input$cases,collapse=", "))}
    else{x=paste("Wordcloud of entries coded within the category(s) of",paste(input$codes,collapse=", "),
                 "found within case(s)",paste(input$cases,collapse=", "))}
    x
  })
  output$Output0 <- renderDataTable({
    if(input$codcat=="Code"){
      codes<-input$codes
    }
    else{
      if(is.null(input$codes)){
        codes<-NULL
      }
      else{
        fc3<-dbGetQuery(con,"select name,catid from codecat")
        codes1<-dbGetQuery(con,paste("select cid from treecode where catid IN",
                                     paste("('",paste(fc3$catid[fc3$name%in%input$codes],collapse="','"),"')",sep="")))$cid
        codes<-dbGetQuery(con,paste("select name from freecode where id IN",
                                    paste("('",paste(codes1,collapse="','"),"')",sep="")))$name
      }
    }



    if(!is.null(input$cases)){
      df<-codeIncase(input$cases,codes,output = "df",connection=connection,files = input$files)

      if(ncol(df)==1){
        out<-df
      }
      else{
        out<-aggregate(position~code+Case,data=df,FUN=length)
        colnames(out)[3]<-"Instances"
      }
      out
    }
      })


   output$Output1 <- renderDataTable({
     if(input$codcat=="Code"){
       codes<-input$codes
     }
     else{
       if(is.null(input$codes)){
         codes<-NULL
       }
       else{
         fc3<-dbGetQuery(con,"select name,catid from codecat")
         codes1<-dbGetQuery(con,paste("select cid from treecode where catid IN",
                                      paste("('",paste(fc3$catid[fc3$name%in%input$codes],collapse="','"),"')",sep="")))$cid
         codes<-dbGetQuery(con,paste("select name from freecode where id IN",
                                     paste("('",paste(codes1,collapse="','"),"')",sep="")))$name
       }
     }


     if(!is.null(input$cases)){
     out<-codeIncase(input$cases,codes,output = "df",connection=connection,files = input$files)
     if(ncol(out)==1){
       out<-out
     }
     else{
       out<-out[,c(5,2,4,3)]
     }
     }
     else{
       out<-data.frame("Case"="No cases selected")
     }
     out
   })

   output$downloadData <- downloadHandler(
     filename = function() {
       paste("Output",ceiling(round(runif(1,1,1000))), ".csv", sep = "")
     },
     content = function(file) {

       if(input$codcat=="Code"){
         codes<-input$codes
       }
       else{
         if(is.null(input$codes)){
           codes<-NULL
         }
         else{
           fc3<-dbGetQuery(con,"select name,catid from codecat")
           codes1<-dbGetQuery(con,paste("select cid from treecode where catid IN",
                                        paste("('",paste(fc3$catid[fc3$name%in%input$codes],collapse="','"),"')",sep="")))$cid
           codes<-dbGetQuery(con,paste("select name from freecode where id IN",
                                       paste("('",paste(codes1,collapse="','"),"')",sep="")))$name
         }
       }


       write.csv(codeIncase(input$cases,codes,output = "df",connection=connection,files = input$files)[,c(3,4,2,5)],
                 file, row.names = FALSE)
     }
   )

   output$downloadData2  <- downloadHandler(
     filename = function() {
       paste("RQDAOutput",ceiling(round(runif(1,1,1000))), ".html", sep = "")
     },
     content = function(file) {
       if(input$codcat=="Code"){
         codes<-input$codes
       }
       else{
         if(is.null(input$codes)){
           codes<-NULL
         }
         else{
           fc3<-dbGetQuery(con,"select name,catid from codecat")
           codes1<-dbGetQuery(con,paste("select cid from treecode where catid IN",
                                        paste("('",paste(fc3$catid[fc3$name%in%input$codes],collapse="','"),"')",sep="")))$cid
           codes<-dbGetQuery(con,paste("select name from freecode where id IN",
                                       paste("('",paste(codes1,collapse="','"),"')",sep="")))$name
         }
       }


       out<-codeIncase(input$cases,codes,output = "df",connection=connection,files = input$files)
         out$seltext<-paste("<p>",gsub("[\n]","<br></br>",out$seltext),"</p>")
       export(out[,c(3,4,2,5)],file, row.names = FALSE)
     }
   )





   output$Output2 <- renderDataTable({


tab<-doc_adjacency(connection=connection,type="unit",level=input$AdjType)

if(input$codcat=="Code"){
  codes<-input$codes
}
else{
  if(is.null(input$codes)){
    codes<-NULL
  }
  else{
    fc3<-dbGetQuery(con,"select name,catid from codecat")
    codes1<-dbGetQuery(con,paste("select cid from treecode where catid IN",
                                 paste("('",paste(fc3$catid[fc3$name%in%input$codes],collapse="','"),"')",sep="")))$cid
    codes<-dbGetQuery(con,paste("select name from freecode where id IN",
                                paste("('",paste(codes1,collapse="','"),"')",sep="")))$name
  }
}


if(length(unique(c(input$cases,codes)))>1){
     tab<-tab[colnames(tab)%in%unique(c(input$cases,codes)),colnames(tab)%in%unique(c(input$cases,codes))]
}
        tab<-data.frame(Code=rownames(tab),tab)
     rownames(tab)<-NULL
   tab
   },options = list(dom = 't'))


   output$Output3 <- renderPlot(width=800,height=800,{

     if(input$codcat=="Code"){
       codes<-input$codes
     }
     else{
       if(is.null(input$codes)){
         codes<-NULL
       }
       else{
         fc3<-dbGetQuery(con,"select name,catid from codecat")
         codes1<-dbGetQuery(con,paste("select cid from treecode where catid IN",
                                      paste("('",paste(fc3$catid[fc3$name%in%input$codes],collapse="','"),"')",sep="")))$cid
         codes<-dbGetQuery(con,paste("select name from freecode where id IN",
                                     paste("('",paste(codes1,collapse="','"),"')",sep="")))$name
       }
     }


  RQDAnetwork(doc_adjacency(connection=connection,type="unit",level=input$NetType),group=input$group,
              maxnodes = input$max)


   })
   output$cloud1 <- renderWordcloud2({
     if(input$codcat=="Code"){
       codes<-input$codes
     }
     else{
       if(is.null(input$codes)){
         codes<-NULL
       }
       else{
         fc3<-dbGetQuery(con,"select name,catid from codecat")
         codes1<-dbGetQuery(con,paste("select cid from treecode where catid IN",
                                      paste("('",paste(fc3$catid[fc3$name%in%input$codes],collapse="','"),"')",sep="")))$cid
         codes<-dbGetQuery(con,paste("select name from freecode where id IN",
                                     paste("('",paste(codes1,collapse="','"),"')",sep="")))$name
       }
     }

     if(is.null(input$cases)){
       if(is.null(input$files)){
         filelist<-dbGetQuery(con,"select name from source")$name
       }
       else{
         filelist<-input$files
       }

       if(is.null(input$files)&is.null(codes)){
         txt<-dbGetQuery(con,"select file from source")$file
       }
       if((!is.null(input$files))&is.null(codes)){

         txt<-dbGetQuery(con,paste("select file from source where name IN",
                                   paste("('",paste(filelist,collapse="','"),"')",sep=""),sep=""))$file
       }

       if(!is.null(codes)){
         sources<-dbGetQuery(con,paste("select id from source where name IN",
                                      paste("('",paste(filelist,collapse="','"),"')",sep=""),sep=""))
         codes<-dbGetQuery(con,paste("select id from freecode where name IN",
                                       paste("('",paste(codes,collapse="','"),"')",sep=""),sep=""))
         txt<-dbGetQuery(con,paste("select seltext from coding where fid IN",
                         paste("('",paste(sources$id,collapse="','"),"')",sep=""),
                         " and cid IN",
                         paste("('",paste(codes$id,collapse="','"),"')",sep=""),sep=""))$seltext
       }
     }

     if(!is.null(input$cases)){
            txt<-codeIncase(input$cases,codes,output = "df",connection=connection,files=input$files)$text
       }
if(length(txt)>0){
     t1<-removePunctuation(tolower(unlist(str_split(stripWhitespace(txt)," "))))
     t1<-t1[!t1%in%stopwords("english")]
     t2<-data.frame(table(t1))
     t2<-slice(t2[rev(order(t2$Freq)),],1:250)

     wordcloud2(t2,size=0.7)
   }
   })

   session$onSessionEnded(function() {
     dbDisconnect(con)
     stopApp()
   })

   }

# Run the application
shinyApp(ui = ui, server = server,options =list(launch.browser = TRUE) )

}
