#' RQDAPlus
#'
#' Shiny interface to additional features using an RQDA project file
#' @param connection .rqda project file
#' @keywords RQDA Shiny Network Cloud
#' @export
RQDAPlus<-function(connection){
  require(shiny)
  require(DT)
  require(wordcloud2)
  require(tm)
  require(stringr)
  require(DBI)
# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("RQDA Plus"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        selectizeInput("cases", "Cases", choices = "",multiple = TRUE),
        selectizeInput("codes", "Codes", choices = "",multiple = TRUE),
        selectizeInput("files", "Files", choices = "",multiple = TRUE)
      ),

      # Show a plot of the generated distribution
      mainPanel(width = 6,
                tabsetPanel(
                  type = "tabs",
    tabPanel("Extracts from Code Overlap",dataTableOutput("Output0"),dataTableOutput("Output1")),
    tabPanel("Adjacency Matrix",selectInput("type1", "Type:",
                                        choices = c("Within Document"="Document","Specific Text"="Text")),
             dataTableOutput("Output2")),
    tabPanel("Network Analysis",selectInput("type", "Type", choices = c("Within Document"="Document","Specific Text"="Text")),
             checkboxInput("group", "Group Nodes?", value=FALSE),
             plotOutput("Output3")),
    tabPanel("WordCloud", wordcloud2Output("cloud1"))
      ))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  con <- dbConnect(RSQLite::SQLite(), connection)


  observe({

 fc<-dbGetQuery(con,"select name,id from freecode")
    avail_codes<- avail_cases<-fc$name[fc$id%in%unique(dbGetQuery(con,"select cid from coding")$cid)]

    avail_files<-dbGetQuery(con,"select name from source")

    updateSelectInput(session, "codes", choices = avail_codes)
    updateSelectInput(session, "cases", choices = avail_cases)
    updateSelectInput(session, "files", choices = avail_files)
  })

  output$Output0 <- renderDataTable({
    if(!is.null(input$cases)){
      df<-codeIncase(input$cases,input$codes,output = "df",connection=connection,files = input$files)

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
     if(!is.null(input$cases)){
     out<-codeIncase(input$cases,input$codes,output = "df",connection=connection,files = input$files)
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

   output$Output2 <- renderDataTable({
     if(input$type1=="Document"){
tab<-doc_adjacency(connection=connection,files = input$files)
     }
     if(input$type1=="Text"){
       tab<-code_overlap(connection=connection,files = input$files)
     }
if(length(unique(c(input$cases,input$codes)))>1){
     tab<-tab[colnames(tab)%in%unique(c(input$cases,input$codes)),colnames(tab)%in%unique(c(input$cases,input$codes))]
}
        tab<-data.frame(Code=rownames(tab),tab)
     rownames(tab)<-NULL
   tab
   })


   output$Output3 <- renderPlot(width=800,height=800,{
     if(input$type=="Document"){
  RQDAnetwork(doc_adjacency(connection=connection,files = input$files),group=input$group)
     }
     if(input$type=="Text"){
       RQDAnetwork(code_overlap(connection=connection,files = input$files),group=input$group)
     }
   })
   output$cloud1 <- renderWordcloud2({

     if(is.null(input$cases)){
       if(is.null(input$files)){
         txt<-dbGetQuery(con,"select seltext from coding")$seltext
       }
       else{
         sources<-dbGetQuery(con,paste("select id from source where name IN",
                                      paste("('",paste(input$files,collapse="','"),"')",sep=""),sep=""))
         txt<-dbGetQuery(con,paste("select seltext from coding where fid IN",
                         paste("('",paste(sources$id,collapse="','"),"')",sep=""),sep=""))$seltext
       }

       }
       else{
            txt<-codeIncase(input$cases,input$codes,output = "df",connection=connection,files=input$files)$text
       }
if(length(txt)>0){
     t1<-removePunctuation(tolower(unlist(str_split(stripWhitespace(txt)," "))))
     t1<-t1[!t1%in%stopwords("english")]
     t2<-data.frame(table(t1))

     wordcloud2(t2,size=0.4)
   }
   })

   }

# Run the application
shinyApp(ui = ui, server = server)

}
