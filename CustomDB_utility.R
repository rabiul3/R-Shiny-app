#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#changelog
doctext<-'
## Change Log
+ (**fixed** - 2021Sept07) Can not get the script work. When I click "run document", it was trying to install a package, then stopped. I used the old version, many data sets were not converted even with 100% data points present.
    + Problem using previous version. Previous version has hard 70% missing data QC parameter so not all data will be processed.
        + Fixed by using the most current version.
    + Problem with packages not installing properly.
        + Fixed by publishing shiny app inorder to control the build process.
+ (**fixed** - 2021Jul06) Not all sets of data are converted most times. Normally those sets with many "NA" or "NF" (not find) were not converted. User would like to have all data sets be converted?
    + Problem was because QC parameter drops metabolites with >70% missing data. 
        + Fixed by making QC parameters interavtive as a shiny app.
+ (**fixed** - 2021Feb09) Parsing AminoAcid CSV file does not read all the metabolites in each page.
    + Problem was with the number of bookkeeping rows (i.e. "created by") that followed the samples. When the number of samples is low compared to the total number of rows like in this file then the % missing data exceeded QA check is triggered and the metabolite is skipped. All metabolies will trigger this QA check.
        + Fixed by masking out the bookeeping rows using the sample type column. only valid samples will have a non NA value in this row.
+ (**fixed** - 2021Feb09) Parsing BadFile.CSV does not read all the metabolites in each page.
    + Problem was when entries in Filename column were not unique (eg multiple entries named "Blank")
        + Fixed by giving duplicate Filenames unique names

## Objectives
1)  accepts Input.CSV and outputs a TSV file
2) From column 4 - find Unique formula list (e.g., C5H9NO4 occurs 3 times; select unique value)
3) Writefile (“id” \t “name” \t ”formula”)	#header row with tab-separated words “id name formula”
'

#Requirements
library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("shinybusy")
library("xlsx")
library("data.table")

# Define globals
#suppress warninings
options(warn = 0)
#random seed
set.seed(44701)
#max file size
options(shiny.maxRequestSize=100*1024^2)


# Define Header
header <- dashboardHeader(
  title = "CustomDB Utility",
  titleWidth = 300
)

# Define Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("CustomDB Utility", tabName = "upload", icon = icon("file-upload")),
    menuItem("Changelog", tabName = "changelog", icon = icon("changelog"))
  ),
  width = 300
)

# Define Body
body <-   dashboardBody(
  # Items for each sidebar tab
  tabItems(
    #Process CSV File
    tabItem(tabName = "upload",
            fluidRow(
              column(4,
                     fileInput("file", "Select CSV files to process",
                               multiple = TRUE, accept = c(".xlsx", ".XLSX", ".xls", ".XLS",".csv"))
              ),
              actionBttn("do", "Process File!", 
                         style="stretch", icon = icon("calculator"),
                         color="primary", block=TRUE),
              # Download button
              downloadButton("downloadData", "Download Processed File(s)."),
            )
    ),#end upload tabItem
    #display changelog
    tabItem(tabName = "changelog", fluidPage( markdown(doctext)))
    
  )#end tabItems
)

# Define UI for application
ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  
  #work in temp directory
  tmpdir <- tempdir()
  setwd(tempdir())
  
  #Process metabolites
  observeEvent(input$do,{
    # show modal spinner to prevent user from interacting while busy
    show_modal_spinner()
    #check if file is loaded
    if(is.null(input$file)){
      #update upload message
      output$uploadmsg <- renderText({"No file Uploaded!"})
    }
    else{
      #init processed files
      outfiles <- c()
      #loop over files
      sfile<-input$file
      dfile<-gsub("\\..*", "", input$file$name)
      dfile<-paste(dfile,"_customDB")
      id=dfile
      message("- - - Proccessing file - - - ")
      message(id)
      message("")
      filepath <- sfile
      df =read.csv(sfile$datapath, skip = 2, header = T)
      
      df<-df [,c("moleculeIds", "moleculeNames","formula","mz")]
      names(df)<-c("id","name","formula","mz")
      
      # define data frame
      Ndf<-data.frame(matrix(nrow=50,ncol=4))
      colnames(Ndf)<-c("id","name","formula","mz")
      # loop over
      for (i in 1:dim(df)[1])
      {
        nameMZ<-df[,"mz"][i]
        idx<-which(df$mz==nameMZ)
        temp<-df[idx,]
        if (length(idx)==1)
        { 
          ######### process id ###################
          Ndf[i,1]<-temp[,1]
          ######### process formula ###################
          Ndf[i,3]<-temp[,3]
          
          ######### process mz ###################
          Ndf[i,4]<-temp[,4] [1]
          
          ######### process name ###################
          Meta_id1<-temp$name
          #Add quatation
          Meta_id2<-unlist(strsplit(Meta_id1,","))
          #Remove whitespace from double quate
          Meta_id2<-trimws(Meta_id2, whitespace = "[\\h\\v]")
          #Remove duplicate
          UnMetaid<-unique(Meta_id2)     
          if (length(UnMetaid)==1)
          {
            #add comma
            lT<-paste(unlist(t(UnMetaid)),  collapse=",")   
            #Put space after comma
            lT<-gsub(",", ", \\1", lT)
            Ndf[i,2]<-lT
          }
          else
          {
            #add comma
            lT<-paste(unlist(t(UnMetaid)),  collapse=",")
            #Put space after comma
            lT<-gsub(",", ", \\1", lT)
            #Put double quate from composite metabolites
            lT<-paste("\"",lT,"\"",sep = "")
            Ndf[i,2]<-lT 
          }
          
        }   
        else
        {
          ######### process id ###################
          Meta_id1<-temp$id
          #Add quatation
          Meta_id2<-unlist(strsplit(Meta_id1," "))
          #remove comma
          RMetaid<-gsub(",", " ", Meta_id2)
          #Remove whitespace from double quate
          RMetaid<-trimws(RMetaid, whitespace = "[\\h\\v]")
          #Remove duplicate
          UnMetaid<-unique(RMetaid)
          #add comma by removing double quate
          lT<-paste(unlist(t(UnMetaid)),  collapse=",")
          #Put space after comma
          lT<-gsub(",", ", \\1", lT)
          Ndf[i,1]<-lT
          
          ######### process formula ###################
          Ndf[i,3]<-temp$formula[1]
          ######### process mz ###################
          Ndf[i,4]<-temp[,4] [1]   
          
          ######### process Name ###################
          Meta_id1<-temp$name
          #Add quatation
          Meta_id2<-unlist(strsplit(Meta_id1,","))
          #Remove whitespace from double quate
          Meta_id2<-trimws(Meta_id2, whitespace = "[\\h\\v]")
          #Remove duplicate
          UnMetaid<-unique(Meta_id2)     
          if (length(UnMetaid)==1){
            #add comma
            lT<-paste(unlist(t(UnMetaid)),  collapse=",")   
            #Put space after comma
            lT<-gsub(",", ", \\1", lT)
            Ndf[i,2]<-lT
          }
          else
          {
            #add comma
            lT<-paste(unlist(t(UnMetaid)),  collapse=",")
            #Put space after comma
            lT<-gsub(",", ", \\1", lT)
            #Put double quate from composite metabolites
            lT<-paste("\"",lT,"\"",sep = "")
            Ndf[i,2]<-lT 
          }
          
        }
      }
     
      # Remove duplicates
      dfout<-Ndf[!duplicated(Ndf$mz),]
      # Remove mz column
      dfout<-dfout[,c("id","name","formula")]
      
      #save processed file
      outfile <- paste(id,".tsv", sep="")
      #write.csv(dfout, file=outfile, row.names = FALSE)
      write.table(dfout, file = outfile, row.names=FALSE, sep="\t",quote = FALSE)
      outfiles <- c(outfiles, outfile)
      
      
      #update upload message
      output$uploadmsg <- renderText({paste(c(length(outfiles), "Files processed:", paste(outfiles, collapse = ", ")), collapse = " ")}) 
      
      #Download proceessed files
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(dfile, "zip", sep=".")
        },
        content = function(fname) {
          
          zip(zipfile=fname, files=outfiles)
        },
        contentType = "application/zip"
      )           
    }
    
    #remove spinner when done
    remove_modal_spinner()
  })#end do proccess file
  
  
}#end Server

# Run the application 
shinyApp(ui = ui, server = server)
