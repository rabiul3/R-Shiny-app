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
+ (**fixed** - 2021Feb09) Parsing AminoAcid XLS file does not read all the metabolites in each page.
    + Problem was with the number of bookkeeping rows (i.e. "created by") that followed the samples. When the number of samples is low compared to the total number of rows like in this file then the % missing data exceeded QA check is triggered and the metabolite is skipped. All metabolies will trigger this QA check.
        + Fixed by masking out the bookeeping rows using the sample type column. only valid samples will have a non NA value in this row.
+ (**fixed** - 2021Feb09) Parsing BadFile.XLS does not read all the metabolites in each page.
    + Problem was when entries in Filename column were not unique (eg multiple entries named "Blank")
        + Fixed by giving duplicate Filenames unique names

## Objectives
1) open xlsx workbook
2) use filename column as record index
3) get "Component Name"" from each page and return "Calculated Amount" as record columns
'

#Requirements
library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("shinybusy")
library("xlsx")

# Define globals
#suppress warninings
options(warn = 0)
#random seed
set.seed(44701)
#max file size
options(shiny.maxRequestSize=100*1024^2)


# Define Header
header <- dashboardHeader(
    title = "XCalibur File Convert utility",
    titleWidth = 300
)

# Define Sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Upload xlsx File", tabName = "upload", icon = icon("file-upload")),
        menuItem("Changelog", tabName = "changelog", icon = icon("changelog"))
    ),
    width = 300
)

# Define Body
body <-   dashboardBody(
    # Items for each sidebar tab
    tabItems(
        #Process XLSX File
        tabItem(tabName = "upload",
                fluidRow(
                    column(4,
                           fileInput("file", "Select xlsx files to process",
                                     multiple = TRUE, accept = c(".xlsx", ".XLSX", ".xls", ".XLS"))
                    ),
                    actionBttn("check", "Check File!", 
                               style="stretch", icon = icon("sliders"),
                               color="primary", block=TRUE), 
                    verbatimTextOutput("print_action"),
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
    # Check file
    observeEvent(input$check,{
      # show modal spinner to prevent user from interacting while busy
      show_modal_spinner() 
      
      sfile = input$file$datapath
      # read all sheets
      filepath <- sfile
      wb <- loadWorkbook(filepath)
      metabolites <- names(getSheets(wb))
      #remove sheets labeled "Component" or "mdlCalcs"
      metabolites <- metabolites[!metabolites%in%c("Component", "mdlCalcs")]
      TempFN <-c()
      for (i in 1:length(metabolites)){
        data <- read.xlsx(filepath, sheetIndex = i,startRow = 5, header = TRUE, stringsAsFactors=FALSE) 
        SheetFN<-as.matrix(data[,c("Filename")])
        colnames(SheetFN)<-c(metabolites[i])
        TempFN<-cbind(TempFN,SheetFN)
      }
      #Check identical 
      TempFN<-as.data.frame(TempFN)
      TempFN<-na.omit(TempFN)
      same = apply(TempFN, 1, function(x) length(unique(x)) == 1)
      #result<-allcolumnsidentity(TempFN$one, TempFN)
      same = apply(TempFN, 1, function(x) length(unique(x)) == 1)
      result<-all(same)
      if (result=="TRUE"){
        showNotification(paste("FileName is identical for all tabs"), duration = 10,type = "warning")
        
      }
      else{
        showNotification(paste("FileName is not identical for all tabs"), duration = 10,type = "warning")
      }
      
      
      #remove spinner when done
      remove_modal_spinner()
    })
    
    
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
            for (ifile in c(1:length(input$file$datapath))){
                sfile = input$file$datapath[ifile]
               #id = input$file$name[ifile]
                id="ConvertedFile"
                message("- - - Proccessing file - - - ")
                message(id)
                message("")
                
                #get file data
                filepath <- sfile
                wb <- loadWorkbook(filepath)
                metabolites <- names(getSheets(wb))
                #valid samples (rows) are defined by the Sampletype column which should not be NA
#               validsamples <- read.xlsx(filepath, sheetIndex = 1, 
#                                          startRow = 5, header = TRUE, stringsAsFactors=FALSE)[,2]
 #               validsamples <- which(!is.na(validsamples))
                
                #remove sheets labeled "Component" or "mdlCalcs"
                metabolites <- metabolites[!metabolites%in%c("Component", "mdlCalcs")]
                
# Rabi processing   

                validsamples <- read.xlsx(filepath, sheetIndex = 1,startRow = 5, header = TRUE, stringsAsFactors=FALSE)
                commonSamples <-validsamples[,c("Filename","Sample.Type")]
                Temp<-c()
                #length(metabolites)
                for (i in 1:length(metabolites)){
                  dfout <- read.xlsx(filepath, sheetIndex = i,startRow = 5, header = TRUE, stringsAsFactors=FALSE)
                  eachSheetdata<-as.matrix(dfout[,c("Amount.1")])
                  colnames(eachSheetdata)<-c(metabolites[i])
                  Temp<-cbind(Temp,eachSheetdata)
                  }
                dfout<-cbind(commonSamples,Temp)
                ColFilename<-dfout["Filename"]
                rowID<-which(ColFilename=="Blank")
                dfout<-dfout[rowID+1:dim(dfout)[1],]
                
                #save processed file
                outfile <- paste(id,".csv", sep="")
                write.csv(dfout, file=outfile, row.names = FALSE)
                outfiles <- c(outfiles, outfile)
                
            }
            #update upload message
            output$uploadmsg <- renderText({paste(c(length(outfiles), "Files processed:", paste(outfiles, collapse = ", ")), collapse = " ")}) 

            #Download proceessed files
            output$downloadData <- downloadHandler(
                filename = function() {
                    paste("HY_data_converter", "zip", sep=".")
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
