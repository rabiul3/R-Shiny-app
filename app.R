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
    title = "Aggregate Amount"
)

# Define Sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Upload xlsx File", tabName = "upload", icon = icon("file-upload")),
        menuItem("Changelog", tabName = "changelog", icon = icon("changelog"))
    )
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
                    #percent missing box
                    box(title = "Percent Missing",
                        sliderInput("pcntmissing",
                                    "Remove metabolites with percent missing greater than:",
                                    min = 0, 
                                    max = 100, 
                                    step = 1,
                                    value = 20
                        )
                    ),
                    actionBttn("do", "Process File!", 
                               style="stretch", icon = icon("calculator"),
                               color="primary", block=TRUE),
                    # Download button
                    downloadButton("downloadData", "Download Processed File(s)."),
                    # processed msg
                    column(12, textOutput("uploadmsg"))
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
    
    #fetch CRPM logo
    output$img <- renderUI({
        tags$logo(src = "https://dmontemayor.github.io/assets/Long_SOM/horizontal/JPG/UTHSA_Long-SOM_H_CMYK.jpg")
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
                id = input$file$name[ifile]
                message("- - - Proccessing file - - - ")
                message(id)
                message("")
                
                #get file data
                filepath <- sfile
                wb <- loadWorkbook(filepath)
                metabolites <- names(getSheets(wb))
                #valid samples (rows) are defined by the Sampletype column which should not be NA
                validsamples <- read.xlsx(filepath, sheetIndex = 1, 
                                          startRow = 5, header = TRUE, stringsAsFactors=FALSE)[,2]
                validsamples <- which(!is.na(validsamples))
                
                #remove sheets labeled "Component" or "mdlCalcs"
                metabolites <- metabolites[!metabolites%in%c("Component", "mdlCalcs")]
                
                #init bad metabolite list
                skippedmetabos <- c()
                
                #loop over sheets
                for (metabolite in metabolites){
                    message(metabolite)
                    #init output file on the first sheet
                    if (metabolite == metabolites[1]){
                        dfout <- read.xlsx(filepath, sheetIndex = 1, startRow = 5, header = TRUE, stringsAsFactors=FALSE)
                        dfout <- dfout[validsamples, 1:2]
                        #save initial number of rows
                        nobv <- nrow(dfout)
                        #make sure every Filename is unique
                        dfout$Filename <- sub('[.]', '_', make.names(dfout$Filename, unique=TRUE))
                    }
                    #read sheet 
                    df <- read.xlsx(filepath, sheetName = metabolite, startRow = 5, header = TRUE, stringsAsFactors=FALSE)[validsamples,]
                    #convert Calculated Amount row to numerics
                    df[,9] <- as.numeric(as.character(df[,9]))
                    #ignore sheet if too many rows are missing
                    QCtest <- sum(is.finite(df[,9]))>=(nobv*(1-input$pcntmissing/100))
                    if(QCtest){
                        #convert "Amount" column to metabolite name
                        names(df)[9] <- metabolite
                        #keep only "filename"" and metabolite columns
                        df <- df[,names(df)%in%c("Filename", metabolite)]
                        #make sure every Filename is unique
                        df$Filename <- sub('[.]', '_', make.names(df$Filename, unique=TRUE))
                        #merge with output dataframe by filename
                        dfout <- merge(dfout, df, by = "Filename")
                    }
                    #record skipped metabolites
                    if(!QCtest){
                        skippedmetabos <- c(skippedmetabos, metabolite)
                    }
                    
                    #assert output file integrity for each metabolite
                    if(dim(dfout)[1]!=nobv){stop("Error number of row in output file is changing!")}
                }
                
                #save processed file
                outfile <- paste(id,".csv", sep="")
                write.csv(dfout, file=outfile, row.names = FALSE)
                outfiles <- c(outfiles, outfile)
                
                #display skipped metabolites
                if(length(skippedmetabos)>0){
                    message("")
                    message(paste(c(length(skippedmetabos), " Metabolites not processed due to too many missing values:"), collapse = ""))
                    message(paste(skippedmetabos, collapse = ", "))
                    message("")
                }
                
                ## Downloadable csv of selected dataset ----
                #output$downloadData <- downloadHandler(
                #    filename = function() {
                #        paste(id, ".csv", sep="")
                #        #paste(input$dataset, ".csv", sep = "")
                #    },
                #    content = function(outfile) {
                #        write.csv(dfout, file=outfile, row.names = FALSE)
                #        #write.csv(datasetInput(), file, row.names = FALSE)
                #    }
                #)
                
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
