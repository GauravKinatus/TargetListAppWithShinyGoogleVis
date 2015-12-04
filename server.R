# Shiny App to review the data from Novation membership, in context of HIMSS Analytics
# Author: Gaurav Garg (gaurav_garg@yahoo.com)
# 

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(markdown))
suppressPackageStartupMessages(library(ISLR))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(datasets))
suppressPackageStartupMessages(library(BH))



# this script read 3 data files and creates. Expected folder structure below:
#       workking director: ~/Data/
#       ~/Data/HIMSS_Matched_Data-Current.xlsx
#       ~/Data/<techFolder>/_ Orgs _By Tech 2_.xlsx
#       ~/Data/<techFolder>/_ Orgs _By Tech 2_ (1).xlsx
#       ~/Data/<geoFolder>/_ of Purchasing Organization.xlsx
# Output:
#       ~/Data/<geoFolder>/Calance_target_listsDec032015.xlsx

techFolder <-"SSO"
geoFolder <- "US"

# build the path to read the Active buyers' lists
activeBuyerListPath <- paste(getwd(),"/",techFolder,"/",sep="")

# Output Direcotry and Filename
outputPath <- paste(getwd(),"/",techFolder,"/",geoFolder,"/", sep = "")

targetFileName <- paste(outputPath,"Calance_Target_Lists-",
                        techFolder,"-",geoFolder,"-"
                        ,format(Sys.time(),"%b%d%Y"),".xlsx",sep="")



# Read the Novation database/matched with HIMSS Analytics Id. 
# This is a large xlsx file; assume in the current working directory.
if (file.exists("HIMSS_Matched_Data-Current.xlsx"))
{
        try ( df.Novation <- read_excel("HIMSS_Matched_Data-Current.xlsx"))
}

# processing block for all prospects that DO NOT have the target technology. This should be the largest set.
if (file.exists(paste(outputPath,"_ of Purchasing Organization.xlsx",sep = "")))
{
        
        
        # read the list of prospects that DOES NOT have the target technology, grouped on GPO
        try (df.HIMSS.NoInstall <- read_excel(paste(outputPath,"_ of Purchasing Organization.xlsx",sep = "")))
        
        
        # Not Installed downloads are grouped by GPO merges the Health System name
        #repeat value of Health System ID
        df.HIMSS.NoInstall$`HS Unique ID` <- na.locf(df.HIMSS.NoInstall$`HS Unique ID`)
        #repeat value of Health System
        df.HIMSS.NoInstall$`Health System` <- na.locf(df.HIMSS.NoInstall$`Health System`)
        #repeat Org Unique Id
        df.HIMSS.NoInstall$`Org Unique Id` <- na.locf(df.HIMSS.NoInstall$`Org Unique Id`)
        #repeat Organization
        df.HIMSS.NoInstall$`Organization` <- na.locf(df.HIMSS.NoInstall$`Organization`)
        #repeat State
        df.HIMSS.NoInstall$`State/Province` <- na.locf(df.HIMSS.NoInstall$`State/Province`)
        #repeat Organization Type
        df.HIMSS.NoInstall$`Organization Type` <- na.locf(df.HIMSS.NoInstall$`Organization Type`)
        
        #Lets find the matching records from NoInstall list in the Novation member list
        df.NoInstall <- intersect(df.HIMSS.NoInstall$'HS Unique ID',df.Novation$'HA UniqueId')
        df.target.NoInstall <- df.Novation[df.Novation$`HA UniqueId` %in% df.NoInstall, ]

        #create a pivot based on state
        xtb1<- as.data.frame(xtabs( formula =   ~ df.target.NoInstall$STATE, 
                                    data = df.target.NoInstall))
        
        colnames(xtb1) <- c("State","Prospects")
        renderData<- xtb1
        
}

# processing block for all Active buyers (First Time)
if (file.exists(paste(activeBuyerListPath,
                      "_ Orgs _By Tech 2_.xlsx",sep=""))){
        # read the list of First time buyers downloaded from HIMSS Analytics. 
        # Assumption: filters for geography and bedsize are applied on the downloads
        try (df.HIMSS.ActiveBuyers <- read_excel(paste(activeBuyerListPath,
                                                       "_ Orgs _By Tech 2_.xlsx",sep="")))
        
        # Active Buyers data is grouped on Technology, Health System and Organization names
        #repeat value of Technology.id from row 1 to all the cells.
        df.HIMSS.ActiveBuyers$'Technology' <- na.locf(df.HIMSS.ActiveBuyers$'Technology')
        #repeat hospital system
        df.HIMSS.ActiveBuyers$'Health System' <- na.locf(df.HIMSS.ActiveBuyers$'Health System')
        #repeat hospital name
        df.HIMSS.ActiveBuyers$Organization <- na.locf(df.HIMSS.ActiveBuyers$Organization)
        #repeat technology purchase plan
        df.HIMSS.ActiveBuyers$'Technology Purchase Plan' <- na.locf(df.HIMSS.ActiveBuyers$'Technology Purchase Plan')
        
        #Lets find the matching records from New Buyers in the Novation member list
        df.New <- intersect(df.HIMSS.ActiveBuyers$Organization,df.Novation$NAME)
        df.target.New<-df.Novation[df.Novation$NAME %in% df.New,]
       
        #update the pivot with Active Buyers
        # 
        xtb2 <- as.data.frame(xtabs( formula =   ~ df.target.New$STATE, 
                                     data = df.target.New))
        colnames(xtb2) <- c("State","Active-New")
        renderData<-merge(xtb1,xtb2,all = TRUE)
}

# processing block for all Active buyers (replacement)
if (file.exists(paste(activeBuyerListPath,"_ Orgs _By Tech 2_ (1).xlsx",sep="")))
{
        # read the list of active replacement buyers
        try (df.HIMSS.ActiveReplace <- read_excel(paste(activeBuyerListPath,
                                                        "_ Orgs _By Tech 2_ (1).xlsx",sep="")))
        
        
        #replacement buyers data is grouped by Technology, Hospital System, Oraganization Name
        #repeat the value of Technology.id from row 1 to all the cells.
        df.HIMSS.ActiveReplace$'Technology' <- na.locf(df.HIMSS.ActiveReplace$'Technology')
        #repeat Health System Name
        df.HIMSS.ActiveReplace$'Health System' <- na.locf(df.HIMSS.ActiveReplace$'Health System')
        #repeat hospital name
        df.HIMSS.ActiveReplace$Organization <- na.locf(df.HIMSS.ActiveReplace$Organization)
        
        #Lets find the matching records from Replacement Buyers in the Novation member list
        df.Replace <- intersect(df.HIMSS.ActiveReplace$Organization,df.Novation$NAME)
        df.target.Replace<-df.Novation[df.Novation$NAME %in% df.Replace,]
        
        
        xtb3 <- as.data.frame(xtabs( formula =   ~ df.target.Replace$STATE, 
                                     data = df.target.Replace))
        colnames(xtb3) <- c("State","Active-Replace")
        
        renderData<-merge(renderData,xtb3, all=TRUE)

        renderData[is.na(renderData)]<-0
        renderData$Total <- rowSums(renderData[,-1])
 
}






shinyServer(function(input, output) {
        output$targetTech <- renderText(paste("Target List for ",techFolder," in US Healthcare Industry"))
        output$TAM <- renderText("TAM: 58006")
        output$SAM <- renderText(paste("SAM: ",nrow(df.HIMSS.NoInstall)))
        output$SOM <- renderText( function(){
                grpBy <- reactive({input$varGroupBy}) 
                
                if(input$varGroupBy == 1) # No-Install
                        totalProspect <- sum(renderData$Prospects)
                else if (input$varGroupBy == 2) # Active New 
                        totalProspect <- sum(renderData$`Active-New`)
                else if (input$varGroupBy == 3) #Active Replace
                        totalProspect <- sum(renderData$`Active-Replace`)
                else if (input$varGroupBy == 4) #All
                        totalProspect <- sum(renderData$Total)
                paste("SOM: ", totalProspect)
        })
        
        output$gvis <- renderGvis( {
                if(input$varGroupBy == 1) # Prospects
                        gvisGeoChart(renderData, "State", "Prospects",
                                     options=list(region="US",
                                                  displayMode="regions",
                                                  resolution="provinces",
                                                  width = 600, 
                                                  height = 300)#Options
                                        )#gvisGeoChart
                else if (input$varGroupBy == 2) # Active-New
                        gvisGeoChart(renderData, "State", "Active-New",
                                     options=list(region="US",
                                                  displayMode="regions",
                                                  resolution="provinces",
                                                  width = 600, 
                                                  height = 300)#Options
                                     ) #gvisGeoChart              
                else if (input$varGroupBy == 3) #Active-Replace
                        gvisGeoChart(renderData, "State", "Active-Replace",
                                     options=list(region="US",
                                                  displayMode="regions",
                                                  resolution="provinces",
                                                  width = 600, 
                                                  height = 300)#Options
                                     ) #gvisGeoChart               
                        
                else if (input$varGroupBy == 4) #All
                        gvisGeoChart(renderData, "State", "Total",
                                     options=list(region="US",
                                                  displayMode="regions",
                                                  resolution="provinces",
                                                  width = 600, 
                                                  height = 300)#Options
                                     )#gvisGeoChart
                
        })#renderGvis

})