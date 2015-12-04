# Author: Gaurav Garg (gaurav_garg@yahoo.com)
# This script reads the HIMSS Analytics data in XLSX format and matches it with Novation's member list XLSX
require(zoo)
require(readxl)
require(data.table)
require(dplyr)
require(reshape)
require(xlsx)
require(plyr)
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
        
        # Dont overwrite the workbook, append a new sheet to the existing workbook
        write.xlsx(df.target.NoInstall, targetFileName, sheetName = "Not Installed", append = FALSE)
        
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
        
        write.xlsx(df.target.New, targetFileName, sheetName = "New Active Buyers", append = TRUE)
        
        #update the pivot with Active Buyers
        # 
        xtb2 <- as.data.frame(xtabs( formula =   ~ df.target.New$STATE, 
                                     data = df.target.New))
        colnames(xtb2) <- c("State","Active-New")
        renderData1<-merge(xtb1,xtb2,all = TRUE)
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
 
        
        write.xlsx(df.target.Replace, targetFileName, sheetName = "Replacement Buyers", append = TRUE)
        
        #update the pivot with Active Replacement Buyers
        # 
        xtb3 <- as.data.frame(xtabs( formula =   ~ df.target.Replace$STATE, 
                                     data = df.target.Replace))
        colnames(xtb3) <- c("State","Active-Replace")
        renderData<-merge(renderData1,xtb3, all=TRUE)
        renderData[is.na(renderData)]<-0
        renderData$Total <- rowSums(renderData[,-1])
}

write.xlsx(renderData , targetFileName, sheetName = "Pivot", append = TRUE)












