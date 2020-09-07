#################################################################################################
#                           Settings and calculation of spectral indices                        #
#                         ################################################                      # 
#                                   Created by Christoph Butz                                   #
#                                        15.Januar 2017                                         #
#                                 Christoph.Butz@giub.unibe.ch                                  #
#################################################################################################
# Latest changes: extensive overhaul
# New papersizes and resolutions available
# RABA output supported
# plotsettings.csv deprecated
# minor bugfixes

# Install and activate rstudioapi - necessary to set the working directory. requires Rstudio 0.99.9x
lib <- library("rstudioapi", logical.return=TRUE)
if(lib==FALSE) install.packages("rstudioapi"); library("rstudioapi")
rm(list=c("lib"))
# automatically set the working directory on script location.
if (rstudioapi::isAvailable()) {setwd(dirname(rstudioapi::getActiveDocumentContext()$path))}

#################################################################################################
#                     Manual Settings                                                           #
#################################################################################################

# optional Settings
#########################
  # (default Settings exist, change only if you need to)    
  
  # Specify if you want to calculate plots that are exactly the size of the core or plots that fit on paper
  Format.Core  <- c(TRUE) # option: FALSE
  Format.Paper <- c(TRUE) # option: FALSE
  
  # Specify the page size and the resolution
  Paper.Size <- c('A4') # option: one of A0,...,A6
  Paper.Res  <- c(300)  # dots per inch (dpi) size. Higher values mean higher resolution.
  
  # Specify which output graph you would like:
  # (single,multi,oplot) option: FALSE
  Graph.Out  <- c(TRUE,TRUE,TRUE)
  
  # If sediment consists of multiple cores specify the depth at the top of the core section here 
  Multi.Core.Start <- 0 # depth in mm (look up from results of previous core)
  
  # specify y range of cracks in the core in the form: y1,y2,y1,y2,y1,y2,...
  # cracks <- matrix(data=c(10588,10602),
  #                  ncol=2,
  #                  byrow=TRUE
  #                  )
  
  # Specify Tiff image. Supported Modes are: "RGB", "NIR" or "CIR" in combination with 
  # "sqrt", "linear", "log", "adheq" or "adheq_FFT"  
  Tiff.Mode  <- c("RGB","linear")
  
  # Mode of output (either: "png" or "svg") Note: svg stores graphs and images separately. 
  #                                               No overplotting/multiplots for svg output
  Output.Mode <- c("png")

  # Choose indices to be calculated manually (optional) (options: c("all") = use all indices from idl (default)
  #                                                              c("none") = use only external data
  #                                 c("Min690","R570R630","R590R690", etc) = use indices sorted as specified
  #                                 Use exactly the spelling of the index folders)     
  User.Indices <- c("all") 
  
  # With the following variables you are able to set specific y coordinates to produce the outputs 
  # If the variables are zero the coordinates of the whole core will be used.(Unit: mm)
  Y.Window.Start <- 0 
  Y.Window.End   <- 0 
  
  # should index calculation be based on "small" or "big" subset?
  Subset.Type <- c("small")
  # If big subset is chosen (i.e. subset_type <- c("big")) provide X1 and X2 for the profile [unit: Pixel].
  # The standard [c(0,0)] is a 2mm profile centered in the big subset.
  Bsub <- c(0,0)
  
  # Moving average filter 
  # half width of the "window" + 1
  k=13 #("2" yields a 5 point window (2,1,2)
  
  # If you want to add another timeseries to be plot together with the spectral indices 
  # copy a csv file of the data to the scripts directory
  # The csv file must contain a Header row specifying the names of the column data
  # The first column must contain the depth in milimeters
  # Filename of data source
  Other.Data <- c("") 
  # Specify the second Tiff file, options like above (Tiff.Mode)
  Tiff.Mode1 <- c("CIR","linear")
  # Specify the title of the multi plot
  OD.Title   <- c("External Data")
  # Specify a single color for the graphs of the external data
  ODL.Color <- c("black")
  
  # Use ages from chronology on left y axis
  Age.Depth <- c("AgeDepth.csv") #


  # Main plot settings
  ####################
  # color of the mean
  Mean.Color   <- c('red')
  # color of the standard deviation (run 'colors()' for more options)
  Sd.Color     <- c('darkgrey')
  # The line style (run '?par') for more options
  Line.Style   <- c('l')
  # The color of the background (use 'transparent' for no background)
  Background   <- c('white')	
  # The unit of the coordinate system of the graphics device (standard is 'px')
  File.Unit    <- c('px')	
  
  # Single Graph axis labels	
  X.Label      <- c('index value')	
  Y.Label.L    <- c('depth [mm]')	
  Y.Label.R    <- c('depth [mm]')	
  
  # multiple graphs labels	
  Main.Title.m <- c('Spectral Indices')	
  X.Label.m    <- c('value') 	
  Y.Label.L.m  <- c('depth [mm]')	
  Y.Label.R.m  <- c('depth [mm]')	
  
#################################################################################################
#        AUTOMATIC SETTINGS !! DON'T CHANGE !! (...unless you know what you're doing)           #
#################################################################################################  
# Load libraries
source("Functions.r")
libs <- libraries() 

# Read IDL output and setting variables from metadata files
###########################################################

# These settings are taken from the IDL output (default)
Settings      <- read.table(file="settings.csv"    , header=FALSE, sep = ",", quote="\"",stringsAsFactors=FALSE)
Process.Info  <- read.table(file="process_info.csv", header=FALSE, sep = ",", quote="\"",stringsAsFactors=FALSE)

# Load Folders
  ###############################################################################################
  # Directory of script location
  File.Dir <- paste(getwd(),"/",sep="")  
  # Split Filedir in character vector including all folders of the path
  Folders  <- Pathbreakdown(File.Dir)
  # Set IDL script folder
  IDL      <- Settings[1,2]
  # Project folder from IDL
  p.Temp   <- Settings[2,2]
  # Extract name of product folder
  Product  <- Pathbreakdown(p.Temp)
  P.Dummy  <- grepl(Product[length(Product)],Folders, fixed=TRUE)
  # Which number has this name in Folders vector?
  P.n      <- which(P.Dummy==TRUE)
  # Create path to project folder
  Product  <- file.path(paste0(Folders[1:P.n],collapse="\\"),'\\',fsep = "")
  
  # important subfolders
  Index.Dir <- file.path(paste0(Folders[1:(length(Folders)-1)],collapse="\\"),"\\",fsep="")  
  Tiff.Dir  <- file.path(paste0(Folders[1:(length(Folders)-2)],collapse="\\"),"\\Photos\\",fsep="")
  Meta.Dir  <- file.path(Product, "metadata\\",fsep="")  
  # create hash of important folders
  PATH <- hash(  keys=c("Product","Index","Script","Tiff","Meta"),
               values=c(Product,Index.Dir,File.Dir,Tiff.Dir,Meta.Dir)
               )
  N.Set <- nrow(Settings)
  # Path of the spectral indices (gsub replaces original project folder with true project folder)
  ENVI.Path <- gsub(file.path(Product,"SpectralIndices\\",fsep=""), Index.Dir,Settings[15:N.Set,2],fixed=TRUE)
  EP        <- hash(keys = gsub("_","",Settings[15:N.Set,1]),values = ENVI.Path)
  # cleanup
  rm(list=c("p.Temp","P.Dummy","P.n","ENVI.Path","libs","Folders"))
  ###############################################################################################
  # Load Spectral Index information
  Index.CSV <- read.table(file.path(Meta.Dir, "Spectral_Indices.csv",fsep=""), 
                         header=FALSE, sep = ",", quote="\"",stringsAsFactors=FALSE)
  ###############################################################################################
    # number of indices
    nc <- ncol(Index.CSV)
  
    # Types of Indices
    I.Types <- Index.CSV[grep('Index_Type',Index.CSV[,1], fixed=TRUE),2:nc]
    # Line of the Graph output for RABA
    Output.RABA <- grep('Graph_output',Index.CSV[,1], fixed=TRUE)
    # check for RABA
    RABA.Present <- grep('Relative absorption band area',Index.CSV[grep('Index_Type',Index.CSV[,1], fixed=TRUE),])
    RABA.Title   <- character(nc-1)
    RABA.File    <- character(nc-1)
    RABA.Folder  <- character(nc-1) 
    if(length(RABA.Present)!=0){
       RABA.Title[RABA.Present-1] <- paste(' (',Index.CSV[Output.RABA,RABA.Present],')',sep="")
        RABA.File[RABA.Present-1] <- paste('_',t(Index.CSV[Output.RABA,RABA.Present])  ,sep="")
      RABA.Folder[RABA.Present-1] <- paste(t(Index.CSV[Output.RABA,RABA.Present]),'\\' ,sep="")
      # Check for plain RABA (which doesn't contain a file expansion) and remove the entries.
      is.RABA <- vector((nc-1),mode='integer')
      for (i in 1:(nc-1)){is.RABA[i] <- match('_RABA',RABA.File[i])}
      if(anyNA(is.RABA)==TRUE){
        is.RABA <- which(is.finite(is.RABA)==T)
          RABA.File[is.RABA] <- c('')
        RABA.Folder[is.RABA] <- c('')
      }
    }
    
    # Hash of index types
    I.Types     <- hash(keys   = (Index.CSV[1,c(2:nc)]),
                        values = I.Types)
    # Hash of output graphs (needed for RABA container)
    G.Types     <- hash(keys   = (Index.CSV[1,c(2:nc)]),
                        values = paste(' (',Index.CSV[Output.RABA,c(2:nc)],')',sep=""))
    # find subscripts of colors and build hashtable with values
    c.Index     <- grep("Rcolor",Index.CSV[,1], fixed=TRUE)
    Line.Colors <- hash(keys   = (Index.CSV[1,c(2:nc)]),
                        values = gsub("_","",(Index.CSV[c.Index,c(2:nc)]),fixed=TRUE))
    Line.Colors$Other.Data <- ODL.Color
    
    # find subscripts of main titles and build hashtable with values
    Mt.Index    <- grep("Main_Titles",Index.CSV[,1], fixed=TRUE)
    Main.Titles <- hash(keys   = Index.CSV[1,c(2:nc)],
                        values = paste(Index.CSV[Mt.Index,c(2:nc)],RABA.Title, sep =""))
   
    # Add Other.Data title to maintitles hash
    Main.Titles$Other.Data <- OD.Title    

    # find subscripts of Sub titles and build hashtable with values
    St.Index   <- grep("Description",Index.CSV[,1], fixed=TRUE)
    Sub.Titles <- hash(keys   = Index.CSV[1,c(2:nc)],
                       values = Index.CSV[St.Index,c(2:nc)])
    
    # Build hashtable with metadata (md)
    md <- hash(keys   = c('Mean.Color','Sd.Color','Line.Style','Background','File.Unit','X.Label','Y.Label.L',
                          'Y.Label.R','Main.Title.m','X.Label.m','Y.Label.L.m','Y.Label.R.m'),
               values = c(Mean.Color,Sd.Color,Line.Style,Background,File.Unit,X.Label,Y.Label.L,
                          Y.Label.R,Main.Title.m,X.Label.m,Y.Label.L.m,Y.Label.R.m))
    # Build dimension hash to store all dimension variables
    dims <- hash(keys=Settings[5:14,1], values=as.numeric(Settings[5:14,2]))
    # Build a hash table containg core depth sequences
    seqs <- hash()
    
    #Add some variables to md hash
    ##############################
    md$Tiff.Mode <- Tiff.Mode
    md$k         <- k
    md$Output    <- Output.Mode
    md$Sub.Type  <- Subset.Type
    md$Bsub      <- Bsub 
    md$Path      <- Product
    md$Graph.Out <- Graph.Out
    # Misc metadata from process_info.txt and settings.csv
    #######################################################
    # Name of the Lake from process_info.txt
    md$Lake.Name    <- gsub('|',',',Process.Info[1,2], fixed=T)
    # Filename with date
    md$File.Base.Name <- Settings[3,2]
    md$Processing     <- Settings[4,2]
    # Core base name without date
    md$Core.Name      <- Process.Info[3,2]
    # Field of View of the scan
    md$FOV            <- as.numeric(Process.Info[ 8,2])
    md$Paper          <- Paper.Size
    md$Res            <- Paper.Res
    
    # clear unneeded variables
    rm(list=c("c.Index","Mt.Index","St.Index","Tiff.Mode","Output.Mode","Bsub",
              "OD.Title","nc","ODL.Color","Paper.Size","Paper.Res",'Mean.Color','Sd.Color','Line.Style',
              'Background','File.Unit','X.Label','Y.Label.L','Y.Label.R','Main.Title.m','X.Label.m',
              'Y.Label.L.m','Y.Label.R.m','Subset.Type','Output.RABA','Graph.Out'))
    
    # Misc dimension parameters
    ###########################
    # Pixel size
    dims$PS <- as.numeric(Process.Info[10,2])
    # Starting line within the core with respect to the SUBSETTED dataset (unit: mm)
    #Y.Start
    if(Y.Window.Start == 0) {dims$Y.Start <- 0
    } else {dims$Y.Start <- floor(Y.Window.Start/dims$PS)}
    #Y.Stop
    # Total number of points (dimensions of the image subset)
    if(Y.Window.End == 0) {dims$Y.Stop  <- dims$B_nl
    } else {dims$Y.Stop <- floor(Y.Window.End/dims$PS)}
    
    # Length parameters of the Core
    ###############################
    dims$M.Core.St <- Multi.Core.Start
    # Length of the core in mm      
    dims$Core.Length <- dims$B_nl*dims$PS
    # number of pixels in the subset
    dims$nsub        <- dims$Y.Stop-dims$Y.Start            
    # Core.length in millimeter (from starting point (Y.Start))
    dims$Sub.Length  <- dims$nsub*dims$PS
    # Start/Stop of the subset in millimeter
    dims$Sub.Start   <- dims$Y.Start*dims$PS
    dims$Sub.Stop    <- dims$Y.Stop*dims$PS    
    
    # Sediment length in millimeter (multiple cores)
    dims$Sedi.Length <- dims$M.Core.St + dims$Core.Length
    # Start/Stop of the core for multiple cores
    dims$Sedi.Start  <- dims$M.Core.St + dims$Sub.Start
    dims$Sedi.Stop   <- dims$M.Core.St + dims$Sub.Stop
    
    # Vector sequences to be stored in seqs hash
    ############################################
    # Pixel index number in output matrices
    seqs$Oid      <- seq(from=dims$Y.Start, to=(dims$Y.Stop-1), by=1)
    # Vector of Core.length in millimeter (from starting point (Y.Start))
    seqs$Sub.Y <- seq(from=0, to=dims$Sub.Length-dims$PS, by=dims$PS)
    # Vector of Core length per millimeter (whole image)    
    seqs$Core.Y   <- seq(from=dims$Sub.Start, to=dims$Sub.Stop-dims$PS, by=dims$PS)
    # Vector of Sediment length in millimeter (multiple cores)
    seqs$Sedi.Y   <- seq(from = dims$M.Core.St + dims$Sub.Start, 
                         to   = dims$M.Core.St + dims$Sub.Stop - dims$PS, by=dims$PS)
    
    # cleanup
    rm(list=c("Y.Window.Start","Y.Window.End","Product","Multi.Core.Start"))
  
    #################################################  
    
    md$Sub.Main.Title.m    <- paste('Mean values of',as.character(dims$S_X2-dims$S_X1), 'samples', sep=" ")	
    md$Sub.Main.Title.MA.m <- paste('Moving average (k=',as.character(k),')',sep=" ")	
    md$Sub.Title.m         <- paste('Lake sediment core:', md$Core.Name, sep=" ")
    
    # Folder names of indices for processing
    t.Index <- Settings[15:N.Set,1]
    if (length(User.Indices)==1){# CHeck state of User.Indices and adjust t.Index
      if(User.Indices != "all" & User.Indices!="none"){t.Index <- User.Indices}
    } 
    
    # Paths to shw subsets, Tiffs and to result output
    SHW.Path     <- file.path(Index.Dir,t.Index,"\\ENVI\\",md$File.Base.Name,"_shw",md$Processing,
                              "_",t.Index,RABA.File,"_spl_",dims$S_X1,"_",dims$S_X2,".dat",fsep="")
    # Hash for Index names and SHW files
    SP           <- hash(keys = gsub("_","",Settings[15:N.Set,1]),values = SHW.Path)
    # Fully qualified paths to the index tiff files
    Index.Tiffs  <- file.path(Index.Dir,t.Index,"\\Tiff\\",RABA.Folder,md$File.Base.Name,md$Processing,"_",
                              t.Index,RABA.File,"_ct.tif",fsep="")
    IT           <- hash(keys = gsub("_","",Settings[15:N.Set,1]),values = Index.Tiffs)
    # Fully qualified output paths to results, without file ending
    Result.Paths <- file.path(Index.Dir,t.Index,"\\Results\\",RABA.Folder,md$File.Base.Name,md$Processing,
                              "_",t.Index,RABA.File,"_spl_",dims$S_X1,"_",dims$S_X2,fsep="")
    RP           <- hash(keys = gsub("_","",Settings[15:N.Set,1]),values = Result.Paths)
    
    for(j in (1:length(Result.Paths))){dir.create(dirname(Result.Paths[j]),showWarnings = F)}
    # Handle the subset type
    ########################
    if (md$Sub.Type=="small"){
      # check if a small subset exists and use big subset if false.
      check <- which(file.exists(values(SP))==FALSE, arr.ind =TRUE)
      if (sum(check)>0){md$Sub.Type=="big"}
      md$Bsub <- c(dims$SubX1,dims$SubX2)
      rm(list=c("check"))
    }else{
      # if user chose big subset with standard settings, set the profile range.
      if(sum(md$Bsub)==0){md$Bsub <- c(dims$SubX1,dims$SubX2)}
    }                              
     
    #cleanup
    rm(list=c("t.Index","Index.Dir","File.Dir","Tiff.Dir","Meta.Dir","k","RABA.File","RABA.Folder",
              "RABA.Present","RABA.Title","SHW.Path"))

# Read external Data source if available
####################################################################################################
  if (file.exists(Other.Data)==TRUE){
    #Output folder name of individual data plots
    FName <- c("external_Data_Results")
    dir.create(file.path(PATH$Index,FName,"\\",fsep=""),showWarnings = FALSE)
    # Read external Data
    Data   <- read.table(file=Other.Data, header=TRUE, sep = ",", quote="\"")  
    Header <- colnames(Data)
   
   odatares <- c("")
    for(i in (2:ncol(Data))){
      odatares[(i-1)] <- file.path(PATH$Index,FName,"\\",md$Core.Name,"_",Header[i],fsep="")
    }
    
  Param_odata <- cbind(Header[2:ncol(Data)],
                         rep(file.path(PATH$Script,Other.Data,fsep=""),times=(ncol(Data)-1)),
                         rep(file.path(PATH$Script,Other.Data,fsep=""),times=(ncol(Data)-1)),
                         rep(Line.Colors$Other.Data,times=(ncol(Data)-1)),
                         rep(Main.Titles$Other.Data,times=(ncol(Data)-1)),
                         colnames(Data[2:ncol(Data)]),
                         rep(file.path(PATH$Tiff,Tiff.Mode1[1],"\\",md$File.Base.Name,"_",
                                       Tiff.Mode1[1],"_",Tiff.Mode1[2],".tif",fsep=""),times=(ncol(Data)-1)),
                         odatares,
                         rep("external",times=(ncol(Data)-1)),
                         rep("",        times=(ncol(Data)-1))
                         )
  }
    
# Sort indices and prepare container for output function    
########################################################
  # Sort Indices according to idl or user specific 
  if(length(User.Indices) == 1){
    # User.Indices gets a new meaning beyond this point.
    switch(User.Indices,
      none={Parameters <- c()
            nsi <- 0
            User.Indices <- FALSE
           },
       all={Index <- Settings[15:N.Set,1]
            User.Indices <- TRUE
           },
           {Index <- User.Indices
            User.Indices <- TRUE
           }
    )
  }else{
    Index <- User.Indices
    User.Indices <- FALSE
    }
  
  if(User.Indices == TRUE ){# handle alternative sorting and index calculation
    # Sort Indices of hashes  
    gtypes     <- character(length(Index))
    itypes     <- character(length(Index))
    colors     <- character(length(Index))
    mTitles    <- character(length(Index))
    sTitles    <- character(length(Index))
    ENVI.Path  <- character(length(Index))
    SHW.Path   <- character(length(Index)) 
    iTiffs     <- character(length(Index))
    rPaths     <- character(length(Index))
    
    
    for (i in (1:length(Index))) {        
      gtypes[i]    <- values(    G.Types[Index[i]])
      itypes[i]    <- values(    I.Types[Index[i]])
      colors[i]    <- values(Line.Colors[Index[i]])
      mTitles[i]   <- values(Main.Titles[Index[i]])
      sTitles[i]   <- values( Sub.Titles[Index[i]])
      ENVI.Path[i] <- values(         EP[Index[i]])
      SHW.Path[i]  <- values(         SP[Index[i]])
      iTiffs[i]    <- values(         IT[Index[i]])
      rPaths[i]    <- values(         RP[Index[i]])
    }
    
   # Output of parameters needed for the Spectral indices function
    Parameters   <-  cbind(Index,                      
                           ENVI.Path,
                           SHW.Path,
                           colors,
                           mTitles,
                           sTitles,
                           iTiffs,
                           rPaths,
                           itypes,
                           gtypes
                           )
    # Number of spectral indices
    nsi <- nrow(Parameters)
    #cleanup
    rm(list=c("Index","ENVI.Path","SHW.Path","colors","mTitles","sTitles",'itypes',"gtypes","iTiffs","rPaths"))
  }
  
  # handle cracks
  if (exists("cracks")){cracks <- cracks}else{cracks <- NA}
  
  #cleanup obsolete hashes and variables
  clear(c("Line.Colors","Main.Titles","Sub.Titles","EP","SP","I.Types","G.Types","RP","IT"))
  rm(list=c("N.Set","Index.Tiffs","Result.Paths","Process.Info","Settings","Index.CSV","i",
            "User.Indices",'Tiff.Mode1'))

  #Add other data to Parameters
  if (file.exists(Other.Data)==TRUE){Parameters <- rbind(Parameters,Param_odata)}
  #Name the columns
  colnames(Parameters) <- c("Index",      "ENVI.Path",   "SHW.Path",   
                            "Line.Colors","Main.Titles", "Sub.Titles", 
                            "Tiff.PATH",  "Result.Path", "Index.Type","Graph.Type")
  
  # Make a container for other RABA plots
  #######################################
  Check.RABA <- grep('Relative absorption band area',Parameters[,"Index.Type"])
  if(length(Check.RABA)!=0){
    RABA.Param <- matrix(data="",nrow=4*length(Check.RABA),ncol(Parameters))
    colnames(RABA.Param) <- colnames(Parameters)
    RABA.Result <- character(length(Check.RABA))
    l <- c(0)
    for(i in 1:length(Check.RABA)){
      # Replicate the ith RABA parameters 4 times
      RABA.Param[(i+l):(i+3*i),] <- rep(Parameters[Check.RABA[i],],each=4)
      # File appendices
      RABA.Files <- c("","_RABAm","_RABD","_RABDm")
      # Paths to big subsets 
      RABA.Param[(i+l):(i+3*i),"ENVI.Path"]   <- file.path(dirname(RABA.Param[(i+l):(i+3*i),"ENVI.Path"]),"/",md$File.Base.Name,
                                                     md$Processing,"_",RABA.Param[(i+l):(i+3*i),1],RABA.Files,".dat",fsep="")
      # Paths to shw subsets 
      RABA.Param[(i+l):(i+3*i),"SHW.Path"]    <- file.path(dirname(RABA.Param[(i+l):(i+3*i),"SHW.Path"]),"/",md$File.Base.Name,"_shw",
                                                     md$Processing,"_",RABA.Param[(i+l):(i+3*i),1],RABA.Files,"_spl_",
                                                     dims$S_X1,"_",dims$S_X2,".dat",fsep="")
      RABA.Folders <- c("","RABAm\\","RABD\\","RABDm\\")
      # Fully qualified paths to the RABA index tiff files
      RABA.Param[(i+l):(i+3*i),"Tiff.PATH"]   <- file.path(PATH$Index,RABA.Param[(i+l):(i+3*i),1],"\\Tiff\\",RABA.Folders,
                                                     md$File.Base.Name,md$Processing,"_",RABA.Param[(i+l):(i+3*i),1],
                                                     RABA.Files,"_ct.tif",fsep="")
      # Fully qualified output paths to results, without file ending
      RABA.Param[(i+l):(i+3*i),"Result.Path"] <- file.path(PATH$Index,RABA.Param[(i+l):(i+3*i),1],"\\Results\\",RABA.Folders,
                                                     md$File.Base.Name,md$Processing,"_",RABA.Param[(i+l):(i+3*i),1],
                                                     RABA.Files,"_spl_",dims$S_X1,"_",dims$S_X2,fsep="")
      # Path to index folder results
      RABA.Result[i] <- file.path(PATH$Index,RABA.Param[(i+l),1],"\\", fsep="")
      # create the subfolders if not existing
      for(j in (1:nrow(RABA.Param))){dir.create(dirname(RABA.Param[j,"Result.Path"]),showWarnings = F)}
      # change Maintitles for Raba plots
      RABA.Titles <- c(" (RABA)"," (RABAm)"," (RABD)"," (RABDm)")
      New.Titles  <- character(4)
      for(j in 1:4){New.Titles[j] <- gsub(RABA.Param[i+l,"Graph.Type"],RABA.Titles[j],RABA.Param[i+l,"Main.Titles"],fixed=TRUE)}
      RABA.Param[(i+l):(i+3*i),"Main.Titles"] <- New.Titles
      RABA.Param[(i+l):(i+3*i),"Index"]       <- paste(RABA.Param[(i+l):(i+3*i),1],RABA.Titles, sep="")
      # append index with Raba graph
      Parameters[Check.RABA[i],"Index"]       <- paste(Parameters[Check.RABA[i],"Index"],      Parameters[Check.RABA[i],"Graph.Type"], sep=" ")
      l <- (l+3)
    }
    rm(list=c("RABA.Files","RABA.Folders","RABA.Titles","New.Titles","i","j"))
  }
  
  
  # Add AgeDepth
    if (file.exists(Age.Depth)==TRUE){
      # Read external Data
      Age_Depth     <- read.table(file=Age.Depth, header=TRUE, sep = ",", quote="\"")
      md$Y.Label.L   <- c("Time [estimated]")
      md$Y.Label.L.m <- c("Time [estimated]")
    }else{Age_Depth <- NA}
#######################################################################################################
#                Output of Report                                                                     #
#######################################################################################################
if(file.exists(file.path(IDL,"Report\\Corereport_fct.r"))==TRUE){
  In.File   <- file.path(IDL,"Report\\Core_report_template.odt",fsep="")  
  Out.File  <- file.path(PATH$Script,"Report_",md$Core.Name,".odt",fsep="")
  Ctrl      <- odfWeaveControl(zipCmd =c("\"C:\\Program Files\\7-Zip\\7z.exe\" a $$file$$",
                                         "\"C:\\Program Files\\7-Zip\\7z.exe\" x -tzip $$file$$"))
  source(file.path(IDL,"Report\\Corereport_fct.r",fsep=""))
  odfWeave(In.File, Out.File,control = Ctrl)
  
  rm(list=c("Adate","Stype","S_ID","tint","fps","Bands","Srange","Sres","Core.Length",
            "Sedi.Length","table","currentDefs","currentStyles","tableStyles","IDL"))
            
}

########################################################################################################
#                Calculate Indices output                                                              #
########################################################################################################


if (Format.Paper==TRUE){
  outprogtitle <- c("Paper format spectral index output")
  .pb <- winProgressBar(title=outprogtitle, 
                       label="0% done", min=0, max=100, initial=0)
  # Calculate plots and time series in A4 paper size
  SpectralIndices(Parameters,PATH,dims,md,cracks,nsi,Age_Depth, Format='Paper')
  warnings()
}
if (Format.Core==TRUE){
  outprogtitle <- c("Core format spectral index output")
  .pb <- winProgressBar(title=outprogtitle, 
                       label="0% done", min=0, max=100, initial=0)
  # Calculate plots and time series in HIGH resolution (original size)
  SpectralIndices(Parameters,PATH,dims,md,cracks,nsi,Age_Depth, Format='Core')
}
l <- c(0)
for(i in 1:length(RABA.Result)){
  PATH$Index <- RABA.Result[i]
  if (Format.Paper==TRUE){
    outprogtitle <- c("Paper format spectral index Raba output")
    .pb <- winProgressBar(title=outprogtitle, 
                          label="0% done", min=0, max=100, initial=0)
    # Calculate plots and time series in A4 paper size
    SpectralIndices(RABA.Param[(i+l):(i+3*i),],PATH,dims,md,cracks,nsi,Age_Depth, Format='Paper')
    warnings()
  } # ENDIF
  if (Format.Core==TRUE){
    outprogtitle <- c("Core format spectral index Raba output")
    .pb <- winProgressBar(title=outprogtitle, 
                          label="0% done", min=0, max=100, initial=0)
    # Calculate plots and time series in HIGH resolution (original size)
    SpectralIndices(RABA.Param[(i+l):(i+3*i),],PATH,dims,md,cracks,nsi,Age_Depth, Format='Core')
  } # ENDIF
  l <- (l+3)
} # ENDFOR 
  
rm(list=ls())
#cat("\014")
###############################
# END OF ROUTINE
