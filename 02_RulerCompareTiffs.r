#################################################################################################
#         Function to add a scale to the subset tiffs and/or to compare them with others        #
#                         ###############################################                       # 
#                                   Created by Christoph Butz                                   #
#                                       04.February 2015                                        #
#                                 Christoph.Butz@giub.unibe.ch                                  #
# Last Modified: 09.01.2017                                                                     #
#################################################################################################
# Install and activate rstudioapi - used to set the working directory. requires Rstudio 0.99.9x
lib <- library("rstudioapi", logical.return=TRUE)
if(lib==FALSE) install.packages("rstudioapi"); library("rstudioapi")
rm(list=c("lib"))
# automatically set the working directory on script location.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#################################################################################################
#                     Manual settings                                                           #
#################################################################################################

# Mandatory settings
#########################

# Pick tif files 
# Single file:    Add scalebar and some text
# Array of files: Add scalebar and plot tiffs next to each other
#                 You may enter as many tiffs as you prefer. 
#                 
# Options:        You can use any combination of "RGB", "NIR" or "CIR" with
#                 "sqrt", "linear", "adheq","adheq_fft" or "log" 
#                 OR: "'Index'", with "" or "ct"
#                 Replace 'Index' with a spectral index in the SpectralIndices folder
#                 The second term determines, whether to use the gray scale or the colour mapped 
#                 index tiff.
#                 You can use "ext" in combination with a file name (e.g. c("ext","Test.tif")).
#                 For this you need to specify the variable "Ext.Tiff.Folder" and to place the tiffs you want to use there.

Tiffs <- rbind(c("RGB","linear")
               #,c("RABD845","ct")
               #,c("ext", "Test.tif")
          )
# if variable "ext" is chosen for Tiffs, copy the external tiff files to a folder 
# and specify the folder (Full Path) in this variable (Make sure to use "\\" instead of "\" for the path separation):
Ext.Tiff.Folder <- c("D:\\Core Samples\\Group\\Test\\Products\\SpectralIndices\\externalTiffs")

# Output filename (without file ending) 
FileName <- c("Photo_comparison")

#Output Folder name will be created in ...\SpectralIndices\
   FName <- c("Tiffcomparison")


# If sediment consists of multiple cores specify the depth here (scalebar starts from there)
  Multi.Core.Start <- 0 # depth in mm (look up from results of previous core)

# With the following variables you are able to set specific y coordinates to produce the outputs 
  # If the variables are Zero the coordinates of the whole core will be used.(Unit: mm)
  Y.window.start <- 0
  Y.window.end   <- 0

# Specify x-coordinates of the images (unit: mm)    
  X.window.start <- 0
  X.window.end   <- 0
  


# output settings 
#########################

# Graph labels
  Main.title       <- c("Photo comparison")  
  Sub.Main.title.m <- c("")  
  Sub.title.m      <- paste("- Lake sediment core: ", "$$.Core.name$$", " -", sep="")   
  Y.label1         <- c("Depth [mm]")
  Y.label2         <- c("Depth [mm]")
  File.unit        <- c("px")    # Pixel
  Background       <- c("white") # c("transparent")
  Texttype         <- c("serif") # serif = Times New roman, sans = Arial
  
# Add Ages from Age depth model. AgeDepth.csv file with time in years and depth in mm
  AgeDepth <- c("AgeDepth1.csv")
  
#################################################################################################
#                     Miscellaneous    (no edit needed)                                         #
#################################################################################################
  # Load Libraries
  source("Functions.r")
  libraries() 
  
#################################################################################################
#                     automatic settings                                                        #
#################################################################################################

# Reading IDL output and setting variables
##########################################
  # These settings are taken from the IDL output (default)
  settings     <- read.table(file="settings.csv",    header=FALSE, sep = ",", quote="\"",stringsAsFactors=FALSE)
  process_info <- read.table(file="process_info.csv",header=FALSE, sep = ",", quote="\"",stringsAsFactors=FALSE)
    
  #Sediment info
  General      <- hash(keys   = c("Main.title","Sub.Main.title.m","Sub.title.m",
                                  "Y.label1","Y.label2","File.unit","Background","Texttype"),
                       values = c(Main.title,Sub.Main.title.m,Sub.title.m,
                                  Y.label1,Y.label2,File.unit,Background,Texttype))
    #Add some variables to General hash
    General$M.core.st  <- Multi.Core.Start
    General$Tiffmode   <- Tiffs
    # Misc variables from process_info.txt and settings.csv
    #######################################################
    # Name of the Lake from process_info.txt
    General$Lake.name    <- process_info[1,2]
    # Filename with date
    General$Filebasename <- settings[3,2]
    General$processing   <- settings[4,2]
    # Core base name without date
    General$Core.name    <- process_info[3,2]
    # Field of View of the scan
    General$FOV          <- as.numeric(process_info[8,2])   
    # Pixel size [mm]
    General$PS           <- as.numeric(process_info[10,2])
    # Number of Total pixels in y-direction
    General$n            <- as.numeric(settings[14,2])
    # Starting/end point of the sediment with respect to the ORIGINAL dataset (unit: Pixel)
    Core.start           <- as.numeric(settings[13,2])
    Core.end             <- General$n+Core.start   
    # File name and time stamp
    General$Date         <- format(Sys.time(), "%d %b %Y") 
    General$FileName     <- FileName
    # Length of the Core in millimeter
    General$Clmm         <- General$n*General$PS
  
  # Starting Line within the core with respect to the SUBSETTED dataset (unit: mm)
  #Y.Start
  if(Y.window.start == 0) {
    Y.Start <- 0
  }else{
    Y.Start <- floor(Y.window.start/General$PS)
   }
  #Y.Stop
  # Total number of points (Dims of the Image subset)
  if(Y.window.end == 0) {
    Y.Stop <- General$n
  }else{
    Y.Stop <- floor(Y.window.end/General$PS)
  }
   
  # container for dimensions
  dims <- c(Y.Start,Y.Stop,Core.start,Core.end)
  
  # Filename  
  General$Sub.title.m <- sub("$$.Core.name$$", General$Core.name,General$Sub.title.m,fixed=TRUE )
  
  # Folders
  #########  
  # Directory of script location
  Filedir <- paste(getwd(),"/",sep="")  
  # Split Filedir in character vector including all folders of the path
  Folders <- Pathbreakdown(Filedir)
  # Product folder from IDL
  p.temp  <- settings[2,2]
  # Extract name of Product Folder
  Product <- Pathbreakdown(p.temp)
  P.dummy <- grepl(Product[length(Product)],Folders, fixed=TRUE)
  # Which number has this name in Folders vector?
  P.n     <- which(P.dummy==TRUE)
  # Create Path to Product folder
  Product <- file.path(paste0(Folders[1:P.n],collapse="\\"),'\\',fsep = "")
  
  # important subfolders
  Indexdir <- file.path(paste0(Folders[1:(length(Folders)-1)],collapse="\\"),"\\",fsep="")  
  Tiffdir  <- file.path(paste0(Folders[1:(length(Folders)-2)],collapse="\\"),"\\Photos\\",fsep="")
  Metadir  <- file.path(Product, "metadata\\",fsep="")  
  # create hash of important folders
  PATH <- hash(keys=c("Product","Index","Script","Tiff","Meta"),
               values=c(Product,Indexdir,Filedir,Tiffdir,Metadir)
               )  
  #create paths
    dir.create(file.path(PATH$Index,FName,as.character("\\"),fsep=""),showWarnings = FALSE)
  PATH$Out <- (file.path(PATH$Index,FName,as.character("\\"),fsep=""))
  
  # Folder names of indices
  t.index  <- as.character(settings[15:nrow(settings),1])  
  # Paths to Tiffs and to Result output
    IndexTiffs    <- file.path(PATH$Index,t.index,"\\Tiff\\",General$Filebasename,General$processing,"_",
                               t.index,".tif",fsep="")
    IndexTiffs_ct <- file.path(PATH$Index,t.index,"\\Tiff\\",General$Filebasename,General$processing,"_",
                               t.index,"_ct.tif",fsep="")
                              
 iTiffs    <- hash(keys   = t.index,
                   values = IndexTiffs)
 iTiffs_ct <- hash(keys   = t.index,
                   values = IndexTiffs_ct)

  Tiffs <- c()
  for (i in 1:nrow(General$Tiffmode)){
    # Handle Photos and external tiffs
    switch(General$Tiffmode[i,1],
           RGB={
             Tiffs[i] <- file.path(PATH$Tiff,General$Tiffmode[i,1],"\\",General$Filebasename,"_",
                                   General$Tiffmode[i,1],"_",General$Tiffmode[i,2],".tif",fsep="")
           },
           CIR={
             Tiffs[i] <- file.path(PATH$Tiff,General$Tiffmode[i,1],"\\",General$Filebasename,"_",
                                   General$Tiffmode[i,1],"_",General$Tiffmode[i,2],".tif",fsep="")
           },
           NIR={
             Tiffs[i] <- file.path(PATH$Tiff,General$Tiffmode[i,1],"\\",General$Filebasename,"_",
                                   General$Tiffmode[i,1],"_",General$Tiffmode[i,2],".tif",fsep="")
           },
           ext={Tiffs[i] <- File.search(General$Tiffmode[i,2], Ext.Tiff.Folder)
             if (length(Tiffs[i])==0){cat("warning: external Tiff file not found")}
           },
           {}# ELSE
    )
    # Handle Index Tiffs
    switch(General$Tiffmode[i,2],
           ct={Tiffs[i] <- values(iTiffs_ct[General$Tiffmode[i,1]])},
           bw={Tiffs[i] <- values(   iTiffs[General$Tiffmode[i,1]])},
           {} # ELSE
           )
  } 
  
  # intermediate cleanup
  rm(list=c("Main.title","Sub.Main.title.m","Sub.title.m","Y.label1","Y.label2","File.unit","Background","Indexdir",
            "Filedir","Tiffdir","Metadir","Core.start","Core.end","Y.window.start","Y.window.end","P.dummy",
            "p.temp","FName","Folders","Texttype","Y.Start","Y.Stop","Multi.Core.Start","Product",
            "P.n","FileName","i","t.index","IndexTiffs","IndexTiffs_ct","iTiffs","iTiffs_ct"))
  
  # Add Age Depth
    if (file.exists(AgeDepth)==TRUE){
      # Read external Data
      Age_Depth        <- read.table(file=AgeDepth, header=TRUE, sep = ",", quote="\"")
      General$Y.label1 <- c("Time - estimated positions")
    }
  
  #########################################################################################################
  # Output Papersize and general dimensions
  #########################################################################################################

    # Native Resolution of data pixels-per-inch (ppi)
    Resolution <- (1/General$PS)*25.4
        
    # number of pixels in the subset
    nsub            <- dims[2]-dims[1]
    # length of the subset in millimeter
    Subset.Start    <- dims[1]*General$PS
    Subset.Stop     <- dims[2]*General$PS    
    # Sediment length in millimeter (multiple cores)
    Sediment.Y      <- seq(from = General$M.core.st + Subset.Start, 
                           to   = General$M.core.st + Subset.Stop - General$PS, by=General$PS)
    Sediment.Start  <- General$M.core.st + Subset.Start
    Sediment.Stop   <- General$M.core.st + Subset.Stop       
    
    
    # Load Tiff & meta Data
    ##########################  
    Image <- readTiff(Tiffs[1]) 

    # Calculate output size and resolution
    #retrieve pixmap size (use "@" instead of "$" to address class4 structure)
    pmx     <- Image@size[2]
    pmy     <- Image@size[1]  
    
    nticks  <- 10
    prettyb <- 5
    yt      <- seq(from=(round(Sediment.Start,digits=0)), to=(round(Sediment.Stop, digits=0)), by=10)
    yt      <- pretty(yt, n=length(yt))
    if(yt[1]<Sediment.Start){yt[1] <- Sediment.Start}
    if(yt[length(yt)]> Sediment.Stop){yt[length(yt)] <- floor(Sediment.Stop)}else{yt <- c(yt,floor(Sediment.Stop))}
   
    
    if(file.exists(AgeDepth)==TRUE){
      yt1 <- closest_values(Age_Depth[,2],yt,0)
      yt1 <- Age_Depth[yt1,1]
      yt2 <- yt
    }else{
      yt1 <- yt
      yt2 <- yt
    }
    
    # Plot settings concerning the size of the plot
    ##################################################
      # Y Frame size of the image (2*1inch*Resolution) upper frame + lower frame
      Fsize_y       <- ceil(2*1.25*Resolution)
      # Size of the Y outer margin in inch 
      outermargin   <- c(1.5,0.5,1,0.5)      
      # Size of the title characters depending on Image resolution
      if (Resolution < 400){Tcex  <- 2}else{Tcex  <- 1}
      # Character size
      Chars    <- 5    
      # Main textbody expension factor
      Mcex     <- 1
      # Axis label expansion factor
      Acex     <- 2
      # X Frame size of the image: left frame + right frame
      Fsize_x  <- ceil(2*0.5*Resolution)
      # Size of inner Margin in Pixel needed for index titles in multiplots
      Mai_size <- ceil(0.8*Resolution) 
      # Line of Text coordinates
      Lines    <- c(1.0,1.5,4.5,3,1.5,2.5,3.5,5,6,7,5,6,8,9,10,6)          
      # Tiff size from file: standard Tiffsize is 600 Pixels c(start,End) 
      #Tiff.x   <- c(1,pmx-1)     CHANGED TO DIMS
      
      # Starting Line within the core with respect to the SUBSETTED dataset (unit: mm)
      #X.Start
      if(X.window.start == 0) {
        X.Start <- 1
      }else{
        X.Start <- floor(X.window.start/General$PS)
      }
      #X.Stop
      # Total number of points (Dims of the Image subset)
      if(X.window.end == 0) {
        X.Stop <- pmx-1
      }else{
        X.Stop <- floor(X.window.end/General$PS)
      }
      dims <- c(dims,X.Start,X.Stop)
      
      # x axis labels and position 10mm based.
      X  <- runif(nsub,min=(X.Start*General$PS),max=(X.Stop*General$PS))
      xt <- pretty(X)#,n=xticks)
      if(xt[1]<(X.Start*General$PS)){xt <- xt[-1]}
      if(xt[length(xt)]>(X.Stop*General$PS)){xt <- xt[-length(xt)]}
      
      # Size of Output png's in Pixel 
      ###############################      
      # Size of multi plot
      png.width  <- (nrow(General$Tiffmode))*(dims[6]-dims[5])+Fsize_x     # Pixel
      png.height <- nsub+2+Fsize_y+Mai_size                  # Pixel 
  
  #progressbar
  outprogtitle <- c("Plotting Tiffs ")
  .pb <- winProgressBar(title=outprogtitle, 
                       label="0% done", min=0, max=100, initial=0)

#################################################################################################
#################################################################################################
#################################################################################################
#                                                                                               #
#                                                Plot                                           #
#                                                                                               #
#################################################################################################
#################################################################################################
#################################################################################################

  
  ###############################################################################################
  #                                      Export Plots to png                                    #
  ###############################################################################################
  count        <- nrow(General$Tiffmode)
  if(count>1){
   Layoutmatrix <- matrix(t(c(seq(from=1, to=count))),
                            nrow=1,
                            ncol=count,
                            byrow=TRUE
                         )
  }
  # plot 
  png(filename = Add_Date(paste(General$Core.name,"_",General$FileName,sep=""),PATH$Out,ext="png"),
       width   = png.width, 
       height  = png.height, 
       units   = General$File.unit,        
       type    = c("windows"),
       bg      = General$Background,
       res     = Resolution
       )
 if(count>1){  
 layout(Layoutmatrix, widths=c(rep(1,count)))
 #layout.show(count)
 }
  
 par(mar    = c(0,0,0,0),
     mai    = c(0,0,0.8,0),      
     omi    = outermargin,      
     yaxs   = "i",         
     xaxs   = "i",
     mgp    = c(2,.3,0),
     ps     = Chars,
     tcl    = -0.2,
     las    = 1,
     family = General$Texttype
    )

   
    plot(Image[dims[1]:dims[2],(dims[5]:dims[6])])
    title(main=paste(General$Tiffmode[1,1],General$Tiffmode[1,2],sep=" "), line=2, outer=F)
    
    par(new = TRUE,
        ps  = Chars,
        tcl = -0.2)
    
    plot(X,-Sediment.Y, type="l",col="transparent", axes=FALSE)
      axis(side=2, cex.axis=Acex, outer=T,labels=yt1,at=-yt2)
      axis(side=1, cex.axis=Acex, outer=T,labels=xt,at=xt)          
      magaxis(majorn=(length(yt)*2), minorn=5, side=c(2),   prettybase=prettyb, labels=FALSE, tcl=-0.2)
      magaxis(majorn=(length(xt)*2), minorn=5, side=c(1,3), prettybase=prettyb, labels=FALSE, tcl=-0.2)      
      mtext(General$Y.label1, side=2, line=2, adj=0.5, outer=T, cex=Acex, las=0)
      box(which = "plot", lty = "solid")
  
  info <- sprintf("%d%% done", Fix(1/count*100))
  setWinProgressBar(.pb, round((1/count)*100), label=info)

  if(count>1){
    if(count>2){
      for(i in 2:(count-1)){
        Image <- readTiff(Tiffs[i])
        plot(Image[dims[1]:dims[2],(dims[5]:dims[6])])
          title(main=paste(General$Tiffmode[i,1],General$Tiffmode[i,2],sep=" "), line=2, outer=F)
        
        par(new = TRUE,
            ps  = Chars,
            tcl = -0.2)
        
        plot(X,-Sediment.Y, type="l",col="transparent", axes=FALSE)    
        if(is.even(i)){axis(side=3,labels=xt,at=xt, cex.axis=Acex, outer=F)
                       magaxis(majorn=(length(xt)*2), minorn=5,side=c(1,3), prettybase=prettyb, labels=FALSE, tcl=-0.2)
            }else{
            axis(side=1,labels=xt,at=xt, cex.axis=Acex, outer=F)
            magaxis(majorn=(length(xt)*2), minorn=5,side=c(1,3), prettybase=prettyb, labels=FALSE, tcl=-0.2)
            }
        box(which = "plot", lty = "solid")
        
        info <- sprintf("%d%% done", Fix(i/count*100))
        setWinProgressBar(.pb, round((i/count)*100), label=info)
       }
      }
     Image <- readTiff(Tiffs[count])
     plot(Image[dims[1]:dims[2],(dims[5]:dims[6])])
      title(main=paste(General$Tiffmode[count,1],General$Tiffmode[count,2],sep=" "), line=2, outer=F) 
     
      par(new = TRUE,
         ps  = Chars,
         tcl = -0.2)
      
     plot(X,-Sediment.Y, type="l",col="transparent", axes=FALSE)     
     if(is.even(count)){axis(side=3,labels=xt,at=xt, cex.axis=Acex, outer=F)
                        magaxis(majorn=(length(xt)*2), minorn=5,side=c(1,3), prettybase=prettyb, labels=FALSE, tcl=-0.2)
          }else{
          axis(side=1,labels=xt,at=xt, cex.axis=Acex, outer=F)
          magaxis(majorn=(length(xt)*2), minorn=5,side=c(1,3), prettybase=prettyb, labels=FALSE, tcl=-0.2)
          }
     axis(side=4, outer=T,labels=yt2,at=-yt2, line=0, cex.axis=Acex)
     if(file.exists(AgeDepth)==FALSE){
       magaxis(majorn=(length(yt)*2), minorn=5, side=c(4), prettybase=prettyb, labels=FALSE, tcl=-0.2)}   
     box(which = "plot", lty = "solid")    
  }else{
     axis(side=4, outer=T,labels=yt2,at=-yt2, line=0, cex.axis=Acex)
     if(file.exists(AgeDepth)==FALSE){
    magaxis(majorn=(length(yt)*2), minorn=5, side=c(4), prettybase=prettyb, labels=FALSE, tcl=-0.2)} 
  }
  mtext(General$Date,       side=3, line=4,   adj=1,   outer=T, cex=Tcex)
  mtext(General$Y.label2,   side=4, line=2,   adj=0.5, outer=T, cex=Acex, las=0)        
  mtext(General$Main.title, side=3, line=3.0, adj=0.5, outer=T, cex=Tcex)
  mtext(General$Lake.name,  side=1, line=2.5, adj=0.5, outer=T, cex=Tcex, font=2)
  mtext(paste("- ",General$FileName," -",sep=""), side=3, line=1.5, adj=0.5, outer=T, cex=(Tcex-0.2))
  mtext(paste("Core:",General$Core.name,sep=" "), side=1, line=3.5, adj=0.5, outer=T, cex=Tcex, font=2)
  
  info <- sprintf("%d%% done", 99)
  setWinProgressBar(.pb, 99, label=info)

  dev.off()
  info <- sprintf("%d%% done", Fix(count/count*100))
  setWinProgressBar(.pb, round((count/count)*100), label=info)
  close(.pb)
  rm(list=ls())
  