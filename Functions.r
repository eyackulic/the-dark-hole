#######################################################################################################
#                                                                                                     #
#                            Functions for spectral index calculations                                #
#                                                                                                     #
#######################################################################################################
#                                                                                                     #
#                                   created by Christoph Butz                                         #
#                                      28. September 2016                                             #
#                                 christoph.butz@giub.unibe.ch                                        #
#                                                                                                     #
# Latest fix: added File.search() function. Tiffs can now be addressed by name in Tiffmode            # 
#             axes labels are now consistent. changed DimS_A4() to paper_Dims() added support for     #
#             other paper sizes.                                                                      #
#######################################################################################################

#######################################################################################################
#                                    Libraries & Packages                                             #
#######################################################################################################
libraries <- function(){
    
  #########################
  # necessary package to read and write ENVI files
  # also provides moving window capabilities and more
  lib1 <- library("caTools", logical.return=TRUE)
  if(lib1==FALSE) install.packages("caTools"); library("caTools")
  
  # package to read tiff images
  lib2 <- library("rtiff", logical.return=TRUE)
  if(lib2==FALSE) install.packages("rtiff"); library("rtiff")
  
  # package to create hash tables
  lib3 <- library("hash", logical.return=TRUE)
  if(lib3==FALSE) install.packages("hash"); library("hash")
  
  # package for mean absolute deviation (MA) Filters
  lib4 <- library("pracma", logical.return=TRUE)
  if(lib4==FALSE) install.packages("pracma"); library("pracma")
  
  # package for mean absolute deviation (MA) Filters
  lib5 <- library("schoolmath", logical.return=TRUE)
  if(lib5==FALSE) install.packages("schoolmath"); library("schoolmath")
  
  # package for creating reports in Open Document format
  lib6 <- library("odfWeave", logical.return=TRUE)
  if(lib6==FALSE) install.packages("odfWeave"); library("odfWeave")
  
  # package for minor tick marks
  lib7 <- library("Hmisc", logical.return=TRUE)
  if(lib7==FALSE) install.packages("Hmisc"); library("Hmisc")
  
  # package for minor tick marks
  lib8 <- library("magicaxis", logical.return=TRUE)
  if(lib8==FALSE) install.packages("magicaxis"); library("magicaxis")
  
  # package for minor tick marks
  lib9 <- library("Cairo", logical.return=TRUE)
  if(lib9==FALSE) install.packages("Cairo"); library("Cairo")
  
  # Remove unneeded variables
  rm(list=c("lib1","lib2","lib3","lib4","lib5","lib6","lib7","lib8","lib9"))
  
}


#######################################################################################################
#                       Function to find the closest match for a value in a vector                    #
#######################################################################################################

closest_values <- function(vector, value, Zero){
 
  # This function finds and returns the closest values in a vector and returns the index subscripts
  # vector = vector to search in
  # value  = vector of search values
  # Zero   = if TRUE index is zero based    
  if (Zero==TRUE) l <- (1) else l <- (0)  
   
  vector  <-  as.vector(vector, mode="numeric")
  Result  <-  as.vector(length(value), mode= "integer")
  
  
  for(i in 1:(length(value))){    
    near  <- which.min(abs(vector-value[i]))
    Result[i] <- near
  }
  Result <- (Result-l)
  return(Result)
}

#######################################################################################################
#                               Function to search for files - returns full paths                     #
#######################################################################################################

File.search <- function(File, Path, recur=TRUE) {
  
  # Returns full path(s) to (a) file(s) 
  # 
  # File: filename to be searched, empty string searches all
  # Path: initial path to search from
  # recur: Recursively search subdirectories? default = TRUE
  
  
  Files <- dir(path=Path, recursive=recur, full.names=T)
  Files <- normalizePath(Files)
  Locations <- c()
  for (i in 1:(length(File))){
    Location  <- grep(File[i], Files)
    Locations <- c(Locations,Location)
  }
  
  result <- Files[Locations]
  return(result)
}


#######################################################################################################
#                               Function to split a Path into its elements                            #
#######################################################################################################

Pathbreakdown <- function(string){
  if (length(grep("\\",string,fixed=TRUE))==1){
    X   <- gregexpr("\\",string,fixed=TRUE)
    Arr <- strsplit(string,"\\",fixed=TRUE)
    Arr <- as.character(Arr[[1]][])
  }else{    
    if (length(grep(("\"") , string,fixed=TRUE))==1){
       X   <- gregexpr("\"",string,fixed=TRUE)
       Arr <- strsplit(string,"\"",fixed=TRUE)
       Arr <- as.character(Arr[[1]][])
    }else{  
      if(length(grep("/",string,fixed=TRUE))==1){
         X   <- gregexpr("/",string,fixed=TRUE)
         Arr <- strsplit(string,"/",fixed=TRUE)
         Arr <- as.character(Arr[[1]][])
      }
    }
  }  
  return(Arr)
}

#######################################################################################################
#               Function to calculate number of pixels for A4 Papersize depending on dpi              #
#######################################################################################################

paper_Dims <- function(dpi=300,orient="p",size='A4'){
  
  switch(size,
         A4={ex <- c(   1)},#A4 Size [mm]
         A3={ex <- c(   2)},#A3 Size [mm]
         A2={ex <- c(   4)},#A2 Size [mm]
         A1={ex <- c(   8)},#A1 Size [mm]
         A0={ex <- c(  16)},#A0 Size [mm]
         A5={ex <- c(0.5 )},#A5 Size [mm]
         A6={ex <- c(0.25)} #A6 Size [mm]
  )
  
  #paper Size [mm]
  dims      <- c(297,210)*ex
  drawdimsP <- c(204,170)*ex
  drawdimsL <- c(271,126)*ex
  
  #Pixel size in mm
  PS <- c(25.4/dpi)
  
  if(orient=="p"){
    #Paper dims portrait
    pdimsy <- c(dims[1])
    pdimsx <- c(dims[2])
    #plot area dims
    ddimsy <- c(drawdimsP[1])
    ddimsx <- c(drawdimsP[2])
  }else{
    #Paper dims landscape
    pdimsy <- c(dims[2])
    pdimsx <- c(dims[1])
    #plot area dims
    ddimsy <- c(drawdimsL[2])
    ddimsx <- c(drawdimsL[1])        
  }
  #Pixelvalues for plotarea  
  ddimsyPX <- ceil(ddimsy/PS)
  ddimsxPX <- ceil(ddimsx/PS)
  #Pixelvalue for maximum tiff x size
  Tiffxmax <- ceil(40/PS)
  #pixelvalues for page size
  pdimsyPX <- ceil(pdimsy/PS)
  pdimsxPX <- ceil(pdimsx/PS)
  
  return(c(ddimsyPX,ddimsxPX,Tiffxmax,pdimsyPX,pdimsxPX))
  
}

#######################################################################################################
#                      Function to add a date and a version number to a file name                     #
#######################################################################################################
Add_Date <- function(Name,Out.Dir,ext,Add=''){
  
  # Parameters
  ############
  # Name:    Filename (string)
  # Out.Dir: Output directory
  # ext:     File extension without "." e.g. "csv"
  # Add:     optional addendum between Name and Date.
  ############
  # Example: Add_Date("Test","C:\\","csv", Add='new')
  
  file.path(Out.Dir,fsep="/")
  # version number
  X <- 1
  if(Add!=''){Add <- paste('_',Add, sep="")}
  # original path+file name
  eName  <- paste(Name,Add,"_",Sys.Date(),"_V",X,".",ext,sep="")
  Result <- file.path(Out.Dir,eName,fsep="/")
  # check if file version exists and create higher version number if yes
  while(file.exists(Result)==TRUE){
    X <- X+1
    eName  <- paste(Name,Add,"_",Sys.Date(),"_V",X,".",ext,sep="")
    Result <- file.path(Out.Dir,eName,fsep="/")
  }
  return(Result)
}


#######################################################################################################
#                                   Function to read an ENVI header                                   #
#######################################################################################################

readENVI.hdr <- function(Header_File){
  
  # package to create hash tables
  lib3 <- library("hash", logical.return=TRUE)
  if(lib3==FALSE) install.packages("hash"); library("hash")
  rm(list=c("lib3"))
  
  Header <- readLines(Header_File)
  
  # create hash table   
  ENVI_header = hash()
  
  # Add metadata from header file
    
    # Define standard numeric variables in a Envi header file
    numvars  <- c("samples","lines","bands","x start","y start","data type",
                  "header offset","byte order","fov","resolution","fps","tint","sensorid")
    numvars1 <- c("samples = ","lines   = ","bands   = ","x start = ","y start = ",
                  "data type = ","header offset = ","byte order = ","fov = ","resolution = ",
                  "fps = ","tint = ","sensorid = ")
    numvars2 <- c("samples","lines","bands","x_start","y_start","data_type","header_offset",
                  "byte_order","fov","resolution","fps","tint","sensorid")
  
    # Read numeric vars into hash  
    for (i in 1:length(numvars)){
      
      temp <- grep(numvars[i],Header, fixed=TRUE)
      if (length(temp)!=0){
        temp <- c(substr(Header[temp[1]],start=nchar(numvars1[i]), stop=nchar(Header[temp[1]])))
        .set(ENVI_header,keys=numvars2[i],values=as.numeric(temp[1]))
      }
    }
    #cleanup
    rm(list=c("numvars","numvars1","numvars2","i"))
    
    ################################################################################################
    # Define std. text vars   
    strvars  <- c("sensor type","file type","wavelength units","acquisition date","lake name","interleave")
    strvars1 <- c("sensor type = ","file type = ","wavelength units = ","acquisition date = ","lake name = ",
                  "interleave = ")
    strvars2 <- c("sensor_type","file_type","wavelength_units","acquisition_date","lake_name","interleave")
    # Read text meta data
    for (i in 1:length(strvars)){
      
      temp <- grep(strvars[i],Header, fixed=TRUE)
      if (length(temp)!=0){
        temp <- c(substr(Header[temp[1]],start=nchar(strvars1[i]), stop=nchar(Header[temp[1]])))
        .set(ENVI_header,keys=strvars2[i],values=temp[1])
      }
    }
     #cleanup
    rm(list=c("strvars","strvars1","strvars2","i"))  
  
    ################################################################################################
    # Special metadata
    # default bands
    temp <- grep("default bands",Header, fixed=TRUE)
    temp <- c(substr(Header[temp],start=(nchar("default bands = {")+1), stop=(nchar(Header[temp])-1)))
    .set(ENVI_header,keys="default_bands",values=as.numeric(unlist(strsplit(temp, "[,]"))))
    #cleanup
    rm(list="temp")
    
    # wavelength
    temp <- grep("wavelength =",Header, fixed=TRUE)
    var1 <- grep("{",Header, fixed=TRUE)
    var2 <- grep("}",Header, fixed=TRUE)
    var3 <- which(var1==temp)
    if (length(temp)!=0){
      wl <- c()
      for (i in (var1[var3]+1):var2[var3]){
        wl <- paste(wl,Header[i],sep="")
      }
      wl <- gsub("}","",wl,fixed=TRUE)  
      .set(ENVI_header,keys="wavelength",values=as.numeric(unlist(strsplit(wl, "[,]"))))
      #cleanup
      rm(list=c("wl","i"))
    }
    #cleanup
    rm(list=c("temp","var3"))
    
    #Full width half maximum (fwhm)
    temp <- grep("fwhm =",Header, fixed=TRUE)    
    var3 <- which(var1==temp)
    if (length(temp)!=0){
      fwhm <- c()
      for (i in (var1[var3]+1):var2[var3]){
        fwhm <- paste(fwhm,Header[i],sep="")
      }
      fwhm <- gsub("}","",fwhm,fixed=TRUE)  
      .set(ENVI_header,keys="fwhm",values=as.numeric(unlist(strsplit(fwhm, "[,]"))))
      #cleanup
      rm(list=c("temp","var3","fwhm","i"))
    }
  
    # Description
    temp <- grep("description =",Header, fixed=TRUE)    
    var3 <- which(var1==temp)
    if (length(temp)!=0){
      Desc <- c()
      for (i in (var1[var3]+1):var2[var3]){
        Desc <- paste(Desc,Header[i],sep="")
      }
      Desc <- gsub("}","",Desc,fixed=TRUE)  
      .set(ENVI_header,keys="description",values=Desc)
      #cleanup
      rm(list=c("Desc","i"))
    }
    rm(list=c("temp","var3"))
  
  return(ENVI_header) 
  
}


#######################################################################################################
#                          Plot function for Spectral Index output                                    #
#######################################################################################################
SpectralIndices <- function(Parameters,PATH,dims,md,cracks,nsi,Age_Depth, Format='Paper') {
    
    # Number of Indices
    md$I.cnt <- nrow(Parameters)   
    # y axis labels and position 10mm/100mm based. if more then 20 tickmarks are needed switch to 100 base.
    Start <- round(dims$Sedi.Start,digits=0)
    Stop  <- round(dims$Sedi.Stop, digits=0)
    switch(Format,
           Paper={# handle ticks for paper format
                  nticks <- length(seq(from=Start,to=Stop, by=10))
                  if(nticks>20){# handle long cores with more than 20 tick marks.
                    yt <- seq(from=Start, to=Stop, by=100)
                    yt <- pretty(yt, n=nticks/10)
                    if(yt[1]<dims$Sedi.Start){yt[1] <- ceiling(dims$Sedi.Start)}
                    if(yt[length(yt)]> dims$Sedi.Stop){yt[length(yt)] <- floor(dims$Sedi.Stop)
                      }else{yt <- c(yt,floor(dims$Sedi.Stop))}
                    prettyb <- 50
                  }else{# handle smaller cores with less than 20 tickmarks.
                    yt <- seq(from=Start, to=Stop, by=10)
                    yt <- pretty(yt, n=nticks)
                    if(yt[1]<dims$Sedi.Start){yt[1] <- dims$Sedi.Start}
                    if(yt[length(yt)]> dims$Sedi.Stop){yt[length(yt)] <- floor(dims$Sedi.Stop)
                      }else{yt <- c(yt,floor(dims$Sedi.Stop))}
                    prettyb <- 5
                  } # ENDIF 
                 }, # END 'Paper'
            Core={# handle ticks for core format
                  # use 10 tick marks
                  nticks  <- 10
                  prettyb <- 5
                  yt <- seq(from=Start, to=Stop, by=10)
                  yt <- pretty(yt, n=length(yt))
                  if(yt[1]<dims$Sedi.Start){yt[1] <- dims$Sedi.Start}
                  if(yt[length(yt)]> dims$Sedi.Stop){yt[length(yt)] <- floor(dims$Sedi.Stop)
                    }else{yt <- c(yt,floor(dims$Sedi.Stop))}
                 } # END 'Core'
    )# End switch
    # cleanup  
    rm(list=c('Start','Stop'))
  
  # Handle Age depth if existent  
  if(length(Age_Depth)>1){
        yt1 <- closest_values(Age_Depth[,2],yt,0)
        yt1 <- Age_Depth[yt1,1]
        yt2 <- yt
      }else{
        yt1 <- yt
        yt2 <- yt
    }
    
    # Today's date
    Current.Date <- format(Sys.time(), "%d %b %Y")
  
  # Load Tiff & metadata
  ##########################
  if(length(md$Tiff.Mode)==1){
      Tiff <- File.search(md$Tiff.Mode, md$Path)  
      
  }else{
      # create Path to image
      Tiff  <- file.path(PATH$Tiff,md$Tiff.Mode[1],"\\",md$File.Base.Name,"_",
                         md$Tiff.Mode[1],"_",md$Tiff.Mode[2],".tif",fsep="") 
  }
  # Open HD image  
  if (Format=='Core'){
    Reduce   <- 0
    Reduce.m <- 0
    Photo    <- readTiff(Tiff)
    Photo.m  <- Photo
    #retrieve pixmap size (use "@" instead of "$" to address class4 structure)
    pmx <- Photo@size[2]
    pmy <- Photo@size[1]    
    # Native resolution of data pixels-per-inch (ppi)
    Resolution  <- (1/dims$PS)*25.4
    ex <- 1
    # HD plot settings concerning the size of the plot
    ##################################################
      # Y frame size of the image (2*1inch*Resolution) upper frame + lower frame
      Fsize_y       <- ceil(2*1.25*Resolution)
      # Size of the outer margin in inch 
      outermargin   <- c(1.5,0.5,1,0.5)
      # Size of the multiplot outer margin in inch 
      outermargin.m <- c(1.5,0.5,1,0.5)  
      # Size of the title characters depending on image resolution
      if (Resolution < 400){Tcex  <- 2}else{Tcex  <- 1}
      # Character size
      Chars   <- 5
      Chars.m <- 5
      # Main textbody expension factor
      Mcex <- 1
      # Axis label expansion factor
      Acex <- 1.5
      # X Frame size of the image: left frame + right frame
      Fsize_x     <- ceil(2*0.5*Resolution)
      # Size of inner margin in pixel needed for index titles in multiplots
      Mai_size    <- ceil(0.8*Resolution) 
      # Line of Text coordinates
      Lines <- c(1.0,1.5,4.5,3,1.5,2.5,3.5,5,6,7,5,6,8,9,10,6)      
    
      # Tiff size from file: standard tiff size is 600 Pixels c(start,End) 
      Tiff.x   <- c(1,pmx-1)
      Tiff.x.m <- c(1,pmx-1)      
      # Layout width (relative)
      lwidth <- c(1,1,2)
      # Size of output png's in pixel 
      ###############################
      # Width (1600=2*400+1*800=Tiff1+Tiff2+Graph)
      png.width    <- 4*pmx+Fsize_x                    # Pixel  
      # size of sample, size of frame
      png.height   <- dims$nsub+2+Fsize_y              # Pixel  
      # Size of multi plot
      png.width.m  <- (md$I.cnt+1)*pmx+Fsize_x # Pixel
      png.height.m <- dims$nsub+2+Fsize_y+Mai_size     # Pixel
      
    if(md$Output!="png"){
      png.width    <- png.width/Resolution             # inch  
      # size of sample, size of frame
      png.height   <- png.height/Resolution            # inch  
      # Size of multi plot
      png.width.m  <- png.width.m/Resolution           # inch
      png.height.m <- png.height.m/Resolution          # inch
      md$File.Unit <- c("in")
      Resolution   <- c("auto")
    }
  }
  
  # Open SD Image  
  if (Format=='Paper'){
    # Calculate A4 dimensions
    Resolution  <- md$Res
    paper_DimsP <- paper_Dims(Resolution,"p",md$Paper)
    paper_DimsL <- paper_Dims(Resolution,"l",md$Paper)
    
    switch(md$Paper,
           A4={ex <- c(   1)},#A4 Size [mm]
           A3={ex <- c(   2)},#A3 Size [mm]
           A2={ex <- c(   4)},#A2 Size [mm]
           A1={ex <- c(   8)},#A1 Size [mm]
           A0={ex <- c(  16)},#A0 Size [mm]
           A5={ex <- c(0.5 )},#A5 Size [mm]
           A6={ex <- c(0.25)} #A6 Size [mm]
    )
    
    # Reducing factor for low res (2400= number of y-pixels in output=DINA4@300dpi)
    Reduce   <- 1-(paper_DimsP[1]/dims$nsub) 
    if(dims$nsub<=paper_DimsP[1]){Reduce <- 0}
    Photo    <- readTiff(Tiff, reduce = Reduce)   
    # Check if image width is in correct bounds
    pmx      <- Photo@size[2]
    pmy      <- Photo@size[1]   
    # Set Tiff.x (single graphs)
    if(pmx > paper_DimsP[3]){
      Tiff.x <- c(round((pmx/2)-paper_DimsP[3]/2),round((pmx/2)+paper_DimsP[3]/2))      
      lwidth <- c(1,1,((paper_DimsP[2]-(2*paper_DimsP[3]))/paper_DimsP[3]))
    }else{
      Tiff.x <- c(1,pmx-1)
      lwidth <- c(1,1,((paper_DimsP[2]-(2*pmx))/pmx))
    }
    
    # 1490=2480-o_margin(750)-inner_margin(240)
    Reduce.m <- 1-(paper_DimsL[1]/dims$nsub)
    if(dims$nsub<=paper_DimsL[1]){Reduce.m <- 0}
    Photo.m  <- readTiff(Tiff, reduce = Reduce.m)
    pmx.m <- Photo.m@size[2]
    pmy.m <- Photo.m@size[1] 
    # Set Tiff.x.m (multiple graphs)
    iw <- floor(paper_DimsL[2]/(md$I.cnt+1))
    if(pmx.m > iw){Tiff.x.m <- c((round(pmx.m/2)-floor(iw/2)),(round(pmx.m/2)+floor(iw/2)))
    }else{Tiff.x.m <- c(1,pmx.m-1)}    
    
    Chars   <- 12
    Chars.m <- 10
    #check if image is to small and make the margin bigger if yes.
    #Happens in lowres when the subset size is so small that the image does not need to be reduced
    # 296.333: Papersize in mm for 3500 Px@300dpi
    if(dims$nsub<=paper_DimsP[1]){small   <- (paper_DimsP[1]-dims$nsub-2)*(297*ex/paper_DimsP[4])/25.4
    }else{small   <- 0}
    if(dims$nsub<=paper_DimsL[1]){small.m <- (paper_DimsL[1]-dims$nsub-2)*(210*ex/paper_DimsL[4])/25.4
    }else{small.m <- 0}
    #Margin: 2inch+small,3cm in inch, 500Pixels in inch for top, 1.0cm in inch 
    outermargin   <- c((2+small),(2.8/2.54),1.6667,(1.2/2.54))*ex
    outermargin.m <- c((1.5+small.m),0.5,1,0.5)*ex
    # Title character expansion factor
    Tcex <- 1.5*ex
    # Main textbody expension factor
    Mcex <- 1*ex
    # Axis label expansion factor
    Acex <- 1.1*ex
    
    Lines <- c(1.5,2.5,6.5,4.5,2.5,5.5,7,5,6.5,8,5,6.5,9.5,11,12.5,9.5)*ex 
    #A4_h <- (297/25.4)*300
    #A4_w <- (210/25.4)*300
        
    png.width    <- paper_DimsP[5] # =DINA4 Portrait  @ Resolution dpi
    png.height   <- paper_DimsP[4] # =DINA4 Portrait  @ Resolution dpi
    png.width.m  <- paper_DimsP[4] # =DINA4 Landscape @ Resolution dpi
    png.height.m <- paper_DimsP[5] # =DINA4 Landscape @ Resolution dpi
    if(md$Output!="png"){
      png.width    <- png.width/Resolution               # inch  
      # size of sample, size of frame
      png.height   <- png.height/Resolution              # inch  
      # Size of multi plot
      png.width.m  <- png.width.m/Resolution             # inch
      png.height.m <- png.height.m/Resolution            # inch
      md$File.Unit <- c("in")
      Resolution   <- c("auto")
    }
  }
  ##############################################################################################
  # Calculation of time series for individual indices                                         #
  ##############################################################################################
  # Create data frames with Y - sequences + add individual data later in the loop -> output for 
  # multiple plots
      Indices   <- as.data.frame(cbind(seqs$Oid,seqs$Sedi.Y,seqs$Core.Y))
      IndicesMA <- as.data.frame(cbind(seqs$Oid,seqs$Sedi.Y,seqs$Core.Y))
  
  for (i in 1:md$I.cnt) {
    
    #i <- 3
    ############################################################################################
    #                           Load Data                                                      #
    ############################################################################################
    if(substr(Parameters[i,2],start=(nchar(Parameters[i,2])-3),stop=nchar(Parameters[i,2]))==".dat"){
    
      if (md$Sub.Type=="big"){
          s_type <- 2
          Bsub <- md$Bsub
      }else{
          s_type <- 3
          # The small subset is opened ranging from 1 (x1) - X (X2).
          Bsub <- c(1, dims$SubX2-dims$SubX1+1)
      }  
      
        Data <- read.ENVI(  filename= Parameters[i,s_type], 
                          headerfile= gsub('.dat','.hdr',Parameters[i,s_type],fixed=T)
                          )
        datadims <- size(Data)
        if(length(datadims)==2){
          Data <- Data[(dims$Y.Start+1):dims$Y.Stop,Bsub[1]:Bsub[2]]  
        }else{
          Data <- Data[(dims$Y.Start+1):dims$Y.Stop,Bsub[1]:Bsub[2],1]
        }
     
    }else{
      Data <- read.table(file=Parameters[i,2], header=TRUE, sep = ",", quote="\"")
      Data <- Data[,c(1,(i-nsi+1))]
      sta  <- closest_values(Data[,1],dims$Sedi.Start,Zero=FALSE)
      sto  <- closest_values(Data[,1],dims$Sedi.Stop, Zero=FALSE)
      
      # Find Na's in data and preserve them during approx
      NA_BIN <- which(is.na(Data[,2])==TRUE)
      if (length(NA_BIN)>0){      
      # create vector of zeros and bind to data
      Data <- cbind(Data,rep.int(0,nrow(Data)))
      # Fill in 1's at location of NA's
      Data[NA_BIN,3] <- 1
      }
      #Approximate data
      Dataapprox <- approx(Data[sta:sto,1],Data[sta:sto,2], n=(dims$nsub)) 
      if (length(NA_BIN)>0){  
      # Approximate NA's (vector of one's)
      NA_BIN  <- approx(Data[sta:sto,1],Data[sta:sto,3], n=(dims$nsub))     
      NA_BIN1 <- which(NA_BIN$y>0)
      }
      #Write approximated data to DATA vector
      Data <- Dataapprox$y
      # Fill in approximated NA's 
      if (length(NA_BIN)>0){Data[NA_BIN1] <- NA}
      # replicate Data for compatibility with usual routine (needs to calculate a mean from rows)
      Data <- cbind(Data,Data,Data)
      
    }
    
    Image  <- readTiff(Parameters[i,7],reduce=Reduce)    
    
    ############################################################################################
    #                     Calculated Parameters                                                #
    ############################################################################################
          
    # remove impossible Data
    ########################
      #  - Set infinite (INF) values to NA
      Data[!is.finite(Data)] <-NA # "!" means "not" 
      
      if (length(cracks)==1){
        if (is.na(cracks)==FALSE) {
          for (j in 1:nrow(cracks)){Data[cracks[j,1]:cracks[j,2],] <- NA}
        }
      }else{for (j in 1:nrow(cracks)){Data[cracks[j,1]:cracks[j,2],] <- NA}}
    # Calculate means
    ##################
      # Calculate rowMeans (Sample)
      Mean.X <- rowMeans(Data, na.rm=TRUE)
      
      #Calculate column Mean (sample)
      Mean <- mean(Mean.X, na.rm=TRUE)
          
      # remove outliers 
      MA <- runmean(Mean.X, md$k,alg="C",endrule="mean")
      #MAD <- hampel(na.omit(Mean.X), k=md$k)
    #############################################################################################
    #                                      Export Data                                          #
    #############################################################################################
  
      # Export X-Y to *.csv (or *.xls) Header= seqs$Oid,Sid,Core Depth[mm],index value, MA value
      ######################################################################################
      # Create Matrix with x-coordinate (Pixel) of original ENVI-File (seqs$Oid) and subset-file (Sid),
      # core position (seqs$Core.Y), index value (Mean.X), and outlier corrected Mean (MA)
                 
      # Reverse na.omit function
      if(is.null(attr(MA, "na.action"))==TRUE){MA.X <- MA
      }else{MA.X <- rep(NA,length(MA)+length(attr(MA,"na.action")))
            MA.X[-attr(MA,"na.action")] <- MA
            }
      MeanMA <- mean(MA.X, na.rm=TRUE)  
      # data frame with output data -> single output
      mat    <- cbind(seqs$Oid,seqs$Sedi.Y,seqs$Core.Y,Mean.X,MA.X)
      # Fill matrix for multiplot
      Indices   <- as.data.frame(cbind(Indices,  Mean.X))
      IndicesMA <- as.data.frame(cbind(IndicesMA,MA.X))
      
      # Where are NA´s in the Data
      NA_BIN <- which(is.na(mat[,5])==TRUE)
      # Where are values > 3* std dev
      MAX <- which(mat[5]>(3*sd(mat[,5])))      
      
      # Create a .csv file using the original filename that stores the Header
      Head <- c("Oid",
                "Sediment Depth[mm]",
                "Core Depth [mm]", 
                Parameters[i,1], 
                "Moving Average"
                )  
      
      write.table(t(Head), 
                  file=paste(Parameters[i,8],".csv", sep=""), 
                  append = FALSE, 
                  quote = FALSE, 
                  sep = ",",
                  na = "NA", 
                  dec = ".", 
                  row.names = FALSE,
                  col.names = FALSE, 
                  qmethod = c("escape", "double"),
                  fileEncoding = "UTF-8"
                  )
      
      # write Data into the same .csv file
      write.table(mat, 
                  file=paste(Parameters[i,8],".csv", sep=""), 
                  append = TRUE, 
                  quote = FALSE, 
                  sep = ",",
                  na = "NA", 
                  dec = ".", 
                  row.names = FALSE,
                  col.names = FALSE, 
                  qmethod = c("escape", "double"),
                  fileEncoding = "UTF-8"
                  )
        
    #############################################################################################
    #                                  Export SINGLE Plots to png                               #
    #############################################################################################
    subnames <- c("(uncorrected)","Moving average - Filter, k=")
    subnames <- gsub("k=",paste("k=",md$k,sep=""),subnames,fixed=TRUE)
    
    if (md$Graph.Out[1]==TRUE){
    if (Format=='Core'){
      tempnames <- c(paste("_core-res.",md$Output,sep=""),
                     paste("_core-res_MA.",md$Output,sep=""))   
    }else{
      tempnames <- c(paste("_",md$Paper,".",md$Output,sep=""),
                     paste("_",md$Paper,"_MA.",md$Output,sep=""))     
    }    
    
    for(j in 1:2){
      # single plots 
      Cairo(file=file.path(Parameters[i,8],tempnames[j], fsep=""),
           width=     png.width, 
           height=    png.height, 
           units=     md$File.Unit,         
           type=      md$Output,
           bg=        md$Background,
           dpi=       Resolution
           )
    
      layout(matrix(c(1,2,3), 1, 3, byrow = TRUE),widths=lwidth)      
      #layout.show(3)
      
      par(mar  = c(0,0,0,0),      
          omi  = outermargin,      
          yaxs = "i", # this extents the y axis to the boundaries of the box           
          mgp  = c(2,.5,0)*ex,
          ps   = Chars,
          las  = 1      
          )
      if(md$Output=="svg"){
        plot(runif(dims$nsub),seqs$Sedi.Y,type="n",axes=FALSE)
      }else{
      plot (Photo[round(dims$Y.Start*(1-Reduce)):round(dims$Y.Stop*(1-Reduce)),(Tiff.x[1]:Tiff.x[2])])
      }
      box(which = "plot", lty = "solid")
      if(md$Output=="svg"){
        plot(runif(dims$nsub),seqs$Sedi.Y,type="n",axes=FALSE)
      }else{
      plot (Image[round(dims$Y.Start*(1-Reduce)):round(dims$Y.Stop*(1-Reduce)),(Tiff.x[1]:Tiff.x[2])])
      abline(v=(md$Bsub[1]*(1-Reduce)), untf=FALSE,col="red",lwd=1.5)
      abline(v=(md$Bsub[2]*(1-Reduce)), untf=FALSE,col="red",lwd=1.5)
      }
      box(which = "plot", lty = "solid")
      
      par(tcl=-0.2)
      plot (mat[,(3+j)], -mat[,2], 
            type=md$Line.Style, 
            axes=FALSE,
            col=Parameters[i,4]
            )
      axis(side=1,labels=T,          outer=T,cex.axis=Acex, mgp=c(2,0.1,0)*ex) 
      axis(side=2,labels=yt1,at=-yt2,outer=T,cex.axis=Acex)    
      axis(side=4,labels=yt2,at=-yt2,outer=T,cex.axis=Acex)  
      magaxis(majorn=(length(yt)*2), minorn=5, side=c(2,4), prettybase=prettyb, labels=FALSE)
      magaxis(side=c(1,3), prettybase=10, labels=FALSE)
      
      box(which = "plot", lty = "solid")
      title(xlab=md$X.Label,line=Lines[1], adj=0.75, cex.lab=Acex, outer=T)
      #title(ylab=md$Y.Label.L, line=Lines[2], adj=0.5, cex.lab=Acex, outer=T)
      mtext(md$Y.Label.L, side=2, line=Lines[2], adj=0.5, outer=T, cex=Acex, las=0)
      mtext(md$Y.Label.R, side=4, line=Lines[2], adj=0.5, outer=T, cex=Acex, las=0)
      abline(v=Mean, untf = FALSE, col=md$Mean.Color)
      abline(v=Mean-sd(Mean.X, na.rm=TRUE), untf = FALSE, col=md$Sd.Color)
      abline(v=Mean+sd(Mean.X, na.rm=TRUE), untf = FALSE, col=md$Sd.Color)
        
      mtext(Parameters[i,5], side=3, line=Lines[3], adj=0.5, outer=T, cex=Tcex)
      mtext(subnames[j],     side=3, line=Lines[4], adj=0.5, outer=T, cex=(Tcex-0.3))
      mtext(Parameters[i,6], side=3, line=Lines[5], adj=0.5, outer=T, cex=(Tcex-0.3))
      mtext(md$Lake.Name,    side=1, line=Lines[6], adj=0.5, outer=T, cex=Mcex, font=2)
      mtext(paste("Core:",md$Core.Name,sep=" "),
                             side=1, line=Lines[7], adj=0.5, outer=T, cex=Mcex, font=2)
      
      mtext(paste("n = ", dims$nsub, sep=""), 
                             side=1, line=Lines[8], adj=1,   outer=T, cex=Mcex)
      mtext(paste("sd = ", round(sd(mat[,4], na.rm=TRUE), digits=3), sep=""), 
                             side=1, line=Lines[9], adj=1,   outer=T, cex=Mcex, col=md$Sd.Color)
      mtext(paste("Mean = ", round(Mean, digits=3), sep=""), 
                             side=1, line=Lines[10],adj=1,   outer=T, cex=Mcex, col=md$Mean.Color)
      mtext(paste("Start = ", round(dims$Sedi.Start, digits=2)," mm", sep=""), 
                             side=1, line=Lines[11],adj=0,   outer=T, cex=Mcex)
      mtext(paste("End = ", round(dims$Sedi.Stop, digits=2)," mm", sep=""), 
                             side=1, line=Lines[12],adj=0,   outer=T, cex=Mcex)
     
      if(dims$Core.Length==dims$Sub.Length & dims$Core.Length==dims$Sedi.Length){
        mtext(paste("Core length [mm] = ", round(dims$Core.Length, digits=2), sep=""), 
                             side=1, line=Lines[13],adj=1,   outer=T, cex=Mcex)
      }else{
      
      mtext(paste("Core length [mm] = ", round(dims$Core.Length, digits=2), sep=""), 
                             side=1, line=Lines[13],adj=1,   outer=T, cex=Mcex)
      mtext(paste("Subset length [mm] = ", round(dims$Sub.Length, digits=2), sep=""), 
                             side=1, line=Lines[14],adj=1,   outer=T, cex=Mcex)
      mtext(paste("Sediment length [mm] = ", round(dims$Sedi.Length, digits=2), sep=""), 
                             side=1, line=Lines[15],adj=1,   outer=T, cex=Mcex)
      } # ENDELSE
      mtext(Current.Date,    side=3, line=Lines[16],adj=1,   outer=T, cex=Acex)
      
      dev.off()
      
      info <- sprintf("%d%% done", round((((i-1)*4)+j)/((md$I.cnt*4)+2)*100))
      setWinProgressBar(.pb, round((((i-1)*4)+j)/((md$I.cnt*4)+2)*100), label=info)
      
    } # ENDFOR Cairo device
    if(md$Output=="svg"){
      if(Format=='Core'){ext <- c("_index_hires.png")}else{ext <- c("_index.png")}
      
      Cairo(file   = file.path(Parameters[i,8],ext, fsep=""),
            width  = Tiff.x[2]-Tiff.x[1], 
            height = round(dims$nsub*(1-Reduce)), 
            units  = "px",         
            type   = c("png"),
            bg     = md$Background,
            dpi    = Resolution
            )
      
      par(mar =c (0,0,0,0),      
          omi =c (0,0,0,0))
      
      plot(Image[round(dims$Y.Start*(1-Reduce)):round(dims$Y.Stop*(1-Reduce)),(Tiff.x[1]:Tiff.x[2])])
      dev.off()
    } # ENDIF svg
    } # ENDIF md$Graph.Out[1]
    rm(list=c("tempnames","j"))
   ###############################################################################################
   #                                  Export Overplot Plots to png                               #
   ############################################################################################### 
   if (md$Graph.Out[3]==TRUE){
   if (Format=='Core'){
    if(md$Output=="png"){
     tempnames <- c(paste("_oplot.",   md$Output,sep=""),
                    paste("_oplot_MA.",md$Output,sep=""))               
    
      for(j in 1:2){          
        # Overplot - uncorrected
        png(filename = file.path(Parameters[i,8],tempnames[j], fsep=""),
             width   = pmx+ceil(2*0.5*Resolution), 
             height  = png.height, 
             units   = md$File.Unit,         
             type    = "windows",
             bg      = md$Background,
             res     = Resolution
             )
          
        par(mar  = c(0,0,0,0),      
            omi  = outermargin,      
            yaxs = "i", # this extents the y axis to the boundaries of the box           
            mgp  = c(2,.3,0),
            ps   = Chars,
            las  = 1
            )
        
        plot (Photo[round(dims$Y.Start):round(dims$Y.Stop),])             
        box(which = "plot", lty = "solid")
        abline(v=md$Bsub[1], untf=FALSE,col="red", lwd=1.5)
        abline(v=md$Bsub[2], untf=FALSE,col="red", lwd=1.5)
          
        par(ps  = Chars,
            tcl = -0.2,
            new = T)
        plot (mat[,j+3], -mat[,2], 
              type=md$Line.Style, 
              axes=FALSE,
              col="white",
              lwd=1.5
              )
        par(ps  = Chars,
            tcl = -0.2,
            new = T,
            omi = outermargin     
            )
        plot (mat[,j+3], -mat[,2], 
              type = md$Line.Style, 
              axes = FALSE,
              col  = Parameters[i,4],
              lwd  = 0.5
              )
        axis(side=1,labels=T,           outer=T,cex.axis=1.5, mgp=c(2,0.1,0)) 
        axis(side=2,labels=yt1,at=-yt2, outer=T,cex.axis=1.5)  
        axis(side=4,labels=yt2,at=-yt2, outer=T,cex.axis=1.5)        
        magaxis(majorn=(length(yt)*2), minorn=5, side=c(2,4), prettybase=prettyb, labels=FALSE,tcl=-0.2)
        magaxis(side=c(1,3), prettybase=10, labels=FALSE,tcl=-0.2)
        box(which = "plot", lty = "solid")
        title(xlab=md$X.Label,         line=1.0, adj=0.5, outer=T, cex.lab=1.5)
        #title(ylab=md$Y.Label.L,      line=1.5, adj=0.5, outer=T, cex.lab=1.5)
        mtext(md$Y.Label.L,    side=2, line=1.5, adj=0.5, outer=T, cex=Acex, las=0)
        mtext(md$Y.Label.R,    side=4, line=1.5, adj=0.5, outer=T, cex=Acex, las=0)
        abline(v=Mean, untf = FALSE, col="gray80")
        abline(v=Mean-sd(Mean.X, na.rm=TRUE), untf = FALSE, col=md$Sd.Color)
        abline(v=Mean+sd(Mean.X, na.rm=TRUE), untf = FALSE, col=md$Sd.Color)
        
        mtext(Parameters[i,5], side=3, line=3.0, adj=0.5, outer=T, cex=Tcex)
        mtext(subnames[j],     side=3, line=2.0, adj=0.5, outer=T, cex=(Tcex-0.2))  
        mtext(Parameters[i,6], side=3, line=1.0, adj=0.5, outer=T, cex=(Tcex-0.2))
        mtext(md$Lake.Name,    side=1, line=2.0, adj=0.5, outer=T, cex=1, font=2)
        mtext(paste("Core:",md$Core.Name,sep=" "), 
                               side=1, line=2.5, adj=0.5, outer=T, cex=1, font=2)
        
        mtext(paste("n = ", dims$nsub, sep=""), 
                               side=1, line=3,   adj=0.5, outer=T, cex=1)
        mtext(paste("sd = ", round(sd(mat[,4], na.rm=TRUE), digits=3), sep=""), 
                               side=1, line=3.5, adj=0.5, outer=T, cex=1, col=md$Sd.Color)
        mtext(paste("Mean = ", round(Mean, digits=3), sep=""), 
                               side=1, line=4.0, adj=0.5, outer=T, cex=1, col="gray80")
        mtext(paste("Start = ", round(dims$Sedi.Start, digits=2)," mm", sep=""), 
                               side=1, line=4.5, adj=0.5, outer=T, cex=1)
        mtext(paste("End = ", round(dims$Sedi.Stop, digits=2)," mm", sep=""), 
                               side=1, line=5.0, adj=0.5, outer=T, cex=1)
        
        if(dims$Core.Length==dims$Sub.Length & dims$Core.Length==dims$Sedi.Length){
          mtext(paste("Core length [mm] = ", round(dims$Core.Length, digits=2), sep=""), 
                               side=1, line=5.5, adj=0.5, outer=T, cex=1)
        }else{
        
        mtext(paste("Core length [mm] = ", round(dims$Core.Length, digits=2), sep=""), 
                               side=1, line=5.5, adj=0.5, outer=T, cex=1)
        mtext(paste("Subset length [mm] = ", round(dims$Sub.Length, digits=2), sep=""), 
                               side=1, line=6.0, adj=0.5, outer=T, cex=1)
        mtext(paste("Sediment length [mm] = ", round(dims$Sedi.Length, digits=2), sep=""), 
                               side=1, line=6.5, adj=0.5, outer=T, cex=1)
        }
        mtext(Current.Date, 
              side=3, line=4, adj=0.5, outer=TRUE, cex=1)
        
        dev.off()
        info <- sprintf("%d%% done", round((((i-1)*4)+(j+2))/((md$I.cnt*4)+2)*100))
        setWinProgressBar(.pb, round((((i-1)*4)+(j+2))/((md$I.cnt*4)+2)*100), label=info) 
      }# concludes overplot
    }else{print("overplot not supported for svg output")}# concludes if clause ("png")
   }# concludes Core/paper format of overplot
   }# concludes if md$Graph.Out=TRUE
  }# concludes cycle through parameters   
    rm=(list=c("i","Mean.X","MA.X","Tiff.x","j","mat","Data","png.height","png.width","tempnames","Head"))
    ###############################################################################################
    ###############################################################################################
    #                                     Multiple Graphs                                         #
    ############################################################################################### 
    if(md$I.cnt>1){
      if (md$Graph.Out[2]==TRUE){
        if(md$Output=="png"){
      #############################################################################################
      #                           Load Data                                                       #
      #############################################################################################
      # Create data frames with Y - sequences and add Data to these dfs
      #Indices   <- as.data.frame(cbind(seqs$Oid,seqs$Sedi.Y,seqs$Core.Y))
      #IndicesMA <- as.data.frame(cbind(seqs$Oid,seqs$Sedi.Y,seqs$Core.Y))
     
      #for(i in 1:md$I.cnt) {
       # G <- read.table(file=paste(Parameters[i,8], ".csv", sep=""), 
        #                                                header=TRUE, 
         #                                               sep=";", 
          #                                              dec=".", 
           #                                             encoding="UTF-8",
            #                                            )
        #Indices[,3+i] <- G[,4]
        #IndicesMA[,3+i] <- G[,5]
      #}
      colnames(Indices)   <- c("Oid","Sediment.Y [mm]","Core.Y [mm]",t(Parameters[,1]))  
      colnames(IndicesMA) <- c("Oid","Sediment.Y [mm]","Core.Y [mm]",t(Parameters[,1]))
             
      ###############################################################################################
      #                                      Export Data                                            #
      ###############################################################################################      
      
      write.table(Indices, 
                  file=file.path(PATH$Index,"Results\\",md$Core.Name,"_uncorrected.csv",fsep=""), 
                  append = FALSE, 
                  quote = FALSE, 
                  sep = ";",
                  na = "NA", 
                  dec = ".", 
                  row.names = FALSE,
                  col.names = TRUE, 
                  qmethod = c("escape", "double"),
                  fileEncoding = "UTF-8"
                  )
      write.table(IndicesMA, 
                  file=file.path(PATH$Index,"Results\\",md$Core.Name,"_MA.csv",fsep=""),                             
                  append = FALSE, 
                  quote = FALSE, 
                  sep = ";",
                  na = "NA", 
                  dec = ".", 
                  row.names = FALSE,
                  col.names = TRUE, 
                  qmethod = c("escape", "double"),
                  fileEncoding = "UTF-8"
                  )
    
    dims$nInd <- ncol(Indices)  
    ###############################################################################################
    #                                       Plot data                                             #
    ###############################################################################################
  
    if (Format=='Core'){
      tempnames <- c(paste("_core-res.",md$Output,sep=""),
                     paste("_core-res_MA.",md$Output,sep=""))   
    }else{
      tempnames <- c(paste("_",md$Paper,".",md$Output,sep=""),
                     paste("_",md$Paper,"_MA.",md$Output,sep=""))     
    }
    Sub.Main.title.m <- c(md$Sub.Main.Title.m, md$Sub.Main.Title.MA.m)
    
    Layoutmatrix <- matrix(t(c(seq(from=1, to=md$I.cnt+1))),
                           nrow  = 1,
                           ncol  = md$I.cnt+1,
                           byrow = TRUE
                          ) 
    #l <- 1 # debug
    for(l in 1:2){
      if(l==2){Indices <- IndicesMA}
      png(filename    = file.path(PATH$Index,"Results\\",md$Core.Name,tempnames[l], fsep=""),
           width      = png.width.m, 
           height     = png.height.m, 
           units      = md$File.Unit, 
           #pointsize = Point.size, 
           #type      = md$Output,
           bg         = md$Background,
           res        = Resolution
           )
      
      layout(Layoutmatrix, widths=c(rep(1,md$I.cnt+1)))
      #layout.show()
      
      
      par(mar  = c(0,0,0,0),
          mai  = c(0,0,0.8,0)*ex,      
          omi  = outermargin.m,      
          yaxs = "i",         
          xaxs = "r",
          mgp  = c(2,.5,0)*ex,
          ps   = Chars.m,
          tcl  = -0.2,
          las  = 1
          )
          
            
      plot (Indices[,4],-Indices[,2],
            type = md$Line.Style, 
            axes = F,            
            col  = Parameters[1,4]
      )
      title(line=2*ex,   outer=F, cex.main=Acex,main=Parameters[1,1])
      axis(side=2,line=0,outer=T, cex.axis=Acex,labels=yt1,at=-yt2)
      axis(side=1,       outer=T, cex.axis=Acex)          
      magaxis(majorn=(length(yt)*2), minorn=5, side=c(2), prettybase=prettyb, labels=FALSE)
      magaxis(side=c(1), prettybase=10, labels=FALSE)
      
      mtext(md$Y.Label.L.m, side=2, line=2.5*ex, adj=0.5, outer=T, cex=Acex, las=0)
      M.Ind <- mean(Indices[,4], na.rm=TRUE)
      S.Ind <-   sd(Indices[,4], na.rm=TRUE)
      abline(v=M.Ind,         untf = FALSE, col = md$Mean.Color)
      abline(v=M.Ind - S.Ind, untf = FALSE, col = md$Sd.Color)
      abline(v=M.Ind + S.Ind, untf = FALSE, col = md$Sd.Color)
      
      box(which = "plot", lty = "solid")
      rm(list=c('M.Ind','S.Ind'))
      
      for(i in 1:ceiling((md$I.cnt-2)/2)){
                  
        plot (Indices[,4+i],-Indices[,2], 
                 type = md$Line.Style, 
                 axes = F,            
                 col  = Parameters[1+i,4]
        )
        title(main=Parameters[1+i,1], line=2*ex, outer=F,cex.main=Acex)
        if(is.even(i)){axis(side=1, cex.axis=Acex, outer=T)
                       magaxis(side=c(1), prettybase=10, labels=FALSE)
        }else{
        axis(side=3, cex.axis=Acex, outer=F)
        magaxis(side=c(3), prettybase=10, labels=FALSE)
        }
        M.Ind <- mean(Indices[,4+i], na.rm=TRUE)
        S.Ind <-   sd(Indices[,4+i], na.rm=TRUE)
        abline(v=M.Ind,         untf = FALSE, col = md$Mean.Color)
        abline(v=M.Ind - S.Ind, untf = FALSE, col = md$Sd.Color)
        abline(v=M.Ind + S.Ind, untf = FALSE, col = md$Sd.Color)
        
        box(which = "plot", lty = "solid")
        rm(list=c('M.Ind','S.Ind'))
      }
      if(md$Output=="svg"){
        plot(runif(dims$nsub),seqs$Sedi.Y,type="n",axes=FALSE)
      }else{  
      plot(Photo.m[round(dims$Y.Start*(1-Reduce.m)):round(dims$Y.Stop*(1-Reduce.m)),(Tiff.x.m[1]:Tiff.x.m[2])])  
      }
      abline(v=(md$Bsub[1]*(1-Reduce.m)), untf=FALSE,col="red", lwd=1.5)
      abline(v=(md$Bsub[2]*(1-Reduce.m)), untf=FALSE,col="red", lwd=1.5)
      title(main=paste(md$Tiff.Mode[1]," image",sep=""), line=2*ex, outer=F, cex.main=Acex)
      box(which = "plot", lty = "solid")
      
      if((md$I.cnt-2)<(i+1)) {
        plot (Indices[,dims$nInd],-Indices[,2], 
            type = md$Line.Style, 
            axes = F, 
            xlab = md$X.Label.m, 
            ylab = "", 
            col  = Parameters[md$I.cnt,4]
      )
      title(main=Parameters[md$I.cnt,1], line=2*ex, outer=F, cex.main=Acex)
      axis(side=1, outer=F, cex.axis=Acex)
      axis(side=4, outer=T, cex.axis=Acex, line=0,labels=yt2,at=-yt2)  
      magaxis(majorn=(length(yt)*2), minorn=5, side=c(4), prettybase=prettyb, labels=FALSE)
      magaxis(side=c(1), prettybase=10, labels=FALSE)
      mtext(md$Y.Label.R.m, side=4, line=2.5*ex, adj=0.5, outer=T, cex=Acex, las=0)
      
      M.Ind <- mean(Indices[,dims$nInd], na.rm=TRUE)
      S.Ind <-   sd(Indices[,dims$nInd], na.rm=TRUE)
      abline(v=M.Ind,         untf = FALSE, col=md$Mean.Color)
      abline(v=M.Ind - S.Ind, untf = FALSE, col=md$Sd.Color)
      abline(v=M.Ind + S.Ind, untf = FALSE, col=md$Sd.Color)
                                                          
      box(which = "plot", lty = "solid")
      rm(list=c('M.Ind','S.Ind'))
        
      }else{
        for(j in (i+1):(md$I.cnt-2)){
                      
          plot (Indices[,4+j],-Indices[,2], 
                type = md$Line.Style, 
                axes = F,                                 
                xlab = md$X.Label.m, 
                ylab = "", 
                col  = Parameters[1+j,4]
          )
          title(main=Parameters[1+j,1], line=2*ex, outer=F, cex.main=Acex)
          if(is.even(j)){axis(side=1, cex.axis=Acex, outer=T)
                         magaxis(side=c(1), prettybase=10, labels=FALSE)
          }else{
          axis(side=3, cex.axis=Acex, outer=F)
          magaxis(side=c(3), prettybase=10, labels=FALSE)
          }
          
          M.Ind <- mean(Indices[,4+j], na.rm=TRUE)
          S.Ind <-   sd(Indices[,4+j], na.rm=TRUE)
          abline(v=M.Ind,         untf = FALSE, col=md$Mean.Color)
          abline(v=M.Ind - S.Ind, untf = FALSE, col=md$Sd.Color)
          abline(v=M.Ind + S.Ind, untf = FALSE, col=md$Sd.Color)
                                                    
          box(which = "plot", lty = "solid")
          rm(list=c('M.Ind','S.Ind'))
        }
      
        plot (Indices[,dims$nInd],-Indices[,2], 
              type = md$Line.Style, 
              axes = F,               
              xlab = md$X.Label.m, 
              ylab = "", 
              col  = Parameters[md$I.cnt,4]
        )
        title(main=Parameters[md$I.cnt,1], line=2*ex, outer=F, cex.main=Acex)    
        if(is.even(j)){axis(side=3, cex.axis=Acex, outer=F)
                       magaxis(side=c(3), prettybase=10, labels=FALSE)
          }else{
          axis(side=1, cex.axis=Acex, outer=T)
          magaxis(side=c(1), prettybase=10, labels=FALSE)
          }
        axis(side=4, outer=T,labels=yt2,at=-yt2, line=0, cex.axis=Acex)
        magaxis(majorn=(length(yt)*2), minorn=5, side=c(4), prettybase=prettyb, labels=FALSE)
        
        M.Ind <- mean(Indices[,dims$nInd], na.rm=TRUE)
        S.Ind <-   sd(Indices[,dims$nInd], na.rm=TRUE)
        abline(v=M.Ind,         untf = FALSE, col=md$Mean.Color)
        abline(v=M.Ind - S.Ind, untf = FALSE, col=md$Sd.Color)
        abline(v=M.Ind + S.Ind, untf = FALSE, col=md$Sd.Color)
                                                            
        mtext(md$Y.Label.R, side=4, line=2.5*ex, adj=0.5, outer=T,cex=Acex,las=0)
        box(which = "plot", lty = "solid")
        rm(list=c('M.Ind','S.Ind'))
      }
      
      mtext(md$Main.Title.m,    side=3, line=3.0*ex, adj=0.5, outer=T, cex=Tcex)
      mtext(Sub.Main.title.m[l],side=3, line=1.0*ex, adj=0.5, outer=T, cex=(Tcex-0.3))
      mtext(paste("x-axes = ",md$X.Label.m, sep=""), 
                                side=1, line=3.0*ex, adj=0.5, outer=T, cex=Acex)
      mtext(md$Sub.Title.m,     side=1, line=5.5*ex, adj=0.5, outer=T, cex=1.2*ex, font=2)
      mtext(md$Lake.Name[1],    side=1, line=8.0*ex, adj=0.5, outer=T, cex=0.8*ex, font=2)
      
      mtext(paste("n = ", dims$nsub, sep=""), 
                                side=1, line=4.0*ex, adj=1.0, outer=T, cex=Acex)
      mtext(paste("length [mm] = ", round(dims$Sub.Length, digits=2), sep=""), 
                                side=1, line=7.5*ex, adj=1.0, outer=T, cex=Acex)
      mtext(paste("Start = ", round(dims$Sedi.Start, digits=2)," mm", sep=""), 
                                side=1, line=4.0*ex, adj=0.0, outer=T, cex=Acex)
      mtext(paste("End = ", round(dims$Sedi.Stop, digits=2)," mm", sep=""), 
                                side=1, line=7.5*ex, adj=0.0, outer=T, cex=Acex)
      
      mtext(Current.Date,       side=3, line=4.0*ex, adj=1.0, outer=T, cex=Acex)
      box(which = "plot", lty = "solid")
      dev.off()
      
      # Progress bar update
      info <- sprintf("%d%% done", round(((md$I.cnt*4)+l)/((md$I.cnt*4)+2)*100))
      setWinProgressBar(.pb, round(((md$I.cnt*4)+l)/((md$I.cnt*4)+2)*100), label=info) 
    }# ENDFOR normal/Ma
    }else{print("Multiplot is not supported for svg output")}#concludes is "png"?
    }# concludes md$Graph.Out=TRUE
    }# concludes if Parameters>1
  if(md$Output=="svg"){
    if(Format=='Core'){ext <- c("_photo_hires.png")}else{ext <- c("_photo.png")}
    Cairo(file   = file.path(PATH$Index,"Results\\",md$Core.Name,ext, fsep=""),
          width  = Tiff.x[2]-Tiff.x[1]+1, 
          height = round(dims$nsub*(1-Reduce)), 
          units  = "px",         
          type   = c("png"),
          bg     = md$Background,
          res    = Resolution
        )
        par(mar  = c(0,0,0,0),      
            omi  = c(0,0,0,0))
        plot(Photo[round(dims$Y.Start*(1-Reduce)):round(dims$Y.Stop*(1-Reduce)),(Tiff.x[1]:Tiff.x[2])])
        dev.off()
  }
  
  close(.pb)
}
