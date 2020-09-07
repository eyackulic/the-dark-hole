#################################################################################################
#                           Basic concept of how to work with ENVI Image Data                   #
#                         ################################################                      # 
#                                   Created by Christoph Butz                                   #
#                                       06. October 2014                                        #
#                                 Christoph.Butz@giub.unibe.ch                                  #
#################################################################################################

# This File is meant to ease your access to data contained in an ENVI (.dat) file

#################################################################################################
#                    libraries                                                                  #
#################################################################################################

# These packages are needed to work with an ENVI Image data file

  # necessary package to read and write ENVI files
  # also provides moving window capabilities and more
  lib1 <- library("caTools", logical.return=TRUE)
  if(lib1==FALSE) install.packages("caTools"); library("caTools")
  
  # package to read tiff images
  lib2 <- library("rtiff", logical.return=TRUE)
  if(lib2==FALSE) install.packages("rtiff"); library("rtiff")

# Load functions.r  
  #The function "readENVI.hdr()" is not part of the package "caTools" but self-written
  source("Functions.r")

# cleanup unneeded variables
  rm(list=c("lib1","lib2"))

#################################################################################################
#                    Load Data                                                                  #
#################################################################################################
# ENVI Images consist of a Data container (Filename.dat) and a Header file (Filename.hdr)
# Both have to be read:

# Path to the File you want to open including File name and extension
  # Information will be stored in a string variable called "File"
  # Folders have to be separated either by "/" or "\\"

  Data_File <- c("D:\\test samples\\New_test\\Subset\\Co1282-1_130708-112406_refl_sub_shw.dat") 

# Path to the Header File of the ENVI image created from the "File" variable 
  # "gsub" searches for a pattern in a string and replaces the pattern by something else
  # here ".dat" is replaced by ".hdr" in the variable "File"

  Header_File <- gsub(".dat", ".hdr",Data_File, fixed=TRUE)

# Open the File and store its information in an array variable called "DATA"

  DATA <- read.ENVI(filename=Data_File, headerfile=Header_File)

#################################################################################################
# Read the Header file seperately

  Header = readENVI.hdr(Header_File)
  
  # show header contents
  Header
  #Header keys
  k <- keys(Header)
  k
  #Header values
  v <- values(Header)
  v

  # Extract a value by key (use "$"together with key)
  Samples <- Header$samples
  Lines   <- Header$lines

#################################################################################################
#                  Access "DATA" information                                                    #
#################################################################################################

  # Data type 
  typeof(DATA)
  # mode of the Data
  mode(DATA)
  # Attributes of the array
  INFO <- attributes(DATA)
  
  # Dimensions of the array
  INFO$dim
    # Number of Lines/rows
    Rows    <- INFO$dim[1]
    # Number of samples/colomns
    Columns <- INFO$dim[2]
    # Number of dimensions/wavelength bands
    Bands   <- INFO$dim[3]


  # Access a single value in the Array: in the 10th row, the 12th column and Bandnr 160
  single_val  <- DATA[10,12,160]
  # Access a single column: the 12th column in Bandnr 160
  single_col  <- DATA[,12,160]
  # Access a single row: the 10th row in Bandnr 160
  single_row  <- DATA[10,,160]
  # Extract Bandnr 160
  single_Band <- DATA[,,160] 
  
  # Access a range of values from 10th-15th row, 12-18th column in Bands 160-170
  multiple_vals <- DATA[c(10:15),c(12:18),c(160:170)]

  
  # Basic stats for a single Band (No.160)
  summary(DATA[,,160])
  
  #Wavelength of Band 160
  wavelength <- Header$wavelength[160]

#################################################################################################
#                  plot image data                                                              #
#################################################################################################

  
  # plot a histogram
  hist(DATA[,,160],freq = T, breaks=1000, angle = 45)
  
  # Scatterplot between band 160 and band 80
  matplot(DATA[,,160],DATA[,,c(80)])
  plot(as.vector(DATA[,,160]),as.vector(DATA[,,80]))
  
  # profile of column 12, Band 160
  plot(DATA[,12,160], type="l")
  
  # plot profiles of columns 12-18
  plot.ts(DATA[,c(12:18),160], type="l")

  # plot row 10 of bands 160-165
  plot.ts(DATA[10,,c(160:165)], type="l")

