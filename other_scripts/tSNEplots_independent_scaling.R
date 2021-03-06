# Title: Looped R script for generating colourised tSNE plots
    # Author: Thomas Ashhurst 
    # Published to github: 2017-04-28
    # Contact: tomashhurst@gmail.com
    # Website: www.sydneycytometry.org.au
    # Github: www.github.com/sydneycytometry

# Description
    # A simple loop to create tSNE plots coloured by each 'parameter'
    # Data can be read from .csv files or .fcs files, with the option to perform a transformation on the imported data
    
    # R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
    # ggplot2 version     2.2.1
    # colorRamps version  2.3
    # ggthemes version    3.4.0
    # scales version      0.4.1


##### SUMMARY #####
  # Step 1: Run section by section - load relevant packages, and setup for the script
  # Step 2: Run section by section - user unput
  # Step 3: Run entire step, after modifying the names of the tSNE parameters


##### STEP 1: Load packages and setup required for script #####

  ## Will install packages (if not already installed)
  if (!require("ggplot2")){install.packages("ggplot2")} # for plotting tSNE graphs
  if (!require("colorRamps")){install.packages("colorRamps")} # for colour scheme management
  if (!require("ggthemes")){install.packages("ggthemes")} # for plot themes
  if (!require("scales")){install.packages("scales")} # for re-scaling if necessary

  ## Load packages
  library(ggplot2) # for plotting tSNE graphs
  library(colorRamps) # for colour scheme management
  library(ggthemes) # for plot themes
  library(scales) # for re-scaling, only if necessary

  ## Create 'jet' colour scheme (not available by default in R)
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


##### STEP 2: USER INPUT #####
  
  ## Set your working directory here (e.g. "/Users/Tom/Desktop/")
  setwd("/Users/Tom/Desktop/CSVfiles/") 

  ## Check your working directory has changed correctly
  getwd() 
  
  ## Assign the working directory as 'PrimaryDirectory'
  PrimaryDirectory <- getwd()
  PrimaryDirectory

  ## Create a list of file names (names of the samples) and check file names
  FileNames <- list.files(path=PrimaryDirectory, pattern = ".csv")
  FileNames

  ## Enter the name of your first sample in between the "", and to check the tSNE parameter names (could be bhSNE, bh.SNE, vSNE, tSNE, etc)
  names(read.csv("Sample1.csv"))
  
  ## IMPORTANT
  ## The last user input required is on lines 88 and 89 --after "CurrentSampleCSV$" type in the name of your 'tSNE parameter, from above
  
  
##### STEP 3: Loop with  samples in separate folders #####  
  ## First, change the tSNE parameters (around lines 88 and 89)
  ## Then run all of the script below
  
  ## Set wd
  setwd(PrimaryDirectory)
  getwd()
  
  ## Create output folder (if a folder called "Output" already exists, nothing will happen)
  dir.create("Output(subfolders)", showWarnings = FALSE)
  
  for (File in FileNames){ 
    
    ## CSV is read into dataframe 'CurrentSampleCSV'
    CurrentSampleCSV <- read.csv(File)
    CurrentSampleCSV
    
    # Modifications to the name are made here
    File <- gsub(" ", "_", File) # replaces empty spaces in the file name with '_'
    File <- gsub(".csv", "", File) # removes ".csv" from the file name 
    
    ## After the '$', define the tSNE parameter names (as they appear in the CSV files) for tSNE1 (x-axis) and tSNE2 (y-axis)
    tSNE1 <- CurrentSampleCSV$tSNE1 # defines the tSNE1 (x-axis) parameter name from your file
    tSNE2 <- CurrentSampleCSV$tSNE2 # defines the tSNE2 (y-axis) parameter name from your file
    
    ## Create subdirectory
    setwd("Output(subfolders)") 
    newdir <- paste0(File)
    dir.create(newdir, showWarnings = FALSE)
    setwd(newdir) 
    getwd()
    
    ## Sub-loop to create one image for every parameter
    for (i in names(CurrentSampleCSV)){ 

      tSNEplotLoop <- ggplot(
        data = `CurrentSampleCSV`,
        aes(x = tSNE1, y = tSNE2)) +
        geom_point(size = 0.5, mapping=aes_string(color=i))+ # 2 for large # 0.5 for small
        scale_colour_gradientn(colours = jet.colors(50),
                               limits = c(quantile(CurrentSampleCSV[[i]], probs = c(0.01)), #0.03-01 seems to work well
                                          quantile(CurrentSampleCSV[[i]], probs = c(0.995))), #0.97-995 seems to work well
                               oob=squish) + 
        ggtitle(i) +
        # scale_x_continuous(limits = 40, -40)+ # use if necessary
        # scale_y_continuous(limits = 40, -40)+ # use if necessary
        # xlab("tSNE1") + # use if desired, must also change theme settings below
        # ylab("tSNE2") + # use if desired, must also change theme settings below
        theme( # panel.background = element_rect(fill = "white", colour = "white", size = 0.5), # change 'colour' to black for informative axis
              axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank(),
              legend.position = "right",
              legend.text=element_text(size=15), # large = 30 # small = 8
              legend.key.height=unit(1,"cm"), # large = 3 # small = 1.2
              legend.key.width=unit(0.4,"cm"), # large = 1 # small = 0.4
              legend.title=element_blank(),
              plot.title = element_text(color="Black", face="bold", size=15, hjust=0) # size 70 for large, # 18 for small
        )
      ggsave(tSNEplotLoop, filename = paste0(File, "-", i,".jpeg"), width = 4, height = 3) # Large size default width = 14.4 height = 12 (11.35 without title), default = PDF, # small w3.6, h3
    }
    
    ## Move back to PrimaryDirectory
    setwd(PrimaryDirectory)
    getwd()  
  }
