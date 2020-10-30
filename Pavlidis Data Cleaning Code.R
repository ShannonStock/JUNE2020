#  Copyright 2020 Shannon Stock

#  Permission is hereby granted, free of charge, to any person
#  obtaining a copy of this software and associated documentation files
#  (the "Software"), to deal in the Software without restriction,
#  including without limitation the rights to use, copy, modify, merge,
#  publish, distribute, sublicense, and/or sell copies of the Software,
#  and to permit persons to whom the Software is furnished to do so,
#  subject to the following conditions:

#  The above copyright notice and this permission notice shall be
#  included in all copies or substantial portions of the Software.

#  THE  SOFTWARE IS  PROVIDED "AS  IS", WITHOUT  WARRANTY OF  ANY KIND,
#  EXPRESS OR IMPLIED,  INCLUDING BUT NOT LIMITED TO  THE WARRANTIES OF
#  MERCHANTABILITY,    FITNESS   FOR    A   PARTICULAR    PURPOSE   AND
#  NONINFRINGEMENT. IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
#  BE LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER  LIABILITY, WHETHER IN AN
#  ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT  OF OR IN
#  CONNECTION WITH  THE SOFTWARE  OR THE USE  OR OTHER DEALINGS  IN THE
#  SOFTWARE.

library(data.table)

#Function to read in all of the study data files
# input- path to directory where R-Friendy study data are stored
# return- dataframe of all study information EXCEPT the subjects in badSubj
readInDataFiles <- function(path) {
  #check for whether a path is given 
  if(missing(path)) {
    p <- getwd();
  } else {
    p <- path;
  }
  temp <- list.files(path=p, pattern="T[[:alnum:]]{3}.csv");
  #Remove bad subjects--those that don't have complete experiments
  badSubj <- c("T007.csv",'T015.csv','T019.csv', 'T021.csv', 'T026.csv','T032.csv','T042.csv','T046.csv','T051.csv'); 
  temp <- temp[!temp %in% badSubj];
  
  readCsv <- function(file) {
    read.delim(file.path(path, file), header=TRUE, sep=",")
  }
  myfiles <- lapply(temp, readCsv);
  
  #Add subject col to each dataframe
  subjects <- gsub(".csv", "", temp)
  myfiles <- lapply(mapply(list, subjects, myfiles, SIMPLIFY=F),
                    function(x) { myjoin <- cbind(x[[2]], Subject=x[[1]]); myjoin;})
  
  
  #print(str(myfiles))
  return(myfiles);
}

# function to process a subject's drive (cognitive (drive = 5); 
# emotional (drive = 6); sensorimotor (drive = 7))
# input-
#   subjData- all of subject's drive data
#   driveNum- drive number to analyze
#   baseline - subject's baseline drive information (ie. drive = 4)
#   variables- the variables of interest
# returns- dataframe of "Subject","Time","Drive","Stimulus", [variables], [baseline variables], phase
#note variable = Columns of interest, e.g. Perinasal.Perspiration, etc
processDrive <- function(subjData, driveNum, baseline, variables) {
  #Select the drive number to analyze
  ddata <- subset(subjData, subjData['Drive'] == driveNum);
  #print(head(ddata))
  #build up a list of phases for this drive which we return
  ls <- data.frame()
  
  #Break drive into 5 phases
  phases <- split(ddata, rleid(ddata$Stimulus));
  breakTimes <- lapply(phases, function(x) { max(x['Time'])});
  #Check to ensure that the drive has 5 phases; otherwise return NULL
  if (length(breakTimes) != 5) {
     return(NULL);
  }
  #print(breakTimes);

  startTime = 0
  for (i in 1:5) {
    base <- subset(baseline, (baseline['Time'] > startTime) & (baseline['Time'] <= breakTimes[i]));
    #IF we can't find a corresponding time point for baseline drive, e.g. for 
    #times in phase5 exceed the max drive time of the baseline, then we match
    #up the phase with the last parts of the base
    if (nrow(base) == 0) { #We ran out of time on the baseline drive
        base <- tail(baseline, nrow(phases[[i]]));
    }
    startTime = breakTimes[i];
    
    #Ensure they are equal length
    len <- min(nrow(phases[[i]]), nrow(base));
    tmp <- head(phases[[i]], len);
    #add in the baseline col
    tmp <- cbind(tmp, Baseline=base[variables], Phase=i);

    ls <- rbind(ls, tmp)
  }
  return(ls)
}

# function to process the experiment data selecting for the variables of
# interest in the input 'variables'
#NOTE: variable = Columns of interest, e.g. Perinasal.Perspiration, etc
processDataFiles <- function(allData, variables) {
  ret <- data.frame()
  for (subject in allData) {
    #Remove practice drives, i.e. drives 1-3
    mydata <- subset(subject , subject['Drive'] >= 4);
    #Select only Time, Drive, Stimulus, variables- e.g. Perinasal.Perspiration
    cols <- c('Subject', 'Time', 'Drive','Stimulus', variables);
    mydata <- mydata[cols];
    
    #Select out the baseline drive, drive 4
    baseline <- subset(mydata, mydata['Drive'] == 4);
    tmp <- data.frame()
    for (dr in 5:7) {
      row <- processDrive(mydata, dr, baseline, variables); 
      if (!is.null(row)) {
          tmp <- rbind(tmp, row);
      }
    }
    ret <- rbind(ret, tmp)
  }
  return(ret)
}

args <- commandArgs( trailingOnly = TRUE )
arg_dir= args[1]
arg_out = args[2]
#arg_dir="~/Downloads/ss/data"
#arg_out="~/Downloads/ss/allProcessedData.csv"

variables = c("Palm.EDA","Heart.Rate","Breathing.Rate", "Perinasal.Perspiration","Lft.Pupil.Diameter","Rt.Pupil.Diameter", "Steering");

data <- readInDataFiles(arg_dir);
tbl = processDataFiles(data, variables);
write.csv(tbl, arg_out, row.names = FALSE);

