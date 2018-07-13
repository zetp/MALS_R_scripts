#' this script will diplay overlayed 280 UV signal of SEC experiments
#' additionally second plot will display the same data but normalised (0,1 range)
#' for each experiment (usefull in case of concnetration dependend oligomerization)

#### Dear user ===========
### First set up some parameters

### How many experiments you would like to compare
REPS_ <- 2 # how many runs to compare?

#' you can provide any file from the experiments folder
#' this script will load apropriate files automatically
#' 
#' N.B. the script will fail if there is space in the path to a file
#' but with our setup it never is and issue

### Set limits (in ml) to be displayed
y_l <- c(9,18)

### now click "source" to run script
####===========

library(dplyr)
library(ggplot2)
library(gridExtra)

# LOOP TO LOAD FILES NICELY
FILES_ <- vector("character", REPS_)
while(REPS_>0) { 
  FILES_[REPS_]<-file.choose()
  REPS_ <- (REPS_-1)
}

tab_list <- vector("list", length(FILES_))
tab_list54 <- vector("list", length(FILES_))

for (i in 1 : length(FILES_)) {
  A_ <- read.delim(paste0(dirname(FILES_[[i]]), "/UV280.dat"), header = F, col.names = c("ml", "UV280"))
  B_ <- read.delim(paste0(dirname(FILES_[[i]]), "/UV254.dat"), header = F,  col.names = c("ml", "UV254"))
  A_ <- full_join(A_, B_)
  A_$samp <- basename(dirname(FILES_[i])) # direcotry name as name of this sample
  
  tab_list[[i]] <- A_
}

TAB_ <- do.call(rbind, tab_list)

TAB_$samp <- factor(TAB_$samp, levels = unique(TAB_$samp))


a <- TAB_ %>% filter(ml > y_l[1], ml < y_l[2]) %>% 
  ggplot(aes(ml, UV280, color=samp)) +geom_line(size=2)

b <- TAB_ %>% filter(ml > y_l[1], ml < y_l[2]) %>% group_by(samp) %>% mutate(UV280_normalised = (UV280-min(UV280))/(max(UV280)-min(UV280))) %>% 
  ggplot(aes(ml, UV280_normalised, color=samp)) +geom_line(size=2) 

grid.arrange(a,b, ncol = 1)
