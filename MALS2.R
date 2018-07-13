#' this script will diplay 280 UV signal of SEC experiment as line
#' additionally the information of nucleic acid content (UV254 to UV280 ratio)
#' will be displayed as color of the line

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
y_l <- c(9,16.75)

### now click "source" to run script
####===========

library(dplyr)
library(ggplot2)

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
  A_ <- A_ %>% mutate(UV280 = UV280+abs(min(UV280)),
                           UV254=UV254+abs(min(UV254)),
                           ratio = UV254/UV280, 
                           ratio = ifelse(is.finite(ratio), ratio, NA))
  A_$samp <- basename(dirname(FILES_[i])) # direcotry name as name of this sample
  
  tab_list[[i]] <- A_
}

TAB_ <- do.call(rbind, tab_list)

TAB_$samp <- factor(TAB_$samp, levels = unique(TAB_$samp))

TAB_ %>% filter(ml > y_l[1], ml < y_l[2]) %>%
  #mutate(ratio = ifelse(ratio>2.5,2.5, ratio)) %>% #uncomment this lien to limit maximum ratio 
  ggplot(aes(ml, UV280, color=ratio)) + geom_line(size=3, color="grey30")+geom_line(size=2)+
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1.144)+
  facet_grid(samp~.)
# alternatively facet_wrap(samp~)

### Note
# midpoint = 5% nucleic acid - 1.144
# midpoint = 2% nucleic acid - 0.8