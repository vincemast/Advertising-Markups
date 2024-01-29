require(ggplot2)
############################################################
############################################################
#########   1: save plot with approiate directs    #########
############################################################
############################################################

save_f <- function(plot, name, directories, w, h, switch) {

  #only run if switch is on
  if (switch == 1) {
    #set wd to folder for saved images
    setwd(directories[3])

    #save plot
    ggsave(plot = plot, filename = name, width = w, height = h, units = "in")

    #return wd to functions folder
    setwd(directories[1])
  }

}