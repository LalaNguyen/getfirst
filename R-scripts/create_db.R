
a <- "345173"
abs_path <- paste0("C:/Users/Administrator/OPAIRS/train/",a,"/",a,".csv")

dat <- read.csv(abs_path, sep = ",", header=TRUE)
