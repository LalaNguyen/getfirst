library(reshape)
abs_path<-"C:/Users/Administrator/OPAIRS/R-scripts/"

# Define number of topics and file to write here
file_name = "345173"

output_dir <- paste0(abs_path,"output","/",file_name,"/")
dat <- read.csv(paste0(output_dir,"/",file_name,"_composition.csv"), header=FALSE,sep ="\t", stringsAsFactors = FALSE)

# Remove the id column
# Create links file for gephi
dat <- dat[,-1]
number_of_topics= ncol(dat)-2

names(dat)<-c("id",paste("Topic",0:number_of_topics))
mdata <- melt(data = dat,id.vars = c("id"))
file_id <- unlist(strsplit(mdata$id,"/"))
names(mdata)<-c("source","target","weight")
mdata$source = gsub(".txt","",file_id[grep(".txt",file_id)])
write.csv(mdata, file = paste0(output_dir,file_name,"_links.csv"))
