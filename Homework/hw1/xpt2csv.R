library('foreign') 
# data=read.xport("OHXDEN_G.XPT") 
# write.csv(data, file ="OHXDEN_G.csv")
# data=read.xport("OHXDEN_H.XPT") 
# write.csv(data, file ="OHXDEN_H.csv")
# data=read.xport("OHXDEN_I.XPT") 
# write.csv(data, file ="OHXDEN_I.csv")
# data=read.xport("OHXDEN_J.XPT") 
# write.csv(data, file ="OHXDEN_J.csv")
name = commandArgs(TRUE)[1]
data=read.xport(name) 
csv_name = paste(substr(name,1,nchar(name)-3),"csv",sep="")
write.csv(data, file=csv_name)
                
