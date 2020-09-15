df = data_frame(
  ID = 1:10,
  Group = sample(0:1,10,replace = TRUE),
  Var1 = rnorm(10),
  Var2 = seq(0,1,length.out = 10),
  Var3 = rep(c('a','b'),each=5)
)

names(df)
colnames(df)
dim(df)
length(df)
nrow(df)
class(df$Var3)
row.names(df)

#subset as a list
df$ID
df[['Var3']]

#subset as a array
df[1:5,]
df[,"Var2"]

lapply(df,length)








