cvec = c()
cvec
length(cvec)
typeof(cvec)

vvec = vector('character', length = 4)
vvec
typeof(vvec)

vvec = vector('integer', length = 4)
vvec
typeof(vvec) 

w = TRUE
x = 1L
y = 1
z = "One"

vapply(list(w,x,y,z,c(w,x,y,z) ),FUN=typeof,FUN.VALUE = 'type')

list(w,x,y,z)[4] # some_list[] select a sub_list
list(w,x,y,z)[[4]] # some_list[[]] select a single element


## Ex1: assign with names()
x = 1:3
names(x) = c('Uno', 'Dos', 'Tres')
x['Uno']

x = c( 'Uno' = 1, 'Dos' = 2, 'Tres' = 3 )
x

x = c( Uno = 1, Dos = 2, Tres = 3)
x['Uno']

attr(x, "name")
class(x) = 'character'
x

# dim
x = matrix(1:10,nrow=5,ncol=2)
dim(x)
class(x)
dim(x) = c(2,5)
x

dim(x) = c(5, 1, 2)
class(x)
x
dim(x)

dim(x) = c(5, 2, 1)
class(x)
x

attr(x,'color') = 'green'
x
attr(x,'color')
attributes(x)








