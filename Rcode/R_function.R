
x = runif( n = 5, min = 0, max = 1)
y = runif(5, 0, 1)
z = runif(5)
round( cbind(x, y, z), 1)

# 1. Use function to avoid repeating yourself


# function to compute z-scores
z_score1 = function(x) {
  #inputs: x - a numeric vector
  #outputs: the z-scores for x
  xbar = mean(x)
  s = sd(x)
  z = (x - mean(x)) / s
  
  return(z)  
}
stopifnot(z_score1(1:3) == -1:1)

z_score2 = function(x){
  #inputs: x - a numeric vector
  #outputs: the z-scores for x
  {x - mean(x)} / sd(x)
}
x = rnorm(10, 3, 1) ## generate some normally distributed values
round( cbind(x, 'Z1' = z_score1(x), 'Z2' = z_score2(x) ), 1)

# Default Parameter
# function to compute z-scores
z_score3 = function(x, na.rm=T){
  {x - mean(x, na.rm=na.rm)} / sd(x, na.rm=na.rm)
}
x = c(NA, x, NA)
round( cbind(x, 'Z1' = z_score1(x), 'Z2' = z_score2(x), 'Z3' = z_score3(x) ), 1)


f1 = function() {
  f1_message = "I'm defined inside of f!"  # `message` is a function in base
  ls()
}
f1()
exists('f1') # 'fi' %in% ls()

environment()
f2 = function(){
  environment()
}
f2()


y = x = 'I came from outside of f!'
f3 = function(){
  x =  'I came from inside of x!'
  list( x = x, y = y )
}
f3()
x

f4 = function(x){
  #x
  45
}
f4( x = stop("Let's pass an error.") )










