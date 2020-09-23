# Question 1
# a
w = sum(1:100)
x = 2 * w
y = x > 5e3
z = typeof( c(x, y) )

# b
x = -1:1
y = rep(1, 10)
z = mean(x * y)

# c
z = 10
f = function(input) {
  z = sqrt(input)
  return( z )
}
input = f(z * z)
output = f(input)

# d
start = 1
goal = 5^2
total = 0
while ( total <= goal ) {
  for ( i in 1:start ) {
    total = total + i 
  }
  start = start + 1
}
z = total

# e
start = list(x = TRUE, y = 'two', z = floor(pi) )
start$xyz = with(start, c(x, y, z) )
out = lapply(start, class)
z = unlist(out)

# f
x = rep(1:3, each = 3)
y = rep(1:3, 3)
dim(x) = dim(y) = c(3, 3)
z = t(x) %*% y
z = z[, 3]


# Question 2
x0 = 1:10000
y0 = x0 * pi / max(x0)
e0 = sum( abs( cos(y0)^2 + sin(y0)^2 - 1 ) )

x1 = 1:100000
y1 = x1 * pi / max(x1)
e1 = sum( abs( cos(y1)^2 + sin(y1)^2 - 1 ) )

z = floor( e1 / e0 )

# Question 3
# b, be, ad

# Question 4
twos = 0
threes = 0
for ( i in 1:10 ) {
  if ( i %% 2 == 0 ) {
    twos = twos + i # 2+4+6+8+10=30
  } else if ( i %% 3 == 0 ) {
    threes = threes + i # 3+9=12
  }
}

# Question 5
x = 0
for ( i in 1:10 ) {
  x = x + switch(1 + {i %% 3}, 1, 5, 10)
}

co = 0
for (i in 1:1000){
  if (sum(sample(c(0,1),size=10,replace = TRUE,prob=c(2/3,1/3)))>=2){
    co = co+1
  }
}

cdf = cumsum(c(0.4615946, 0.4196315, 0.1094691, 0.009122423, 0.0001824485))
x = 0:4
plot(x,cdf)
# lines(c(0,1), c(0.4615946,0.4615946))
for (i in 1:5){
  lines(c(i-1,i),c(cdf[i],cdf[i]))
  lines(c(i,i),c(cdf[i],cdf[i+1]))
}

