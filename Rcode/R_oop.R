library(sloop)

f <- factor(c("a", "b", "c"))
typeof(f)
attr(f,'class')
unclass(f)

print
summary
head
UseMethod

mat = matrix(1:45, nrow = 9, ncol = 5)
class(mat)
head(mat)
head.matrix
sloop::s3_dispatch(head(mat))

## base r
methods(head)
head.matrix
getS3method('head', 'matrix')
getS3method('head', 'array')


## sloop
class(mat) = c('green', class(mat))
class(mat) = 'green'
head(mat)
sloop::s3_dispatch(head(mat))

head.green = function(obj) {
  # Green escape sequences, from e.g. crayon::green("green"). 
  g1 = '\033[32m'
  g2 = '\033[39m'
  
  # Make sure obj is an object
  if ( !is.object(obj) ) warning("Object 'obj' is not an object!")
  
  # Check if its green
  if ('green' %in% class(obj) ) {
    if ( length(class(obj)) > 1 ) {
      next_class = class(obj)[-grep('green', class(obj))][1]
      cat( sprintf('This is a %sgreen %s%s.\n', g1, next_class, g2) )
      
      # This calls the next available method, allowing us to offload work in
      # a method for the subclass to an existing method for the superclass.
      NextMethod("head")
      
    } else {
      cat(sprintf('This a %sgeneric green object%s.\n', g1, g2))
      getS3method('head', 'default')(obj)
    }
  } else {
    cat(sprintf('The object is not %sgreen%s!\n', g1, g2))
  }
  
}


# Generic Color finder
getColor = function(obj) {
  UseMethod("getColor")
}

# Default method 
getColor.default = function(obj) {
  # Are any classes colors?
  ind = class(obj) %in% colors()
  if ( any(ind) ) {
    # Yes. Return color with highest class predence.
    class(obj)[which(ind)[1]]
  } else {
    # No return a random color.
    sample(colors(), 1)
  }
}

# Specific method aliasing green to "darkgreen"
getColor.green = function(obj) {
  "darkgreen"
}

# A box plot function that uses the class attribute to define colors.
col_boxplot = function(dat, ...) {
  if ( is.atomic(dat) ) {
    boxplot(dat, col = getColor(dat), ...)
  } else{
    col = sapply(dat, getColor)
    boxplot(dat, col = col, ...)
  }
}

# Define some iid data
x = rnorm(100, 1, 1); class(x) = 'green'
y = rnorm(100, 0, 2); class(y) = 'red'
z = rnorm(100, 0, 1)
col_boxplot(list(x = x, y = y, z = z), las = 1)

class(x + y)
class(y + x)
class(mean(x))


## S4
setClass("color_vector",
         slots = c(
           name = 'character',
           data = 'numeric',
           color = 'character'
         )
)
x = new("color_vector", name = "x", color = "darkgreen")
x

color_vector = 
  setClass("color_vector",
           slots = c(
             name = 'character',
             data = 'numeric',
             color = 'character'
           )
  )
y = color_vector(name = "y", data = rnorm(100, 0, 2), color = "red")
class(y)

x@color
x@data = rnorm(10,1,1)
slot(x, 'color')
attr(x, 'name') = 'Green Values'
names(attributes(x))


## compare to S3
s3 = factor(1:10, levels = 1:10, labels = letters[1:10])
attr(s3, "levslot(s3, 'levels')els")
tryCatch({ s3@levels }, error = identity)

color = function(obj) {
  # Accessor function for the "color" slot in the color_vector class
  # Inputs: obj - an object of class color vector
  # Returns: the value of the color slot
  # If the object is not of class color vector, a random color is returned with
  # a warning.
  
  y = as.character(match.call())
  if ( !{"color_vector" %in% class(obj) } ) {
    msg = sprintf('Object %s is not of class color_vector.\n', y[2] )
    warning(msg)
    return( sample(colors(), 1) )
  }
  
  slot(obj, 'color')
  
}
color(x)
color(y)
color(LETTERS)


setValidity("color_vector", function(object) {
  if ( !{object@color %in% colors()} ) {
    sprintf('@color = %s is not a valid color. See colors().', object@color)
  } else {
    TRUE
  }
})
tryCatch({color_vector(name = 'test', data = 1:3, color = 'A')}, 
         error = identity)

show(x)

## Change how color_vector objects are shown.
setMethod('show', 'color_vector',
          function(object) {
            msg = sprintf('name: %s, color: %s\n\n', object@name, object@color) 
            cat(msg)
            cat('Data:')
            str(object@data)
            cat('\n')
          }
)
show(x)

# Define a new accessor for setting the color
setGeneric("color<-", function(object, value) standardGeneric('color<-'))









