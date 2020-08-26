## In this program, I wrote a pair of functions, namely, "makeCacheMatrix"
## and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that
## can cache its inverse for the input(which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
      set <- function(y)
      {
           x <<- y
           inv <<- NULL
      }
      get <- function(){x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)     
}


## cacheSolve is a function which computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.if the inverse has already been calculated 
## (and the matrix has not changed),then the cacheSolv should retreieve 
## the inverse from the cache 


cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv))
      {
             message("getting cached data")
             return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}

## --------------------------checking the program-----------------------------

## p <- matrix(rnorm(9),3,3)
## p1 <- makeCacheMatrix(p)
## cacheSolve(p1) 

##            [,1]       [,2]       [,3]
## [1,]  0.1970638  0.3093324 -0.4585106
## [2,] -0.3313558  0.1846536  0.6087729
## [3,]  0.6750986 -0.2676764 -0.1432893
