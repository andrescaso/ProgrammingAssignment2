## In this Programming Assignment will take advantage of the scoping rules of the R language
## and how they can be manipulated to preserve state inside of an R object.

## The makeCacheMatrix function, creates a special "matrix", which is really a list containing
## a function to:
## 1.- set the value of the matrix
## 2.- get the value of the matrix
## 3.- set the value of the inverse matrix
## 4.- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(myMatrix_object, ...) {
  m <- myMatrix_object$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- myMatrix_object$get()
  m <- solve(data, ...)
  myMatrix_object$setsolve(m)
  m
}