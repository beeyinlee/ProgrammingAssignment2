## cache inverse matrix computations 

## Function to create special function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function to check to see if the inverse matrix has already been calculated
## Function checks if the inverse matrix is calculated
## If caclculated, return from the cache
## If not calculated, calculate the inverse matrix and set the value in cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }
