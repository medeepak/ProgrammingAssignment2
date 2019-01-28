## Objective of this assignment is to find inverse of a matrix and cache its result
## so that when the inverse needs to be computed again, result could be retrieved from
## cache instead of recompting it there by improving performance. We achieve this by
## creating a custom matrix which could store the result of inverse of matrix and allow 
## to retrieve it as and when required.

## Function takes a matrix as input and returns a list of functions which allow to 
## 1. set value to the matrix
## 2. get value of the matrix
## 3. set value of inverse of the matrix
## 4. get value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  set_inverted_matrix <- function(inverted_matrix) inv <<- inverted_matrix 
  get_inverted_matrix <- function() inv
  
  list(set = set, get = get,
       set_inverted_matrix = set_inverted_matrix,
       get_inverted_matrix = get_inverted_matrix)
}


## Returns the inverse of a matrix if result is present in cache. 
## Else, function computes the inverse, caches the result and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    i <- x$get_inverted_matrix()
    if (! is.null(i)) {
      message("Getting inverted matrix from cached data")
      return(i)
    }
   
    data <- x$get()
    i <- solve(data, ...)
    x$set_inverted_matrix(i)    
    i
}
