## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix will create a function that returns a list that will hold a matrix's value and it's inverse value
#cacheSolve will take in a list variable coming from the output of makeCacheMatrix.
#it will check to see if there is the inverse of the matrix already stored in it, if so then it will just return that value
#otherwise it will calculate it on the fly and then store it into the list via setsolve

## Write a short comment describing this function
#this function is very similar to the set up of the makeVector function provided
#it takes in a matrix instead and the solve function is used as a part of the functionality
#similarly it stores the output in a list
makeCacheMatrix <- function(x = matrix()) 
{ 
  m <- NULL
  set<-function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function
#this funtion takes in the list coming from makeCacheMatrix
#basically checks whether the inverse has been calculated, if so then just use that value, 
#otherwise calculate it with solve and then store into the list
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() 
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setsolve(m)
  m
}