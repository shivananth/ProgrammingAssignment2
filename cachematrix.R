##--------------------------------------------------------------------------------
## makeCacheMatrix accepts a matrix as input and creates a list of 4 functions 
## with a provision to cache the inverse of matrix which is passed
##--------------------------------------------------------------------------------
## cacheSolve checks if the inverse of the special matrix created by makeCacheMatrix 
## function being passed in already in cache memory. If it is in cache memory, it
## retrieves that value, else it computes the inverse for the first time. 

## Imp Note: This function doesn't work on normal matrices. The matrix being passed
## to cacheSolve must be created using makeCacheMatrix function
##--------------------------------------------------------------------------------

##--------------------------------------------------------------------------------
## makeCacheMatrix
##--------------------------------------------------------------------------------
## This function accepts a matrix as input ad a creates a list of 4 functions
## 1.setMatrix ---> assigns the matrix value being passed to matrix x defined in 
##                  parent environment
## 2.getMatrix ---> fetches the value of the matrix x
## 3.setInverse ---> creates a cached copy of the inverse
## 4.getInverse ---> fetches the cached copy of the inverse
##--------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL ## this variable is used to store the inverse & cache it
  setMatrix <- function(y) {
    x <<- y ## cache-ing the matrix
    cache <<- NULL
  }
  getMatrix <- function() x 
  setInverse <- function(inverse) cache <<- inverse ## cache-ing the inverse
  getInverse <- function() cache
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

##--------------------------------------------------------------------------------
## cacheSolve
##--------------------------------------------------------------------------------
## This function accepts a cached matrix created using makeCacheMatrix function
## as input and gives the inverse

## If the cached inverse of the cached matrix is not null, it retrieves the
## value stored in cache variable in the above function

## If the cached inverse is null for the cached matrix, it computes the inverse 
## and returns the value

## When retrieving cached inverse it displays "getting cached data"
##--------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse() ##gets the cached inverse of cached matrxi
  if(!is.null(inverse)) { 
    message("getting cached data")
    return(inverse)
  }
  data <- x$getMatrix()
  inverse <- solve(data) ## computing the inverse if cached inverse is null
  x$setInverse(inverse) ## caching the computed inverse value
  inverse
}

##--------------------------------------------------------------------------------
## sample check
##--------------------------------------------------------------------------------

cacheMatrix<-makeCacheMatrix(matrix(1:4,2,2))
cacheMatrix$getMatrix()

##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4

cacheSolve(cacheMatrix) ##first instance when inverse is created
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve(cacheMatrix) ##second instance when inverse is fetched from cache

##getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5