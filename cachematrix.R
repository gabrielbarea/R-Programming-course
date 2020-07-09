## This code is for use in the R Programming Coursera Data Science course in Week 3: 
## Assignment 1: Caching the Inverse of a Matrix
## Code by: Gabriel E. B. de Barros
## GitHub user: gabrielbarea

## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. In this code is 
## a pair of functions that cache the inverse of a matrix

## Start of the code
## Part 1: makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) { ## This function define the arguments and the 
                                            ## matrix mode
      inv_m <- NULL ## sets inv_m as NULL
      
      set <- function(y){ ## function that set the space to the matrix
            x <<- y
            inv_m <<- NULL ## if there's a matrix set in the function, reset it to NULL
      }
      
      ## 'get' function: Returns the value of the defined matrix
      get <- function() x 
      
      ## 'setInv' function: Forms the allocation of a value of 'Inv'
      setInv <- function(Inv) inv_m <<- Inv 
      
      ## 'getInv' function: Get the value of 'Inv'
      getInv <- function() inv_m
      
      ## Set the list of the functions to be called when using the '$'
      list(set = set, get = get, setInv = setInv, getInv = getInv) 
}

## Part 2: cacheSolve 
cacheSolve <- function(x, ...){ ## Form a matrix inverse of the above
      
      inv_m <- x$getInv() ## Calculate the inverse of the matrix created with 
                          ##'makeCacheMatrix'
      
      if(!is.null(inv_m)){ ## Checks if the inverse has already been calculated
            message("getting cached data")
            return(inv_m) ## If positive: return the inverse and shows the message above
      }
      
      ## Else: calculates the inverse of the matrix and sets cache, returning the result
      data <- x$get()
      inv_m <- solve(data, ...)
      x$setInv(inv_m)
      inv_m 
}

## End
