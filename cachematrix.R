## R-Programming - Assignment 2
## Author: Mark Natale
## email: mnatalex54@gmail.com
## Date: 05/18/2015
## Function names: makeCacheMatrix(), cacheSolve()
## Purpose: Create 2 functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## How to use: a <- makeCacheMatrix(matrix(c(1,2,5,6), 2 ,2))
##
## Create four functions:
##		set() - to initialize the cache at function's parent level.
##		get() - grab the matrix
##		setinverse() - write inverse result to cache
##		getinverse() - read cached inverse value
##


makeCacheMatrix <- function(x = matrix()) {
	### Initialize local inv to NULL
	inv <- NULL

	set <- function(y) {
		x   <<- y
		inv <<- NULL
	}

	get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	
	### Create access list with named list members
	list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix() above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve() should retrieve the 
## inverse from the cache.
## How to use: cacheSolve(a)

cacheSolve <- function(x, ...) {
### check for a square matrix
	matrix_size <- dim(x$get())
	if (matrix_size[1] != matrix_size[2]){
		print("Matrix dimensions must be equal!")
		return (NULL)
	}
	### Check for cached copy of inverse matrix. If it exists, use it!
	inv <- x$getinverse()
	### if not the first time to calculate, use cached value.
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
  }
	### Time to read the data
	data <- x$get()
	### Otherwise, check if inversion is possible and compute the inverse 
	### For a given sq. matrix A, & reciprical A^-1 solve for inverse. 
	if (det(data) != 0) {
		inv <- solve(data) %*% data
	} else {
		### On the rare occassion that the inversion does not exist, return NaN
		inv <- NaN	
	}
	### Cache the result
	x$setinverse(inv)
	return (inv)
}
