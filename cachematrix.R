## Put comments here that give an overall description of what your
## functions do

## These two functions will cache the inverse of a matrix
## This is an "expensive" operation (in the old days, they actually charged
## lots of money for "expensive" operations ;^)  ), so we save the
## results and return the saved results if the inverse was already calculated

## Write a short comment describing this function

## The first function creates a special matrix object that actually stores
## four functions to set and get values of the matrix, and set and get 
## the inverse of the matrix

## I think that this would be called to create an object like this:
## MyCleverMatrix <- makeCacheMatrix

## I don't think I need to actually specify the argument x, as an object
## lexically in the created function (the first one is called "MyCleverMatrix"
## in my case) will hold the value of the matrix after we call the set
## function within the MyCleverMatrix object which is named as if it's a 
## matrix, but really it's a function with objects and functions inside...

## Suppose I create a matrix like this:

## TruelyAMatrix <- matrix(1:10000, nrow=100, ncol=100)

## Then the tricky thing is now that MyCleverMatrix$set(TruelyAMatrix)
## will set my clever matrix object to have that same value by assigning
## the matrix to the hidden "x" matrix object inside the created function
## MyCleverMatrix, which is really a function object with four functions
## and two hidden matrix objects

## Similarly, MyCleverMatrix$get() will return the value of the matrix we
## stored in the secret hidden object place inside the MyCleverMatrix
## function object

## The second function below will use the setinverse and getinverse
## functions, but with MyCleverMatrix passed as an argument, so it looks
## like x$setinverse inside the definition of that function

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y		# capture the value of the passed matrix here
		i <<- NULL	# this resets i (inverse) when we get a new matrix
	}
	get <- function() x		# just retrieve stored value of the matrix
	setinverse <- function(inv) i <<- inv	# store inverse in hidden i
	getinverse <- function() i				# fetch inverse in hidden i
	list (set = set, get = get,
		  setinverse = setinverse,	# this returns a list, this is what
		  getinverse = getinverse)	# ...makes the functions visible
}


## Write a short comment describing this function

## This function will operate on a function object created by makeCacheMatrix
## the function has two hidden objects which can store the matrix and its
## inverse, as well as four "published" functions which are used to get and
## set the matrix and inverse values hidden deep within

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()	# first get the stored inverse value
	if ( !is.null(i) )	{	# if stored inverse not NULL, it was calculated
		message ("Getting cached value of inverse")
		return (i)
	}
	data <- x$get()		# get value of matrix stored in function object
	i <- solve (data, ...)	# note we pass other arguments to solve
	x$setinverse(i)			# now tuck this away in the hidden i object
							# ...in the function created by makeCacheMatrix
	i				# still have to return the inverse
}
