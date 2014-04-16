## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function setups us a cache ofr a matrix and it's inverse
# It starts by creating the empy variable inv, to store teh inverse of matrix x

# Then if defines the function set, fucntion that is responsible for storing 
# the values of the given matrix (argument y) in x and assigning teh value NULL 
# to the inverse of matix x (variable inv)

# It then defines the function get, that simply returns the matrix x

# then defined the function setInverse, function that simply stores the value of inverse matrix
#(vararible inverse) into the variable inv

# The function getInverse, simply returns the stored value for inv

#Then all the functions are store in a list, for easy access from cacheSolve 
makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x <<-y
		inv <<-NULL
	}
	get	<- function() x
	setInverse <-function(inverse) inv <<- inverse
	getInverse <-function() inv
	print(list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))
}


## Write a short comment describing this function
# This function gets a matrix x and returns it's inverse, either by 
# returning the cached value, or calculating it with teh function solve

#The first thing the function does is to get the cached value, using the $getInverse field, 
# that calls the funtion getInverse from makeCacheMatrix
# if the value returned by getInverse is not NULL we simply return the retrieved value, exiting the function.
# If the value returned by getInverse is NULL we don't have the value cached, we need to calculate the inverse of x.
# So we go ont by getting the matrix (using x$get) and calculating the inverse value with solve(matrix);
# We then store the obtained value (x$setInverse(inv)) and return the calculated value (inv)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return (inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInverse(inv)
        inv
}
