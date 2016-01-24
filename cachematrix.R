## the Overall purpose of the two function is to calculate the inverse of a matrix 
## and store the results, so that if the same matrix is called again, It's inverse doesen't need to be recalculated

## The makeCacheMatrix function crates a list of functions that set and displays first a matrix and then its inversion

makeCacheMatrix <- function(x = matrix()) {

Inv<-NULL

set <- function(y) {

                	x <<- y
			Inv<<-NULL
        } 
get<-function()x

setInverse<-function(inverse) Inv<<-inverse 

getInverse<-function()Inv

list(set=set,get=get, setInverse=setInverse, getInverse=getInverse) 

}


## The cacheSolve function checks if inversion was calculated.
##If so, It returns the calculated Inverse matrix. 
##If not, It retrives the original matrix, calculates its inverse # and caches it
 
cacheSolve <- function(x, ...) {		

		Inv<-x$getInverse() 
		
		if(!is.null(Inv)){
			message("getting cached inverse matrix")
			
                	return(Inv)
		}
		Original_Matrix<-(x$get()) 
		
		Inv<-solve(Original_Matrix) #...calculates  the inverse matrix
		
		x$setInverse(Inv)

		Inv
        ## Return a matrix that is the inverse of 'x'
}
