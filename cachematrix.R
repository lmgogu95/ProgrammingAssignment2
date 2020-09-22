#The functions compute the inverse of a square matrix. 

## This function verifies if the inverse of a matrix is already cached. 
### If it is cached, it avoids computing it and just pulls it out of cache. 
### If it is not cached, then the function computes the inverse of a matrix. 

 
makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse)i<<-inverse
        getinverse<-function()i
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}


## This function computes the inverse of a matrix ONLY if it is not already cached.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cache data")
                return(i)
        }
        data<-x$get()
        i<-solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
        }
