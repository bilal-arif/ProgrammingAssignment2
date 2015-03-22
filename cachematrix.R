##This function create a Matrix and places it in Cache memory.
##It is suppose to be a square matrix inorder to solve the inverse of the matrix
##Then the inverse is solved using the solve()function.
## If the inverse already exist then it wont be calculated again
##Otherwise it will be calculated again

##This function create a chache Matrix of the matrix defined in the arguement i.e x
## This also stores the square Matrix and the inverse value
##set function stores the value of Matrix x
##get function displays the value of Matrix x
## setinverse function sets the value of the solve()in the inverse variable
## getinverse function displays the value of the inverse variable

makeCacheMatrix<-function(x=matrix()){
    inverse<-NULL
    set<-function(y){
        x<<- y
        inverse<<-NULL
    }
    get<- function(){x}
    setinverse <-function(solve){inverse<<-solve}
    getinverse<-function(){inverse}
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## This function calculates the inverse
## Then places it in the Cache
## if the inverse already exist in the cache then it will return its value
## Otherwise it will calculate
## the matrix value will fetched from the cache and stored in data variable
## then it will be inversed using solve()
## and later displayed

cacheSolve <-function(x, ...){
    inverse<-x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data<-x$get()
    inverse<-solve(data)
    x$setinverse(inverse)
    inverse
}