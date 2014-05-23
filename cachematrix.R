## Funciones que permiten calcular la inversa de una matriz, una vez que 
## se calcula, se guarda en cache y siempre y cuando la matriz no haya 
## no haya cambiadoel valor devuelto será el que esta en cache previamente
## calculado

## Esta función crea una matriz especial que puede almacenar en cache su inversa

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set =  set, get = get,
           setinverse = setinverse, 
           getinverse = getinverse)
}


## Esta función calcula la inversa de la matriz devuelta por makeCacheMatrix

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)){
            message("Getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      #b <- solve(data)
      #y <- data %*% x
      #x <- b %*% y
      #m <- b %*% data
      x$setinverse(m)
      ## Return a matrix that is the inverse of 'x'
      m
}
