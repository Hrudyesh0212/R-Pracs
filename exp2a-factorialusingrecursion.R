fact <- function(n){
  if(n <= 1){
    return(1)
  }
  else {
    return(n * fact(n-1))
  }
}
n = as.integer(readline(prompt = "Enter Number: "))
f = fact(n)
print(paste("Factorial of ", n, " is ", f))

  