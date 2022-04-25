matrix_input <- function(){
  i<-as.integer(readline(prompt = "Enter number of rows : "))
  j<-as.integer(readline(prompt = "Enter number of columns : "))
  m<-c()
  print("Enter matrix elements : ")
  for(k in 1:(i*j))
  {
    m[k]<-as.integer(readline())
  }
  matix<-matrix(m,i,j)
  print(matix)
}
matrix_input()