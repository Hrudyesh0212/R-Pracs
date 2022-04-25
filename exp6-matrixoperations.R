repeat
{
  print("**Matrix Operations**")
  print("1.Addition")
  print("2.Subtraction")
  print("3.Multiplication")
  print("4.Division")
  print("5.Exit")
  
  c<-as.integer(readline(prompt = "Enter choice : "))
  
  if(c==5)
  {
    break
  } else
  {
    i1<-as.integer(readline(prompt = "Enter number of rows for
matrix : "))
    j1<-as.integer(readline(prompt = "Enter number of columns
for matrix : "))
    m1<-c()
    print("Enter matrix 1 elements : ")
    for(k1 in 1:(i1*j1))
    {
      m1[k1]<-as.integer(readline())
    }
    matrix1<-matrix(m1,i1,j1)
    print(matrix1)
    m2<-c()
    print("Enter matrix 2 elements : ")
    for(k2 in 1:(i1*j1))
    {
      m2[k2]<-as.integer(readline())
    }
    matrix2<-matrix(m2,i1,j1)
    print(matrix2)
  }
  print("Answer : ")
  switch (c,
          "1" =print(matrix1+matrix2),
          "2" =print(matrix1-matrix2),
          "3" =print(matrix1*matrix2),
          "4" =print(matrix1/matrix2)
  )
}