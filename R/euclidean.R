#' Euclidean method to find greatest Common divisor
#' @description 
#' it takes two numbers and calculate the greatest common divisor of given 2 numbers
#' 
#' @param Num1,Num2 Two numbers for which greatest common divisor is to be calculated
#' @return The greatest common divisor of `Num1` and `Num2`.
#' @examples
#' euclidean(1, 1)
#' euclidean(10, 1)
#' euclidean(100, -1000)
#' euclidean(-100, 1000)
#' euclidean(-10, -100)
#' @export

euclidean <-
function (Num1, Num2)
{
  #condition to check if number is less then 0
  if (Num1 < 0 || Num2 <0)
  {
    Num1 <- abs(Num1)
    Num2 <- abs(Num2)
  }
  #check for the given input is non numeric
  if ( !is.numeric(Num1) || !is.numeric(Num2) )
  {
    stop("Input is non-numeric")
  }
  
  else if (Num1 == 0 && Num2 == 0)
  {
    return (0)
  }
  
  else if (Num1 != 0 && Num2 == 0)
  {
    return (Num1)
  }
  
  else if (Num1 == 0 && Num2 != 0)
  {
    return (Num2)
  }
  
  #Now, implement wiki algo
  else if (Num1 != 0 && Num2!= 0)
  {
    while(Num1 != Num2)
    {
      if(Num1 > Num2)
      {
        Num1 <- Num1 - Num2
      } 
      else
      {
        Num2 <- Num2 - Num1
      }
      
    }
  }
  
  return(Num1)
}
