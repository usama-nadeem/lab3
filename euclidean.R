#' @description 
#' Find euclidean dist between 2 numbers
#' 
#' @param Num1,Num2 Two numbers for which euclidean is to be calculated
#' @return The euclidean dist of `Num1` and `Num2`.
#' @examples
#' euclidean(1, 1)
#' euclidean(10, 1)
#' @export

euclidean = function (Num1, Num2)
{
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