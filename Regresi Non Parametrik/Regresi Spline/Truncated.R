trun <- function(data,a,power)
{
  data[data<a] <- a
  (data-a)^power
}
