inc.mformat <- function(div = 1000){
  function(x) format(x / div, digits = 2) 
}
