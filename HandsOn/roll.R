roll2 <- function() {
  die <- 1:6
  vals <- sample(die, 2, replace = TRUE)
  sum(vals)
}

roll3 <- function(){
  die <- 1:6
  vals <- sample(die, 2, replace = TRUE, prob=c(1,1,1,1,1,3))
  sum(vals)
}