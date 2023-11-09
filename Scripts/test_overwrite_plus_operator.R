`+.foo` <- function(e1, e2) {
  e1$a <- e1$a + e2$a
  e1$b <- paste0(e1$b, e2$b)
  return(e1)
}

foo1 <- structure(.Data = list(a = 1 , b = "aa"), class = "foo")
foo2 <- structure(.Data = list(a = 2 , b = "bb"), class = "foo")

foo1 + foo2
