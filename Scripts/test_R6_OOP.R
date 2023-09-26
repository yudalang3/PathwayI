Parent <- R6Class(
  classname = "Parent",
  public = list(
    f1 = function(x) {
      cat("I am Parent f1 function in Parent\n")

      self$f2()

      self$f3()
    }
    # f2 = function() {
    #   cat("I am Parent f2 function in Parent\n")
    # },
    # f3 = function() {
    #   cat("I am Parent f3 function in Parent\n")
    # }

  )
)


Son1 <-
  R6Class(
    classname = "Son1",
    inherit = Parent,
    public = list(
      f2 = function() {
        cat("I am Parent f2 function in Son1\n")
      },
      f3 = function() {
        cat("I am Parent f3 function in Son1\n")
      }

    )
  )

# p <- Parent$new()
# p$f1()

ss1 <- Son1$new()
ss1$f1()

# should output:
# I am Parent f1 function in Parent
# I am Parent f2 function in Son1
# I am Parent f3 function in Son1
