#' genProportions
#'
#' function to generate random proportions whose rowSums = 1
#' @param ncol numeric
#' @param nrow numeric
#' @param var.names a vector of strings
#' @export
#' @examples
#' genProportions(ncol=5, nrow=5)
#' genProportions(ncol=3, nrow=25)
#' genProportions(ncol=3, nrow=5, var.names=c("red", "blue", "green"))


genProportions <- function(ncol, nrow, var.names=NULL){
  if (ncol < 2) stop("ncol must be greater than 1")
  p <- function(n){
    y <- 0
    z <- sapply(seq_len(n-1), function(i) {
      x <- sample(seq(0, 1-y, by=.01), 1)
      y <<- y + x
      return(x)
    }
    )
    w <- c(z , 1-sum(z))
    return(w)
  }
  DF <- data.frame(t(replicate(nrow, p(n=ncol))))
  if (!is.null(var.names)) colnames(DF) <- var.names
  return(DF)
}



#' insertNA
#'
#' RANDOMLY INSERT A CERTAIN PROPORTION OF NAs INTO A DATAFRAME
#' @param df a data frame
#' @param prop numeric
#' @export
#' @examples
#' insertNA(genProportions(ncol=10, nrow=10)  , .1)


insertNA = function(df, prop = .1){
  n <- nrow(df)
  m <- ncol(df)
  num.to.na <- ceiling(prop*n*m)
  id <- sample(0:(m*n-1), num.to.na, replace = FALSE)
  rows <- id %/% m + 1
  cols <- id %% m + 1
  sapply(seq(num.to.na), function(x){
    df[rows[x], cols[x]] <<- NA
  }
  )
  return(df)
}




#' genDF
#'
#' GENERATE A RANDOM DATA SET.  CAN BE SET TO LONG OR WIDE. DATA SET HAS FACTORS
#' AND NUMERIC VARIABLES AND CAN. OPTIONALLY GIVE BUDGET EXPENDITURES AS A
#' PROPORTION. CAN ALSO TELL A PROPORTION OF CELLS TO BE MISSING VALUES
#' @param n numeric number of rows
#' @param type either wide or long, defaults to wide
#' @param digits for rounding, defaults to 2
#' @param proportion defaults to FALSE
#' @param na.rate numeric, defaults to 0
#' @examples
#' genDF()
#' genDF(type="long")
#' genDF(20000)
#' genDF(prop=T)
#' genDF(na.rate=.3)



genDF <- function(n=10, type=wide, digits=2,
                             proportion=FALSE, na.rate=0) {

  rownamer <- function(dataframe){
    x <- as.data.frame(dataframe)
    rownames(x) <- NULL
    return(x)
  }

  dfround <- function(dataframe, digits = 0){
    df <- dataframe
    df[,sapply(df, is.numeric)] <-round(df[,sapply(df, is.numeric)], digits)
    return(df)
  }

  TYPE <- as.character(substitute(type))
  time1 <- sample(1:100, n, replace = TRUE) + abs(rnorm(n))
  DF <- data.frame(id = paste0("ID.", 1:n),
                   group= sample(c("control", "treat"), n, replace = TRUE),
                   hs.grad = sample(c("yes", "no"), n, replace = TRUE),
                   race = sample(c("black", "white", "asian"), n,
                                 replace = TRUE, prob=c(.25, .5, .25)),
                   gender = sample(c("male", "female"), n, replace = TRUE),
                   age = sample(18:40, n, replace = TRUE),
                   m.status = sample(c("never", "married", "divorced", "widowed"),
                                     n, replace = TRUE, prob=c(.25, .4, .3, .05)),
                   political = sample(c("democrat", "republican",
                                        "independent", "other"), n, replace= TRUE,
                                      prob=c(.35, .35, .20, .1)),
                   n.kids = rpois(n, 1.5),
                   income = sample(c(seq(0, 30000, by=1000),
                                     seq(0, 150000, by=1000)), n, replace=TRUE),
                   score = rnorm(n),
                   time1,
                   time2 = c(time1 + 2 * abs(rnorm(n))),
                   time3 = c(time1 + (4 * abs(rnorm(n)))))
  if (proportion) {
    DF <- cbind (DF[, 1:10],
                 props(ncol=3, nrow=n, var.names=c("food",
                                                   "housing", "other")),
                 DF[, 11:14])
  }
  if (na.rate!=0) {
    DF <- cbind(DF[, 1, drop=FALSE], NAins(DF[, -1],
                                           prop=na.rate))
  }
  DF <- switch(TYPE,
               wide = DF,
               long = {DF <- reshape(DF, direction = "long", idvar = "id",
                                     varying = c("time1","time2", "time3"),
                                     v.names = c("value"),
                                     timevar = "time", times = c("time1", "time2", "time3"))
               rownamer(DF)},
               stop("Invalid Data \"type\""))
  return(dfround(DF, digits=digits))
}
