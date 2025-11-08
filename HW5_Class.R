## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

setValidity(
  Class = "sparse_numeric",
  method = function(object){
    check <- anyNA(object@value) || anyNA(object@pos) || anyNA(object@length)
    if (check){
      return("Values are NA")
    }
    check <- length(object@length) != 1L || object@length <= 0L
    if (check){
      return("length needs to be a single positive integer")
    }
    check <- length(object@value) != length(object@pos)
    if (check){
      return("Position vector and value vector are not the same length")
    }
    check <- any(object@pos < 1L) || any(object@pos > object@length)
    if (check){
      return("Position not in range")
    }
    check <- any(duplicated(object@pos))
    if (check){
      return("Duplicate positions")
    }
    check <- any(object@value == 0)
    if (check){
      return("Zero in value vector")
    }
  }
)
setAs("numeric","sparse_numeric",
      function(from){
        new("sparse_numeric",
            value = from[which(from != 0)],
            pos = as.integer(which(from != 0)),
            length = as.integer(length(from)))
      })
setAs("sparse_numeric","numeric",
      function(from){
        num <- numeric(length = from@length)
        num[from@pos] <- from@value
        num
      })
setGeneric("sparse_add", function(x, y, ...){
  standardGeneric("sparse_add")
})

setMethod("sparse_add", "sparse_numeric",
          function(x, y, ...){
            if (x@length != y@length) {
              stop("Lengths do not match match")
            }
            poses <- sort(unique(c(x@pos, y@pos)))
            values <- numeric(length(poses))
            for (i in seq_along(poses)){
              pos <- poses[i]
              x_v <- 0
              y_v <- 0
              if (pos %in% x@pos){
                x_v <- x@value[which(x@pos == pos)]
              }
              if (pos %in% y@pos){
                y_v <- y@value[which(y@pos == pos)]
              }
              values[i] <- x_v + y_v
            }
            
            keep <- values != 0
            poses <- poses[keep]
            values <- values[keep]
            new("sparse_numeric",
                value = values,
                pos = as.integer(poses),
                length = x@length)
          }
)       
setMethod(
  "+",
  c("sparse_numeric", "sparse_numeric"),
  function(e1, e2) {
    sparse_add(e1, e2)
  }
)
x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
sparse_add(x, y)
setGeneric("sparse_sub", function(x, y, ...){
  standardGeneric("sparse_sub")
})

setMethod("sparse_sub", "sparse_numeric",
          function(x, y, ...){
            if (x@length != y@length) {
              stop("Lengths do not match match")
            }
            poses <- sort(unique(c(x@pos, y@pos)))
            values <- numeric(length(poses))
            for (i in seq_along(poses)){
              pos <- poses[i]
              x_v <- 0
              y_v <- 0
              if (pos %in% x@pos){
                x_v <- x@value[which(x@pos == pos)]
              }
              if (pos %in% y@pos){
                y_v <- y@value[which(y@pos == pos)]
              }
              values[i] <- x_v - y_v
            }
            keep <- values != 0
            poses <- poses[keep]
            values <- values[keep]
            new("sparse_numeric",
                value = values,
                pos = as.integer(poses),
                length = x@length)
          }
)       
setMethod(
  f = "-", c("sparse_numeric", "sparse_numeric"),
  function(e1, e2) {
    sparse_sub(e1, e2)
  }
)
setGeneric("sparse_mult", function(x, y, ...){
  standardGeneric("sparse_mult")
})

setMethod("sparse_mult", "sparse_numeric",
          function(x, y, ...){
            if (x@length != y@length) {
              stop("Lengths do not match match")
            }
            poses <- sort(unique(c(x@pos, y@pos)))
            values <- numeric(length(poses))
            for (i in seq_along(poses)){
              pos <- poses[i]
              x_v <- 0
              y_v <- 0
              if (pos %in% x@pos){
                x_v <- x@value[which(x@pos == pos)]
              }
              if (pos %in% y@pos){
                y_v <- y@value[which(y@pos == pos)]
              }
              values[i] <- x_v * y_v
            }
            keep <- values != 0
            poses <- poses[keep]
            values <- values[keep]
            new("sparse_numeric",
                value = values,
                pos = as.integer(poses),
                length = x@length)
          }
)       
setMethod(
  f = "*", c("sparse_numeric", "sparse_numeric"),
  function(e1, e2) {
    sparse_sub(e1, e2)
  }
)
setGeneric("sparse_crossprod", function(x, y, ...){
  standardGeneric("sparse_crossprod")
})

setMethod("sparse_crossprod", "sparse_numeric",
          function(x, y, ...){
            if (x@length != y@length) {
              stop("Lengths do not match match")
            }
            poses <- sort(unique(c(x@pos, y@pos)))
            values <- numeric(length(poses))
            for (i in seq_along(poses)){
              pos <- poses[i]
              x_v <- 0
              y_v <- 0
              if (pos %in% x@pos){
                x_v <- x@value[which(x@pos == pos)]
              }
              if (pos %in% y@pos){
                y_v <- y@value[which(y@pos == pos)]
              }
              values[i] <- x_v * y_v
            }
            as.numeric(sum(values, na.rm = FALSE))
            
          }
)  

setMethod("show", "sparse_numeric",
          function(object){
            cat("Length:", object@length, "\n")
            cat("with elements",object@value, "at positions", object@pos)
          })
setMethod("plot",signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            # 1. Check lengths
            if (x@length != y@length) {
              stop("Vectors must have the same overall length to compare")
            }
            
            # 2. Find overlapping nonzero positions
            overlap <- intersect(x@pos, y@pos)
            if (length(overlap) == 0L) {
              message("No overlapping nonzero positions to plot.")
              return(invisible(NULL))
            }
            
            # 3. Extract corresponding values from each vector
            x_vals <- sapply(overlap, function(p) x@value[which(x@pos == p)])
            y_vals <- sapply(overlap, function(p) y@value[which(y@pos == p)])
            
            # 4. Combine into matrix for barplot
            data_mat <- rbind(x_vals, y_vals)
            colnames(data_mat) <- paste0("Pos", overlap)
            rownames(data_mat) <- c("x", "y")
            
            # 5. Create side-by-side bar plot
            barplot(
              data_mat,
              beside = TRUE,
              col = c("steelblue", "tomato"),
              main = "Overlapping Non-Zero Elements",
              xlab = "Overlapping position",
              ylab = "Value",
              legend.text = rownames(data_mat),
              args.legend = list(x = "topright", bty = "n", inset = 0.02)
            )
          })
setMethod(
  "mean",
  signature(x = "sparse_numeric"),
  function(x, ...) {
    total_sum <- sum(x@value)
    total_len <- x@length
    total_sum / total_len
  }
)

mean(x)
