print.dagRdag <-
#function(x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL, 
#         right = FALSE, max = NULL, useSource = TRUE, ...){

# use print.default...
#         args <- pairlist(digits = digits, quote = quote, na.print = na.print, 
#                       print.gap = print.gap, right = right, max = max, useSource = useSource, 
#                       ...)
#      missings <- c(missing(digits), missing(quote), missing(na.print), 
#                    missing(print.gap), missing(right), missing(max), missing(useSource))
#      .Internal(print.default(x, args, missings))


function(x, ...)
{ class(x)<-"list";
  print(x);
               
cat(c("Directed acyclic graph (DAG) object created by dagR version ",
      ifelse(is.null(x$version),"1.0.1 (or manually)", x$version),".\n",
      "For a convenient interpreted presentation, use summary().\n"),
          sep="");
      
       
}
