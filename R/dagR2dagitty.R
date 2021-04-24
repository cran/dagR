dagR2dagitty<-function(x, alt.symb=TRUE, only.code=TRUE)
{ 
  dagcode <- c("dag {");
  for(i in 1:dim(x$arc)[1]) 
  { dagcode <- c(dagcode, dag.letter2(x, x$arc[i,1], alt.symb=alt.symb),
                 ifelse(x$arc.type[i]==0, "->", "<->"),
                 dag.letter2(x, x$arc[i,2], alt.symb=alt.symb),
                 ";");
  }
  dagcode <- c(dagcode, dag.letter2(x,1,alt.symb=alt.symb), "->", 
               dag.letter2(x,length(x$cov.types),alt.symb=alt.symb),";");
  for(i in 1:length(x$cov.types))
  { if(i==1)  
    dagcode <- c(dagcode, dag.letter2(x,i,alt.symb=alt.symb), "[exposure];")
  else if(x$names[i]=="unknown" || x$cov.types[i]==2)
    dagcode <- c(dagcode, dag.letter2(x,i,alt.symb=alt.symb), "[unobserved];")
  else if(i==length(x$cov.types))
    dagcode <- c(dagcode, dag.letter2(x,i,alt.symb=alt.symb), "[outcome];");
  }
  dagcode <- paste0(c(dagcode, '}'), collapse=" ");
  #if(only.code==FALSE) return(dagitty::dagitty(x=dagcode))
  if(only.code==FALSE) return(eval(parse(text = paste0('dagitty::dagitty(x="',dagcode,'")'))))
  else return(dagcode);
}