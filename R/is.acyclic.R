is.acyclic <-
function(dag)
{ #
  # Function to check by try() if a DAG appears cyclic.
  # using dag.ancestors, this only evaluates directed arcs;
  #
  acyclic<-rep(TRUE,length(dag$names));
  for(i in 1:length(dag$names))
  { 
    error<-try(dag.ancestors(dag, i), silent=TRUE);
    if(regexpr(pattern='infinite recursion', text=error[1])[1]>=0)
    {
      acyclic[i]<-FALSE;
    }
  }

  if(length(acyclic[acyclic==FALSE])==0)
  { overall<-TRUE;
  } else overall<-FALSE;

  rv<-list(overall, acyclic);
  names(rv)<-c('acyclic','nodewise');
  return(rv);
}

