dag.letter <-
function(dag, letter, x, y)
{ # function to draw the letters in the DAG;
 if(letter==1)
 { text(x, y, expression(X));
 } else
 if(letter==length(dag$x))
 { text(x, y, expression(Y));
 } else
 {
  i_c<-0; # covariable counter for subscripts
  i_u<-0; # unknown covs counter for...
  for(i1 in 2:letter)
  {
    if(dag$names[i1]=="unknown" || dag$cov.types[i1]==2)
    { i_u<-i_u+1;
    } else
    { i_c<-i_c+1;
    }
  }
  
  if(dag$names[letter]=="unknown" || dag$cov.types[i1]==2) 
  {
    text(x, y, bquote(U[.(i_u)]));
  } else
  { if(is.in(letter, dag$adj)==TRUE)
      { text(x, y, bquote(underline(bar(C))[.(i_c)]));
      } else
      { text(x, y, bquote(C[.(i_c)]));
      }
  }
 }
}

