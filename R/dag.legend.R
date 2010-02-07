dag.legend <-
function(dag, lx=-0.15, ly=-0.075)
{ # write legend
  i1<-0;
  i_c<-0; # covariable counter for subscripts
  i_u<-0; # unknown covs counter for...
  nodes<-length(dag$x);
  while (i1 < nodes)
  {
    i1<-i1+1;
    text(lx, ly-0.05*i1, i1);
    if(dag$names[i1]=="unknown" || dag$cov.types[i1]==2)
    { i_u<-i_u+1;
      text(lx+0.1, ly-0.05*i1, bquote(U[.(i_u)]));
    } else
    { if(i1==1)
      { text(lx+0.1, ly-0.05*i1, expression(X));
      } else
      { if(i1==nodes)
        { text(lx+0.1, ly-0.05*i1, expression(Y));
        } else
        { i_c<-i_c+1;
          text(lx+0.1, ly-0.05*i1, bquote(C[.(i_c)]));
        }
      }
    }
    text(lx+0.15, ly-0.05*i1, dag$names[i1], pos=4);
  }
}

