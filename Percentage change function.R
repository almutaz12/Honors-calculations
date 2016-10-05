# this function is to calculate returns 
```{r}
percentage_change=function(x,lag=1){
  n=lenght(x)
  pchange = c((x[(1+lag):n] - x[1:(n-lag)])/x[1:(n-lag)],NA)
  return(pchange)
}
```

