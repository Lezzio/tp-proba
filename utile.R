Frequency <- function(x, nb) 
#x = Tableau des 1000 vals, nb = nombres de bits
{
  size = length(x)
  s=0
  for(j in 1:size){
    bin = binary(x[j])
    for(i in 32:(32-nb+1)){
      s=s+2*bin[i]-1
    }
  }
  sobs=abs(s)/sqrt(size * nb)
  p = 2 * (1 - pnorm(sobs))
  return(p)
}

Runs <- function(x, nb)
{
  # pre-test
  uns    <- 0
  totalN <- 0
  
  for(i in x)
  {
    totalN   <- totalN + nb
    
    bin <- rev(binary(i))
    
    for(n in 1:nb)
    {
      if(bin[n] == 1) uns <- uns + 1
    }
  }
  
  pi  <- uns/totalN
  tau <- 2/sqrt(totalN)
  
  if(abs(pi - 0.5) >= tau) return(0.0)
  # Vn(obs)
  V <- 1
  
  prevLastBit <- rev(binary(x[1]))[nb]
  
  for(i in x)
  {
    bin <- rev(binary(i))
    
    if(bin[1] != prevLastBit) V <- V + 1
    
    for(n in 1:(nb-1))
    {
      if(bin[n] != bin[n+1]) V <- V + 1
    }
    
    prevLastBit <- bin[nb]
    
  }
  
  pi2pi <- 2*pi*(1-pi)
  Pvaleur <- 2*(1 - pnorm( abs(V - totalN*pi2pi) / (sqrt(totalN)*pi2pi) ))
  
  return(Pvaleur)
}