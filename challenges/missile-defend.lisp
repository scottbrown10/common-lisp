for at each time step
  if there is an h-missile with same freq as e-missile
    do nothing
  else if no h-missile can change freq in time
    launch new h-missile
  else if only 1 h-missile can change freq in time
    change it
  else if multiple h-missiles can change freq in time
    ??? keep track of which freqs h-missile i can change to in time. backtrack as needed
