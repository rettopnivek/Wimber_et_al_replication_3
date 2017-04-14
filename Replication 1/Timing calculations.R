
n.cat = 24

total.sec =
  # Familiarization phase
  ( (n.cat*3*2)*3.5 ) + 
  # Word-association phase (targets)
  # Association
  ( ((n.cat*3)/2)*4.5 + 
      # Recall
      ((n.cat*3)/2)*4 + 
      # Recognition
      ((n.cat*3)/2)*4.5 + 
      # Recall
      ((n.cat*3)/2)*4 + 
      # Recognition
      ((n.cat*3)/2)*4.5 ) + 
  # Word-association phase (competitors)
  # Association
  ( ((n.cat*3)/2)*4.5 + 
      # Recall
      ((n.cat*3)/2)*4 + 
      # Recognition
      ((n.cat*3)/2)*4.5 ) + 
  # Selective retrieval
  ( ((n.cat*3)/4)*4*2.5 ) + 
  # Final recognition
  ((n.cat*3)*4.5) + 
  # Instructions, rest periods
  60*7
total.sec/60 # Number of minutes