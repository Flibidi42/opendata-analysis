Apriori<-function(data_set, threshold=50){
  
  data_fac <- data.frame(lapply(data_set, factor));
  sy = dim(data_fac)[1];
  sx = dim(data_fac)[2];
  pattern = vector("character");
  candidates_old = list();#old candidates
  candidates_new = list();#new candidates
  candidates_1 = list();#candidates with size 1 to generate patterns
  tempo_fac = factor();
  
  for(size_pat in 1:sx){
    
    if(size_pat ==1){
      
      for(col in 1:sx){
          
          tempo_fac <- as.factor(levels(data_fac[,col]));
          
          for(possibilities in 1:length(tempo_fac)){
            
            if(sum(data_fac[,col]==tempo_fac[possibilities], na.rm=TRUE) >= threshold){
              pattern <- c(pattern, paste(names(data_fac)[col], 
                                          " = ", as.character(tempo_fac[possibilities])));
              candidates_old <- c(candidates_old, list(list(col, tempo_fac[possibilities])));
              candidates_1 = candidates_old;
            }
          }
      }
      
    }
    
    else{
        
      for(cand in 1:length(candidates_old)){
        
        for(cand1 in 1:length(candidates_1)){
          
          if(all(candidates_old[[cand]][[1]] != candidates_1[[cand1]][[1]])){
            #checking if we are not dealing with the same col
            tempo_fac <- data_fac;
            for(i in 1:length(candidates_old[[cand]][[1]])){
              tempo_fac <- tempo_fac[tempo_fac[,candidates_old[[cand]][[1]][i]]==candidates_old[[cand]][[2]][i],];
            }
            tempo_fac <- tempo_fac[tempo_fac[,candidates_1[[cand1]][[1]]]==candidates_1[[cand1]][[2]],];
            if(nrow(tempo_fac)>= threshold){#the pattern has to be added
              candidates_new <- c(candidates_new, list(c(candidates_old[[cand]][[1]], candidates_1[[cand1]][[1]]), 
                                                       c(candidates_old[[cand]][[2]], candidates_1[[cand1]][[2]])));
            }
            
          }
          
        }
        
      }
      
      candidates_old <- candidates_new;
      candidates_new <- list();
      
      for(pat in 1:length(candidates_old)){
        for(col in candidates_old[[pat]][1]){
         pattern <- c(pattern, paste(names(data_fac[,col])[col], 
                            " = ", as.character(candidates_old[[pat]][[2]][candidates_old[[pat]][[1]]==col]), " "));
      
        }
        if(pat !=1){
          pattern <- c(pattern, " / ");
        }
      
    }
    
  }
  pattern
  
  }
}