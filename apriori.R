Apriori<-function(data_set, threshold=50){
  
  data_char <- data.frame(lapply(data_set, as.character));
  sy = dim(data_char)[1];
  sx = dim(data_char)[2];
  pattern = vector("character");
  candidates_old = list();#old candidates
  candidates_new = list();#new candidates
  candidates_1 = list();#candidates with size 1 to generate patterns
  tempo_fac = character();
  tempo_string = character();
  
  for(size_pat in 1:3){
    if(size_pat ==1){
      
      for(col in 1:sx){
        
        tempo_fac <- unique(data_char[,col]);
        
        for(possibilities in 1:length(tempo_fac)){
          
          if(sum(data_char[,col]==tempo_fac[possibilities], na.rm=TRUE) >= threshold){
            pattern <- c(pattern, paste(names(data_char)[col], 
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
            tempo_fac <- data_char;
            for(i in 1:length(candidates_old[[cand]][[1]])){
              tempo_fac <- tempo_fac[tempo_fac[,candidates_old[[cand]][[1]][i]]==candidates_old[[cand]][[2]][i],];
            }
            tempo_fac <- tempo_fac[tempo_fac[,candidates_1[[cand1]][[1]]]==candidates_1[[cand1]][[2]],];
            if(nrow(tempo_fac)>= threshold){#the pattern has to be added
              candidates_new <- c(candidates_new, list(list(c(candidates_old[[cand]][[1]], candidates_1[[cand1]][[1]]),
                                                            c(as.character(candidates_old[[cand]][[2]]), as.character(candidates_1[[cand1]][[2]])))));
            }
            
          }
          
        }
        
      }
      
      candidates_old <- candidates_new;
      candidates_new <- list();
      
      for(pat in 1:length(candidates_old)){
        for(col in candidates_old[[pat]][[1]]){
          tempo_string <- paste(tempo_string, names(data_char)[col], 
                                "=", as.character(candidates_old[[pat]][[2]][candidates_old[[pat]][[1]]==col]));
          
        }
        pattern <- c(pattern, tempo_string);
        tempo_string = character();
      }
      
    
  }
  }
  pattern
}