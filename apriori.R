Apriori<-function(data_set, threshold=50){
  
  sy = dim(data_set)[1];
  sx = dim(data_set)[2];
  pattern = vector("character");
  candidates = vector();
  tempo_fac = factor();
  tempo_num = vector();
  
  for(size_pat in 1:sx){
    
    if(size_pat ==1){
      
      for(col in 1:sx){
        
        if(class(data_set[,col])=="factor"){
          
          tempo_fac <- levels(data_set[,col]);
          
          for(possibilities in 1:length(tempo_fac)){
            
            if(sum(data_set[,col]==tempo_fac[possibilities], na.rm=TRUE) >= threshold)
              pattern <- c(pattern, paste(names(data_set)[col], 
                                          " = ", as.character(tempo_fac[possibilities])));
            
          }
        }
        else if(class(data_set[,col])=="integer"){
          
          tempo_num <- unique(data_set[,col]);
          
          for(possibilities in 1:length(tempo_num)){
            
            if(sum(data_set[,col]==tempo_num[possibilities], na.rm=TRUE) >= threshold)
              pattern <- c(pattern, paste(names(data_set)[col], 
                                          " = ", as.character(tempo_num[possibilities])));
            
          }
          
        }
      }
      
    }
    
  }
  pattern
  
}