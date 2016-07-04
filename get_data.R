Parti<- function(value, min, max){
  
  border = mean(max, min);
  return_value = character();
  if(value < border){
    return_value <- "L";
  }
  else{
    return_value <- "H";
  }
  return_value
}

Parti_abs<- function(value){
  
  return_value = character();
  if(value < 4){
    return_value <- "L";
  }
  else if(value<=10){
    return_value <- "M";
  }
  else{
    return_value <- "H";
  }
  return_value
}

Parti_grades<- function(value){
  
  return_value = character();
  if(value <= 7){
    return_value <- "L";
  }
  else if(value<=13){
    return_value <- "M";
  }
  else{
    return_value <- "H";
  }
  return_value
}

my_data <- read.csv("student-mat.csv", sep=";");
my_data$Medu <- lapply(my_data$Medu, Parti, min=0, max=4);
my_data$Fedu <- lapply(my_data$Fedu, Parti, min=0, max=4);
my_data$traveltime <- lapply(my_data$traveltime, Parti, min=1, max=4);
my_data$studytime <- lapply(my_data$studytime, Parti, min=1, max=4);
my_data$failures <- lapply(my_data$failures, Parti, min=0, max=4);
my_data$famrel <- lapply(my_data$famrel, Parti, min=1, max=5);
my_data$freetime <- lapply(my_data$freetime, Parti, min=1, max=5);
my_data$goout <- lapply(my_data$goout, Parti, min=1, max=5);
my_data$Dalc <- lapply(my_data$Dalc, Parti, min=1, max=5);
my_data$Walc <- lapply(my_data$Walc, Parti, min=1, max=5);
my_data$health <- lapply(my_data$health, Parti, min=1, max=5);
my_data$absences <- lapply(my_data$absences, Parti_abs);
my_data$G1 <- lapply(my_data$G1, Parti_grades);
my_data$G2 <- lapply(my_data$G2, Parti_grades);
my_data$G3 <- lapply(my_data$G3, Parti_grades);
