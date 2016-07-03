association_rule<- function(val_1, val_2){

my_data = read.table("student-mat.csv", TRUE, ";");

#Computation for the support and confidence of the relation adress=>famsize
my_add = "U"

union_frame <- my_data[my_data$address == val_1 & my_data$famsize == val_2,];

s <- dim(union_frame)[1];

  #confidence

c <- s/dim(my_data[my_data$address == val_1,])[1];

c(s, c)

}
