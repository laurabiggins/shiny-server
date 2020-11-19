
get_advice <- function(name, dataset){

  top_characteristic <- dataset %>%
    filter(Name == name) %>%
    filter(percentage_rank == max(percentage_rank))
  
  text <- "<b><font size='6'>"
  
  if(top_characteristic$percentage_rank[1] == 100){
    text <- paste0(text, "Top score!<br>")
  }
  
  if(nrow(top_characteristic) == 2){
    text <- paste0(text, "Two characteristics rank the highest, choose between ", 
                   top_characteristic$characteristic[1], 
                   " and ",
                   top_characteristic$characteristic[2], 
                   ". ")
  }
  else if(nrow(top_characteristic) > 2){
    text <- paste0(text, "More than 2 highest categories all with the same ranking - really?!")
  }
  else if(nrow(top_characteristic) == 1){
    text <- paste0(text, top_characteristic$characteristic[1], " is the highest ranking category")
  }
  else{
    text <- paste0(text, "something's gone wrong here")
  }
  paste0(text, "</b></font>")
}
