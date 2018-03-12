tabel <-read.csv(file="dataset.csv");
generation <- c(0,0,0,0)
allrelations <- c()
for (item in 5:nrow(tabel) ){
  related_person <- tabel$Relatedto[[item]];
  related_person <- as.numeric(as.character(related_person))
  relation <- tabel$Relation[item]
  relation <- as.character(relation)
  if(relation == "Spouse"){
    generation<- c(generation,generation[[related_person]])
  }
  else{
    gt <- generation[[related_person]]
    gt <- gt + 1
    generation <-c(generation,gt)
  }
  
}
generation0 <- function(a,b,gt_a,gt_b,d){
  if(a <5 | b < 5)
  { tempa <- 0
    tempb <- 0
    if(a < 5 & b < 5){
      return (7)
    }
    else if(a<5 & b >4)
    {tempb <- tabel$Relatedto[[b]]
    tempb <- as.numeric(as.character(tempb))}
    else{
      tempa <- tabel$Relatedto[[a]]
      tempa <- as.numeric(as.character(tempa))
    }
    
    if(tempa == b | tempb ==a){
      return (8)
    }
    else{return (7)}   
  }
  else{
  tempa <- tabel$Relatedto[[a]]
  tempa <- as.numeric(as.character(tempa))
  # print (tempa)
  tempb <- tabel$Relatedto[[b]]
  tempb <- as.numeric(as.character(tempb))
  gt_tempa <- generation[[tempa]]
  gt_tempb <- generation[[tempb]]
  if(gt_tempa == gt_tempb)
  {
    if(tempa == tempb){return (6)}
    else{return(7)}
  }
  else{
    if(tempa == b|tempb == a){
      return(8)
    }
    else{return (9)}
  }
  }
}

generation1 <- function(a,b,gt_a,gt_b,d){
  if (d == 1){
    if( a < 5){
      temp <- 1
    }
    else{
    temp <- tabel$Relatedto[[a]]
    }
    temp <- as.numeric(as.character(temp))
    gt_temp <- generation[[temp]]
    if( gt_b == gt_temp ){
      if(temp == b){
          return (0)
      }
      else{
        if (b < 5){return (4)}
        else{secondtemp <- tabel$Relatedto[[b]]}
        secondtemp <- as.numeric(as.character(secondtemp))
        if(temp == secondtemp){
          return (0)
        }
        else{
          return (4)
        }
      }
    }
    else{
      spouse_rel <- generation1(temp,b,gt_temp,gt_b,1)
      if(spouse_rel == 0){
        return (1)    
      }
      else{
        return (4)
      }
    }
  }
  else{
    reversed_rel <- generation1(b,a,gt_b,gt_a,1)
    if (reversed_rel == 0 | reversed_rel== 1){
      if(reversed_rel ==0){return (2)}
      else {return (3)}
    }
    else{
      return (5)
    }
  }
}

generation2 <- function(a,b,gt_a,gt_b,d){
   if (d == 2) {
     temp <- tabel$Relatedto[[a]]
     temp <- as.numeric(as.character(temp))
     gt_temp <- generation[[temp]]
     if(gt_temp - gt_b == 1){
       fir_gen_rel <- generation1(temp,b,gt_temp,gt_b,1)
       #based on its returns we decide
       
       return (fir_gen_rel)

     }
     else{
       fir_gen_rel <- generation2(temp,b,gt_temp,gt_b,2)
        return (fir_gen_rel) 
     }
   }
   else{
      sec_gen_rel <- generation2(b,a,gt_b,gt_a,2)
      if(sec_gen_rel ==0 | sec_gen_rel == 1){
        if(sec_gen_rel ==0) {return(2)}
        else{return(3)}
      }
      else{return(5)}
    }
  
}

generation3 <- function (a,b,gt_a,gt_b,d){
  if (d == 3){
    temp <- tabel$Relatedto[[a]]
    temp <- as.numeric(as.character(temp))
    # print (temp)
    gt_temp <- generation[[temp]]
    # print (gt_temp)
    # print (gt_b)
    if(gt_temp - gt_b == 2){
      sec_gen_rel <- generation2(temp,b,gt_temp,gt_b,2)
      return (sec_gen_rel)
    }
    else{
      sec_gen_rel <- generation3(temp,b,gt_temp,gt_b,3)
      return (sec_gen_rel)
    }
  }
    else{
      sec_gen_rel <- generation3(b,a,gt_b,gt_a,3)
      if(sec_gen_rel ==0 | sec_gen_rel == 1){
        if(sec_gen_rel ==0) {return(2)}
        else{3}
      }
      else{return(5)}
    }
}
Name1 <- readline(prompt="Name:")
Name2 <- readline(prompt="Relation:")
for (names in 1:nrow(tabel)){
    # print (name1)
# Name1 <- readline(prompt="Name1:")
# Name2 <- readline(prompt="Name2:")

p2 <- subset(tabel$PKey,tabel$Name == Name1)
# p2 <- subset(tabel$PKey,tabel$Name == Name2)
p2 <- as.numeric(as.character(p2))
# p2 <- as.numeric(as.character(p2))
# # print (p1
# print (p2)
# p1 <- 1
# p1 <- as.numeric(p1)
p1 <- names
p1 <- as.numeric(p1)
# print (p2)
gt_p1 <- generation[[p1]]
gt_p2 <- generation[[p2]]
gender <- tabel$Gender[[p1]]
gender <- as.character(gender)
diff_gen <- gt_p1 - gt_p2
# print (diff_gen)
if (p1 != p2){
if(diff_gen == 0){
  realation <- generation0(p1,p2,gt_p1,gt_p2,diff_gen)
  if(realation == 6){
    if(gender == "M"){allrelations <- c(allrelations,'Brother')}
    else{allrelations <- c(allrelations,'Sister')}
  }
  if(realation == 7){
    if(gender == "M"){allrelations <- c(allrelations,'Cousin')}
    else{allrelations <- c(allrelations,'Cousin')}
  }
  if(realation == 8){
    if(gender == "M"){allrelations <- c(allrelations,'Husband')}
    else{allrelations <- c(allrelations,'Wife')}
  }
  if(realation == 9){
    allrelations <- c(allrelations,'Complicated')
  }
}
if( diff_gen == 1 | diff_gen == -1){
 realation <- generation1(p1,p2,gt_p1,gt_p2,diff_gen)
 if (realation == 0){
    if(gender == "M"){allrelations <- c(allrelations,'Son')}
    else{allrelations <- c(allrelations,'Daughter')} 
  }
  if (realation == 1){
    if(gender == "M"){allrelations <- c(allrelations,'Son-in-law')}
    else{allrelations <- c(allrelations,'Daughter-in-law')} 
  }
  if (realation == 2){
    if(gender == "M"){allrelations <- c(allrelations,'Father')}
    else{allrelations <- c(allrelations,'Mother')} 
  }
  if (realation == 3){
    if(gender == "M"){allrelations <- c(allrelations,'Father-in-law')}
    else{allrelations <- c(allrelations,'Mother-in-law')} 
  }
  if (realation == 4){
    if(gender == "M"){allrelations <- c(allrelations,'Nephew')}
    else{allrelations <- c(allrelations,'Neice')} 
  }
  if (realation == 5){
    if(gender == "M"){allrelations <- c(allrelations,'Uncle')}
    else{allrelations <- c(allrelations,'Aunty')} 
  }
}
if(diff_gen == 2 | diff_gen == -2){
  realation <- generation2(p1,p2,gt_p1,gt_p2,diff_gen)
  if (realation == 0| realation ==1|realation ==4){
    if(gender == "M"){allrelations <- c(allrelations,'Grandson')}
    else{allrelations <- c(allrelations,'Granddaughter')} 
  }
  if (realation == 2 | realation ==3){
    if(gender == "M"){allrelations <- c(allrelations,'Grandfather')}
    else{allrelations <- c(allrelations,'Grandmother')} 
  }
  if (realation == 5){
    if(gender == "M"){allrelations <- c(allrelations,'Granduncle')}
    else{allrelations <- c(allrelations,'Grandaunty')} 
  }
}
if( diff_gen == 3 | diff_gen == -3){
  realation <- generation3(p1,p2,gt_p1,gt_p2,diff_gen)
  if (realation == 0| realation ==1|realation ==4){
    if(gender == "M"){allrelations <- c(allrelations,'Greatgrandson')}
    else{allrelations <- c(allrelations,'Greatgranddaughter')} 
  }
  if (realation == 2 | realation ==3){
    if(gender == "M"){allrelations <- c(allrelations,'Greatgrandfather')}
    else{allrelations <- c(allrelations,'Greatgrandmother')} 
  }
  if (realation == 5){
    if(gender == "M"){allrelations <- c(allrelations,'Greatgranduncle')}
    else{allrelations <- c(allrelations,'Greatgrandaunty')} 
  }
}
}
else {
  allrelations <- c(allrelations,'Self')
}
}
for (i in 1:nrow(tabel)){
  current_relation <- allrelations[[i]]
  current_relation <- as.character(current_relation)
  # print (current_relation)
  if(current_relation == Name2 )
  {
    p1 <- tabel$Name[i]
    p1 <- as.character(p1)
    print (p1)
  }
}








