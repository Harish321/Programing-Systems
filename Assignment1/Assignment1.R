tabel <-read.csv(file="dataset.csv");
generation <- c(0,0,0,0)
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
Name1 <- readline(prompt="Name1:")
Name2 <- readline(prompt="Name2:")

p1 <- subset(tabel$PKey,tabel$Name == Name1)
p2 <- subset(tabel$PKey,tabel$Name == Name2)
p1 <- as.numeric(as.character(p1))
p2 <- as.numeric(as.character(p2))
# print (p1)
# print (p2)
generation0 <- function(a,b,gt_a,gt_b,d){
  if(a==1|a==2|a==3|a==4|b==1|b==2|b==3|b==4)
  { tempa <- 0
    tempb <- 0
    if(a==1|a==2|a==3|a==4){tempb <- tabel$Relatedto[[b]]
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
  print (tempa)
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
    temp <- tabel$Relatedto[[a]]
    temp <- as.numeric(as.character(temp))
    gt_temp <- generation[[temp]]
    if( gt_b == gt_temp ){
      if(temp == b){
          return (0)
      }
      else{
        secondtemp <- tabel$Relatedto[[b]]
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
      fir_gen_rel <- generation2(b,a,gt_b,gt_a,3)
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
    gt_temp <- generation[[temp]]
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


gt_p1 <- generation[[p1]]
gt_p2 <- generation[[p2]]
gender <- tabel$Gender[[p1]]
gender <- as.character(gender)
diff_gen <- gt_p1 -gt_p2
if(diff_gen == 0){
  realation <- generation0(p1,p2,gt_p1,gt_p2,diff_gen)
  if(realation == 6){
    if(gender == "M"){print("P1 is Brother to P2")}
    else{print("P1 is sister to P2")}
  }
  if(realation == 7){
    if(gender == "M"){print("P1 is Cousin to P2")}
    else{print("P1 is Cousin to P2")}
  }
  if(realation == 8){
    if(gender == "M"){print("P1 is Husband of P2")}
    else{print("P1 is wife of P2")}
  }
  if(realation == 9){
    print("its a complicated relation between p1 and p2")
  }
}
if( diff_gen == 1 | diff_gen == -1){
 realation <- generation1(p1,p2,gt_p1,gt_p2,diff_gen)
 if (realation == 0){
    if(gender == "M"){print ("P1 is Son to P2")}
    else{print("P1 is Daughter of P2")} 
  }
  if (realation == 1){
    if(gender == "M"){print ("P1 is Son-in-law to P2")}
    else{print("P1 is Daughter-in-law of P2")} 
  }
  if (realation == 2){
    if(gender == "M"){print ("P1 is Father to P2")}
    else{print("P1 is Mother of P2")} 
  }
  if (realation == 3){
    if(gender == "M"){print ("P1 is Father-in-law to P2")}
    else{print("P1 is Mother-in-law of P2")} 
  }
  if (realation == 4){
    if(gender == "M"){print ("P1 is Nephew to P2")}
    else{print("P1 is Neice of P2")} 
  }
  if (realation == 5){
    if(gender == "M"){print ("P1 is Uncle to P2")}
    else{print("P1 is Aunty of P2")} 
  }
}
if(diff_gen == 2 | diff_gen == -2){
  realation <- generation2(p1,p2,gt_p1,gt_p2,diff_gen)
  if (realation == 0| realation ==1|realation ==4){
    if(gender == "M"){print ("P1 is Grand Son to P2")}
    else{print("P1 is Grand Daughter of P2")} 
  }
  if (realation == 2 | realation ==3){
    if(gender == "M"){print ("P1 is Grand Father to P2")}
    else{print("P1 is Grand Mother of P2")} 
  }
  if (realation == 5){
    if(gender == "M"){print ("P1 is Grand Uncle to P2")}
    else{print("P1 is Grand Aunty of P2")} 
  }
}
if( diff_gen == 3 | diff_gen == -3){
  realation <- generation3(p1,p2,gt_p1,gt_p2,diff_gen)
  if (realation == 0| realation ==1|realation ==4){
    if(gender == "M"){print ("P1 is Great Grand Son to P2")}
    else{print("P1 is Great Grand Daughter of P2")} 
  }
  if (realation == 2 | realation ==3){
    if(gender == "M"){print ("P1 is Great Grand Father to P2")}
    else{print("P1 is Great Grand Mother of P2")} 
  }
  if (realation == 5){
    if(gender == "M"){print ("P1 is Great Grand Uncle to P2")}
    else{print("P1 is Great Grand Aunty of P2")} 
  }
}







