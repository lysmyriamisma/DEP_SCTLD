# # Function_geno_cast returns a cast data frame with Sample_ID as columns plus the columns 
#describing if the samples have control pair scrapes (C_PE_C_EE) or diseased pair scrapes(D_PE_D_EE)

function_geno_cast<- function(Exp) {
  
  geno<- unique(Exp$Genotype)
  
  temp_data<- data.frame()
  
  for(i in 1:length(geno)) {
    
    geno_select<- dcast(Exp, Experiment + Genotype ~ Treatment + R_TP, value.var = 'uniqsample', fun.aggregate = sum) %>% 
      
      filter(Genotype == geno[i]) %>% 
      
      mutate(C_PE_C_EE = C_PE == 1 & C_EE == 1) %>% 
      
      mutate(D_PE_D_EE = D_PE == 1 & D_EE == 1) %>% 
      
      dplyr::select(Genotype, C_PE_C_EE, D_PE_D_EE)
    
    geno_select<- as.list(geno_select) 
    
    geno_all<- data.frame(Genotype = geno[i],
                          C_PE_C_EE = geno_select$C_PE_C_EE,
                          D_PE_D_EE = geno_select$D_PE_D_EE,
                          stringsAsFactors = F)
    
    temp_data<- rbind(temp_data, geno_all) 
    
    Pairs_C_T<- dcast(Exp, Experiment + Genotype ~ Sample_ID) %>% 
      
      left_join(temp_data, by="Genotype") 
  }
  return(Pairs_C_T)
}
write_rds(function_geno_cast,'function_geno_cast.rds')
