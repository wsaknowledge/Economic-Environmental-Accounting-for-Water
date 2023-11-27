
##### Cuentas Ambientales de Agua #####

rm(list=ls())
library(readxl)
#install.packages("dplyr")
library(dplyr)

#### Aggregation matrix #####

  AGG_IC <- as.matrix(read_xlsx("OriginalData/AGG_table.xlsx", sheet = "AGG_IC" , col_names = FALSE))
  AGG_FD_lin <- as.matrix(read_xlsx("OriginalData/AGG_table.xlsx", sheet = "AGG_FD_lin" , col_names = FALSE))
  AGG_FD_col <- as.matrix(read_xlsx("OriginalData/AGG_table.xlsx", sheet = "AGG_FD_col" , col_names = FALSE))


#### Reading OECD input-output tables & Water accounting data #####
  
  files <- list.files("OriginalData/OCDE/", full.names = FALSE)
  water_files <- list.files("OriginalData/output_WaterData/", full.names = FALSE)
  deflator <- read_excel("OriginalData/Deflation_factors.xlsx", sheet = "Deflator ratios for code")


####################### Reading OECD data #######################################################

      # Loop through the list of files
      for(i in 1:length(files)){
        # Read the current file
        df <- read_xlsx(paste0("OriginalData/OCDE/", files[i]), col_types = "numeric")
      
        print(paste0("Iteration: ", i, "  | Economic data: ", files[i], " | Water data : ", water_files[i], "| Deflator: ", colnames(deflator)[i]))
        # Perform any operations on the dataframe
        # reading Z, FD and x matrix and deflating them
        
        # Z = interindustry consumption (sector x setor)
        Z <- as.matrix(df[8:52,3:47])*as.numeric(deflator[1,i])
        
        # FD = Final demand (sector x components)
        FD <- as.matrix(df[8:52,48:55])*as.numeric(deflator[1,i])
        
        # x = total output (sector x 1 column)
        x <- as.matrix(df[102,3:47])*as.numeric(deflator[1,i])
        
        #### Aggregation process #####
          # These operations aggregate the original OECD input-output tables in 3 sectors
          # The suffix _AGG means the variable is aggregated
        
        Z_AGG <- AGG_IC %*% Z %*% t(AGG_IC)
        x_AGG <- x %*% t(AGG_IC)
        X_diag <- matrix(diag(as.vector(x_AGG)), ncol=3)
      
        FD_AGG <- AGG_FD_lin %*% FD %*% AGG_FD_col
        

  ####The Input-Output model#####
        
        
        ### Technical coefficients matrix - (A) 
          # Direct requirements: the inputs each sector needs directly (from itself and other sectors) to produce its output 
          # Equation 2 in Technical Note 
        
        A <- Z_AGG %*% solve(X_diag)
        
        
        ### Total requirement matrix - Leontief - (L) 
          # Total requirements: the inputs each sector needs directly and indirectly "triggered" (from itself and other sectors) to produce its output
          # First part of equation 3 Technical Note 
        
        L <- solve(diag(rep(1,3)) - A) 
        
        ### Check if L is properly estimated 
        
        error_file <- (sum(L %*% FD_AGG) - sum(X_diag)) / sum(X_diag)
        if (error_file >= 1.0e-6)
          print("Don't trust L matrix")
        
        
        #### Reading water data
        
          df_water <- read_xlsx(paste0("OriginalData/output_WaterData/", water_files[i]), col_names = FALSE, col_types = "numeric")
         
          w <- as.matrix(df_water)
          W_diag <- matrix(diag(as.vector(w)), ncol=3)
          
      ##### Environmental-Extended IO model #####
          
          
          # Technical coefficients of water consumption - (A_water) 
          # How much water is directly required to accomplished the sector output
          # Sector direct water requirement 
          # Equation 5 Technical Note 
          
          A_water <- W_diag %*% solve(X_diag)
          
          direct_WF <- apply(A_water %*% A, 2, sum)
          
          
          #### Total requirement matrix of Water consumption - (L_water) #####
          # How much water is required directly and indirectly
          # Sector total water requirement 
          # First part of equation 6 Technical Note 
          
          L_water <- A_water %*% L
          
          #### Total Water consumption - (X_water) ####
          # How much water is embodied in Final Demand 
          # This is the water measure we want to decompose in 3 effects
          # Equation 6 Technical Note 
          
          X_water <- L_water %*% FD_AGG
          
        ##### Linkages #####
        
        # Backward linkage (total output multiplier)
        
        BL_water <- apply(L_water, 2, sum) 
        
        # Forward linkage 
        
        FL_water <- apply(L_water, 1, sum) 
        
        
        #### Append the results to the results list
        
        dir.create("results")
        tables <- list("A" = A, "L" = L, "FD_AGG" = FD_AGG, "X_diag" = X_diag, "A_water" = A_water, "L_water" = L_water, "X_water" = X_water, "BL_water" = BL_water, "FL_water" = FL_water, "W_balance" = W_diag, "direct_WF" = direct_WF)
      
          write.csv(tables, paste0("results/results_iso_year/", files[i], "_results.csv"), row.names=T)
      
      }


  ### Combined datasets to one sheet per country ###
 
  library(dplyr)

      #dir.create("results/results_combined")
      countries <- c("BRA", "COL", "CRI")
      for(i in 1:length(countries)){
        files <- list.files("results/results_iso_year",pattern = paste0("\\",countries[i]), full.names = FALSE)
        for(f in 1:length(files)){
          d<-read.csv(paste0("results/results_iso_year/", files[f]))
          
          d$year <- substr(files[f], 5,8)
          if(f==1){
            df<-d
          }
          if(f>1){
            df<-rbind(df,d) 
          } 
        }
        
         write.csv(df, paste0("results/results_combined/",countries[i], ".csv"), row.names = F)
      }



#### Structural Decomposition Analysis ####  2017 - 2013 

#for each country

#dir.create("results/decomp_analysis")
    
    for(i in 1:length(countries)){
      df<- read.csv(paste0("results/results_combined/",countries[i], ".csv")) 
      data_2013<-filter(df, year == 2013)
      data_2017<-filter(df, year == 2017)

      X_water.1<-which( colnames(data_2017)=="X_water....1")
      X_water.3<-which( colnames(data_2017)=="X_water....3")
      
      W_balance.1<-which( colnames(data_2017)=="A_water.1")
      W_balance.3<-which( colnames(data_2017)=="A_water.3")
      
      L.1<-which( colnames(data_2017)=="L.1")
      L.3<-which( colnames(data_2017)=="L.3")
      
      FD_AGG.1<-which( colnames(data_2017)=="FD_AGG....1")
      FD_AGG.3<-which( colnames(data_2017)=="FD_AGG....3")
      
      # delta_X_water = X_water(2017) - X_water(2013)
      delta_X_water <- as.matrix(data_2017[X_water.1:X_water.3]) - as.matrix(data_2013[X_water.1:X_water.3]) 
	  
	    delta_W_balance = as.matrix(data_2017[W_balance.1:W_balance.3]) - as.matrix(data_2013[W_balance.1:W_balance.3])
	  
	    delta_L = as.matrix(data_2017[L.1:L.3]) - as.matrix(data_2013[L.1:L.3])
	  
	    delta_FD = as.matrix(data_2017[FD_AGG.1:FD_AGG.3]) - as.matrix(data_2013[FD_AGG.1:FD_AGG.3])

      # water_input_change = 0.5*{delta_W_diag           * [((L(2013)*FD_AGG(2013))                                                   + (L(2017)    *FD_AGG(2017))]      };
      water_input_change = 0.5 * (delta_W_balance %*% ((as.matrix(data_2013[L.1:L.3]) %*% as.matrix(data_2013[FD_AGG.1:FD_AGG.3])) +  (as.matrix(data_2017[L.1:L.3]) %*% as.matrix(data_2017[FD_AGG.1:FD_AGG.3]))))
      
      # structural_change = 0.5*[[W_diag(2013)                                   *  delta_L         * FD_AGG(2017)                            + W_diag(2017)                                  *delta_L       *  FD_AGG(2013)]];
      structural_change = 0.5 * ((as.matrix(data_2013[W_balance.1:W_balance.3]) %*% (delta_L) %*% as.matrix(data_2017[FD_AGG.1:FD_AGG.3])) + (as.matrix(data_2017[W_balance.1:W_balance.3]) %*% delta_L %*% as.matrix(data_2013[FD_AGG.1:FD_AGG.3])))
      
      # final_demand_change = 0.5*[(W_diag(2013)                                             * L(2013)                +      (W_diag(2017)                                  *L(2017))                        *delta_FD];
      final_demand_change <- 0.5 * (((as.matrix(data_2013[W_balance.1:W_balance.3]) %*% as.matrix(data_2013[L.1:L.3])) + (as.matrix(data_2017[W_balance.1:W_balance.3]) %*% as.matrix(data_2017[L.1:L.3])))  %*% delta_FD)
      
      delta_X_water_1 <- water_input_change + structural_change + final_demand_change
      
      
      
      if((sum(delta_X_water) - sum(delta_X_water_1)) < 10^(-8)){
        print("ok")
      } else{
        print("wrong!")
      }
	
  
  ###############################################################################################
  ######### Append the results to the results list ##############################################
  ###############################################################################################
      water_input_change<- as.data.frame(water_input_change)
      structural_change<- as.data.frame(structural_change)
      final_demand_change<-as.data.frame(final_demand_change)
      delta_X_water<- as.data.frame(delta_X_water)
      delta_X_water_1<- as.data.frame(delta_X_water_1)

      water_input_change$var <- "water_input_change"
      water_input_change$sector <- c("Agriculture, cattle, and others","Other","Water and Sewage")

      structural_change$var<- "structural_change"
      structural_change$sector <- c("Agriculture, cattle, and others","Other","Water and Sewage")


      final_demand_change$var <-"final_demand_change"
      final_demand_change$sector <- c("Agriculture, cattle, and others","Other","Water and Sewage")

      delta_X_water$var<- "delta_X_water"
      delta_X_water$sector <- c("Agriculture, cattle, and others","Other","Water and Sewage")

      delta_X_water_1$var<- "delta_X_water_1"
      delta_X_water_1$sector <- c("Agriculture, cattle, and others","Other","Water and Sewage")


      dca<-rbind(water_input_change,structural_change,final_demand_change, delta_X_water_1)

      dca<-dplyr::rename(dca, "Households"="FD_AGG....1",
                 "Other_Internal" = "FD_AGG....2",
                 "Exports"="FD_AGG....3")

      dc_analysis<-select(dca, var, sector, Households, Other_Internal, Exports)
      write.csv(dc_analysis,paste0("results/decomp_analysis/dca_",countries[i],".csv"), row.names = F)


      dc_analysis$country <- countries[i]
      if(i ==1){
       decomp<-dc_analysis
      }
      if(i>1){
       decomp<-rbind(decomp, dc_analysis)
      }

    write.csv(dc_analysis,"results/decomp_analysis/dca_analysis.csv", row.names = F)
    }



