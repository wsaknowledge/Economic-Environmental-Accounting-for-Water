library(readxl)
library(tidyverse)
library(tidyr)
library(plyr)
#### Data Setup ####
setwd("C:/Users/jesse/Inter-American Development Bank Group/Giovanna Naspolini - ESW-D1223_CuentasAmbientales/")

files<-list.files("results/results_combined")

dataframe<-data.frame(Country=as.character(), 
                 Year = as.numeric(),
                 `Agriculture cattle and others` = as.numeric(), 
                 Other = as.numeric(),
                 `Water and Sewage`=as.numeric())
#"Agriculture, cattle, and others";
#"Heavy Industries, Manufacturing, Construction and Services ";
#"Electricity and Natural Gas";
#"Water and Sewage";

for(i in 1:length(files)){
  print(files[i])
  
  # Read the current file
  df <- read.csv(paste0("results/results_combined/", files[i]), header = T)
  df$country <- substr(files[i], 1,3)
  if(i == 1){
    data<-df
  }
  if(i>1){
  data<-rbind(data,df )
  }
    }
  data$X<-ifelse(data$X %in% c(1), "AGRI",
         ifelse(data$X %in% c(2), "OTHER",
                ifelse(data$X %in% c(3), "WASA", NA)))


  data<- dplyr::rename(data,
              "Sector"="X",
              "Technical coefficients - Agriculture, cattle, and others"="A.1",
              "Technical coefficients - Other" ="A.2",
              "Technical coefficients - Water and Sewage" ="A.3",
              "Total water requirement - Agriculture, cattle, and others"="L.1",
              "Total water requirement - Other"="L.2",
              "Total water requirement - Water and Sewage"="L.3",
              "Final Demand - Households & NPISH" ="FD_AGG....1",
              "Final Demand - Government"="FD_AGG....2",
              "Final Demand - GFGC"="FD_AGG....3" ,
             # "Final Demand - Change in Inventories"="FD_AGG....4",
             # "Final Demand - Exports"="FD_AGG....5",
              # ="X_diag.1"
              # ="X_diag.2"
              # ="X_diag.3"
              "Technical coefficients of water consumption - Ag" ="A_water.1",
              "Technical coefficients of water consumption - Other" ="A_water.2",
              "Technical coefficients of water consumption - W&S" ="A_water.3",
              "Total requirement matrix of water consumption - Ag" ="L_water.1",
              "Total requirement matrix of water consumption - Other" ="L_water.2",
              "Total requirement matrix of water consumption - W&S" ="L_water.3",
              "Total water consumption - Households & NPISH" ="X_water....1",
              "Total water consumption - Government" ="X_water....2",
              "Total water consumption - GFCF" = "X_water....3",
              #"Total water consumption - Change in inventories" ="X_water....4",
              #"Total water consumption - Exports" ="X_water....5",
              "Backward linkages" ="BL_water",
              "Forward linkages"="FL_water",
              "Water balance - Agriculture, cattle, and others" ="W_balance.1",
              "Water balance - Other"="W_balance.2" ,
              "Water balance - Water and Sewage"="W_balance.3")


   #### Water balance ####
    
    osa_wb<- select(data, Sector,"Water balance - Agriculture, cattle, and others","Water balance - Other","Water balance - Water and Sewage", country, year)
      
    osa_wb_d<-pivot_longer(osa_wb, c(2:4))
    osa_wb_d <- filter(osa_wb_d, value !=0)
    osa_wb_d$name<- "Water balance"
    
    total<- osa_wb_d%>%
      group_by(country, year)%>%
      dplyr::summarise(value = sum(value))
    
    total$Sector <- "Total"
    total$name <- "Water balance"
    
    osa_wb_data<-rbind(total, osa_wb_d)
    
    data
    
    ## OVERALL BAR CHART
    options(scipen = 999)
    osa_stacked<-ggplot(osa_wb_data, aes(fill=Sector, y=value, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_grid(~country)
    library(cowplot)
    
    ### Better overall bar chart
    BRA<-ggplot(subset(osa_wb_data,Sector != "Total" & country == "BRA"), aes(fill=Sector, y=value, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    COL<-ggplot(subset(osa_wb_data, Sector != "Total" &country == "COL"), aes(fill=Sector, y=value, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    CRI<-ggplot(subset(osa_wb_data,Sector != "Total" & country == "CRI"), aes(fill=Sector, y=value, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    overall_wb<-plot_grid(BRA,COL,CRI,
              nrow = 1,
              align = "v"
    )
    
    png(file=paste0("results/graphics_tn/overall_wb.png"), height=500, width = 1600,
        res = 150)
    print(overall_wb)
    dev.off()  
    
    ### Sector Charts comparing sector wb across countries ###
    
    sector<-c("AGRI", "OTHER", "WASA")
    for(s in 1:length(sector)){
    
    AGRI<-ggplot(subset(osa_wb_data, Sector ==sector[s] & country == "BRA"), aes(fill=Sector, y=value, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    OTHER<-ggplot(subset(osa_wb_data,Sector ==sector[s] & country == "COL"), aes(fill=Sector, y=value, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    WASA<-ggplot(subset(osa_wb_data, Sector ==sector[s] &country == "CRI"), aes(fill=Sector, y=value, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    sector_wb<-plot_grid(AGRI, OTHER, WASA,
                          nrow = 1,
                          align = "v"
    )
    
    png(file=paste0("results/graphics_tn/",sector[s],"_wb.png"), height=500, width = 1600,
        res = 150)
    print(sector_wb)
    dev.off()  
    
    }
    
    
    ### Country Charts comparing country wb across sectors ###
    
    countries<-c("BRA", "COL", "CRI")
    for(i in 1:length(countries)){
        options(scipen=999)
            AGRI<-ggplot(subset(osa_wb_data, country == countries[i] & Sector == "AGRI"), aes(fill=Sector, y=value, x=year)) + 
              geom_bar(position="stack", stat="identity")
            OTHER<-ggplot(subset(osa_wb_data, country == countries[i] & Sector == "OTHER"), aes(fill=Sector, y=value, x=year)) + 
              geom_bar(position="stack", stat="identity")
            WASA<-ggplot(subset(osa_wb_data, country == countries[i] & Sector == "WASA"), aes(fill=Sector, y=value, x=year)) + 
              geom_bar(position="stack", stat="identity")
            Total<-ggplot(subset(osa_wb_data, country == countries[i] & Sector == "Total"), aes(fill=Sector, y=value, x=year)) + 
              geom_bar(position="stack", stat="identity")
            
            
            library(cowplot)
            iso_wb<-plot_grid(AGRI, OTHER, WASA,Total,
                      nrow = 1,
                      align = "v"
            )
            png(file=paste0("results/graphics_tn/",countries[i],"_wb.png"), height=500, width = 2200,
                res = 150)
            print(iso_wb)
            dev.off()  
    }  


        # wb <-rbind(osa_wb_data,osa_total)
        library(webshot)
        library(networkD3)
        # Sankey
        year <- c(2013, 2017)
        country<- c("BRA", "COL", "CRI")
        for (i in 1:length(country)){
          print(country[i])
          for( j in 1:length(year)){
            sankey<- osa_wb_data[osa_wb_data$year==year[j]& osa_wb_data$country == country[i],]
            print(sankey)
            sankey$source <- "Total"
            sankey<- sankey[sankey$Sector != "Total",]
            sankey$target<- sankey$Sector
            sankey$IDsource <- 0
            sankey$IDtarget <- ifelse(sankey$Sector =="AGRI",1,
                                         ifelse(sankey$Sector =="OTHER",2,
                                                ifelse(sankey$Sector =="WASA",3,NA)))
            link<-select(sankey, source, target, value, IDsource, IDtarget)
            nodes <- data.frame(
              name=c(as.character(link$source), 
                     as.character(link$target)) %>% unique())
            
            p <- sankeyNetwork(Links = link, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name", 
                          sinksRight=FALSE)
        
        saveNetwork(p, paste0("results/graphics_tn/sankey/sankey_",country[i],year[j],".html"))
        
        
        # you convert it as png
        webshot(paste0("results/graphics_tn/sankey/sankey_",country[i],year[j],".html"),
                paste0("results/graphics_tn/sankey/sankey_",country[i],year[j],".png"), vwidth = 1000, vheight = 900)
        
          }
          
          }

  
        

    w_b<-pivot_wider(osa_wb_data, names_from = Sector, values_from = value)
    write.csv(w_b, "results/water_balance.csv")
    
   #### Economic output ####
    
    economic_output<- select(data, Sector,X_diag.1,X_diag.2,X_diag.3, country, year)
    economic_output$monetary_output<- economic_output$X_diag.1+economic_output$X_diag.2+economic_output$X_diag.3
  
    eo<- select(economic_output, Sector, country, year, monetary_output)
      total<- eo%>%
      group_by(country, year)%>%
      dplyr::summarise(monetary_output = sum(monetary_output))
    
      
      
      
    total$Sector <- "Total"
    
    economic_output<-rbind(eo, total)
    
    ### economic output bar chart ###
        BRA<-ggplot(subset(economic_output, Sector != "Total" & country == "BRA"), aes(fill=Sector, y=monetary_output, x=year)) + 
          geom_bar(position="stack", stat="identity")+
          facet_wrap(~country)
        COL<-ggplot(subset(economic_output,Sector != "Total" &  country == "COL"), aes(fill=Sector, y=monetary_output, x=year)) + 
          geom_bar(position="stack", stat="identity")+
          facet_wrap(~country)
        CRI<-ggplot(subset(economic_output, Sector != "Total" & country == "CRI"), aes(fill=Sector, y=monetary_output, x=year)) + 
          geom_bar(position="stack", stat="identity")+
          facet_wrap(~country)
        overall_econ_out<-plot_grid(BRA, COL, CRI,
                  nrow = 1,
                  align = "v"
        )
        png(file=paste0("results/graphics_tn/overall_econ_out.png"), height=500, width = 1600,
            res = 150)
        print(overall_econ_out)
        dev.off()  
        

        ### Sector Charts comparing sector economic output across countries ###
        
        sector<-c("AGRI", "OTHER", "WASA")
        for(s in 1:length(sector)){
          
          AGRI<-ggplot(subset(economic_output, Sector ==sector[s] & country == "BRA"), aes(fill=Sector, y=monetary_output, x=year)) + 
            geom_bar(position="stack", stat="identity")+
            facet_wrap(~country)
          OTHER<-ggplot(subset(economic_output,Sector ==sector[s] & country == "COL"), aes(fill=Sector, y=monetary_output, x=year)) + 
            geom_bar(position="stack", stat="identity")+
            facet_wrap(~country)
          WASA<-ggplot(subset(economic_output, Sector ==sector[s] &country == "CRI"), aes(fill=Sector, y=monetary_output, x=year)) + 
            geom_bar(position="stack", stat="identity")+
            facet_wrap(~country)
          sector_eo<-plot_grid(AGRI, OTHER, WASA,
                        nrow = 1,
                        align = "v"
          )
          
          png(file=paste0("results/graphics_tn/",sector[s],"_eo.png"), height=500, width = 1600,
              res = 150)
          print(sector_eo)
          dev.off()  
          
        }
        
        
        ### Country Charts comparing country economic output across sectors ###
        
        countries<-c("BRA", "COL", "CRI")
        for(i in 1:length(countries)){
          options(scipen=999)
          AGRI<-ggplot(subset(economic_output, country == countries[i] & Sector == "AGRI"), aes(fill=Sector, y=monetary_output, x=year)) + 
            geom_bar(position="stack", stat="identity")
          OTHER<-ggplot(subset(economic_output, country == countries[i] & Sector == "OTHER"), aes(fill=Sector, y=monetary_output, x=year)) + 
            geom_bar(position="stack", stat="identity")
          WASA<-ggplot(subset(economic_output, country == countries[i] & Sector == "WASA"), aes(fill=Sector, y=monetary_output, x=year)) + 
            geom_bar(position="stack", stat="identity")
          Total<-ggplot(subset(economic_output, country == countries[i] & Sector == "Total"), aes(fill=Sector, y=monetary_output, x=year)) + 
            geom_bar(position="stack", stat="identity")
          
          
          library(cowplot)
          iso_eo<-plot_grid(AGRI, OTHER, WASA,Total,
                            nrow = 1,
                            align = "v"
          )
          png(file=paste0("results/graphics_tn/",countries[i],"_eo.png"), height=500, width = 2200,
              res = 150)
          print(iso_eo)
          dev.off()  
        }  
        

    econ_out<-pivot_wider(economic_output, names_from = Sector, values_from = monetary_output)
      econ_out$psc_agri<- (econ_out$AGRI/econ_out$Total)*100
      econ_out$psc_other<- (econ_out$OTHER/econ_out$Total)*100 
      econ_out$psc_wasa<-(econ_out$WASA/econ_out$Total)*100
      
   write.csv(econ_out, "results/economic_output.csv")
    
   #### Direct Water footprint ####
    
    wf<- select(data, Sector,direct_WF ,country, year)
    
    total<- wf%>%
      group_by(country, year)%>%
      dplyr::summarise(direct_WF = sum(direct_WF))
    
    total$Sector <- "Total"
    
    wf<-rbind(wf, total)
    
    ## Overall country charts
    
    same_scale<-ggplot(subset(wf, Sector != "Total"), aes(fill=Sector, y=direct_WF, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    
    
    BRA<-ggplot(subset(wf, country == "BRA"), aes(fill=Sector, y=direct_WF, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    COL<-ggplot(subset(wf, country == "COL"), aes(fill=Sector, y=direct_WF, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    CRI<-ggplot(subset(wf, country == "CRI"), aes(fill=Sector, y=direct_WF, x=year)) + 
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    overall_dwf<-plot_grid(BRA, COL, CRI,
              nrow = 1,
              align = "v"
    )
    
    png(file="results/graphics_tn/overall_dwf.png", height=500, width = 1600,
        res = 150)
    
    print(overall_dwf)
    dev.off()  
    
    
    ### Sector Charts comparing sector economic output across countries ###
    
    sector<-c("AGRI", "OTHER", "WASA")
    for(s in 1:length(sector)){
      
      sector_wf<-ggplot(subset(wf, Sector ==sector[s] & year %in%c(2013, 2014,2015,2016,2017)), aes(fill=Sector, y=direct_WF, x=year)) + 
        geom_bar(position="stack", stat="identity")+
        facet_wrap(~country)

      
      png(file=paste0("results/graphics_tn/",sector[s],"_wf.png"), height=500, width = 1600,
          res = 150)
      print(sector_wf)
      dev.off()  
      
    }
    
    
    ### Country Charts comparing country economic output across sectors ###
    
    countries<-c("BRA", "COL", "CRI")
    for(i in 1:length(countries)){
      iso_wf<-ggplot(subset(wf, country == countries[i]), aes(fill=Sector, y=direct_WF, x=year)) + 
        geom_bar(position="stack", stat="identity")+
        facet_wrap(~Sector, ncol = 4)
      
      png(file=paste0("results/graphics_tn/",countries[i],"_wf.png"), height=500, width = 1600,
          res = 150)
      print(iso_wf)
      dev.off()  
    }  
    
    
    directWF<-pivot_wider(wf, names_from =Sector, values_from = direct_WF)
    write.csv(directWF, "results/direct_water_footprint.csv")
    

   #### Total Water Footprint ####
    
    twf<- select(data, Sector,"Total requirement matrix of water consumption - Ag",  "Total requirement matrix of water consumption - Other" , "Total requirement matrix of water consumption - W&S" ,country, year)
    twf$total_water_footprint<- twf$`Total requirement matrix of water consumption - Ag`+twf$`Total requirement matrix of water consumption - Other`+twf$`Total requirement matrix of water consumption - W&S`
    
    totalwf<-twf%>%
      dplyr::group_by(country, year)%>%
      dplyr::summarise(totalwf = sum(total_water_footprint))
    
    totalwf%>%
      dplyr::group_by(country)%>%
      dplyr::summarise(avtwf = mean(totalwf))

    
    # ## Overall country charts
    # 
    Same_scale<-ggplot(subset(totalwf), aes(y=totalwf, x=year)) +
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    
    
    
    BRA<-ggplot(subset(totalwf, country == "BRA"), aes(y=totalwf, x=year)) +
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    COL<-ggplot(subset(totalwf, country == "COL"), aes(y=totalwf, x=year)) +
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    CRI<-ggplot(subset(totalwf, country == "CRI"), aes(y=totalwf, x=year)) +
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~country)
    overall_twf<-plot_grid(BRA, COL, CRI,
              nrow = 1,
              align = "v"
    )
    
    png(file="results/graphics_tn/overall_twf.png", height=500, width = 1600,
        res = 150)
    
    print(Same_scale)
    dev.off() 

    
    
   #### Backward Linkages ####
    bl<- select(data, Sector,`Backward linkages` ,country, year)
    total<- bl%>%
      group_by(country, year)%>%
      dplyr::summarise(`Backward linkages` = sum(`Backward linkages`))
    
    total$Sector <- "Total"
    
    bl<-rbind(bl, total)
    
    

    ### Sector Charts comparing sector economic output across countries ###
    
    sector<-c("AGRI", "OTHER", "WASA")
    for(s in 1:length(sector)){
      
      sector_bl<-ggplot(subset(bl, Sector ==sector[s] & year %in%c(2013, 2014,2015,2016,2017)), aes(fill=Sector, y=`Backward linkages`, x=year)) + 
        geom_bar(position="stack", stat="identity")+
        facet_wrap(~country)

      png(file=paste0("results/graphics_tn/",sector[s],"_bl.png"), height=500, width = 1600,
          res = 150)
      print(sector_bl)
      dev.off()  
      
    }
    
    
    ### Country Charts comparing country economic output across sectors ###
    
    countries<-c("BRA", "COL", "CRI")
    for(i in 1:length(countries)){
      iso_bl<-ggplot(subset(bl, country == countries[i]), aes(fill=Sector, y=`Backward linkages`, x=year)) + 
        geom_bar(position="stack", stat="identity")+
        facet_wrap(~Sector, ncol = 4)
      
      png(file=paste0("results/graphics_tn/",countries[i],"_bl.png"), height=500, width = 1600,
          res = 150)
      print(iso_bl)
      dev.off()  
    }  

    backward_l<-pivot_wider(bl, names_from =Sector, values_from = `Backward linkages`)
    backward_l$total<- backward_l$AGRI+ backward_l$OTHER+ backward_l$WASA 
    
    write.csv(backward_l, "results/backward_linkages.csv")
    
    
    
   #### Forward Linkages ####

    fl<- select(data, Sector,`Forward linkages` ,country, year)
    
    total<- fl%>%
      group_by(country, year)%>%
      dplyr::summarise(`Forward linkages` = sum(`Forward linkages`))
    
    total$Sector <- "Total"
    
    fl<-rbind(fl, total)
    
    
    
    ### Sector Charts comparing sector economic output across countries ###
    
    sector<-c("AGRI", "OTHER", "WASA")
    for(s in 1:length(sector)){
      
      sector_fl<-ggplot(subset(fl, Sector ==sector[s] & year %in%c(2013, 2014,2015,2016,2017)), aes(fill=Sector, y=`Forward linkages`, x=year)) + 
        geom_bar(position="stack", stat="identity")+
        facet_wrap(~country,ncol=4)

      
      png(file=paste0("results/graphics_tn/",sector[s],"_fl.png"), height=500, width = 1600,
          res = 150)
      print(sector_fl)
      dev.off()  
      
    }
    
    
    ### Country Charts comparing country economic output across sectors ###
    options(scipen = 999)
    countries<-c("BRA", "COL", "CRI")
    for(i in 1:length(countries)){
      iso_fl<-ggplot(subset(fl, country == countries[i]), aes(fill=Sector, y=`Forward linkages`, x=year)) + 
        geom_bar(position="stack", stat="identity")+
        facet_wrap(~Sector, ncol = 4)+ 
       # geom_text(aes(label=round(`Forward linkages`,2), size = 3, vjust=0)) 
      
      png(file=paste0("results/graphics_tn/",countries[i],"_fl.png"), height=500, width = 1600,
          res = 150)
      print(iso_fl)
      dev.off()  
    }  
    
    forward_l<-pivot_wider(fl, names_from =Sector, values_from = `Forward linkages`)
    write.csv(forward_l, "results/forward_linkages.csv")
    
    
    

