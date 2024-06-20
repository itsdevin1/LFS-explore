#poc

#test script

data <- read_csv("data.csv")
test<-func(data,9,5,40,0,9,0,100,1000,0)
test
print(test)
output<-test

#function to output graph

func <- function(data,gender,AGE_12.min,AGE_12.max,EDUC.min,EDUC.max,prov,ATOTHRS.min,ATOTHRS.max,graphparams){
  
  #declare libraries
  
  library(readr)
  library(ggplot2)
  library(dplyr)
  
  df <- data
  
  #define filtering functions
  #all inputs should be expressed in the same format as the LMF header except flags
  #flags should be ran as 0 (false) or 1 (true)
  
  #filter by min/max | ran with minimum value (expressed in the same format as the LMF header)

  
  filter_by_minmax <- function(minval,maxval,df,table_name){
    
    df<-df%>%
      filter(
        .data[[table_name]] >= minval,
        .data[[table_name]] <= maxval,
      )
    df
  }
  
  #filter by min/max | ran with minimum value (expressed in the same format as the LMF header)
  filter_by_flag <- function(flag,df,table_name){
    
    df<-df%>%
      filter(
        .data[[table_name]] > flag
      )
    df
  }
  
  #filter by min/max | ran with minimum value (expressed in the same format as the LMF header)
  filter_by_value <- function(val,df,table_name){
    
    df<-df%>%
      filter(
        .data[[table_name]] == val
      )
    df
  }
  
  
  #apply filters to data
  apply_filters <- function(df,AGE_12.min,AGE_12.max,EDUC.min,EDUC.max,ATOTHRS.min,ATOTHRS.max,prov,gender){
      #age
      df<-filter_by_minmax(AGE_12.min,AGE_12.max,df,"AGE_12")
      #education
      df<-filter_by_minmax(EDUC.min,EDUC.max,df,"EDUC")
      #hours
      df<-filter_by_minmax(ATOTHRS.min,ATOTHRS.max,df,"ATOTHRS")
      #prov
      # if (df$PROV != 0){df<- filter_by_value(prov,df,"PROV")}
      #sex
      # if (df$SEX == 0 | 1){df<- filter_by_value(gender,df,"SEX")}
      #return data frame
      df
  }
  
  1!=0
  
  #plot data(very wip)
  
  plot<- function(filtered_data){
    #placeholder
    geom<-"placeholder"
    
    df <- filtered_data
    
    #calculate yearly wages
    df$ANNEARN <- df$HRLYEARN * df$ATOTHRS * 52 * (.001)
    
    #plot1
    #debug
    print(df$ANNEARN)
    print(max(na.omit(df$ANNEARN)))
    print(min(na.omit(df$ANNEARN)))
    
    ggplot(df)+
      aes(ANNEARN)+
      geom_histogram()+
      scale_x_log10()
    
    summary(df$ANNEARN)
  }
  
  
  #run function
  
  filtered_data<-apply_filters(df,AGE_12.min,AGE_12.max,EDUC.min,EDUC.max,ATOTHRS.min,ATOTHRS.max,prov,gender)
  plot(filtered_data)
}

