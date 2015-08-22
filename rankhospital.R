rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!any(state == outcome_data$State)){
                stop("invalid state")     
        }
        if (!any(outcome == c("heart attack" , "heart failure"  ,"pneumonia" ) )){
                stop("invalid outcome")     
        }
        
        if (outcome == "heart attack"){
                oc <- 11
        }
        
       else if (outcome == 'heart failure'){
                oc <- 17       
        }
        
       else if (outcome == "pneumonia"){
                oc <- 23       
        }
        
        
        outcome_table <- outcome_data[outcome_data$State == state, ] # get all state data
        outcome_table[, oc] <- as.numeric(outcome_table[, oc])
       outcome_table <- outcome_table[complete.cases(outcome_table), ]
        
        # if num greater than number of hospitals will return NA
        
        if (is.numeric(x=num) && num > nrow(outcome_table)){
                return(NA)
        }
        
        else if (num == "best"){
                num = 1
                
        }
        else if (num == "worst"){
               
                num = nrow(outcome_table)
        }
        
        ## Return hospital name in that state with the given rank 30-day death rate
        
        outcome_table <- outcome_table[order(outcome_table[,oc], outcome_table$Hospital.Name), ]
        #get hospital name with rank
        Hospital_name <- outcome_table[num, ]$Hospital.Name
        
        print(Hospital_name [1]) 
        
       
        
        
        
}


