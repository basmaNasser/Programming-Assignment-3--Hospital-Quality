best <- function(state, outcome) {
        ## Read outcome data
        
        outcome_table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
 
        ## Check that state and outcome are valid
        if (!any(state == outcome_table$State)){
               stop("invalid state")     
        }
        
        if (!any(outcome == c("heart attack", "heart failure" ,"pneumonia") )){
                stop("invalid outcome")     
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
       
        if (outcome == "heart attack"){
                splitted_state <- split(outcome_table,outcome_table$State)
                outcome_table <- splitted_state[[state]]
                
                Hospital_name <- outcome_table[(outcome_table[, 11] == min(na.omit(splitted_state[[state]][,11]))), ]$Hospital.Name
             
        #   outcome_table <- outcome_table[outcome_table$State == state, ] # get all state data
            #outcome_table[, 11] <- as.numeric(x=outcome_table[, 11])  #get col no 11
          #  return.names <- outcome_table[(outcome_table[, 11] == min(na.omit(outcome_table[, 11]))), ]$Hospital.Name
       
        
        }
        if (outcome == 'heart failure'){
                splitted_state <- split(outcome_table,outcome_table$State)
                outcome_table <- splitted_state[[state]]
                Hospital_name <- outcome_table[(outcome_table[, 17] == min(na.omit(splitted_state[[state]][,17]))), ]$Hospital.Name              
                
        }
        
        if (outcome == "pneumonia"){
               
                
                outcome_table <- outcome_table[outcome_table$State == state, ] # get all state data
                outcome_table[, 23] <- as.numeric(outcome_table[, 23])  #get col no 11
                Hospital_name  <- outcome_table[(outcome_table[, 23] == min(na.omit(outcome_table[, 23]))), ]$Hospital.Name
                
                
        }
        
        print (sort(Hospital_name)[1])
        
        
        
}
