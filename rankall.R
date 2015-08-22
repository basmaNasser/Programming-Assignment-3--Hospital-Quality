rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        
#         if (!any(outcome == c("heart attack" , "heart failure"  ,"pneumonia" ) )){
#                 stop("invalid outcome")     
#         }

        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        if (outcome == "heart attack"){
                oc <- 11
        }
        
        else if (outcome == 'heart failure'){
                oc <- 17       
        }
        
        else if (outcome == "pneumonia"){
                oc <- 23       
        }
        else {
                
                stop("invalid outcome") 
        }

outcome_data <- outcome_data[complete.cases(outcome_data), ]
splitted_state <- split(outcome_data,outcome_data$State)

results <- list()
# if num greater than number of hospitals will return NA

if (is.numeric(x=num) && num > nrow(outcome_data)){
        return(NA)
}

else if (num == "best"){
        num = 1
        
}


for (state in unique(sort(outcome_data$State)) ){
        if (num == "worst"){
                
                num = nrow(splitted_state[[state]][oc])
        }
        sorted_hospitals <- splitted_state[[state]][order(splitted_state[[state]][oc]),]$Hospital.Name
      #results <- c(sorted_hospitals[num],state)
     
      results <- rbind(results,list(sorted_hospitals[num],state))
      #sorted <- splitted_state$"AL"[order(splitted_state$"AL"[11]),]$Hospital.Name 
       
      
}
results <- as.data.frame(results)
colnames(results) <- c('hospital', 'state') 
print (results)
}


