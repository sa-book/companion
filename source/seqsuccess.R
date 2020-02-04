seqsuccess <-
  function(seqdata, success, weight = 1, digits = 2, time.varying = FALSE) {
    
    if(missing(success)) {
      stop("State indicating success has to be specified")  
    } 
    
    if (!inherits(seqdata, "stslist")) 
      stop("data is not a sequence object, use 'seqdef' function to create one")
    
    if (class(weight) != "numeric") {
      stop("weight must be a single number or a vector")
    }
    
    if (class(success) == "character") {
      success <- match(success,alphabet(seqdata))
    }
    
    if (!all(success %in% 1:length(alphabet(seqdata)))) {
      stop("'success' must be an element of the alphabet; provide correct number or string") 
    }
    
    
    if (digits != as.integer(digits)) {
      stop("digits must be an integer")
    }
    
    ##
    
    if (time.varying == TRUE) {
      if (length(weight) != 1) {
        stop("If 'time.varying = TRUE' only a single weight is permitted")
      }
      
      corpus <- matrix(data=NA,
                       nrow= nrow(seqdata),
                       ncol= ncol(seqdata))
      
      for (c in 1:ncol(seqdata)) {
        
        counter <- vector(mode = "list", length = nrow(seqdata))
        denominator <- counter
        
        seqdata.col <- seqdata[,1:c]
        
        for(i in seq_along(counter)) {
          counter[[i]] <- sum(which(seqdata[i,1:c] %in% success)^weight)
          denominator[[i]] <- sum(seq(1:c)^weight)
          
          if (c==1) counter[[i]] <- 1*(as.numeric(seqdata[i,1:c]) %in% success)
          
        }
        
        counter <- unlist(counter)
        denominator <- unlist(denominator)
        
        corpus[,c] <- round((counter/denominator),digits)
      }
      
    }
    
    if (time.varying == FALSE) {
      
      corpus <- matrix(data=NA, 
                       nrow=nrow(seqdata), 
                       ncol=length(weight))
      
      for (c in 1:length(weight)) {
        
        counter <- vector(mode = "list", length = nrow(seqdata))
        denominator <- counter
        
        for(i in seq_along(counter)) {
          counter[[i]] <- sum(which(seqdata[i,] %in% success)^weight[c])
          denominator[[i]] <- sum(seq(1:seqlength(seqdata[i,]))^weight[c])
        }
        
        counter <- unlist(counter)
        denominator <- unlist(denominator)
        
        corpus[,c] <- round((counter/denominator),digits)
        
        colnames(corpus) <- paste0("w=",weight)
        
      }
    }
    
    return(corpus)
    
  }

