######
#function for getting regression weights
# note that this subsets the input data frame for faster wt selection
# subset is by limiting window for product of weights (dec_time)
# subsetted weights are recombined to equal vector of length = nrow(dat_in)
#'wt_vars' is name of three variables to weight
#'ref_in' is row of dat.in that is used as reference
#'dat_in' is data to get weights from
#'wins' are the windows for the three wt.vars, values represent halves
#'all' will return all weights, rather than the product of all three
#'slice' is logical for subsetting 'dat_in' for faster wt selection
#'subs_only' is logical for returning only wt vectors that are non-zero
wt_fun <- function(ref_in, dat_in,
                   wt_vars = c('dec_time', 'hour', 'Tide'),
                   wins = list(4, 12, NULL),
                   all = F, 
                   slice = T, 
                   subs_only = F){
  
  # sanity check
  if(sum(wt_vars %in% names(dat_in)) != length(wt_vars))
    stop('Weighting variables must be named in "dat_in"')
  
  # windows for each of three variables
  wins_1<-wins[[1]]
  wins_2<-wins[[2]]
  wins_3<-wins[[3]]
  
  # default window width for third variable is half its range
  if(is.null(wins[[3]])) wins_3 <- diff(range(dat_in[, wt_vars[3]]))/2
  
  # weighting tri-cube function
  # mirror extends weighting function if vector repeats, e.g. monthly
  # 'dat_cal' is observation for weight assignment
  # 'ref' is reference observation for fitting the model
  # 'win' is window width from above (divided by two)
  # 'mirr' is logical indicating if distance accounts for repeating variables (e.g., month)
  # 'scl_val' is range for the ref vector of obs, used to get correct distance for mirrored obs
  wt_fun_sub <- function(dat_cal, ref, win, mirr = F, scl_val = 1){
    
    # dist_val is distance of value from the ref
    dist_val <- sapply(ref, function(x) abs(dat_cal - x))
    
    # repeat if distance is checked on non-continuous number line
    if(mirr){
      
      dist_val <- pmin(
        sapply(ref, function(x)
          abs(x + scl_val - dat_cal)),
        sapply(ref, function(x) abs(dat_cal + scl_val - x)),
        dist_val
      )
      
    }
    
    # get wts within window, otherwise zero
    win_out <- dist_val > win
    dist_val <- (1 - (dist_val/win)^3)^3
    dist_val[win_out] <- 0
    
    return(dist_val)
    
  }
  
  #reference (starting) data
  ref_1 <- as.numeric(ref_in[, wt_vars[1]])
  ref_2 <- as.numeric(ref_in[, wt_vars[2]])
  ref_3 <- as.numeric(ref_in[, wt_vars[3]])
  
  ##
  # subset 'dat_in' by max window size for faster calc
  # this is repeated if min number of wts > 0 is not met
  # subset vector is all T if not using subset
  dec_rng <- range(dat_in$dec_time)
  ref_time <- unique(ref_in$dec_time)
  dec_sub <- with(dat_in, 
                  dec_time > 
                    ref_time - wins_1 * 5 & dec_time < ref_time + wins_1 * 5
  )
  if(!slice) dec_sub <- rep(T, length = nrow(dat_in))
  dat_sub <- dat_in[dec_sub, ]
  
  ##
  # weights for each observation in relation to reference
  # see comments for 'wt_fun_sub' for 'scl_val' argument
  
  # jday
  wts_1 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[1]]), 
                      ref = ref_1, win = wins_1, mirr = F) 
  # hour
  wts_2 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[2]]), 
                      ref = ref_2, win = wins_2, mirr = T, scl_val = 24)
  # tide
  wts_3 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[3]]), 
                      ref = ref_3, win = wins_3, mirr = F)
  # all as product 
  out <- sapply(1:nrow(ref_in), function(x) wts_1[, x] * wts_2[, x] * wts_3[, x])
  
  gr_zero <- colSums(out > 0)
  #cat('   Number of weights greater than zero =',gr.zero,'\n')
  
  # extend window widths of weight vector is less than 100
  while(any(gr_zero < 100)){
    
    # increase window size by 10%
    wins_1 <- 1.1 * wins_1
    wins_2 <- 1.1 * wins_2
    wins_3 <- 1.1 * wins_3 
    
    # subset again
    dec_sub <- with(dat_in, 
                    dec_time > ref_time - wins_1 * 5 & dec_time < ref_time + wins_1 * 5
    )
    if(!slice) dec_sub <- rep(T, length = nrow(dat_in))
    dat_sub <- dat_in[dec_sub, ]
    
    #weights for each observation in relation to reference
    wts_1 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[1]]), 
                        ref = ref_1, win = wins_1, mirr = F)
    wts_2 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[2]]), 
                        ref = ref_2, win = wins_2, mirr = T, scl_val = 24)
    wts_3 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[3]]), 
                        ref = ref_3, win = wins_3, mirr = F)
    
    out <- sapply(1:nrow(ref_in), 
                  function(x) wts_1[, x] * wts_2[, x] * wts_3[, x])
    
    gr_zero <- colSums(out > 0)
    
  }
  
  if(subs_only){
    
    nms <- which(dec_sub)
    out <- alply(out, 2, function(x) {
      
      to_sel <- x > 0
      tmp <- x[to_sel]
      names(tmp) <- which(dec_sub)[to_sel]
      tmp
      
    })
    
    return(out)
    
  }
  
  # extend weight vectors to length of dat_in
  empty_mat <- matrix(0, ncol = nrow(ref_in), nrow = nrow(dat_in))
  empty_fill <- function(wts_in) {
    out <- empty_mat
    out[dec_sub,] <- wts_in
    out
  }
  wts_1 <- empty_fill(wts_1)
  wts_2 <- empty_fill(wts_2)
  wts_3 <- empty_fill(wts_3)  
  out <- empty_fill(out)
  
  #return all weights if T
  if(all){
    out <- data.frame(dat_in$DateTimeStamp, 
                      wts_1, wts_2, wts_3, out)
    names(out) <- c('DateTimeStamp', wt_vars, 'final')
    return(out)
  }
  
  #final weights are product of all three
  out
  
}

