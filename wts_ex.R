source('funcs.R')
load('ELKVM_to_proc.RData')

Sys.setenv(TZ = 'Pacific/Pitcairn')

dat.in <- to_proc
dat.in$Date <- as.Date(dat.in$DateTimeStamp)

require(ggplot2)
require(scales)
require(gridExtra)
require(reshape2)
require(RColorBrewer)

reftime <- 12

pdf('C:/Users/Marcus/Desktop/wts_ex.pdf', height = 3.5, width = 7, family = 'serif')
for(i in 1:15){
  i <- 1
  reftime <- reftime + 0.5
  
  input <- list(
    daterange = c("2012-06-29", "2012-07-04"), 
    refdate = "2012-07-01",
    reftime = reftime, 
    win_1 = 2,
    win_2 = 12,
    win_3 = 1
  )
  
  
  # dates for plot window
  dt_rng <- as.POSIXct(c(input$daterange[1], input$daterange[2]), tz = 
                         'Pacific/Pitcairn')
  
  # center of viewing window
  ref.dt <- as.Date(input$refdate, tz = 'Pacific/Pitcairn')
  ref.time <- strsplit(as.character(input$reftime), '.', fixed = T)[[1]]
  if(length(ref.time) == 1 ) ref.time[2] <- '0'
  if(nchar(ref.time[1]) == 1) ref.time[1] <- paste0('0', ref.time[1])
  ref.time[2] <- as.character((as.numeric(ref.time[2])/10)*60)
  ref.time <- paste(ref.time[1], ref.time[2], '00', sep = ':')
  ref.in <- as.POSIXct(paste(ref.dt, ref.time), '%Y-%m-%d %H:%M:%S', 
             tz = 'Pacific/Pitcairn')
  
  # get reference data (at center of window) and windows
  ref.in<-dat.in[dat.in$DateTimeStamp %in% ref.in,]
  
  win_1 <- input$win_1
  win_2 <- input$win_2
  win_3 <- input$win_3
  wins_in <- list(win_1, win_2, win_3)
  
  ##
  # get weights from wt_fun
  ref_wts <- wt_fun(ref.in, dat.in, all = T, slice = F, wins = wins_in)
  
  titles<-with(
    ref.in,
    c(paste('Day window', win_1),
      paste('Hour window', win_2),
      paste('Tidal height window', win_3), 
      'Combined weights',
      paste(DateTimeStamp)
      )
  )
  
  dat.in$final <- ref_wts$final
  p5 <- ggplot(dat.in, aes(x = DateTimeStamp, y = Tide)) + 
    geom_vline(xintercept=as.numeric(ref.in$DateTimeStamp)) +
    geom_line(alpha = 0.2) +
    geom_point(aes(size = final, colour = final), 
                   alpha = 0.8) + 
    scale_y_continuous(name = 'Tide height (m)') +
    scale_x_datetime(name=element_blank(), limits = dt_rng) +
    theme_bw() +
    ggtitle(titles[5]) +
    scale_size(range = c(1, 10)) +
    theme(legend.position = 'none', axis.text = element_text(size = 18), 
          text = element_text(size = 18))
  print(p5)
}
dev.off()
