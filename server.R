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

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  output$simplot <- renderPlot({
    
    # add plotting code here

#     # for debugging
#     input <- list(refdate = '2012-07-01', reftime = 0, 
#                   daterange = c('2012-06-26', '2012-07-07'),
#                   win_1 = 10, win_2 = 4, win_3 = 1)
    
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
        paste('All weights and tidal series', DateTimeStamp)
        )
    )
    
    p1 <- ggplot(ref_wts, aes(x = DateTimeStamp, y = dec_time)) + 
      geom_line() + 
      ggtitle(titles[1]) +
      scale_y_continuous(name = 'Weight', limits = c(0,1)) +
      scale_x_datetime(name=element_blank(), limits = dt_rng) +
      theme_bw() +
      theme(axis.text = element_text(size = 14), 
            text = element_text(size = 16))
    
    p2 <- ggplot(ref_wts, aes(x = DateTimeStamp, y = hour)) + 
      geom_line() + 
      ggtitle(titles[2]) +
      scale_y_continuous(name = 'Weight', limits = c(0,1)) +
      scale_x_datetime(name=element_blank(), limits = dt_rng) +
      theme_bw() +
      theme(axis.text = element_text(size = 14), 
            text = element_text(size = 16))
    
    p3 <- ggplot(ref_wts, aes(x = DateTimeStamp, y = Tide)) + 
      geom_line() + 
      ggtitle(titles[3]) +
      scale_y_continuous(name = 'Weight', limits = c(0,1)) +
      scale_x_datetime(name=element_blank(), limits = dt_rng) +
      theme_bw() +
      theme(axis.text = element_text(size = 14), 
            text = element_text(size = 16))
    
    p4 <- ggplot(ref_wts, aes(x = DateTimeStamp, y = final)) + 
      geom_line() + 
      ggtitle(titles[4]) +
      scale_y_continuous(name = 'Weight', limits = c(0,1)) +
      scale_x_datetime(name=element_blank(), limits = dt_rng) +
      theme_bw() +
      theme(axis.text = element_text(size = 14), 
            text = element_text(size = 16))
    
    dat.in$final <- ref_wts$final
    p5 <- ggplot(dat.in, aes(x = DateTimeStamp, y = Tide)) + 
      geom_vline(xintercept=as.numeric(ref.in$DateTimeStamp)) +
      geom_line(alpha = 0.2) +
      geom_point(aes(size = final, colour = final), 
                     alpha = 0.8) + 
      scale_y_continuous(name = 'Tidal height (m)') +
      scale_x_datetime(name=element_blank(), limits = dt_rng) +
      theme_bw() +
      ggtitle(titles[5]) +
      scale_size(range = c(1, 7)) +
      theme(legend.position = 'none', axis.text = element_text(size = 14), 
            text = element_text(size = 16))
   
    grid.arrange(
      p5,
      arrangeGrob(p1,p2,p3,p4,nrow=2,left=textGrob('Weights',rot=90)),
      sub='Date', heights = c(5, 10)
    )
    
    },height = 600, width = 750)

    })