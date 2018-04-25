## Keith Mertan ##
## 4/14/2018 ##
## Create visualizations for CS7641 assignment 4 ##

library(dplyr)
library(magrittr)

setwd('/Users/kmertan/Documents/CS7641/kmertan3/Solution/')

dataFiles <- c('Easy Value',
               'Easy Policy',
               'Hard Value',
               'Hard Policy')

dataFiles %<>% paste0('.csv')

valuePolicyColors <- c(#'green',
                       'red',
                       'blue',
                       'purple')

timeColors <- c('')


colors <- c(rep('black', 20))

summarizeVariable <- function(csv, colName = 'reward'){
  output <- data.frame(file = character(1), 
                       min = numeric(1), 
                       max = numeric(1), 
                       last = numeric(1),
                       stringsAsFactors = F)
  
  for(file in csv){
    data <- read.csv(file)
    min <- min(data[ , colName])
    max <- max(data[ , colName])
    last <- data[nrow(data), colName]
    output %<>% rbind(c(file, min, max, last))
    #print(paste(file, min, max, last))
    #writeLines(c(file, min, max, last, '\n'), sep = '\t\t')
  }
  output <- output[-1, ]
  output
}

stripCSV <- function(csv){
  gsub('.csv', '', csv)
}

findDomainAndRange <- function(csv){
  for(i in 1:length(csv)){
    data <- read.csv(csv[i])
    
    if(i == 1){
      names(data)
      minMaxDF <- data.frame(matrix(ncol = ncol(data), nrow = 0))
      names(minMaxDF) <- names(data)
      
      minMaxDF[1, ] <- apply(data, 2, min)
      minMaxDF[2, ] <- apply(data, 2, max)
    }
    
    mins <- apply(data, 2, min)
    maxs <- apply(data, 2, max) 
    
    minMaxDF[1, ] <- apply(rbind(minMaxDF[1, ], mins), 2, min)
    minMaxDF[2, ] <- apply(rbind(minMaxDF[2, ], maxs), 2, max)
  }
  
  minMaxDF
}

timePlot <- function(csv, color = colors, lty = 'o', title = '', legend = stripCSV(csv), dr = findDomainAndRange(csv), fontSize = 1){
  #dr <- findDomainAndRange(csv)
  
  for(i in 1:length(csv)){
    data <- read.csv(csv[i])
    
    if(i == 1){
      data %$% plot(iter, time, 
                    xlim = dr$iter,
                    ylim = dr$time,
                    type = lty, 
                    col = color[i],
                    xlab = 'Iteration',
                    ylab = 'Time',
                    main = title,
                    cex.main = fontSize,
                    cex.lab = fontSize,
                    cex.axis = fontSize)
    }
    
    else{
      data %$% lines(time, type = lty, col = color[i])
    }
    
    #legend('bottomright', legend = legend, pch = 1, col = color[seq(1, length(csv))])
  }
}

rewardPlot <- function(csv, color = colors, lty = 'o', title = '', legend = stripCSV(csv), dr = findDomainAndRange(csv), fontSize = 1){
  #dr <- findDomainAndRange(csv)
  
  for(i in 1:length(csv)){
    data <- read.csv(csv[i])
    
    if(i == 1){
      data %$% plot(iter, reward, 
                    xlim = dr$iter,
                    ylim = dr$reward,
                    type = lty, 
                    col = color[i],
                    xlab = 'Iteration',
                    ylab = 'Reward',
                    main = title,
                    cex.main = fontSize,
                    cex.lab = fontSize,
                    cex.axis = fontSize)
    }
    
    else{
      data %$% lines(reward, type = lty, col = color[i])
    }
    
    #legend('bottomright', legend = legend, pch = 1, col = color[seq(1, length(csv))])
  }
}

convPlot <- function(csv, color = colors, lty = 'o', title = '', legend = stripCSV(csv)){
  dr <- findDomainAndRange(csv)
  
  for(i in 1:length(csv)){
    data <- read.csv(csv[i])
    
    if(i == 1){
      data %$% plot(iter, convergence, 
                    xlim = dr$iter,
                    ylim = dr$convergence,
                    type = lty, 
                    col = color[i],
                    xlab = 'Iteration',
                    ylab = 'Convergence',
                    main = title)
    }
    
    else{
      data %$% lines(convergence, type = lty, col = color[i])
    }
    
    #legend('topright', legend = legend, pch = 1, col = color[seq(1, length(csv))])
  }
}

algoLegend <- c('Value Iteration', 'Policy Iteration')


jpeg('Easy Grid Time Plot.jpg', height = 360)
dataFiles[1:2] %>% timePlot(valuePolicyColors, 
                            title = 'Easy Grid World\nRun Time & Iterations', 
                            legend = algoLegend)
dev.off()

jpeg('Easy Grid Reward Plot.jpg', height = 360)
dataFiles[1:2] %>% rewardPlot(valuePolicyColors,
                              title = 'Easy Grid World\nMean Empirical Reward - 50 Trials',
                              legend = algoLegend)
dev.off()

dataFiles[1:2] %>% convPlot(valuePolicyColors)

jpeg('Hard Grid Time Plot.jpg', height = 360)
dataFiles[3:4] %>% timePlot(valuePolicyColors,
                            title = 'Hard Grid World\nRun Time & Iterations', 
                            legend = algoLegend)
dev.off()

jpeg('Hard Grid Reward Plot.jpg', height = 360)
dataFiles[3:4] %>% rewardPlot(valuePolicyColors,
                              title = 'Hard Grid World\nMean Empirical Reward - 50 Trials',
                              legend = algoLegend)
dev.off()



ls <- c('0.1', '0.9')
qs <- c('-100.0', '0.0', '100.0')
es <- c('0.1', '0.3', '0.5')

for(l in ls){
  for(q in qs){
    for(e in es){
      
      qFile <- paste0('Easy Q-Learning L', l, ' q', q, ' E', e)
      
      if(!exists('easyQFiles')){
        easyQFiles <<- c()
      }
      
      else if(length(easyQFiles) == length(ls) * length(qs) * length(es)){
        easyQFiles <- c()
      }
      
      easyQFiles %<>% c(qFile)
    }
  }
}
easyQFiles %<>% paste0('.csv')
hardQFiles <- gsub('Easy', 'Hard', easyQFiles)

qcolors <- c('lightblue', 'blue', 'darkblue', 'pink', 'red', 'maroon')
slices <- c('q-100', 'q0', 'q100')

allDReasy <- findDomainAndRange(easyQFiles)
allDRhard <- findDomainAndRange(hardQFiles)
allDReasycustom <- allDReasy
allDReasycustom$reward <- c(-1200, 50)

combos <- expand.grid(c('0.1', '0.3', '0.5'), c('0.1', '0.9'))
legend = c()
for(i in 1:nrow(combos)){
  legend %<>% c(paste0(expression(alpha), ': ', combos[i, 2], '\n', expression(epsilon), ': ', combos[i, 1]))
}

jpeg('Easy Q Time Plot.jpg', height = 480, width = 840)
layout(matrix(c(1, 2, 3, 4, 4, 4), nrow=2, byrow=TRUE), heights=c(7, 1))

easyQFiles[grep(slices[1], easyQFiles)] %>% timePlot(lty = 'l', color = qcolors, dr = allDReasy, title = 'Time\nInitial Q = -100', fontSize = 1.7)
easyQFiles[grep(slices[2], easyQFiles)] %>% timePlot(lty = 'l', color = qcolors, dr = allDReasy, title = 'Time\nInitial Q = 0', fontSize = 1.7)
easyQFiles[grep(slices[3], easyQFiles)] %>% timePlot(lty = 'l', color = qcolors, dr = allDReasy, title = 'Time\nInitial Q = 100', fontSize = 1.7)

par(mai = c(0, 0, 0, 0))
plot.new()
legend(x = "center", ncol = 6,legend = legend, fill = qcolors, bty = 'n', cex = 1.7)
dev.off()



jpeg('Easy Q Param Select.jpg', height = 480, width = 840)
layout(matrix(c(1, 2, 3, 4, 4, 4), nrow=2, byrow=TRUE), heights=c(7, 1))

easyQFiles[grep(slices[1], easyQFiles)] %>% rewardPlot(lty = 'l', color = qcolors, dr = allDReasycustom, title = 'Reward\nInitial Q = -100', fontSize = 1.7)
easyQFiles[grep(slices[2], easyQFiles)] %>% rewardPlot(lty = 'l', color = qcolors, dr = allDReasycustom, title = 'Reward\nInitial Q = 0', fontSize = 1.7)
easyQFiles[grep(slices[3], easyQFiles)] %>% rewardPlot(lty = 'l', color = qcolors, dr = allDReasycustom, title = 'Reward\nInitial Q = 100', fontSize = 1.7)

par(mai = c(0, 0, 0, 0))
plot.new()
legend(x = "center", ncol = 6,legend = legend, fill = qcolors, bty = 'n', cex = 1.7)
dev.off()



jpeg('Hard Q Time Plot.jpg', height = 480, width = 840)
layout(matrix(c(1, 2, 3, 4, 4, 4), nrow=2, byrow=TRUE), heights=c(7, 1))

hardQFiles[grep(slices[1], hardQFiles)] %>% timePlot(lty = 'l', color = qcolors, dr = allDRhard, title = 'Time\nInitial Q = -100', fontSize = 1.7)
hardQFiles[grep(slices[2], hardQFiles)] %>% timePlot(lty = 'l', color = qcolors, dr = allDRhard, title = 'Time\nInitial Q = 0', fontSize = 1.7)
hardQFiles[grep(slices[3], hardQFiles)] %>% timePlot(lty = 'l', color = qcolors, dr = allDRhard, title = 'Time\nInitial Q = 100', fontSize = 1.7)

par(mai = c(0, 0, 0, 0))
plot.new()
legend(x = "center", ncol = 6,legend = legend, fill = qcolors, bty = 'n', cex = 1.7)
dev.off()



jpeg('Hard Q Param Select.jpg', height = 480, width = 840)
layout(matrix(c(1, 2, 3, 4, 4, 4), nrow=2, byrow=TRUE), heights=c(7, 1))

hardQFiles[grep(slices[1], hardQFiles)] %>% rewardPlot(lty = 'l', color = qcolors, dr = allDRhard, title = 'Reward\nInitial Q = -100', fontSize = 1.7)
hardQFiles[grep(slices[2], hardQFiles)] %>% rewardPlot(lty = 'l', color = qcolors, dr = allDRhard, title = 'Reward\nInitial Q = 0', fontSize = 1.7)
hardQFiles[grep(slices[3], hardQFiles)] %>% rewardPlot(lty = 'l', color = qcolors, dr = allDRhard, title = 'Reward\nInitial Q = 100', fontSize = 1.7)

par(mai = c(0, 0, 0, 0))
plot.new()
legend(x = "center", ncol = 6,legend = legend, fill = qcolors, bty = 'n', cex = 1.7)
dev.off()



size_p <- read.csv('size Policy.csv')
size_v <- read.csv('size Value.csv')
size_q <- read.csv('size Q-learning L0.1 E0.5.csv')

jpeg('Size Reward.jpg', height = 360)
size_p %$% plot(shape, reward, type = 'o', col = 'blue',
                main = 'Empty Grid of Size N x N\nMean Empirical Reward - 50 Trials',
                xlab = 'N',
                ylab = 'Reward')
size_v %$% lines(shape, reward, type = 'o', col = 'red')
size_q %$% lines(shape, reward, type = 'o', col = 'green')
legend('topright', 
       legend = c('Policy Iteration', 'Value Iteration', 'Q-Learning'), 
       pch = 1,
       col = c('blue', 'red', 'green'))
dev.off()


jpeg('Size Time.jpg', height = 360)
size_p %$% plot(shape, time, type = 'o', col = 'blue',
                main = 'Empty Grid of Size N x N\nTime to Convergence',
                xlab = 'N',
                ylab = 'Time')
size_v %$% lines(shape, time, type = 'o', col = 'red')
size_q %$% lines(shape, time, type = 'o', col = 'green')
legend('topleft', 
       legend = c('Policy Iteration', 'Value Iteration', 'Q-Learning'), 
       pch = 1,
       col = c('blue', 'red', 'green'))
dev.off()

avgLast <- function(val, var = 'reward'){
  data <- summarizeVariable(easyQFiles, var) %>% filter(file %in% easyQFiles[grep(val, easyQFiles)])
  #print(data)
  mean(as.numeric(data$last))
}

avgLastHard <- function(val, var = 'reward'){
  data <- summarizeVariable(hardQFiles, var) %>% filter(file %in% hardQFiles[grep(val, hardQFiles)])
  #print(data)
  mean(as.numeric(data$last))
}

avgList <- c(paste0('L', ls), paste0('E', es), paste0('q', qs))

for(i in avgList){
  print(paste0(i, ": ", avgLast(i)))
}

for(i in avgList){
  print(paste0(i, ": ", avgLastHard(i)))
}
