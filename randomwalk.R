createBMLGrid = function(r = 100, c = 99, ncars = c(red = 2000, blue = 2000)){
  
  if(length(ncars) == 1 && ncars < 1){
    ncars = rep(ceiling(r * c * ncars/2), 2)
    names(ncars) = c('red','blue')
  }
  if(sum(ncars) < 1){
    stop("No cars, no need to simulate")
  }
  if(sum(ncars) > r*c){
      stop("The number of cars is larger than grid")
  }
  
  if(ncars['red'] < 0 | ncars['blue'] < 0){
    stop("There is one kind of car which number <0")
  }
  
  grid = matrix(as.integer(0), r, c)
  
  #red 1 blue 2
  pos = sample(1:(r*c), sum(ncars))
  grid[pos] = sample(rep(c(as.integer(1), as.integer(2)), 
                         ceiling(ncars)))[seq(along = pos)]
  class(grid) = c("BML", class(grid))
  return(grid)
}


plot.BML = function(x,...){
  if(max(x) == 1){
    image(t(x),col=c("white","red"), axes = FALSE,...)
  }else{
    image(t(x),col=c("white","red","blue"), axes = FALSE, ...)
  }
  
}

cmoveCars = function(g, color = 1){
  nr = nrow(g)
  nc = ncol(g)
  v = 0
  out = .C("move",nr = as.integer(nr), nc = as.integer(nc),g = g, col = as.integer(color),v = as.numeric(v))
  return(list(grid = out$g, velocity = out$v))
}

cmoveCars2 = function(g, color = 1){
  nr = nrow(g)
  nc = ncol(g)
  v = 0
  out = .C("move2",nr = as.integer(nr), nc = as.integer(nc),g = g, col = as.integer(color),v = as.numeric(v))
  return(list(grid = out$g, velocity = out$v))
}


crunBMLGrid = function(g, numSteps = 10000){
  if(numSteps > 0){
    nr = nrow(g)
    nc = ncol(g)
    v = rep(0, numSteps)
    out = .C("runSteps", numSteps = as.integer(numSteps), nr = as.integer(nr), nc = as.integer(nc),g = g, v = as.numeric(v))
    return(list(grid = out$g, velocity = out$v))
  }else{
    return(list(grid = g, velocity = 0))
  } 
}

crunBMLGrid2 = function(g, numSteps = 10000){
  if(numSteps > 0){
    nr = nrow(g)
    nc = ncol(g)
    v = rep(0, numSteps)
    out = .C("runSteps2", numSteps = as.integer(numSteps), nr = as.integer(nr), nc = as.integer(nc),g = g, v = as.numeric(v))
    return(list(grid = out$g, velocity = out$v))
  }else{
    return(list(grid = g, velocity = 0))
  } 
}


moveCars = function(g, color = 1){
    if(color == 1){
        # move red cars right
        numCar = sum(g == 1)
        if(ncol(g) > 1){
            temp = g[, c(ncol(g), 1:(ncol(g) - 1))]
            move = (temp == 1 & g == 0)
            numMove = sum(move)
            if(numMove > 0){
                g[move] = 1
                delete = move[, c(2:ncol(g), 1)]
                g[delete] = 0
                velocity = numMove / numCar
            }else{
                velocity = 0
            }
            
        }else{
            velocity = 0
        }
    }else{
        #move blue cars upward
        numCar = sum(g == 2)
        if(nrow(g) > 1){
            temp = g[c(nrow(g), 1:(nrow(g) - 1)),]
            move = (temp == 2 & g == 0)
            numMove = sum(move)
            if(numMove > 0){
                g[move] = 2
                delete = move[c(2:nrow(g), 1),]
                g[delete] = 0
                velocity = numMove / numCar
            }else{
                velocity = 0
            }
            
        }else{
            velocity = 0
        }
    } 
    
    return(list(grid = g, velocity = velocity))
    
}

runBMLGrid = function(g, numSteps = 10000){
  if(numSteps > 0){
    velocity = rep(0, numSteps)
    for (i in 1:numSteps){
      tmp = moveCars(g, i%%2 + 1)
      g = tmp$grid
      velocity[i] = tmp$velocity
    }
    return(list(grid = g, velocity = velocity))
  }
  return(list(grid = g, velocity = 0))
  
}


summary.BML = function(object,...){
  r = nrow(object)
  col = ncol(object)
  nred = sum(object == 1)
  nblue = sum(object == 2)
  l1 = paste(" Number of rows:", r,'\n')
  l2 = paste("Number of columns:", col,'\n')
  l3 = paste("Number of red cars:",nred,'\n')
  l4 = paste("Number of blue cars:",nblue,'\n')
  dens = (nred + nblue) / (r * col)
  l5 = paste("Density of cars:", dens,'\n')
  redbml = cmoveCars(object,color = 1)
  bluebml = cmoveCars(object, color = 2)
  l6 = paste("If red Cars will move, the number of cars will be blocked:", 
             nred- round(nred * redbml$velocity), ";\n \t the number of cars can move:",
             round(nred * redbml$velocity), ";\n \t the velocity will be:", 
             redbml$velocity,'\n')
  l7 = paste("If blue Cars will move, the number of cars will be blocked:", 
             nblue - round(nblue * bluebml$velocity), ";\n \t the number of cars can move:",
             round(nblue * bluebml$velocity), ";\n \t the velocity will be:", 
             bluebml$velocity)
  cat(c(l1, l2, l3, l4, l5, l6, l7))
}




