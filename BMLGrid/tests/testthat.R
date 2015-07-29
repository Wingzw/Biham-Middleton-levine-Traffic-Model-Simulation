

test.createBMLGrid = function(){
  
  #test if row number < 0
  checkException(createBMLGrid(r = -1, c = 4, 0.3))
  
  #test if density<0
  checkException(createBMLGrid(10, 10 , -0.5 ))
  
  #test if number of cars larger than grid
  checkException(createBMLGrid(10, 10, c(red = 50, blue = 51)))
  
  checkEquals(nrow(createBMLGrid(10,9,0.3)), 10)
  
  checkEquals(sum(createBMLGrid(10,9,c(red = 10, blue = 11)) > 0), 21)
  
}

test.cmoveCars = function(){
  
  #case 1
  g = matrix(as.integer(c(0, 1, 0,2,1,0)),2,3)
  class(g) = c('BML', class(g))
  
  redg = matrix(as.integer(c(1, 1, 0,2,0,0)),2,3)
  class(redg) = c('BML', class(redg))
  
  blueg = matrix(as.integer(c(0, 1, 2,0,1,0)),2,3)
  class(blueg) = c('BML', class(blueg))
  
  #check move red cars
  checkEquals(cmoveCars(g,1)$grid, redg)
  checkEquals(cmoveCars(g,2)$grid, blueg)
  
  
  #case 2: only one kind of car
  g = matrix(as.integer(c(1, 1, 0,0,1,0)),2,3)
  class(g) = c('BML', class(g))
  
  redg = matrix(as.integer(c(0, 0, 1,1,1,0)),2,3)
  class(redg) = c('BML', class(redg))
  
  blueg = matrix(as.integer(c(1, 1, 0,0,1,0)),2,3)
  class(blueg) = c('BML', class(blueg))
  
  #check move red cars
  checkEquals(cmoveCars(g,1)$grid, redg)
  checkEquals(cmoveCars(g,2)$grid, blueg)
  
  
  #case 3: only one row
  g = matrix(as.integer(c(0, 1, 2,0,1,0)),1,6)
  class(g) = c('BML', class(g))
  
  redg = matrix(as.integer(c(0, 1, 2,0,0,1)),1,6)
  class(redg) = c('BML', class(redg))
  
  blueg = matrix(as.integer(c(0, 1, 2, 0, 1, 0)),1,6)
  class(blueg) = c('BML', class(blueg))
  
  #check move red cars
  checkEquals(cmoveCars(g,1)$grid, redg)
  checkEquals(cmoveCars(g,2)$grid, blueg) 
}


test.cmoveCars2 = function(){
  
  #case 1
  g = matrix(as.integer(c(0, 1, 0,2,1,0)),2,3)
  class(g) = c('BML', class(g))
  
  redg = matrix(as.integer(c(1, 1, 0,2,0,0)),2,3)
  class(redg) = c('BML', class(redg))
  
  blueg = matrix(as.integer(c(0, 1, 2,0,1,0)),2,3)
  class(blueg) = c('BML', class(blueg))
  
  #check move red cars
  checkEquals(cmoveCars2(g,1)$grid, redg)
  checkEquals(cmoveCars2(g,2)$grid, blueg)
  
  
  #case 2: only one kind of car
  g = matrix(as.integer(c(1, 1, 0,0,1,0)),2,3)
  class(g) = c('BML', class(g))
  
  redg = matrix(as.integer(c(0, 0, 1,1,1,0)),2,3)
  class(redg) = c('BML', class(redg))
  
  blueg = matrix(as.integer(c(1, 1, 0,0,1,0)),2,3)
  class(blueg) = c('BML', class(blueg))
  
  #check move red cars
  checkEquals(cmoveCars2(g,1)$grid, redg)
  checkEquals(cmoveCars2(g,2)$grid, blueg)
  
  
  #case 3: only one row
  g = matrix(as.integer(c(0, 1, 2,0,1,0)),1,6)
  class(g) = c('BML', class(g))
  
  redg = matrix(as.integer(c(0, 1, 2,0,0,1)),1,6)
  class(redg) = c('BML', class(redg))
  
  blueg = matrix(as.integer(c(0, 1, 2, 0, 1, 0)),1,6)
  class(blueg) = c('BML', class(blueg))
  
  #check move red cars
  checkEquals(cmoveCars2(g,1)$grid, redg)
  checkEquals(cmoveCars2(g,2)$grid, blueg) 
}



test.moveCars = function(){
  
  #case 1
  g = matrix(as.integer(c(0, 1, 0,2,1,0)),2,3)
  class(g) = c('BML', class(g))
  
  redg = matrix(as.integer(c(1, 1, 0,2,0,0)),2,3)
  class(redg) = c('BML', class(redg))
  
  blueg = matrix(as.integer(c(0, 1, 2,0,1,0)),2,3)
  class(blueg) = c('BML', class(blueg))
  
  #check move red cars
  checkEquals(moveCars(g,1)$grid, redg)
  checkEquals(moveCars(g,2)$grid, blueg)
  
  
  #case 2: only one kind of car
  g = matrix(as.integer(c(1, 1, 0,0,1,0)),2,3)
  class(g) = c('BML', class(g))
  
  redg = matrix(as.integer(c(0, 0, 1,1,1,0)),2,3)
  class(redg) = c('BML', class(redg))
  
  blueg = matrix(as.integer(c(1, 1, 0,0,1,0)),2,3)
  class(blueg) = c('BML', class(blueg))
  
  #check move red cars
  checkEquals(moveCars(g,1)$grid, redg)
  checkEquals(moveCars(g,2)$grid, blueg)
  
  
  #case 3: only one row
  g = matrix(as.integer(c(0, 1, 2,0,1,0)),1,6)
  class(g) = c('BML', class(g))
  
  redg = matrix(as.integer(c(0, 1, 2,0,0,1)),1,6)
  class(redg) = c('BML', class(redg))
  
  blueg = matrix(as.integer(c(0, 1, 2, 0, 1, 0)),1,6)
  class(blueg) = c('BML', class(blueg))
  
  #check move red cars
  checkEquals(moveCars(g,1)$grid, redg)
  checkEquals(moveCars(g,2)$grid, blueg) 
}

test.crunBMLGrid = function(){
  
  #case 1: no car is blocked in the end
  g = createBMLGrid(10, 10, 0.3)
  checkEquals(crunBMLGrid(g,1000), runBMLGrid(g, 1000))
  checkEquals(crunBMLGrid(g,1000), crunBMLGrid2(g, 1000))
  
  #case 2: all cars are blocked in the end
  g = createBMLGrid(100, 100, 0.5)
  checkEquals(crunBMLGrid(g,1000), runBMLGrid(g, 1000))
  checkEquals(crunBMLGrid(g,1000), crunBMLGrid2(g, 1000))
  
  #case 3: only one kind of car
  g = createBMLGrid(100, 100, c(red = 500, blue = 0))
  checkEquals(crunBMLGrid(g,1000), runBMLGrid(g, 1000))
  checkEquals(crunBMLGrid(g,1000), crunBMLGrid2(g, 1000))
  
  #case 4: only one column
  g = createBMLGrid(100, 1, c(red = 20, blue = 20))
  checkEquals(crunBMLGrid(g,1000), runBMLGrid(g, 1000))
  checkEquals(crunBMLGrid(g,1000), crunBMLGrid2(g, 1000))
  
  #case 5:
  g = matrix(as.integer(c(0, 1, 0,2,1,0)),2,3)
  class(g) = c('BML', class(g))
  
  g2 = matrix(as.integer(c(1,0,2,1,0,0)),2,3)
  class(g2) = c('BML', class(g2))
  
  #check move red cars
  checkEquals(crunBMLGrid(g,10)$grid, g2)
}




test.crunBMLGrid2 = function(){
  
  #case 1: no car is blocked in the end
  g = createBMLGrid(10, 10, 0.3)
  checkEquals(crunBMLGrid2(g,1000), runBMLGrid(g, 1000))
  checkEquals(crunBMLGrid2(g,1000), crunBMLGrid(g, 1000))
  
  #case 2: all cars are blocked in the end
  g = createBMLGrid(100, 100, 0.5)
  checkEquals(crunBMLGrid2(g,1000), runBMLGrid(g, 1000))
  checkEquals(crunBMLGrid2(g,1000), crunBMLGrid(g, 1000))
  
  #case 3: only one kind of car
  g = createBMLGrid(100, 100, c(red = 500, blue = 0))
  checkEquals(crunBMLGrid2(g,1000), runBMLGrid(g, 1000))
  checkEquals(crunBMLGrid2(g,1000), crunBMLGrid(g, 1000))
  
  #case 4: only one column
  g = createBMLGrid(100, 1, c(red = 20, blue = 20))
  checkEquals(crunBMLGrid2(g,1000), runBMLGrid(g, 1000))
  checkEquals(crunBMLGrid2(g,1000), crunBMLGrid(g, 1000))
  
  #case 5:
  g = matrix(as.integer(c(0, 1, 0,2,1,0)),2,3)
  class(g) = c('BML', class(g))
  
  g2 = matrix(as.integer(c(1,0,2,1,0,0)),2,3)
  class(g2) = c('BML', class(g2))
  
  #check move red cars
  checkEquals(crunBMLGrid2(g,2)$grid, g2)
}


test.runBMLGrid = function(){
  
  #case 1: no car is blocked in the end
  g = createBMLGrid(10, 10, 0.3)
  checkEquals(runBMLGrid(g,1000), crunBMLGrid(g, 1000))
  checkEquals(runBMLGrid(g,1000), crunBMLGrid2(g, 1000))
  
  #case 2: all cars are blocked in the end
  g = createBMLGrid(100, 100, 0.5)
  checkEquals(runBMLGrid(g,1000), crunBMLGrid(g, 1000))
  checkEquals(runBMLGrid(g,1000), crunBMLGrid2(g, 1000))
  
  #case 3: only one kind of car
  g = createBMLGrid(100, 100, c(red = 500, blue = 0))
  checkEquals(runBMLGrid(g,1000), crunBMLGrid(g, 1000))
  checkEquals(runBMLGrid(g,1000), crunBMLGrid2(g, 1000))
  
  #case 4: only one column
  g = createBMLGrid(100, 1, c(red = 20, blue = 20))
  checkEquals(runBMLGrid(g,1000), crunBMLGrid(g, 1000))
  checkEquals(runBMLGrid(g,1000), crunBMLGrid2(g, 1000))
  
  #case 5:
  g = matrix(as.integer(c(0, 1, 0,2,1,0)),2,3)
  class(g) = c('BML', class(g))
  
  g2 = matrix(as.integer(c(1,0,2,1,0,0)),2,3)
  class(g2) = c('BML', class(g2))
  
  #check move red cars
  checkEquals(runBMLGrid(g,2)$grid, g2)
}

