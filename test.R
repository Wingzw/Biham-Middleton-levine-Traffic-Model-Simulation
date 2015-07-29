out = .C("move",r = 5L, c = 4L,g = matrix(as.integer(g),5,4), col = as.integer(2),v = as.numeric(5))

require(ggplot2)
require(reshape)
library(animation)
saveGIF(for(i in 1:100) {
  z = moveCars(g,i%%2 + 1)
  g = z$grid
  plot(g)
})



u = createBMLGrid(100,100,c(red = 1500, blue = 1500))
Rprof('first.out')
z = runBMLGrid(u, 10000)
Rprof(NULL)
summaryRprof('first.out')

Rprof('second.out')
z = crunBMLGrid(u, 10000)
Rprof(NULL)
summaryRprof('second.out')

Rprof('third.out')
z = crunBMLGrid2(u, 10000)
Rprof(NULL)
summaryRprof('third.out')



density = seq(0.1,0.8,by = 0.1)
size = c(10,20,50,100,200,500)
nruns = 10000
v1 = matrix(rep(0,6*8),6,8)
v2 = matrix(rep(0,48),6, 8)
v3 = matrix(rep(0,48),6, 8)
for(i in 1:length(size)){
  for(j in 1:length(density)){
    g = createBMLGrid(size[i],size[i],density[j])
    v1[i,j] = unname(system.time(runBMLGrid(g, 5000))[1])
    v2[i,j] = unname(system.time(crunBMLGrid(g, 5000))[1])
    v3[i,j] = unname(system.time(crunBMLGrid2(g, 5000))[1])
  }
}

tmpv1 = as.data.frame(t(v1))
names(tmpv1) = size
tmpv1$density = density
tmpv1$method = 'R'
tmpv2 = as.data.frame(t(v2))
names(tmpv2) = size
tmpv2$density = density
tmpv2$method = 'C1'
tmpv3 = as.data.frame(t(v3))
names(tmpv3) = size
tmpv3$density = density
tmpv3$method = 'C2'
v = rbind(tmpv1,tmpv2)
v = rbind(v, tmpv3)
v = melt(v, id = c('density','method'), variable_name = "Size")
v$x = paste("method:", v$method,",Size:",v$Size)
v$density_method = paste("Density:", v$density, ",Method:",v$method)


#save(v, file = 'ratio.rda')
a = as.integer(as.character(v$Size))
ggplot(v[v$density =='0.3' & v$method %in% c('C1','C2'),], 
       aes(a[v$density == '0.3' & v$method %in% c('C1','C2')],value))  + 
  geom_line(aes(colour = method)) + 
  xlab("Number of rows(square grid)") + ylab("User time(in seconds)") + 
  ggtitle("Running time with different grid size(density = 0.3, nrunSteps = 10000)")


qplot(x=density, y=value, fill=method, data=v[v$Size == '200',], geom="bar", 
      stat="identity", xlab = "Density", ylab = "User Time(seconds)",
      position="dodge", main = "Running time with different density(Grid Size: 200*200)")

save(v1,v2,v3,v,file = 'test2.rda')


p = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6,0.7, 0.8, 0.9)
res = data.frame(time = rep(0,27), method = rep(c("R","C1","C2"),each = 9), ratio = rep(p, 3))
res_average = replicate(5, list())
for(j in 1:5){
for(i in p){
  tmp = c(3000*i, 3000*(1 - i))
  names(tmp) = c('red', 'blue')
  g = createBMLGrid(100,100,tmp)
  res[res$method == 'R' & res$ratio == i, ]$time = unname(system.time(runBMLGrid(g, 1000))[1])
  res[res$method == 'C1' & res$ratio == i, ]$time = unname(system.time(crunBMLGrid(g, 1000))[1])
  res[res$method == 'C2' & res$ratio == i, ]$time = unname(system.time(crunBMLGrid2(g, 1000))[1])
}
   res_average[[j]] = res
}

res$time = (res_average[[1]][,1] + res_average[[2]][,1] + res_average[[3]][,1] + res_average[[4]][,1] +
  res_average[[5]][,1] ) / 5




runBML = function(g, numSteps = 10000){
  if(numSteps > 0){
    velocity = rep(0, numSteps)
    for (i in 1:numSteps){
      tmp = cmoveCars(g, i%%2 + 1)
      g = tmp$grid
      velocity[i] = tmp$velocity
    }
    return(list(grid = g, velocity = velocity))
  }
  return(list(grid = g, velocity = 0))
  
}


density = seq(0.1,0.8,by = 0.1)
size = c(10,20,50,100,200,500)
nruns = 1000
v1 = matrix(rep(0,6*8),6,8)
v2 = matrix(rep(0,48),6, 8)
for(i in 1:length(size)){
  for(j in 1:length(density)){
    g = createBMLGrid(size[i],size[i],density[j])
    v1[i,j] = unname(system.time(runBML(g, nruns))[1])
    v2[i,j] = unname(system.time(crunBMLGrid(g, nruns))[1])
  }
}

tmpv1 = as.data.frame(t(v1))
names(tmpv1) = size
tmpv1$density = density
tmpv1$method = 'R'
tmpv2 = as.data.frame(t(v2))
names(tmpv2) = size
tmpv2$density = density
tmpv2$method = 'C'
v = rbind(tmpv1,tmpv2)
v = melt(v, id = c('density','method'), variable_name = "Size")
v$method_size = paste("method:", v$method,",Size:",v$Size)
v$density_method = paste("Density:", v$density, ",Method:",v$method)

ggplot(v[v$Size %in% c("100", "200","500"),], 
       aes(density,value))  + 
  geom_line(aes(colour = x)) 
  xlab("density") + ylab("User time(in seconds)") + 
  ggtitle("Running time of different methods(Square grid)")
