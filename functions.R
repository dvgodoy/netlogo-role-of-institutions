calc.prob <- function(base, wrong.prob) {
  # base = base probabilities of each node. -1 indicates "wrong" nodes
  # wrong.prob = chance of any of the "wrong" nodes being selected
  
  size <- dim(base)[1]
  total.w <- colSums(base * (base > 0))
  total.prob <- t(t((base > 0)*base)/total.w)
  total.prob[!is.finite(total.prob)] <- 0
  total.prob[base == -1] <- -1
  total.wrong <- wrong.prob / colSums(total.prob == -1)
  total.wrong[!is.finite(total.wrong)] <- 0
  total.wrong <- t(t(total.prob == -1) * total.wrong)
  total.prob[total.prob == -1] <- 0
  total.prob <- (1 - wrong.prob) * total.prob + total.wrong
  total.prob.orig <- total.prob
  total.prob <- (total.prob > 0) * t(t(matrix(cumsum(total.prob * (total.prob > 0)), nrow=size)) - c(0, cumsum(colSums(total.prob * (total.prob > 0)))[1:(size - 1)]))
  # probabilities of zero are set to -1 so they don't get selected
  total.prob[total.prob.orig == 0] <- -1
  
  return(total.prob)
}

calc.dest <- function(prob, chance=NULL) {
  # prob = matrix of probabilities along the columns
  # chance = random chance to determine which node will be selected
  #          if NULL, the function will draw chance from an uniform distribution
  size <- dim(prob)[1]
  if (is.null(chance)) {
    chance <- runif(1:size)
  }   
  # select a target agent in order to move into his direction
  total.dest <- t(chance < t(prob))
  total.dest <- (t(t(matrix(cumsum(total.dest), ncol=size)) - c(0, cumsum(colSums(total.dest))[1:(size-1)])) * total.dest == 1) * 1
  return(total.dest)  
}

calc.torus.dist <- function(x, y, torus.size, normalize=TRUE) {
  size = length(x)
  dist.x <- as.matrix(dist(cbind(x,rep(1,size))))
  dist.y <- as.matrix(dist(cbind(y,rep(1,size))))
  
  dist.torus <- sqrt((pmin(dist.x, torus.size + 1 - dist.x))^2+(pmin(dist.y, torus.size + 1 - dist.y))^2)
  if (normalize) {
    dist.torus <- dist.torus * sqrt(2) / torus.size
  }
  return(dist.torus)
}

calc.dist <- function(x, y, lattice.size, wrap=TRUE, normalize=TRUE) {
  if (wrap) {
    dist.geo <- calc.torus.dist(x, y, lattice.size, normalize)
  } else {
    dist.geo <- as.matrix(dist(cbind(x,y)))   
  }
  return (dist.geo)
}

find.geo.neigh <- function (x, y, lattice.size, wrap=TRUE, radius, neigh.type='Von Neumann') {
  # lattice.size <- 10
  # radius <- 1
  # x<-c(0,1,0,1,0,10,10,3,3,5)
  # y<-c(0,0,1,1,10,10,0,0,10,5)
  if (neigh.type != 'Von Neumann') {
    radius <- radius * sqrt(2) + 0.01
  }
  geo.neigh <- (calc.dist(x, y, lattice.size, wrap=TRUE, FALSE) <= radius)
  geo.neigh <- geo.neigh - diag(1,length(x))  
  return(geo.neigh)
}

calc.satisf <- function(edges, obs, defector) {
  pop.size <- dim(edges)[1]
  # calculates agents' degrees
  degrees <- colSums(edges)  
  # determines observed edges
  observed.edges <- ((edges - obs) == 0) * edges
  # satisfaction matrix calculation
  # calculates if a link is neutral (0), not good (-1) or bad (-2) to an agent from the point of view of an agent in a column
  # 0/0 = (C/C), -1/-1 = (D/D), -2/1 = (C/D)
  # for instance, if 1 cooperates and 2 defects [2,1] = -2 and [1,2] = 1
  satisf.matrix <- matrix(rep(defector,pop.size),ncol=pop.size,byrow=TRUE) + 2*matrix(rep(-defector,pop.size),ncol=pop.size,byrow=FALSE)
  # unobserved edges (UE) are 0
  satisf.matrix <- satisf.matrix * observed.edges
  # accumulates both payoffs to determine edge weigth
  # (D-UE/C) = 0/-2 = -2
  # (D/D) = -1/-1 = -2
  # (D/C) = 1/-2 = -1
  satisf.links <- satisf.matrix + t(satisf.matrix)
  return(satisf.links)
}

social.adjust <- function(edges, obs, defector, rewire.prob=1.0) {
  pop.size <- dim(edges)[1]
  # calculates agents' degrees
  degrees <- colSums(edges)
  satisf.links <- calc.satisf(edges, obs, defector)
  
  insatisf <- (satisf.links < 0) * satisf.links
  
  bad.links <- t(t(insatisf) * (1 - colSums(insatisf == -2)))
  prob.bad.links <- t(t(bad.links) / colSums(bad.links))
  prob.bad.links[!is.finite(prob.bad.links)] <- 0
  awful.links <- (insatisf == -2) * insatisf
  destroy.dest <- calc.dest(calc.prob(abs(bad.links),0),NULL) + abs(awful.links)
  
  replace.links <- matrix(0, pop.size, pop.size)
  for (i in 1:pop.size) {
    pivot.node <- c(1:pop.size)[destroy.dest[,i] > 0]
    if (length(pivot.node) > 0) {
      if (length(pivot.node) > 1) {
         replace.links[,i] <- (rowSums(edges[,pivot.node]) > 0)
       } else {
         replace.links[,i] <- edges[,pivot.node]
       }
      replace.links[i,i] <- 0
    }
  }
  replace.links <- ((replace.links - edges) > 0) * degrees
  replace.dest <- calc.dest(rewire.prob * calc.prob(replace.links,0),NULL)    
  
  return(destroy.dest * (-1) + replace.dest)
}

social.geo <- function(edges, obs, defector, basic.prob=1.0) {
  # calculates agents' degrees
  degrees <- colSums(edges)
  satisf.links <- calc.satisf(edges, obs, defector)
  satisf <- (((satisf.links >= 0) - edges) > 0) * degrees
  create.dest <- calc.dest(basic.prob * calc.prob(satisf,0),NULL)
  return(create.dest)
}

social.tryad <- function(edges, basic.prob=1.0) {
  pop.size <- dim(edges)[1]
  
  # calculates agents' degrees
  degrees <- colSums(edges)
  
  # finds candidates for tryadic closure by obtaining edges to neighbors' neighbors
  # determines tryadic closure candidates
  tryad.cand <- ((edges %*% edges + edges) > 0) - diag(rep(1,pop.size)) - edges
  # prob weighted by degrees
  tryad.prob <- tryad.cand * degrees
  tryad.dest <- calc.dest(basic.prob * calc.prob(tryad.prob,0),NULL)
  return(tryad.dest)
}

cultural.sigmoid <- function(boldness, vengefulness, attr.norm.factor=1.0, sel.str, base) {
  pop.size <- length(boldness)
  # calculates cultural distance
  dist <- as.matrix(dist(cbind(boldness,vengefulness), diag=TRUE, upper=TRUE) * attr.norm.factor / sqrt(2))
  
  # probability of imitation is given by Fermi-Dirac distribution and selection strength
  imit.prob <- 1 / (1 + exp(sel.str * (dist - 0.5)))
  imit.chance <- matrix(runif(1:pop.size^2), ncol=pop.size)
  dest <- base[1:pop.size,1:pop.size] * (imit.chance < imit.prob)
  return(dest)
}

social.adjust.sigmoid <- function(edges, obs, defector, rewire.prob, boldness, vengefulness, attr.norm.factor, sel.str) {
  sel.imit <- abs(sel.str)
  sel.rej <- -sel.imit
  links <- social.adjust(edges, obs, defector, rewire.prob)
  destroy.dest <- -(cultural.sigmoid(boldness,vengefulness,attr.norm.factor,sel.rej,(links == -1)) + (links == -2))
  create.dest <- cultural.sigmoid(boldness,vengefulness,attr.norm.factor,sel.imit,(links == 1))
  return(destroy.dest + create.dest)
}

social.tryad.sigmoid <- function(edges, basic.prob, boldness, vengefulness, attr.norm.factor, sel.str) {
  sel.imit <- abs(sel.str)
  links <- social.tryad(edges, basic.prob)
  create.dest <- cultural.sigmoid(boldness,vengefulness,attr.norm.factor,sel.imit,links)
  return(create.dest)
}

social.geo.sigmoid <-  function(geo.neigh, obs, defector, basic.prob, boldness, vengefulness, attr.norm.factor, sel.str) {
  sel.imit <- abs(sel.str)
  links <- social.geo(geo.neigh, obs, defector, basic.prob)
  create.dest <- cultural.sigmoid(boldness,vengefulness,attr.norm.factor,sel.imit,links)
  return(create.dest)  
}

make.obs <- function(chance.being.seen=NULL, edges) {
  size <- dim(edges)[1]
  # draw chances of being seen
  if (is.null(chance.being.seen)) {
    chance.being.seen <- runif(1:size)
  } 
  # draw chances of agents seeing one another... A may see B without being seen by B
  chance.seeing <- matrix(runif(1:size^2),ncol=size,nrow=size)
  # it is only possible to see another agent if there is an edge linking them
  possible.seeing <- chance.seeing * edges  
  possible.seeing <- (possible.seeing == 0) * 1 + possible.seeing
  # actual observations happen here
  obs <- chance.being.seen > possible.seeing
  result <- list(being.seen = chance.being.seen, obs.edges = obs)
  return(result)
}

define.roles <- function(boldness, vengefulness, attr.norm.factor=1, chance.being.seen=NULL) {
  # boldness = vector of agents' attribute
  # vengefulness = vector of agents' attribute
  # attr.norm.factor = factor for normalizing attributes to 1

  size <- length(boldness)
  # normalize attributes
  boldness <- boldness * attr.norm.factor
  vengefulness <- vengefulness * attr.norm.factor
  # draw chances of being seen and of punishing other
  chance.punishing <- runif(1:size)
  if (is.null(chance.being.seen)) {
    chance.being.seen <- runif(1:size)
  }
  # bold agents will defect
  defection <- boldness > chance.being.seen
  punishment <- vengefulness > chance.punishing
  result <- list(defection = defection, punishment = punishment, being.seen = chance.being.seen)
  return(result)
}

make.actions <- function(defection, punishment, edges, observations=NULL, obs.orig.def=TRUE) {
  # edges = edges of the graph
  # obs.orig.def = TRUE if punishers must observe original defection in order to punish the metadefector
  
  # defections will occur along the lines and punishments along the columns
  size <- dim(edges)[1]
  obs <- observations$obs.edges
  if (is.null(obs)) {
    # draw chances of agents seeing one another... A may see B without being seen by B
    chance.seeing <- matrix(runif(1:size^2),ncol=size,nrow=size)
    # it is only possible to see another agent if there is an edge linking them
    possible.seeing <- chance.seeing * edges  
    possible.seeing <- (possible.seeing == 0) * 1 + possible.seeing
    # actual observations happen here
    obs <- chance.being.seen > possible.seeing
  }
  # set defection along lines
  defectors <- matrix(rep(defection,size),ncol=size,nrow=size)
  # vengeful agents will be punishers - set along columns
  punishers <- matrix(rep(punishment, size),ncol=size,nrow=size,byrow=TRUE)
  # if there is a defector and a punisher and an edge linking them, a punishment will occur
  punishments <- punishers * obs * defectors
  # not punishers are potential metadefectors
  not.punishers <- (punishers == 0)
  # determine agents who observed defections and have done nothing about it
  meta.defectors <- t(not.punishers * obs * defectors)
  # transpose and replicate metadefectors to get them along the lines
  meta.punishments <- matrix(rep(as.matrix(rowSums(meta.defectors)),size),nrow=size,ncol=size) * obs * punishers
  # ... but if the original defection must be observed, multiply matrices and do not replicate it
  meta.punishments.obs <- (meta.defectors %*% (obs * punishers)) * obs
  # total of punishments an agent enforced on defectors
  punished <- colSums(punishments)
  # total of punishments an agent suffered for defecting
  was.punished <- rowSums(punishments)
  
  if (obs.orig.def) {
    # total of punishments an agent enforced on metadefectors
    meta.punished <- colSums(meta.punishments.obs)
    # total of punishments an agent suffered for metadefecting
    was.meta.punished <- rowSums(meta.punishments.obs)
  } else {
    # total of punishments an agent enforced on metadefectors
    meta.punished <- colSums(meta.punishments)
    # total of punishments an agent suffered for metadefecting
    was.meta.punished <- rowSums(meta.punishments)    
  }
  result <- data.frame(cbind(def = defectors[,1], pun = punishers[1,], pnd = punished, waspnd = was.punished, metapnd = meta.punished, wasmetapnd = was.meta.punished))
  return(result)  
}

calc.payoffs <- function(roles, T, E, P, ME, MP) {
  return(roles$def * T + roles$pnd * E + roles$waspnd * P + roles$metapnd * ME + roles$wasmetapnd * MP)
}

imitation <- function(attrs, edges, geo.dist, nodes.w, nodes.w.margin=TRUE, inst.attr, inst.w, inst.w.min=0.0, const.inst.w=FALSE, dist.pow=1, attr.norm.factor=1, attr.chg.step=1, wrong.prob=0.0, sel.str=0.0, sel.str.inst=0.0, mutation.rate=0.0) {
  # attrs = data frame where each column represents a different attribute
  # edges = edges of the graph
  # geo.dist = matrix of geographic distances
  # nodes.w = weights of the nodes of the graph (payoffs)
  # nodes.w.margin = FALSE will consider the full node's weight for probability assignment
  #                  TRUE will consider only node's marginal weight for probability assignment
  # inst.attr = attributes of the institutional arrangement
  #             set it to NULL if you want institutional arrangement to be the mean of the population attrs
  # inst.w = institutional weight
  # inst.w.min = minimal institutional weight to be assigned if an agent is at maximum cultural distance
  # const.inst.w = if TRUE it will assume inst.w constant for every agent
  #                otherwise it will decrease it linearly proportionally to the distance between the agent and the
  #                institutional arrangement in the cultural space
  # dist.pow = weights 1 / dist ^ power
  # attr.norm.factor = factor for normalizing attributes to 1
  # attr.chg.step = if an agent moves towards or away from another it will increase its attributes by a step
  # wrong.prob = probability of the agent chooses to imitate another one with a lower weight than its own
  # sel.str = selection strength of imitation/rejection
  # sel.str.inst = selection strength of imitation of the institution
  
  # EXAMPLE OF ARGUMENTS
  # initx <- c(1,2,3,4,5,4)
  # inity <- c(1,4,2,1,3,5)
  # attrs <- data.frame(cbind(initx, inity))
  # edges <- matrix(c(0,0,0,1,0,1,
  #                   0,0,1,0,1,0,
  #                   0,1,0,1,0,1,
  #                   1,0,1,0,1,1,
  #                   0,1,0,1,0,0,
  #                   1,0,1,1,0,0),ncol=6,byrow=TRUE)
  # nodes.w <- sqrt((7-initx)^2 + inity^2)/3
  # nodes.w.margin = TRUE
  # inst.attr = c(0,7)
  # inst.w = 0.20
  # inst.w.min = 0.0
  # const.inst.w = TRUE
  # dist.pow = 1
  # attr.norm.factor = 1/7
  # attr.chg.step = 1
  # wrong.prob = 0.01
  # sel.str = 10
  
  pop.size <- dim(edges)[1]
  
  # calculate distances between agents for each attribute
  x <- attrs[[1]]
  y <- attrs[[2]]
  if (is.null(inst.attr)) {
    x[pop.size + 1] <- median(x)
    y[pop.size + 1] <- median(y)
  } else {
    x[pop.size + 1] <- inst.attr[[1]]
    y[pop.size + 1] <- inst.attr[[2]]
  }  
  x.matrix <- matrix(rep(x, pop.size + 1), ncol=pop.size + 1, byrow=TRUE) * attr.norm.factor
  y.matrix <- matrix(rep(y, pop.size + 1), ncol=pop.size + 1, byrow=TRUE) * attr.norm.factor
  x.dist <- x.matrix - t(x.matrix)
  y.dist <- y.matrix - t(y.matrix)
  
  # calculates differences of payoffs between agents and normalizes it
  w.lins <- matrix(rep(nodes.w, pop.size), ncol=pop.size)
  w.cols <- t(w.lins)
  
  # calculates cultural distance
  w.dist <- as.matrix(dist(cbind(x,y), diag=TRUE, upper=TRUE) * attr.norm.factor / sqrt(2))
  
  #######################
  # attractive dynamics #
  #######################
  # considers those neighboring agents with higher payoffs weighted by their geographical distance
  attdyn.w <- (edges * (w.cols < w.lins))
  attdyn.w.margin <- attdyn.w * (w.lins - w.cols)
  if (nodes.w.margin) {
    attdyn.w <- attdyn.w.margin
  }
  attdyn.w <- attdyn.w / geo.dist
  attdyn.w[!is.finite(attdyn.w)] <- 0
  
  # considers the remaining agents - the "wrong" ones since they have lower payoffs
  attdyn.w.wrong <- edges - ((attdyn.w * nodes.w) > 0)
  # make sure they are not linked to themselves
  attdyn.w.wrong <- attdyn.w.wrong * (1 - diag(1, pop.size))
  # calculates probabilities by normalizing it by dividing by the column's sum
  attdyn.prob <- t(t(attdyn.w * nodes.w) / colSums(attdyn.w * nodes.w))
  # removes NaNs
  attdyn.prob[!is.finite(attdyn.prob)] <- 0
  # calculates probabilities of the "wrong" agents to imitate, dividing defined probability by their quantity
  attdyn.prob.wrong <- t(t(attdyn.w.wrong) * wrong.prob / colSums(attdyn.w.wrong))
  # removes NaNs
  attdyn.prob.wrong[!is.finite(attdyn.prob.wrong)] <- 0
  # adjust probabilities of the "right" agents by multiplying it by (1 - wrong.prob)
  attdyn.prob <- t(t(attdyn.prob) * (1 - ((colSums(attdyn.w.wrong) > 0) * wrong.prob)))
  # the final probability of moving towards another agent
  attdyn.prob <- attdyn.prob + attdyn.prob.wrong
  
  # if institutional weight is constant to every agent
  if (const.inst.w) {
    # adjust probabilities of all agents to accomodate institutional weight in the n+1 th line and column
    attdyn.prob <- cbind(rbind(attdyn.prob * (1 - inst.w), rep(inst.w, pop.size)), rep(inst.w, pop.size + 1))
    # otherwise, institutional weight is linearly inversely proportional to cultural distance
  } else {
    # calculates institutional weight vector according to cultural distance
    inst.w.vector <- (1 - (w.dist[pop.size + 1, 1:pop.size]) ^ (1 / dist.pow)) * (inst.w - inst.w.min) + inst.w.min
    # calculates adjustment factor
    inst.factor <- 1 - inst.w.vector
    # adjust probabilities of all agents to accomodate institutional weight
    attdyn.prob <- t(t(attdyn.prob) * inst.factor)
    # adds institutional weight in the n+1 th line and column
    attdyn.prob <- cbind(rbind(attdyn.prob, inst.w.vector), c(inst.w.vector, 0))
  }
  
  attdyn.prob.orig <- attdyn.prob
  # accumulates probabilities so they will sum up to 1.0 in every column where there is at least one "right" agent
  # and up to (wrong.prob + inst.w) otherwise
  attdyn.prob <- (attdyn.prob > 0) * t(t(matrix(cumsum(attdyn.prob), nrow=pop.size + 1)) - c(0, cumsum(colSums(attdyn.prob))[1: pop.size]))
  # probabilities of zero are set to -1 so they don't get selected
  attdyn.prob <- (attdyn.prob.orig == 0) * (-1) + attdyn.prob
  
  # draws a random number
  attdyn.chance <- runif(1:(pop.size + 1))
  # select a target agent in order to move into his direction
  attdyn.dest <- t(attdyn.chance < t(attdyn.prob))
  attdyn.dest <- (t(t(matrix(cumsum(attdyn.dest), ncol=pop.size + 1)) - c(0, cumsum(colSums(attdyn.dest))[1:pop.size])) * attdyn.dest == 1) * 1
  # the institution itself won't move anywhere
  attdyn.dest[,pop.size + 1] <- 0
  
  # probability of imitation is given by Fermi-Dirac distribution and selection strength
  imit.prob <- 1 / (1 + exp(sel.str * (w.dist - 0.5)))
  # different selection strength for institution
  imit.prob[,pop.size+1] <- 1 / (1 + exp(sel.str.inst * (w.dist[,pop.size+1] - 0.5)))
  imit.prob[pop.size+1,] <- t(imit.prob[,pop.size+1])
  imit.chance <- matrix(runif(1:(pop.size + 1)^2), ncol=pop.size + 1)
  attdyn.dest <- attdyn.dest * (imit.chance < imit.prob)
  
  # moves towards target agent trying to reduce the largest distance in the cultural space
  x.mov <- (abs(x.dist) >= abs(y.dist)) * attdyn.dest * sign(-x.dist) * attr.chg.step
  y.mov <- (abs(y.dist) >= abs(x.dist)) * attdyn.dest * sign(-y.dist) * attr.chg.step
  
  x.dest <- colSums(x.mov) + x
  y.dest <- colSums(y.mov) + y
  
  ######################
  # repulsive dynamics #
  ######################
  # considers those neighboring agents with lower payoffs weighted by their geographical distance
  repdyn.w <- (edges * (w.cols > w.lins))
  repdyn.w.margin <- repdyn.w * (w.lins - w.cols) * (-1)
  if (nodes.w.margin) {
    repdyn.w <- repdyn.w.margin
  }
  repdyn.w <- repdyn.w / geo.dist
  repdyn.w[!is.finite(repdyn.w)] <- 0
  
  # considers the remaining agents - the "wrong" ones since they have higher payoffs
  repdyn.w.wrong <- edges - ((repdyn.w * nodes.w) > 0)
  # make sure they are not linked to themselves
  repdyn.w.wrong <- repdyn.w.wrong * (1 - diag(1, pop.size))
  # calculates probabilities by normalizing it by dividing by the column's sum
  repdyn.prob <- t(t(repdyn.w * nodes.w) / colSums(repdyn.w * nodes.w))
  # removes NaNs
  repdyn.prob[!is.finite(repdyn.prob)] <- 0
  # calculates probabilities of the "wrong" agents to avoid imitating, dividing defined probability by their quantity
  repdyn.prob.wrong <- t(t(repdyn.w.wrong) * wrong.prob / colSums(repdyn.w.wrong))
  # removes NaNs
  repdyn.prob.wrong[!is.finite(repdyn.prob.wrong)] <- 0
  # adjust probabilities of the "right" agents by multiplying it by (1 - wrong.prob)
  repdyn.prob <- t(t(repdyn.prob) * (1 - ((colSums(repdyn.w.wrong) > 0) * wrong.prob)))
  # the final probability of moving towards another agent
  repdyn.prob <- repdyn.prob + repdyn.prob.wrong
  
  repdyn.prob.orig <- repdyn.prob
  # accumulates probabilities so they will sum up to 1.0 in every column where there is at least one "right" agent
  # and up to (wrong.prob + inst.w) otherwise
  repdyn.prob <- (repdyn.prob > 0) * t(t(matrix(cumsum(repdyn.prob), nrow=pop.size)) - c(0, cumsum(colSums(repdyn.prob))[1:(pop.size - 1)]))
  # probabilities of zero are set to -1 so they don't get selected
  repdyn.prob <- (repdyn.prob.orig == 0) * (-1) + repdyn.prob
  
  # draws a random number
  repdyn.chance <- runif(1:(pop.size))
  # select a target agent in order to move away from
  repdyn.dest <- t(repdyn.chance < t(repdyn.prob))
  repdyn.dest <- (t(t(matrix(cumsum(repdyn.dest), ncol=pop.size)) - c(0, cumsum(colSums(repdyn.dest))[1:(pop.size - 1)])) * repdyn.dest == 1) * 1
  # there's no institution to move away from
  repdyn.dest <- cbind(rbind(repdyn.dest, rep(0, pop.size)), rep(0, pop.size + 1))
  
  # probability of rejection is given by Fermi-Dirac distribution and selection strength
  rej.prob <- 1 / (1 + exp(-sel.str * (w.dist - 0.5)))
  rej.chance <- matrix(runif(1:(pop.size + 1)^2), ncol=pop.size + 1)
  repdyn.dest <- repdyn.dest * (rej.chance < rej.prob)
  
  # move away from target agent trying to increase the smallest distance in the cultural space
  x.mov <- (abs(x.dist) <= abs(y.dist)) * repdyn.dest * sign(x.dist) * attr.chg.step
  y.mov <- (abs(y.dist) <= abs(x.dist)) * repdyn.dest * sign(y.dist) * attr.chg.step
  
  x.dest <- colSums(x.mov) + x.dest
  y.dest <- colSums(y.mov) + y.dest
  
  #mutation rate
  if (runif(1:pop.size+1) < mutation.rate) {
    x.dest <- x.dest + sample(c(-1,1),pop.size+1,replace=TRUE) * attr.chg.step
  }
  if (runif(1:pop.size+1) < mutation.rate) {
    y.dest <- y.dest + sample(c(-1,1),pop.size+1,replace=TRUE) * attr.chg.step
  }
  
  # make sure agents will not move away from the cultural space
  x.dest <- pmin(x.dest, rep(1 / attr.norm.factor, pop.size + 1))
  x.dest <- pmax(x.dest, rep(0, pop.size + 1))
  y.dest <- pmin(y.dest, rep(1 / attr.norm.factor, pop.size + 1))
  y.dest <- pmax(y.dest, rep(0, pop.size + 1))
  
  # no need to return institution
  df.dest <- data.frame(cbind(x.dest[1:pop.size], y.dest[1:pop.size]))
  return(df.dest)
}