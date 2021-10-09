
rm(list=ls())
library(tictoc)
library(pryr)
library(igraph)

CR = 52 * 30  # CR = can retire. The minimum age (in weeks) that a police officer agent can retire.
CpP = 400   # # of civilian agents per police officer agent

# This function creates a new agent. An agent is an environment that has several 
# variables: id, n_accept, n_refuse, and tiw. tiw is tenure in weeks and it holds
# value for only police agents. id is the physical address of the environment. It
# is a unique identifier because each environment (i.e., agent) holds a unique address
# in the memory. n_accept is the number of times that the agent has accepted bribes,
# and n_refuse is the number of times that the agent has refused bribes.
new_agent = function(class='...') {
  e = new.env(parent = globalenv())
  e$id = address(e)
  class(e) = class
  e$n_accept = 0
  e$n_refuse = 0
  e$tiw = 0
  return(e)
}

# This function connects agent a and agent b. Each agent (i.e., a and b) has a list
# object that holds the list of agents that the agent is friends with. This function
# adds a's physical address (or its id) to b's list of friends, and it adds b's
# physical address (or its id) to a's list of friends. Thus, a and b become connected.
wire = function(a,b) {
  if (a$id != b$id) {
    a$nei = unique(append(a$nei, b))
    b$nei = unique(append(b$nei, a))
  }
}

# This function disconnects agent a and agent b. It does the opposite of what wire() 
# function does. It removes a's physical address (or its id) from b's list of friends,
# and also removes b's physical address (or its id) from a's list of friends. Thus, 
# a and b become disconnected.
unwire = function(a,b) {
  nei = unlist(lapply(a$nei, function(x) x$id))
  a$nei = a$nei[-which(nei == b$id)]
  nei = unlist(lapply(b$nei, function(x) x$id))
  b$nei = b$nei[-which(nei == a$id)]
}

# This function wires all agents in a list with randomly selected n other agents.
# The default value of n = 4.
wire_all = function(agents, n=4) {
  for (i in 1:n) {
    m = matrix(
      c(sample(agents, round(length(agents)/2)),
        sample(agents, round(length(agents)/2))),
      ncol=2)
    apply(m, 1, function(x) {
      a = x[[1]]
      b = x[[2]]
      wire(a,b)
    })
  }
  agents = unlist(lapply(agents, function(x) if (length(x$nei) > 0) x else NULL))
  return(agents)
}

# This function returns a random threshold value. x is the inverse of the skew of
# the distribution that the new threshold value will come from. The larger the skew
# (i.e., x) the higher the new threshold value, because the distribution ranges 
# from 0 to 1 and it is skewed to the left.
new_t = function(x) 1 - exp(-rnorm(1, x, .25))

# This function first creates n number of agents and then it initializes these agents'
# threshold value. The threshold values are drawn from a distribution that is skewed
# to the left by s_t. Then, it randomly wires all these agents. Lastly, it purges
# agents that do not have any friends. Thus, the number of agents returned is slightly
# smaller than n.
init.agents = function(N=1000, s_t=2) {
  tic()
  agents = lapply(c(1:N), function(x) {
    a = new_agent()
    a$t = new_t(s_t)
    return(a)
  })
  agents = wire_all(agents)
  toc()
  return(agents)
}

# This function randomly selects N/CpP of the agents and turns them into police 
# agents. N is the total number of agents and CpP is number of civilian agents per
# a police agent. The function does not have any input value, because 'agents' is 
# assumed to exist in the global environment.
init.pol = function() {
  pol = sample(agents, round(length(agents)/CpP))
  for (i in 1:3)
    invisible(lapply(pol, function(x) if (length(x$nei) > 1)  unwire(x, x$nei[[1]])))
  invisible(lapply(pol, function(x) {
    class(x) = 'police'
    x$tiw = sample(c(1:CR), 1)   # tenure in weeks
  }))
  invisible(wire_all(pol))
}

# This function only displays agents as a graph object.
display = function(agents) {
  G = graph_from_data_frame(  
    data.frame(t(matrix(unlist(lapply(agents, function(x) {
      unlist(t(data.frame(u = x$id, y = unlist(lapply(x$nei, function(y) y$id)))))
    })), nrow=2)))
  )
  G = as.undirected(G)
  plot(G, vertex.label='', vertex.size=3)
}

# This function turns a police agent a into a civilian agent. It removes a's all 
# ties to other police agents. It also wires a with a randomly selected civilian 
# agent.
retire = function(a, how = 'normal') {
  lapply(a$nei, function(x)
    if (class(x) == 'police') unwire(x, a))
  if (how == 'normal') class(a) = 'ex-cop' else class(a) = 'ex-d-cop'
  wire(a, sample(civ, 1)[[1]])
}

# This function turns n number of civilian agents into police agents. The default
# value of n = 1. When a civilian agent is turned into a police agent, its ties to 
# three of other civilian agents are removed. After that, this new police agent is
# connected to a randomly selected nei number of police agents. Here, nei is the 
# average number of friends that police agents have.
recruit = function(n=1) {
  nei = round(mean(unlist(lapply(pol, function(x) length(x$nei)))))
  pool = unlist(lapply(agents, function(x) if (class(x)!='police' & class(x)!='ex-cop' & class(x)!='ex-d-cop') x else NULL))
  for (i in 1:n) {
    a = sample(pool, 1)[[1]]
    for (i in 1:3) if (length(a$nei) > 0) unwire(a, a$nei[[1]])
    class(a) = 'police'
    nei = sample(pol, nei)
    invisible(lapply(nei, function(x) wire(x, a)))
  }
}

# This function updates agent a's threshold value by the inverse of x. If x has
# a negative value, Then threshold goes down. Otherwise, the threshold goes up. 
# This function updates a's first neighbors' threshold values as well. But, they 
# are updated by the inverse of x/2.
update_t = function(a, x) {
  a$t = a$t + ((a$t * (1 - a$t)) / (x*30))
  invisible(lapply(a$nei, function(y) {
    if (class(a) == class(y))
      y$t = y$t + ((y$t * (1 - y$t)) / (x*2*30))
  }))
}

# This is the main play function. Players are p (police agent) and c (civilian 
# agent). First, one of them offers a bribe if a random number is larger than its
# threshold value. The other agent accepts the bribe if a random number is larger 
# than its threshold value. If an agent offers a bribe and the other agent accepts
# it, then bribery happens. Both agents' threshold values go down. If an agent 
# offers a bribe but the other agent refuses it, then offerer's threshold value 
# goes up and the refuser's threshold value goes down.
play0 = function(p, c) {
  bri = 0
  if (runif(1) > .5) {
    if (runif(1) > c$t) {
      update_t(p, -.5)
      if (runif(1) > p$t) {
        bri = 1
        update_t(c, -1)
        c$n_accept = c$n_accept + 1
      } else {
        update_t(c, 1)
        c$n_refuse = c$n_refuse + 1
      }
    } else {
      update_t(p, 1.5)
    }
  } else {
    if (runif(1) > p$t) {
      update_t(c, -.5)
      if (runif(1) > c$t) {
        bri = 1
        update_t(p, -1)
        p$n_accept = p$n_accept + 1
      } else {
        update_t(p, 1)
        p$n_refuse = p$n_refuse + 1
      }
    } else {
      update_t(c, 1.5)
    }
  }
  return(bri)
}

# This function updates lists of police and civilian agents (i.e., pol and civ) in 
# the global environment.
update_polciv = function() {
  pol = unlist(lapply(agents, function(x) if (class(x)=='police') x else NULL))
  assign('pol', pol, envir=globalenv())
  civ = unlist(lapply(agents, function(x) if (class(x)!='police') x else NULL))
  assign('civ', civ, envir=globalenv())
}

# This function is a cover for the main play function (i.e., play0). The function 
# first selects two random agents; one police agent and one civilian agent. Then, 
# it calls the play0 function. If bribery happens, then it calls an if statement. 
# if a random number is smaller than p_rem (probability of removing a police agent)
# times mean threshold of all police agents, then it removes the police agent that 
# accepted the bribe and recruits a new police agent. Lastly, it updates lists of
# police and civilian agents.
play = function() {
  p = sample(pol, 1)[[1]]
  c = sample(agents, 1)[[1]]
  bri = play0(p,c)
  if (bri) {
    if (exists('N_BRI', envir = globalenv())) N_BRI <<- N_BRI + 1
    if (runif(1) < p_rem*median_t(pol)) {
      tu = unlist(lapply(p$nei, function(x) x$nei))
      tu = unique(unlist(lapply(tu, function(x) if (class(x)=='police') x else NULL)))
      invisible(lapply(tu, function(x) {
        x$t = x$t + (x$t * (1 - x$t)) / 10
      }))
      replace(p, 'kick-out')
      update_polciv()
    }
  }
  return(bri)
}

replace = function(p, how = 'normal') {
  if (how == 'normal') retire(p, 'normal') else retire(p, 'kick-out')
  recruit()
}

# This is the main iteration function. In every iteration, n pairs of police and 
# civilian agents play. Also, in every iteration, police agents that can retire 
# retire if a random number is smaller than a certain value (i.e., (x$tiw - 1040)
# / 52000)
tick = function(n) {
  for (i in 1:n) play()
  invisible(lapply(pol, function(x) x$tiw = x$tiw + 1))
  cr = unlist(lapply(pol, function(x) if (x$tiw >= CR) x else NULL))
  invisible(lapply(cr, function(x) {
    if (runif(1) < (x$tiw - 1040) / 52000) {
      replace(x, 'normal')
      update_polciv()
    }
  }))
}

# This function returns the median threshold of a list of agents.
median_t = function(agents) median(unlist(lapply(agents, function(x) x$t)))

# This function returns the number of acceptances of a list of agents.
n_accept = function(agents) sum(unlist(lapply(agents, function(x) x$n_accept)))

# ==============================================================================


# This function extract threshold trajectories for each police officer agent from M_POL
get_threshold_trajectories = function(l, pol) {
  df = data.frame(matrix(unlist(l), nrow = length(l), byrow = TRUE))
  df$X2 = as.numeric(df$X2)
  df = df[with(df, order(X1, X2)), ]
  
  ids = aggregate(cbind(dum = X2) ~ X1, df, function(x) 1)
  l = list()
  i = 0
  while (i < nrow(ids)) {
    i = i + 1
    l = append(l, list(df[which(df$X1 == ids[i,1]), c(2,3)]))
  }
  return(l)
}

# This function displays threshold trajectories for each police officer agent.
disp_thre = function(l, add = T, line_col = 'blue', point_col = 'firebrick', pcx = .3, main = '') {
  if (!add) {
    plot(c(0,0), cex = .01, xlim = c(1,1800), ylim = c(0,1), 
         ylab = 'Threshold', xlab = 'Police officer tenure in weeks',
         main = main)
  }
  i = 0
  while (i < length(l)) {
    i = i + 1
    p = l[[i]]
    points(rbind(c(NA,NA), as.matrix(p)[1,]), col = point_col, cex = pcx, pch = 20)
    lines(as.matrix(p), col = line_col)
    points(rbind(c(NA,NA), as.matrix(p)[nrow(as.matrix(p)),]), col = point_col, cex = pcx, pch = 20)
  }
}

# This function gets threshold trends from a list (l)
get_threshold_trends = function(l) {
  ret = c()
  i = 0
  while (i < length(l)) {
    i = i + 1
    p = l[[i]]
    p = summary(lm(X3 ~ X2, p))
    p = p$coefficients[2]
    ret = c(ret, p)
  }
  return(ret)
}

