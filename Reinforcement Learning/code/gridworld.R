# Nathan Harmon
# CS7641, Spring 2015, Markov Decision Processes
# Grid World Problem

#install.packages("MDPtoolbox")
#install.packages("rgl")
require("MDPtoolbox")
require("rgl")

###
### Environment
###

# World is a 15x15 grid
x <- 1:15
y <- 1:15

# Terminal states
terminal.states <- list(c("x" = 8, "y" = 8))

# Rewards
rewards <- matrix(rep(-0.5, max(x)*max(y)), nrow=max(y))
rewards[ 8, 8] <-   99.5
rewards[ 6, 8] <- -100.5
rewards[ 8, 6] <- -100.5
rewards[ 8,10] <- -100.5
rewards[10, 8] <- -100.5

# Boundarys
rewards[ 6, 6] <- NA
rewards[ 6,10] <- NA
rewards[10, 6] <- NA
rewards[10,10] <- NA

# Actions are four cardinal directions of travel.
actions <- c("N", "S", "E", "W")
action.values <- list("N" = c("x" = 0, "y" = 1),
                      "S" = c("x" = 0, "y" = -1),
                      "E" = c("x" = 1, "y" = 0),
                      "W" = c("x" = -1, "y" = 0))

# Transition probability
transition <- list("N" = c("N" = 0.8, "S" = 0.0, "E" = 0.1, "W" = 0.1),
                   "S" = c("N" = 0.0, "S" = 0.8, "E" = 0.1, "W" = 0.1),
                   "E" = c("N" = 0.1, "S" = 0.1, "E" = 0.8, "W" = 0.0),
                   "W" = c("N" = 0.1, "S" = 0.1, "E" = 0.0, "W" = 0.8))

# Get result of action
# Get result of action
act <- function(action, state) {
    for (terminal.state in terminal.states) {
        if (state['x'] == terminal.state['x'] &&
                state['y'] == terminal.state['y']) {
            return(state)
        }        
    }
    
    # Calculate new state and ensure it is within environment.
    new.state <- state
    action.value <- action.values[[action]]
    new.x <- state['x'] + action.value['x']
    new.y <- state['y'] + action.value['y']    
    new.state['x'] <- min(x[length(x)], max(x[1], new.x))
    new.state['y'] <- min(y[length(y)], max(y[1], new.y))
    
    # Checks if new state is boundary.
    if(is.na(rewards[new.state['y'], new.state['x']])) {
        return(state)
    }
    
    return(new.state)
}

# Build transition probability and reward arrays
# <S, S, A>
P <- array(0, c(max(x) * max(y),max(x) * max(y),length(actions)))
R <- array(0, c(max(x) * max(y),max(x) * max(y),length(actions)))
for (i in x) {
    for (j in y) {
        int_state = i + ((j - 1) * max(x))
        for (k in 1:length(actions)) {
            for (l in 1:length(actions)) {
                prob <- as.numeric(transition[[actions[k]]][actions[l]])
                next_state <- act(actions[l],c("x"=i,"y"=j))
                next_int_state = next_state['x'] + ((next_state['y'] - 1) * max(x))
                P[int_state,next_int_state,k] <- P[int_state,next_int_state,k] + prob
                R[int_state,next_int_state,k] <- rewards[next_state['y'],next_state['x']]
            }
        }
    }
}
# Terminal states
for (terminal.state in terminal.states) {
    int_state <- terminal.state['x'] + ((terminal.state['y'] - 1) * max(x))
    for (k in 1:length(actions)) {
        R[int_state,int_state,k] <- 100
    }
}


### Value Iteration
value_iteration_solution <- mdp_value_iterationGS(P, R, discount=0.4)

value_iteration_policy <- t(matrix(actions[value_iteration_solution$policy], nrow=max(y)))
value_iteration_policy <- value_iteration_policy[rev(seq_len(nrow(value_iteration_policy))),]
value_iteration_policy

open3d(windowRect=c(50,50,800,800))
palette <- colorRampPalette(c("blue", "green", "yellow", "red")) 
col.table <- palette(256)
col.ind <- cut(value_iteration_solution$V, 256)
persp3d(seq(1,15, length=15),
        seq(1,15, length=15),
        value_iteration_solution$V,
        col=col.table[col.ind],
        xlab = '', ylab = '', zlab = '')


### Policy Iteration
policy_iteration_solution <- mdp_policy_iteration_modified(P, R, discount=0.4)

policy_iteration_policy <- t(matrix(actions[policy_iteration_solution$policy], nrow=max(y)))
policy_iteration_policy <- policy_iteration_policy[rev(seq_len(nrow(policy_iteration_policy))),]
policy_iteration_policy

open3d(windowRect=c(50,50,800,800))
palette <- colorRampPalette(c("blue", "green", "yellow", "red")) 
col.table <- palette(256)
col.ind <- cut(policy_iteration_solution$V, 256)
persp3d(seq(1,15, length=15),
        seq(1,15, length=15),
        policy_iteration_solution$V,
        col=col.table[col.ind],
        xlab = '', ylab = '', zlab = '')


### q-Learning
ptm <- proc.time()
q_learning_solution <- mdp_Q_learning(P, R, discount=0.4)
cat("Time:",(proc.time() - ptm),"\n")

q_learning_policy <- t(matrix(actions[q_learning_solution$policy], nrow=max(y)))
q_learning_policy <- q_learning_policy[rev(seq_len(nrow(q_learning_policy))),]
q_learning_policy

open3d(windowRect=c(50,50,800,800))
palette <- colorRampPalette(c("blue", "green", "yellow", "red")) 
col.table <- palette(256)
col.ind <- cut(q_learning_solution$V, 256)
persp3d(seq(1,15, length=15),
        seq(1,15, length=15),
        q_learning_solution$V,
        col=col.table[col.ind],
        xlab = '', ylab = '', zlab = '')
