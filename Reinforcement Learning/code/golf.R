# Nathan Harmon
# CS7641, Spring 2015, Markov Decision Processes
# Golf Problem

#install.packages("MDPtoolbox")
#install.packages("rgl")
require("MDPtoolbox")
require("rgl")

###
### Environment
###

# World is a 5x20 grid
x <- 1:20
y <- 1:5

# Terminal states
terminal.states <- list(c("x" = 18, "y" = 3))

# Rewards
rewards <- matrix(rep(-1, max(x)*max(y)), nrow=max(y))
rewards[3,   18] <-  4
rewards[5, 6:10] <- -2
rewards[4, 6:10] <- -2
rewards[3,  7:9] <- -2
rewards[2,15:16] <- -2
rewards[1,15:17] <- -2

# Actions are four cardinal directions of travel.
actions <- c("N_P",  "N_M",  "N_L",  "N_P",
             "NE_P", "NE_M", "NE_L", "NE_P",
             "E_P",  "E_M",  "E_L",  "E_P",
             "SE_P", "SE_M", "SE_L", "SE_P",
             "S_P",  "S_M",  "S_L",  "S_P",
             "SW_P", "SW_M", "SW_L", "SW_P",
             "W_P",  "W_M",  "W_L",  "W_P",
             "NW_P", "NW_M", "NW_L", "NW_P")
action.values <- list("N_P" = c("x" = 0, "y" = 1),
                      "N_L" = c("x" = 0, "y" = 2),
                      "N_M" = c("x" = 0, "y" = 5),
                      "N_D" = c("x" = 0, "y" = 10),
                      "NE_P" = c("x" = ceiling(1/sqrt(2)),  "y" = ceiling(1/sqrt(2))),
                      "NE_L" = c("x" = ceiling(2/sqrt(2)),  "y" = ceiling(2/sqrt(2))),
                      "NE_M" = c("x" = ceiling(5/sqrt(2)),  "y" = ceiling(5/sqrt(2))),
                      "NE_D" = c("x" = ceiling(10/sqrt(2)), "y" = ceiling(10/sqrt(2))),
                      "E_P" = c("x" = 1,  "y" = 0),
                      "E_L" = c("x" = 2,  "y" = 0),
                      "E_M" = c("x" = 5,  "y" = 0),
                      "E_D" = c("x" = 10, "y" = 0),
                      "SE_P" = c("x" = ceiling(1/sqrt(2)),  "y" = -ceiling(1/sqrt(2))),
                      "SE_L" = c("x" = ceiling(2/sqrt(2)),  "y" = -ceiling(2/sqrt(2))),
                      "SE_M" = c("x" = ceiling(5/sqrt(2)),  "y" = -ceiling(5/sqrt(2))),
                      "SE_D" = c("x" = ceiling(10/sqrt(2)), "y" = -ceiling(10/sqrt(2))),
                      "S_P" = c("x" = 0, "y" = -1),
                      "S_M" = c("x" = 0, "y" = -2),
                      "S_L" = c("x" = 0, "y" = -5),
                      "S_P" = c("x" = 0, "y" = -10),
                      "SW_P" = c("x" = -ceiling(1/sqrt(2)),  "y" = -ceiling(1/sqrt(2))),
                      "SW_L" = c("x" = -ceiling(2/sqrt(2)),  "y" = -ceiling(2/sqrt(2))),
                      "SW_M" = c("x" = -ceiling(5/sqrt(2)),  "y" = -ceiling(5/sqrt(2))),
                      "SW_D" = c("x" = -ceiling(10/sqrt(2)), "y" = -ceiling(10/sqrt(2))),
                      "W_P" = c("x" = -1, "y" = 0),
                      "W_M" = c("x" = -2, "y" = 0),
                      "W_L" = c("x" = -5, "y" = 0),
                      "W_P" = c("x" = -10, "y" = 0),
                      "NW_P" = c("x" = -ceiling(1/sqrt(2)),  "y" = ceiling(1/sqrt(2))),
                      "NW_L" = c("x" = -ceiling(2/sqrt(2)),  "y" = ceiling(2/sqrt(2))),
                      "NW_M" = c("x" = -ceiling(5/sqrt(2)),  "y" = ceiling(5/sqrt(2))),
                      "NW_D" = c("x" = -ceiling(10/sqrt(2)), "y" = ceiling(10/sqrt(2))))

# Build transition probability and reward arrays
# <S, S, A>
P <- array(0, c(max(x) * max(y),max(x) * max(y),length(actions)))
R <- array(0, c(max(x) * max(y),max(x) * max(y),length(actions)))
for (i in x) {
    for (j in y) {
        start.state <- c("x"=i,"y"=j)
        start.state.int <- i + ((j - 1) * max(x))
        
        is.terminal <- 0
        for (terminal.state in terminal.states) {
            if (start.state['x'] == terminal.state['x'] &&
                    start.state['y'] == terminal.state['y']) {
                P[start.state.int,start.state.int,1:length(actions)] <- 1
                R[start.state.int,start.state.int,1:length(actions)] <- 4
                is.terminal <- 1
                break
            }        
        }
        if (is.terminal == 1) break
                
        for (k in 1:length(actions)) {
            action.value <- action.values[[actions[k]]]
            new.x <- start.state['x'] + action.value['x']
            new.y <- start.state['y'] + action.value['y']

            for (n in new.x-1:new.x+1) {
                for (m in new.y-1:new.y+1) {
                    if (n == new.x && m == new.y) prob <- 0.6
                    else prob <- 0.05
                    
                    next.state <- start.state
                    next.state['x'] <- min(x[length(x)], max(x[1], n))
                    next.state['y'] <- min(y[length(y)], max(y[1], m))
                    next.state.int = next.state['x'] + ((next.state['y'] - 1) * max(x))

                    P[start.state.int,next.state.int,k] <- P[start.state.int,next.state.int,k] + prob
                    R[start.state.int,next.state.int,k] <- rewards[next.state['y'],next.state['x']]
                }
            }

        }
    }
}


### Value Iteration
value_iteration_solution <- mdp_value_iterationGS(P, R, discount=0.2)

value_iteration_policy <- matrix(rep(0, max(x)*max(y)), nrow=max(y))
for (i in x) {
    for (j in y) {
        value_iteration_policy[j,i] <- actions[value_iteration_solution$policy[
            as.integer(i + ((j - 1) * max(x)))]]
    }
}
value_iteration_policy <- value_iteration_policy[rev(seq_len(nrow(value_iteration_policy))),]

value_iteration_policy

open3d(windowRect=c(50,50,800,800))
palette <- colorRampPalette(c("blue", "green", "yellow", "red")) 
col.table <- palette(256)
col.ind <- cut(value_iteration_solution$V, 256)
persp3d(seq(1,20, length=20),
        seq(1,5, length=5),
        value_iteration_solution$V,
        col=col.table[col.ind],
        xlab = '', ylab = '', zlab = '')


### Policy Iteration
policy_iteration_solution <- mdp_policy_iteration_modified(P, R, discount=0.2)

policy_iteration_policy <- matrix(rep(0, max(x)*max(y)), nrow=max(y))
for (i in x) {
    for (j in y) {
        policy_iteration_policy[j,i] <- actions[policy_iteration_solution$policy[
            as.integer(i + ((j - 1) * max(x)))]]
    }
}
policy_iteration_policy <- policy_iteration_policy[rev(seq_len(nrow(policy_iteration_policy))),]

policy_iteration_policy

open3d(windowRect=c(50,50,800,800))
palette <- colorRampPalette(c("blue", "green", "yellow", "red")) 
col.table <- palette(256)
col.ind <- cut(policy_iteration_solution$V, 256)
persp3d(seq(1,20, length=20),
        seq(1,5, length=5),
        policy_iteration_solution$V,
        col=col.table[col.ind],
        xlab = '', ylab = '', zlab = '')


### q-Learning
ptm <- proc.time()
q_learning_solution <- mdp_Q_learning(P, R, discount=0.2)
cat("Time:",(proc.time() - ptm),"\n")

q_learning_policy <- matrix(rep(0, max(x)*max(y)), nrow=max(y))
for (i in x) {
    for (j in y) {
        q_learning_policy[j,i] <- actions[q_learning_solution$policy[
            as.integer(i + ((j - 1) * max(x)))]]
    }
}
q_learning_policy <- q_learning_policy[rev(seq_len(nrow(q_learning_policy))),]

q_learning_policy

open3d(windowRect=c(50,50,800,800))
palette <- colorRampPalette(c("blue", "green", "yellow", "red")) 
col.table <- palette(256)
col.ind <- cut(q_learning_solution$V, 256)
persp3d(seq(1,20, length=20),
        seq(1,5, length=5),
        q_learning_solution$V,
        col=col.table[col.ind],
        xlab = '', ylab = '', zlab = '')


