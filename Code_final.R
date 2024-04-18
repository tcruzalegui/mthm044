#### SECTION 3 - DATA ACQUISITION ####

# Relevant files for Premier League, Championship and League One downloaded from:
# https://www.football-data.co.uk/englandm.php
# before being combined into one CSV file of consecutive matches from all 3 divisions
# File available at https://github.com/tcruzalegui/mthm044
all_matches <- read.csv("[FILEPATH]/all_matches.csv") # Load file into R once downloaded
#all_matches <- read.csv("C:/Users/user/Documents/Uni/Y4/MTHM044/Data/all_matches.csv") # Load file into R once downloaded

View(all_matches)

#### SECTION 3 - 2017/18 Premier League observed and predicted rankings ####
require(rvest) # For web scraping
require(stringr) # For manipulating strings
scraped_page <- read_html("https://en.wikipedia.org/wiki/2017%E2%80%9318_Premier_League")
Position <- c(1:20) # Ordered positions will just be the numbers 1 to 20
Teams <- scraped_page%>%html_nodes("td+ th a")%>%html_text() %>% as.character() # Ordered teams
Teams[c(1,2,3,9,10,13,15,16,18,19,20)] <- c("Man City","Man United","Tottenham",
                                            "Leicester","Newcastle","West Ham",
                                            "Brighton","Huddersfield","Swansea",
                                            "Stoke","West Brom")
# Make team names consistent with those in our data

P <- rep(38, 20) # P = Played; each team plays 38 games
# Scrape in columns for wins, draws, losses, goal difference and points
W <- scraped_page%>%html_nodes("h2+ .wikitable tbody:nth-child(1) td:nth-child(4)")%>%html_text() %>% as.character()
D <- scraped_page%>%html_nodes("h2+ .wikitable tbody:nth-child(1) td:nth-child(5)")%>%html_text() %>% as.character()
L <- scraped_page%>%html_nodes("h2+ .wikitable tbody:nth-child(1) td:nth-child(6)")%>%html_text() %>% as.character()
GD <- scraped_page%>%html_nodes("h2+ .wikitable td:nth-child(9)")%>%html_text() %>% as.character()
Pts <- scraped_page%>%html_nodes("h2+ .wikitable td:nth-child(10)")%>%html_text() %>% as.character()

strcorr <- function(vector){ # Define function which formats the columns correctly
  vector <- vector[(1:20)] # Keep just the first 20 values in the vector
  vector <- str_sub(vector, end = -2)
  return(vector)
}
W <- strcorr(W)
D <- strcorr(D)
L <- strcorr(L)
GD <- strcorr(GD)
Pts <- strcorr(Pts)

#### Predicted rankings ####
# Predictions made before new season of team rankings from three media sources
Guardian <- c("Man City","Chelsea","Man United","Tottenham","Liverpool","Arsenal","Everton","Leicester","Southampton","West Ham","Bournemouth","West Brom","Newcastle","Crystal Palace","Watford","Stoke","Swansea","Burnley","Huddersfield","Brighton")
BBC <- c("Man City","Chelsea","Man United","Liverpool","Arsenal","Tottenham","Everton","West Ham","Leicester","Bournemouth","Newcastle","West Brom","Crystal Palace","Southampton","Stoke","Swansea","Burnley","Watford","Huddersfield","Brighton")
TalkSport <- c("Man City","Man United","Chelsea","Tottenham","Arsenal","Liverpool","Everton","West Ham","Southampton","Crystal Palace","West Brom","Leicester","Swansea","Bournemouth","Newcastle","Stoke","Watford","Burnley","Brighton","Huddersfield")

# Go through each team in the observed table in descending order and store their predicted position according to each media outlet
# E.g. i=1 for Manchester City (1st in real life); i=2 for Man United
Rank.guard = Rank.BBC = Rank.TS = rep(NA,20) # Empty vectors to store predicted ranks
for (i in 1:20) {
  Rank.guard[i] <- which(Guardian==Teams[i])
  Rank.BBC[i] <- which(BBC==Teams[i])
  Rank.TS[i] <- which(TalkSport==Teams[i])
}

Table.real <- data.frame(Position,Teams,P,W,D,L,GD,Pts,Rank.guard,Rank.BBC,Rank.TS)
# Data frame produced for loading into LaTeX


#### SECTION 3.1 - DATA EXPLORATION ####
library(ggplot2) # Load ggplot2 for graphics
library(tidyverse) # Load tidyverse for data manipulation
train <- filter(all_matches, Week < 376) # Filter out the target season and validation data
x <- train$FTHG # Extract data for home goals
y <- train$FTAG # Extract data for away goals

# Bar chart to compare distribution of home goals and away goals:
goals <- data.frame(Goals = c(x,y),Location = rep(c("Home","Away"),each=length(x))) # Create data frame 
ggplot(data=goals) +
  scale_x_continuous(breaks = seq(0,9,1)) + # Label x axis
  geom_bar(mapping = aes(x=Goals, y = ..prop.., fill=Location), position = "dodge") +
  labs(
    x = "Number of goals",
    y = "Relative frequency"
  ) # Bars coloured by location (home/away)

# Compute the mean and variance of the home and away goals over the eight seasons:
cat("The mean home score is", mean(x), "goals.\n") # 1.49
cat("The mean away score is", mean(y), "goals.\n") # 1.17
cat("The variance of the home score is", var(x), ".\n") # 1.55
cat("The variance of the away score is", var(y), ".\n") # 1.23

#### SECTION 4.3 - MODEL FITTING ####

all_teams <- as.character(sort(unique(all_matches$HomeTeam), decreasing = FALSE))
n_teams <- length(all_teams) # Set the total no. of teams (86)
team.names.PL <- sort(unique(all_matches$HomeTeam[11873:12252])) # Extract the Premier League team names
parameter_list <- list(attack = rep(0.12, n_teams),
                       defense = rep(0.01, n_teams-1),
                       home = 0.3) # Initial parameter values for optim

# Remember: higher alpha means more likely to score; higher beta means more likely to concede
names(parameter_list$attack) <- all_teams # Assign teams attack parameters
names(parameter_list$defense) <- all_teams[-1] # Same for defence, save for \hat\beta_1

data <- filter(all_matches, Week < 376) # Extract training data: first seven seasons plus 10 weeks of the eighth

#### The independent Poisson model (Lee, 1997) ####

# Define log-likelihood:
#   params = set of parameters; goals_home = vector of home scores;
#   goals_visitor = vector of away scores; team_home and team_visitor are home/away team names;
#   param_skeleton = list structure for initial parameter values
lee_negloglik <- function(params, goals_home, goals_visitor,
                          team_home, team_visitor, param_skeleton){
  
  # Takes a set of parameters and relists it like the list of starting values
  plist <- relist(params, param_skeleton)
  
  # \hat\beta_1 computed after rearranging sum-to-zero constraint.
  plist$defense <- c(sum(plist$defense)*-1, plist$defense)
  names(plist$defense)[1] <- names(plist$attack[1]) # add name to first element.
  
  # Home team expected goals - exponentiated so that values are always +ve
  lambda_home <- exp(plist$attack[team_home] + plist$defense[team_visitor] + plist$home)
  
  # Away team expected goals
  lambda_visitor <- exp(plist$attack[team_visitor] + plist$defense[team_home])
  
  # The log-likelihood
  log_lik_home <- dpois(goals_home, lambda = lambda_home, log=TRUE)
  log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log=TRUE)
  
  log_lik <- sum(log_lik_home + log_lik_visitor) # Sum of logs
  
  return(log_lik*-1) # make the result negative so it can be minimised by R.
  
}

# Minimise the negative log-likelihood by BFGS algorithm. No Hessian needed for now.
optim_res.lee <- optim(par = unlist(parameter_list), fn=lee_negloglik,
                    goals_home = data$FTHG,
                    goals_visitor = data$FTAG,
                    team_home = data$HomeTeam, team_visitor = data$AwayTeam,
                    param_skeleton=parameter_list, method = 'BFGS', hessian = F)

# Storing estimates #
parameter_est <- relist(optim_res.lee$par, parameter_list) # Put the estimates into a list of alphas, betas and \gamma
parameter_est$defense <- c(-1*sum(parameter_est$defense), parameter_est$defense) # Add in \hat\beta_1
names(parameter_est$defense)[1] <- names(parameter_est$attack[1]) # Add the team name for \hat\beta_1

alphas.Lee <- parameter_est$attack # Put the attack parameters in one vector
betas.Lee <- parameter_est$defense # Put the defense parameters in one vector
gamma.Lee <- parameter_est$home # Store the home adv parameter on its own

#### The dependent Poisson ####

parameter_list$rho <- 0.00 # Add rho = 0 to initial parameter list

# Dependence adjustment tau, defined case-wise
tau <- Vectorize(function(home, away, lambda, mu, rho){
  if (home == 0 & away == 0){return(1 - (lambda*mu*rho)) # 0-0
  } else if (home == 0 & away == 1){return(1 + (lambda*rho)) # 0-1
  } else if (home == 1 & away == 0){return(1 + (mu*rho)) # 1-0
  } else if (home == 1 & away == 1){return(1 - rho) # 1-1
  } else {return(1)} # otherwise
})

# Log-likelihood. Notice addition of tau.
dc_negloglik <- function(params, goals_home, goals_visitor,
                         team_home, team_visitor, param_skeleton){
  
  plist <- relist(params, param_skeleton)
  
  plist$defense <- c(sum(plist$defense)*-1, plist$defense)
  names(plist$defense)[1] <- names(plist$attack[1])
  
  lambda_home <- exp(plist$attack[team_home] + plist$defense[team_visitor] + plist$home)
  lambda_visitor <- exp(plist$attack[team_visitor] + plist$defense[team_home])
  
  # Dixon-Coles adjustment for the dependence using the tau function
  dc_adj <- tau(goals_home, goals_visitor, lambda_home, lambda_visitor, rho = plist$rho)
  
  # Trick to avoid warnings. Stops the value of tau from being negative.
  if (any(dc_adj <= 0)){
    return(Inf)
  }
  
  log_lik_home <- dpois(goals_home, lambda = lambda_home, log=TRUE)
  log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log=TRUE)
  
  log_lik <- sum(log_lik_home + log_lik_visitor + log(dc_adj)) # Sum of logs now includes the log of the tau function
  
  return(-log_lik)
}

optim_res.dep <- optim(par = unlist(parameter_list), fn=dc_negloglik,
                       goals_home = data$FTHG,
                       goals_visitor = data$FTAG,
                       team_home = data$HomeTeam, team_visitor = data$AwayTeam,
                       param_skeleton=parameter_list, method = 'BFGS', hessian = F)

# Storing estimates #
parameter_est <- relist(optim_res.dep$par, parameter_list)
parameter_est$defense <- c(-1*sum(parameter_est$defense), parameter_est$defense)
names(parameter_est$defense)[1] <- names(parameter_est$attack[1])

alphas.dep <- parameter_est$attack # Put the attack parameters in one vector
betas.dep <- parameter_est$defense # Put the defense parameters in one vector
gamma.dep <- parameter_est$home # Store the home adv parameter on its own
rho.dep <- parameter_est$rho # Store the dependence parameter on its own

#### Unweighted Dixon-Coles ####
# Log-likelihood and initial parameters here have same form as dependent Poisson
# Estimates are now made after each week. We need matrices and vectors to store them all for the validation step later.
alphas.unw <- matrix(ncol = n_teams, nrow = 418) # For the alphas up until week 418 - one row for each week
betas.unw <- matrix(ncol = n_teams, nrow = 418) # For the betas up until week 418
colnames(alphas.unw) = colnames(betas.unw) = all_teams # Assign team names to the columns of both matrices
rhos.unw <- rep(NA,418) # For \rho up until week 418 - one element for each week
gammas.unw <- rep(NA,418) # For \gamma up until week 418

t_values <- c((10:40),(52:94),(104:145),(158:198),
              (208:249),(261:303),(313:355),(365:407),418) # Times at which we will estimate.
# We can skip the break between seasons since no matches are being played

# Warning: slow #
for (t in t_values) {
  print(t) # Print the week we are on, to track progress
  # Moving time-window created using dplyr::filter
  if(t <= 52){
    data <- filter(all_matches, Week <= t) # If before Week 52 just take all previous matches
  } else data <- filter(all_matches,Week<=t & Week > t-52) # If after, cut off matches over 52 weeks old
  
  # Optimise
  optim_res.unw <- optim(par = unlist(parameter_list), fn=dc_negloglik,
                     goals_home = data$FTHG,
                     goals_visitor = data$FTAG,
                     team_home = data$HomeTeam, team_visitor = data$AwayTeam,
                     param_skeleton=parameter_list, method = 'BFGS',
                     hessian = ifelse(t==418,T,F)) # No need for Hessian until final week
  
  # List estimates and calculate \hat\beta_1 as usual
  parameter_est <- relist(optim_res.unw$par, parameter_list)
  parameter_est$defense <- c((-1)*sum(parameter_est$defense), parameter_est$defense)
  names(parameter_est$defense)[1] <- names(parameter_est$attack[1])

  # Store all estimates in the relevant matrices/vectors for Week t
  # Teams that didn't play keep their initial parameter values.
  alphas.unw[t,] <- parameter_est$attack
  betas.unw[t,] <- parameter_est$defense
  rhos.unw[t] <- parameter_est$rho
  gammas.unw[t] <- parameter_est$home
  
  # Redefine initial parameters using new estimates to speed up optimisation
  # Using exact values throws up errors - use the mean instead for the alphas and betas
  parameter_list$attack <- rep(mean(parameter_est$attack),times=n_teams)
  parameter_list$defense <- rep(mean(parameter_est$defense[-1]),times=n_teams-1) # Shorten by one due to beta_1 constraint
  names(parameter_list$attack) <- all_teams # Reassign names
  names(parameter_list$defense) <- all_teams[-1] # Reassign names
  parameter_list$home <- parameter_est$home
  parameter_list$rho <- parameter_est$rho
}
# When finished we have a set of estimates before each week where matches are being played up until the start of the 2017/18 season

#### Weighted Dixon-Coles model ####

# Weighting function phi, for the weighted Dixon-Coles models
phi <- function(x,xi = 0.0259){ # Change xi to 0.0850 for the 2nd DC model
  return(exp(-(x*xi)))
}

# Similar structure again, but notice extra window argument
dc_negloglik.wei <- function(params, goals_home, goals_visitor,
                         team_home, team_visitor, window, param_skeleton){
  
  plist <- relist(params, param_skeleton)
  plist$defense <- c(sum(plist$defense)*-1, plist$defense)
  names(plist$defense)[1] <- names(plist$attack[1])
  
  lambda_home <- exp(plist$attack[team_home] + plist$defense[team_visitor] + plist$home)
  lambda_visitor <- exp(plist$attack[team_visitor] + plist$defense[team_home])
  
  dc_adj <- tau(goals_home, goals_visitor, lambda_home, lambda_visitor, rho = plist$rho)
  if (any(dc_adj <= 0)){
    return(Inf)
  }
  
  # The log-likelihood
  log_lik_home <- dpois(goals_home, lambda = lambda_home, log=TRUE)
  log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log=TRUE)
  log_lik <- sum((log_lik_home + log_lik_visitor + log(dc_adj))*phi(window))
  # Multiply the log likelihood by phi (initially phi was an exponent but the log turns it into a multiplier)
  
  return(log_lik*-1)
}

# Re-define initial values and empty matrices
parameter_list <- list(attack = rep(0.12, n_teams),
                       defense = rep(0.01, n_teams-1),
                       home = 0.3,
                       rho= 0.00) # Parameter list in full

alphas.wei1 <- matrix(ncol = n_teams, nrow = 418) # For the alphas up until week 418
betas.wei1 <- matrix(ncol = n_teams, nrow = 418) # For the betas up until week 418
colnames(alphas.wei1) = colnames(betas.wei1) = all_teams # Names
rhos.wei1 <- rep(NA,418) # For \rho up until week 418
gammas.wei1 <- rep(NA,418) # For \gamma up until week 418

for (t in t_values) {
  print(t) # Track progress
  if(t <= 52){
    data <- filter(all_matches,Week <= t)
  } else data <- filter(all_matches,Week<=t & Week > t-52) # Filter as before
  
  
  # Optimise log likelihood with weights applied within the time window
  optim_res <- optim(par = unlist(parameter_list), fn=dc_negloglik.wei,
                     goals_home = data$FTHG,
                     goals_visitor = data$FTAG,
                     team_home = data$HomeTeam, team_visitor = data$AwayTeam, window = t-data$Week,
                     param_skeleton=parameter_list, method = 'BFGS', hessian = F)
  
  # Relist estimates and calculate \hat\beta_1 as usual for Week t
  parameter_est <- relist(optim_res$par, parameter_list)
  parameter_est$defense <- c((-1)*sum(parameter_est$defense), parameter_est$defense)
  names(parameter_est$defense)[1] <- names(parameter_est$attack[1])

  # Now we store all the weekly estimates away in their own matrices/vectors
  # Teams that didn't play kept their initial parameter values.
  alphas.wei1[t,] <- parameter_est$attack
  betas.wei1[t,] <- parameter_est$defense
  rhos.wei1[t] <- parameter_est$rho
  gammas.wei1[t] <- parameter_est$home
  
  # Redefine initial parameters using new estimates (except beta_1):
  parameter_list$attack <- rep(mean(parameter_est$attack),times=n_teams) # Attack
  parameter_list$defense <- rep(mean(parameter_est$defense[-1]),times=n_teams-1) # Defence
  names(parameter_list$attack) <- all_teams # Names
  names(parameter_list$defense) <- all_teams[-1]
  parameter_list$home <- parameter_est$home # Gamma
  parameter_list$rho <- parameter_est$rho # Rho
  
}
# For second unweighted Dixon-Coles, change xi to 0.0850, reset initial parameters and use alphas.wei2 etc to store estimates

#### Selection of second \xi value ####

m.start <- 350 # We have estimates from the end of Week 10 so we can start with Week 11 for the validation
m.end <- 5936 # The ID of the final match in the fourth season

# DC joint probability
DCprob <- function(x,y,lambda,mu,rho){
  return(tau(xx=x,yy=y,lambda = lambda, mu = mu, rho = rho)*dpois(x,lambda)*dpois(y,mu))
}

xi_test <- seq(0.085,0.100,l=10) # Values of xi to test
LS = RPS = rep(NA,length(xi_test)) # Skeleton vectors for log score and RPS
for (ii in seq_len(length(xi_test))) {
  
  phi <- function(x,xi = xi_test[ii]){
    return(exp(-(x*xi)))
  }
  
  dc_negloglik <- function(params, goals_home, goals_visitor,
                           team_home, team_visitor, window, param_skeleton){

    plist <- relist(params, param_skeleton)
    
    plist$defense <- c(sum(plist$defense)*-1, plist$defense)
    names(plist$defense)[1] <- names(plist$attack[1])
    
    lambda_home <- exp(plist$attack[team_home] + plist$defense[team_visitor] + plist$home)
    
    lambda_visitor <- exp(plist$attack[team_visitor] + plist$defense[team_home])
    
    dc_adj <- tau(goals_home, goals_visitor, lambda_home, lambda_visitor, rho = plist$rho)
    
    if (any(dc_adj <= 0)){
      return(Inf)
    }
    
    log_lik_home <- dpois(goals_home, lambda = lambda_home, log=TRUE)
    log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log=TRUE)
    
    log_lik <- sum((log_lik_home + log_lik_visitor + log(dc_adj))*phi(window))
    
    return(log_lik*-1)
    
  }
  
  parameter_list <- list(attack = rep(0.12, n_teams),
                         defense = rep(0.01, n_teams-1),
                         home = 0.3,
                         rho= 0.00)

  names(parameter_list$attack) <- all_teams
  names(parameter_list$defense) <- all_teams[-1] # the first beta is computed from the rest. This means the defense vector is shorter by one.
  
  alphas <- matrix(ncol = n_teams, nrow = 198)
  betas <- matrix(ncol = n_teams, nrow = 198)
  rhos <- rep(NA,198)
  gammas <- rep(NA,198)
  colnames(alphas) = colnames(betas) = all_teams
  
  #### The optimisation loop ####
  
  t_values <- c((10:40),(52:93),(104:144),(158:197))
  
  for (t in t_values) {
    print(t)
    if(t <= 52){
      data <- filter(all_matches,Week <= t)
    } else data <- filter(all_matches,Week<=t & Week > t-52)
    
    
    optim_res <- optim(par = unlist(parameter_list), fn=dc_negloglik,
                       goals_home = data$FTHG,
                       goals_visitor = data$FTAG,
                       team_home = data$HomeTeam, team_visitor = data$AwayTeam, window = t-data$Week,
                       param_skeleton=parameter_list, method = 'BFGS', hessian = F)
    
    parameter_est <- relist(optim_res$par, parameter_list) #  Breaks the vector
    parameter_est$defense <- c( sum(parameter_est$defense) * -1, parameter_est$defense)
    names(parameter_est$defense)[1] <- names(parameter_est$attack[1])

    alphas[t,] <- parameter_est$attack
    betas[t,] <- parameter_est$defense
    rhos[t] <- parameter_est$rho
    gammas[t] <- parameter_est$home
    
    parameter_list$attack <- rep(mean(parameter_est$attack),times=n_teams)
    parameter_list$defense <- rep(mean(parameter_est$defense[-1]),times=n_teams-1)
    parameter_list$home <- parameter_est$home
    parameter_list$rho <- parameter_est$rho
    names(parameter_list$attack) <- all_teams
    names(parameter_list$defense) <- all_teams[-1]
  }
  
  preds <- matrix(nrow = m.end-m.start+1, ncol = 4) # Matrix to contain the probability
  # forecasts of each match. First 349 matches (9 weeks) ignored since we do not have estimates for those.
  # Fill the last column with the real outcome
  preds[,4] <- all_matches$Outcome[m.start:m.end]
  colnames(preds) = c("p_h","p_d","p_a","Outcome") # Label the columns with the forecast prob of each outcome
  
  for (m in m.start:m.end) {
    HomeTeam <- all_matches$HomeTeam[m] # Label the home team from match m
    AwayTeam <- all_matches$AwayTeam[m] # Label the away team from match m
    # Store the expected scoring rate of the home team based the parameters from the week before
    lambdaH <- exp(alphas[all_matches$Week[m]-1,HomeTeam]+betas[all_matches$Week[m]-1,AwayTeam]+gammas[all_matches$Week[m]-1]) # Expected home score
    # Store the expected scoring rate of the away team based the parameters from the week before
    muA <- exp(alphas[all_matches$Week[m]-1,AwayTeam]+betas[all_matches$Week[m]-1,HomeTeam]) # Expected away score
    # Also store the estimated value of rho from that week
    rho.curr <- rhos[all_matches$Week[m]-1]
    
    probs <- matrix(nrow=7,ncol=7) # Empty matrix to store probability forecasts
    colnames(probs) = rownames(probs) = c((0:5),"6+") # Label the columns and rows with the goal counts
    for (x in 0:5) {
      for (y in 0:5) {
        probs[x+1,y+1] <- prob(x,y,lambdaH,muA,rho.curr) # Fill the array layer with model probabilities for most of the scores
      }
    }
    
    # Group together the outcomes where either team scores more than 5 goals
    probs[7,(1:6)] <- dpois((0:5),muA)*(1-ppois(5,lambdaH))
    probs[(1:6),7] <- dpois((0:5),lambdaH)*(1-ppois(5,muA))
    # Probability of both teams scoring more than 5 goals
    probs[7,7] <- 1-sum(probs,na.rm = T)
    # Table is now complete for that value of m
    
    # Group together the probabilities corresponding to home/away wins and a draw and enter them in the preds matrix
    preds[m-m.start+1,"p_h"] <- sum(probs*lower.tri(probs)) # Home
    preds[m-m.start+1,"p_d"] <- sum(diag(probs)) # Draw
    preds[m-m.start+1,"p_a"] <- sum(probs*upper.tri(probs)) # Away
    
  }
  
  LS[ii] <- sum(I(preds[,"Outcome"]==1)*log(preds[,"p_h"]) +
                  I(preds[,"Outcome"]==2)*log(preds[,"p_d"]) +
                  I(preds[,"Outcome"]==3)*log(preds[,"p_a"]))
  RPS[ii] <- rps(obs = preds[,"Outcome"], pred = preds[,c(1,2,3)])$rps # RPS for the current value of xi
}
plot(xi_test,LS) # Plot the log scores
plot(xi_test,RPS) # Plot the ranked probability scores


#### SECTION 4.3.2 - SIGNIFICANCE TESTS ON GAMMA AND RHO ####
# (i) Gamma
data <- filter(all_matches,Week < 376) # Training data
# Define negative log-likelihood under null distribution, without home advantage:
nullnegloglik <- function(params, goals_home, goals_visitor,
                          team_home, team_visitor, param_skeleton){
  
  plist <- relist(params, param_skeleton)
  plist$defense <- c(sum(plist$defense)*-1, plist$defense) 
  names(plist$defense)[1] <- names(plist$attack[1])
  
  lambda_home <- exp(plist$attack[team_home] + plist$defense[team_visitor]) # Removed plist$home
  lambda_visitor <- exp(plist$attack[team_visitor] + plist$defense[team_home])
  
  # The log-likelihood
  log_lik_home <- dpois(goals_home, lambda = lambda_home, log=TRUE)
  log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log=TRUE)
  log_lik <- sum(log_lik_home + log_lik_visitor)
  
  return((-1)*log_lik) # make the result negative so it can be minimised by R.
}
# Now optimise:
parameter_list <- list(attack = rep(0.12, n_teams),
                       defense = rep(0.01, n_teams-1)) # Initial values excluding gamma
names(parameter_list$attack) <- all_teams # Names
names(parameter_list$defense) <- all_teams[-1] # No \beta_1
optim_res_null <- optim(par = unlist(parameter_list), 
                   fn=nullnegloglik,
                   goals_home = data$FTHG,
                   goals_visitor = data$FTAG,
                   team_home = data$HomeTeam, team_visitor = data$AwayTeam,
                   param_skeleton=parameter_list, method = 'BFGS', 
                   hessian = F)

l0 <- -optim_res_null$value # We optimised the negative log-likelihood; we need the positive version
l1 <- -optim_res.lee$value # The alternative is the independent Poisson
LRT <- -2*(l0-l1) # Likelihood ratio test statistic is 404.7

# Under H_0 (\gamma=0), the LRT follows a chisq(1) distribution
qchisq(0.95,1) # Critical value 3.84
1-pchisq(LRT,1) # p-value

# (ii) Rho
# Compare between independent and dependent Poisson models
l0 <- -optim_res.lee$value # Null model: independent (Lee)
l1 <- -optim_res.dep$value # Alternative model: dependent
LRT <- -2*(l0-l1) # Likelihood ratio test statistic is 9.79
qchisq(0.95,1) # Critical value 3.84
1-pchisq(LRT,1) # p-value 0.0018

#### SECTION 4.4 - VALIDATION ####

# Validation using rest of eighth season #
# Define joint PMFs of each model
Leeprob <- function(h,a,lambda,mu){ # Independent Poisson, no rho
  return(dpois(h,lambda)*dpois(a,mu))
} 
DCprob <- function(h,a,lambda,mu,rho){ # Dependent Poisson onwards
  return(tau(home=h,away=a,lambda = lambda, mu = mu, rho = rho)*dpois(h,lambda)*dpois(a,mu))
}

# Define scoring rules - we forecast probabilities of home win, draw and away win and compare them with the observed outcome
LS <- function(v){ # Log score (LS)
  H <- v[1] # Calculated probability of home win
  D <- v[2] # Draw
  A <- v[3] # Away win
  O <- v[4] # Observed outcome
  return(I(O==1)*log(H) + I(O==2)*log(D) + I(O==3)*log(A)) # Log of probability of observed outcome
}

require(verification) # Contains an RPS function, rps()
RPS <- function(v){ # Ranked Probability Score (RPS)
  O <- v[4] # Observed outcome
  P <- matrix(v[c(1,2,3)], nrow = 1, ncol = 3) # rps() takes a matrix input containing forecast probabilities
  return(rps(obs = O, pred = P)$rps)
}

#### Independent Poisson ####
preds.lee <- matrix(nrow = m.end-m.start+1, ncol = 6) # Matrix to contain the probability forecasts and scores from each match
preds.lee[,4] <- all_matches$Outcome[m.start:m.end] # Fill the outcome column with the observed outcomes (1=home;2=draw;3=away)
colnames(preds.lee) = c("p_h","p_d","p_a","Outcome","LS","RPS") # Label the columns
# First three columns for prob forecasts; last two for scores

for (m in m.start:m.end) { # Loop for computing the LS and RPS from each match
  HomeTeam <- as.character(all_matches$HomeTeam[m]) # Label the home team from match m
  AwayTeam <- as.character(all_matches$AwayTeam[m]) # Label the away team from match m
  lambdaH <- exp(alphas.Lee[HomeTeam]+betas.Lee[AwayTeam]+gamma.Lee) # Expected home score - extract each team's parameters
  muA <- exp(alphas.Lee[AwayTeam]+betas.Lee[HomeTeam]) # Expected away score
  
  probs <- matrix(nrow=7,ncol=7) # Empty matrix to store probability forecasts of scorelines up to 5-5
  colnames(probs) = rownames(probs) = c((0:5),"6+") # Label the columns and rows with the goal counts
  for (x in 0:5) {
    for (y in 0:5) {
      probs[x+1,y+1] <- Leeprob(x,y,lambdaH,muA) # Main body
    }
  }
  # Use dependence assumption for high-scoring games to fill in the rest
  probs[7,(1:6)] <- dpois((0:5),muA)*(1-ppois(5,lambdaH)) # Events where the home team scores 6 or more
  probs[(1:6),7] <- dpois((0:5),lambdaH)*(1-ppois(5,muA)) # Events where the away team scores 6 or more
  probs[7,7] <- 1-sum(probs,na.rm = T) # Probability of both teams scoring more than 5 goals to complete the table
  
  # Group together the probabilities corresponding to home wins, draws and away wins and a draw and enter them in the preds matrix
  n <- m-m.start+1 # The required row of the preds matrix
  preds.lee[n,"p_h"] <- sum(probs*lower.tri(probs)) # Home win
  preds.lee[n,"p_d"] <- sum(diag(probs)) # Draw
  preds.lee[n,"p_a"] <- sum(probs*upper.tri(probs)) # Away win
  preds.lee[n,"LS"] <- LS(preds.lee[n,1:4]) # Log score
  preds.lee[n,"RPS"] <- RPS(preds.lee[n,1:4]) # RPS
}
# Overall scores found by taking the sum of the columns in the preds matrix
LS.lee <- sum(preds.lee[,"LS"]) # Overall LS
RPS.lee <- sum(preds.lee[,"RPS"]) # Overall RPS

#### Dependent Poisson onwards ####
# Use DCprob instead of Leeprob for rest of models
# Create preds matrix with new suffix for storing new forecasts, for instance:
preds.dep <- matrix(nrow = m.end-m.start+1, ncol = 6)
preds.dep[,4] <- all_matches$Outcome[m.start:m.end]
colnames(preds.dep) = c("p_h","p_d","p_a","Outcome","LS","RPS")

# Parameters:
# For dependent Poisson use alphas.dep instead of alphas.Lee, etc
# For time-dependent models, extract the estimates from just before Week 376 (when validation period starts) and forecast using those
# For example:
alphas.val <- alphas.unw[375,]
betas.val <- betas.unw[375,]
gamma.val <- gammas.unw[375]
rho.val <- rhos.unw[375]

# We can calculate log scores and RPS for each model in this way

#### SECTION 4.5 - REFITTING ####
#### Dependent Poisson ####
data <- filter(all_matches, Week < 418) # Cut off at Week 418 when the ninth season begins

# Optimise log-likelihood for dependent model
# Hessian = T so we can get standard errors
optim_res2.dep <- optim(par = unlist(parameter_list), fn=dc_negloglik,
                    goals_home = data$FTHG,
                    goals_visitor = data$FTAG,
                    team_home = data$HomeTeam, team_visitor = data$AwayTeam,
                    param_skeleton=parameter_list, method = 'BFGS', hessian = T)

# Storing new estimates #
parameter_est <- relist(optim_res2.dep$par, parameter_list) # Organise into list format
parameter_est$defense <- c(-1*sum(parameter_est$defense), parameter_est$defense) # \hat\beta_1
names(parameter_est$defense)[1] <- names(parameter_est$attack[1])
alphas.dep <- parameter_est$attack
betas.dep <- parameter_est$defense
gamma.dep <- parameter_est$home
rho.dep <- parameter_est$rho

# Get the standard errors of each parameter from the Hessian
H <- optim_res2$hessian # Extract the Hessian from optim output
cov.matrix <- solve(H) # Invert it to get the information matrix (doesn't cover \beta_1)
sd.err <- sqrt(diag(cov.matrix)) # Square root along diagonal gives standard errors
est.sd.err <- relist(sd.err, parameter_list) # List the estimated standard errors

# beta_1 is the sum of all the other betas
# Find its variance by adding up the covariances between all the other betas
Varb1 <- sum(cov.matrix[87:171,87:171])
est.sd.err$defense <- c(sqrt(Varb1),est.sd.err$defense) # Add it to the list
names(est.sd.err$defense)[1] <- names(est.sd.err$attack)[1] # Attach a name (AFC Wimbledon)

# We have variances of all parameters in \theta.
# The variance of e^{\theta} can be found by the delta method:
# Var[h(\theta)] = [h'(\theta)]^2 * Var[\hat\theta] where h(\theta)=e^{\theta}
#                = e^{2*\theta}   * Var[\hat\theta]

ExpVar <- exp(2*unlist(parameter_est)) * unlist(est.sd.err)^2 # Delta method to find variance of e^{\theta}
ExpSE <- sqrt(ExpVar) # Take square roots to get standard errors
est.sd.err.exp <- relist(ExpSE,parameter_est) # Make into a list
# We can now produce the estimates for e^{\alpha_k}, e^{\beta_k} and e^{\gamma} with their standard errors

PL.est <- data.frame("Team" = team.names.PL,
                     "Attack" = round(exp(parameter_est$attack[team.names.PL]),digits = 3),
                     "Att.se" = round(est.sd.err.exp$attack[team.names.PL],digits = 3),
                     "Defence" = round(exp(parameter_est$defense[team.names.PL]),digits = 3),
                     "Def.se" = round(est.sd.err.exp$defense[team.names.PL],digits = 3))

# Plot the new estimates
require(ggrepel)
ggplot(data = PL.est, mapping = aes(x=Attack, y=Defence, label=Team)) +
  geom_point() +
  geom_label_repel() +
  theme(axis.title = element_text(size=14)) +
  theme(title = element_text(size=14)) +
  annotate("text", x = 1.25, y = 0.6, label = "Weaker\n defence", colour = "red") +
  annotate("text", x = 2, y = 0.9, label = "Stronger\n attack", colour = "red")

#### Unweighted Dixon-Coles ####
# Update parameters to be used for simulation
alphas.unw.sim <- alphas.unw[418,] # Extract values from week 418 rather than 375
betas.unw.sim <- betas.unw[418,]
gamma.unw.sim <- gammas.unw[418]
rho.unw.sim <- rhos.unw[418]

H <- optim_res.unw$hessian
cov.matrix <- solve(H+diag(0.0001,nrow(H))) # H is originally singular, so we add a small ridge along the diagonal to make it invertible.
# Follow same process as before to get the standard errors at Week 418:
sd.err <- sqrt(diag(cov.matrix)) # Square root along diagonal
est.sd.err <- relist(sd.err, parameter_list) # List
Varb1 <- sum(cov.matrix[87:171,87:171]) # S.E. of \hat\beta_1 done separately
est.sd.err$defense <- c(sqrt(Varb1),est.sd.err$defense)
names(est.sd.err$defense)[1] <- names(est.sd.err$attack)[1]

ExpVar <- exp(2*unlist(optim_res.unw$par)) * unlist(est.sd.err)^2 # Delta method to find variance of exponentiated parameters
ExpSE <- sqrt(ExpVar) # Take square roots to get standard errors
est.sd.err.exp <- relist(ExpSE,parameter_list) # Make into a list

PL.est.unw <- data.frame("Team" = team.names.PL,
                         "Attack" = round(exp(alphas.sim[team.names.PL]), digits = 3),
                         "se.att" = round(est.sd.err.exp$attack[team.names.PL],digits = 3),
                         "Defence" = round(exp(betas.sim[team.names.PL]), digits = 3),
                         "se.def" = round(est.sd.err.exp$defense[team.names.PL],digits = 3))
# Plot
ggplot(data = PL.est, mapping = aes(x=Attack, y=Defence, label=Team)) +
  geom_point() +
  geom_label_repel() +
  theme(axis.title = element_text(size=14)) +
  theme(title = element_text(size=14)) +
  annotate("text", x = 0.95, y = 0.63, label = "Weaker\n defence", colour = "red") +
  annotate("text", x = 1.63, y = 1.25, label = "Stronger\n attack", colour = "red")

# Get up to date values for both weighted Dixon-Coles models:
alphas.wei1.sim <- alphas.wei1[418,]
betas.wei1.sim <- betas.wei1[418,]
gamma.wei1.sim <- gammas.wei1[418]
rho.wei1.sim <- rhos.wei1[418]

alphas.wei2.sim <- alphas.wei2[418,]
betas.wei2.sim <- betas.wei2[418,]
gamma.wei2.sim <- gammas.wei2[418]
rho.wei2.sim <- rhos.wei2[418]

# Standard errors found in the same way as above.


#### SIMULATION ####

# Create matrix of scores (strings) to sample from, up to 6-6 using stringr package
scores <- matrix(nrow=7,ncol=7)
for (i in 0:6) {
  for (j in 0:6) {
    scores[i+1,j+1] <- str_c(as.character(i), "-", as.character(j))
  }
}

# Initialise the following to store information from the simulations from each model:

Points.dep = Points.unw = Points.wei1 = Points.wei2 = matrix(0L,nrow = 100, ncol = 20) # Total points of each team
colnames(Points.dep) = colnames(Points.unw) = colnames(Points.wei1)= colnames(Points.wei2) = team.names.PL
Score.preds.dep = Score.preds.unw = Score.preds.wei1 = Score.preds.wei2 = array(dim = c(380,7,100)) # Simulated score of each matches over the 100 iterations
colnames(Score.preds.dep) = colnames(Score.preds.unw) = colnames(Score.preds.wei1) = colnames(Score.preds.wei2) =
  c("MatchID","HomeTeam","AwayTeam","HomeScore","AwayScore","PredOC","RealOC") # Will show match ID (from 1 to 380),
  # team names, simulated score, predicted and observed outcomes (home win/draw/away win)
# For when the whole season has been simulated:
Sim.rank.dep = Sim.rank.unw = Sim.rank.wei1 = Sim.rank.wei2 = matrix(nrow = 20, ncol = 100) # Simulated rankings for each team from each iteration
rownames(Sim.rank.dep) = rownames(Sim.rank.unw) = rownames(Sim.rank.wei1) = rownames(Sim.rank.wei2) = Teams.ord # One row per team (in order of observed ranking)
colnames(Sim.rank.dep) = colnames(Sim.rank.unw) = colnames(Sim.rank.wei1) = colnames(Sim.rank.wei2) = 1:100 # One column per iteration
correl.dep = correl.unw = correl.wei1 = correl.wei2 = rep(NA,100) # Correlation between each simulated set of rankings and the observed ranking

# Sim loop

m.start <- 11873 # Set the match index of the first game of 2017/18
m.end <- 12252 # Same for the last game

for (i in 1:100) {
  set.seed(i)
  # Empty arrays to store probability forecasts for each scoreline up to 5-5 in a match. One layer for each match.
  probs.dep = probs.unw = probs.wei1 = probs.wei2 = array(dim=c(7,7,380))
  # Label the columns and rows with the goal counts
  colnames(probs.dep) = rownames(probs.dep) = c((0:5),"6+")
  colnames(probs.unw) = rownames(probs.unw) = c((0:5),"6+")
  colnames(probs.wei1) = rownames(probs.wei1) = c((0:5),"6+")
  colnames(probs.wei2) = rownames(probs.wei2) = c((0:5),"6+")
  
  # Fill in "RealOC" column with observed column from data.
  Score.preds.dep[,"RealOC",i] = Score.preds.unw[,"RealOC",i] = 
    Score.preds.wei1[,"RealOC",i] = Score.preds.wei2[,"RealOC",i] = all_matches$Outcome[m.start:m.end]
  
  for (m in m.start:m.end) {
    n <- m-m.start+1 # n = match counter within the 2017/18 season
    Score.preds.dep[n,"MatchID",i] = Score.preds.unw[n,"MatchID",i] = 
      Score.preds.wei1[n,"MatchID",i] = Score.preds.wei2[n,"MatchID",i] = n # Enter match index into Score.preds matrices
    
    HomeTeam <- as.character(all_matches$HomeTeam[m]) # Identify home team name
    AwayTeam <- as.character(all_matches$AwayTeam[m]) # Identify away team name
    
    Score.preds.dep[n,"HomeTeam",i] = Score.preds.unw[n,"HomeTeam",i] = 
      Score.preds.wei1[n,"HomeTeam",i] = Score.preds.wei2[n,"HomeTeam",i] = HomeTeam # Enter home team into Score.preds matrices
    
    Score.preds.dep[n,"AwayTeam",i] = Score.preds.unw[n,"AwayTeam",i] = 
      Score.preds.wei1[n,"AwayTeam",i] = Score.preds.wei2[n,"AwayTeam",i] = AwayTeam # Enter away team into Score.preds matrices
    
    # Expected home scores under each model
    lambdaH.dep <- exp(alphas.dep[HomeTeam]+betas.dep[AwayTeam]+gamma.dep) # Dependent Poisson
    lambdaH.unw <- exp(alphas.unw.sim[HomeTeam]+betas.unw.sim[AwayTeam]+gamma.unw.sim) # Unweighted DC
    lambdaH.wei1 <- exp(alphas.wei1.sim[HomeTeam]+betas.wei1.sim[AwayTeam]+gamma.wei1.sim) # 1st weighted DC
    lambdaH.wei2 <- exp(alphas.wei2.sim[HomeTeam]+betas.wei2.sim[AwayTeam]+gamma.wei2.sim) # 2nd weighted DC
    # Expected away scores under each model
    muA.dep <- exp(alphas.dep[AwayTeam]+betas.dep[HomeTeam]) # Dependent Poisson
    muA.unw <- exp(alphas.unw.sim[AwayTeam]+betas.unw.sim[HomeTeam]) # Unweighted DC
    muA.wei1 <- exp(alphas.wei1.sim[AwayTeam]+betas.wei1.sim[HomeTeam]) # 1st weighted DC
    muA.wei2 <- exp(alphas.wei2.sim[AwayTeam]+betas.wei2.sim[HomeTeam]) # 2nd weighted DC
    
    
    for (x in 0:5) { # Fill in the main body of the probability forecasts table for match n of the season
      for (y in 0:5) {
        probs.dep[x+1,y+1,n] <- DCprob(x,y,lambdaH.dep,muA.dep,rho.dep) # Models in same order
        probs.unw[x+1,y+1,n] <- DCprob(x,y,lambdaH.unw,muA.unw,rho.unw.sim)
        probs.wei1[x+1,y+1,n] <- DCprob(x,y,lambdaH.wei1,muA.wei1,rho.wei1.sim)
        probs.wei2[x+1,y+1,n] <- DCprob(x,y,lambdaH.wei2,muA.wei2,rho.wei2.sim)
      }
    }
    
    # Extreme outcomes (more than 5 goals for either team)
    probs.dep[7,(1:6),n] <- dpois((0:5),muA.dep)*(1-ppois(5,lambdaH.dep)) # Home
    probs.dep[(1:6),7,n] <- dpois((0:5),lambdaH.dep)*(1-ppois(5,muA.dep)) # Away
    probs.dep[7,7,n] <- 1-sum(probs.dep[,,n],na.rm = T) # More than 5 for each team
    
    probs.unw[7,(1:6),n] <- dpois((0:5),muA.unw)*(1-ppois(5,lambdaH.unw))
    probs.unw[(1:6),7,n] <- dpois((0:5),lambdaH.unw)*(1-ppois(5,muA.unw))
    probs.unw[7,7,n] <- 1-sum(probs.unw[,,n],na.rm = T)
    
    probs.wei1[7,(1:6),n] <- dpois((0:5),muA.wei1)*(1-ppois(5,lambdaH.wei1))
    probs.wei1[(1:6),7,n] <- dpois((0:5),lambdaH.wei1)*(1-ppois(5,muA.wei1))
    probs.wei1[7,7,n] <- 1-sum(probs.wei1[,,n],na.rm = T)
    
    probs.wei2[7,(1:6),n] <- dpois((0:5),muA.wei2)*(1-ppois(5,lambdaH.wei2))
    probs.wei2[(1:6),7,n] <- dpois((0:5),lambdaH.wei2)*(1-ppois(5,muA.wei2))
    probs.wei2[7,7,n] <- 1-sum(probs.wei2[,,n],na.rm = T)
    
    # Sample a scoreline based on the estimated probabilities in each model. This is our simulated score.
    pred.dep <- sample(scores,1,prob = probs.dep[,,n])
    pred.unw <- sample(scores,1,prob = probs.unw[,,n])
    pred.wei1 <- sample(scores,1,prob = probs.wei1[,,n])
    pred.wei2 <- sample(scores,1,prob = probs.wei2[,,n])
    
    # From each simulated scoreline, extract the home and away scores as numbers
    h.dep <- as.numeric(str_sub(pred.dep,1,1))
    a.dep <- as.numeric(str_sub(pred.dep,3,3))
    h.unw <- as.numeric(str_sub(pred.unw,1,1))
    a.unw <- as.numeric(str_sub(pred.unw,3,3))
    h.wei1 <- as.numeric(str_sub(pred.wei1,1,1))
    a.wei1 <- as.numeric(str_sub(pred.wei1,3,3))
    h.wei2 <- as.numeric(str_sub(pred.wei2,1,1))
    a.wei2 <- as.numeric(str_sub(pred.wei2,3,3))
    
    # Enter simulated home scores into the Score.preds matrix for each model
    Score.preds.dep[n,"HomeScore",i] <- h.dep
    Score.preds.unw[n,"HomeScore",i] <- h.unw
    Score.preds.wei1[n,"HomeScore",i] <- h.wei1
    Score.preds.wei2[n,"HomeScore",i] <- h.wei2
    
    # Enter simulated away scores into the Score.preds matrix for each model
    Score.preds.dep[n,"AwayScore",i] <- a.dep
    Score.preds.unw[n,"AwayScore",i] <- a.unw
    Score.preds.wei1[n,"AwayScore",i] <- a.wei1
    Score.preds.wei2[n,"AwayScore",i] <- a.wei2
    
    # Allocate points according to the predicted scores from each model
    if(h.dep>a.dep) {Points.dep[i,HomeTeam] <- Points.dep[i,HomeTeam]+3; Score.preds.dep[n,"PredOC",i] <- 1
    } else if(h.dep<a.dep) {Points.dep[i,AwayTeam] <- Points.dep[i,AwayTeam]+3; Score.preds.dep[n,"PredOC",i] <- 3
    } else if(h.dep==a.dep) {Points.dep[i,HomeTeam] <- Points.dep[i,HomeTeam]+1; Points.dep[i,AwayTeam] <- Points.dep[i,AwayTeam]+1; Score.preds.dep[n,"PredOC",i] <- 2
    }
    if(h.unw>a.unw) {Points.unw[i,HomeTeam] <- Points.unw[i,HomeTeam]+3; Score.preds.unw[n,"PredOC",i] <- 1
    } else if(h.unw<a.unw) {Points.unw[i,AwayTeam] <- Points.unw[i,AwayTeam]+3; Score.preds.unw[n,"PredOC",i] <- 3
    } else if(h.unw==a.unw) {Points.unw[i,HomeTeam] <- Points.unw[i,HomeTeam]+1; Points.unw[i,AwayTeam] <- Points.unw[i,AwayTeam]+1; Score.preds.unw[n,"PredOC",i] <- 2
    }
    if(h.wei1>a.wei1) {Points.wei1[i,HomeTeam] <- Points.wei1[i,HomeTeam]+3; Score.preds.wei1[n,"PredOC",i] <- 1
    } else if(h.wei1<a.wei1) {Points.wei1[i,AwayTeam] <- Points.wei1[i,AwayTeam]+3; Score.preds.wei1[n,"PredOC",i] <- 3
    } else if(h.wei1==a.wei1) {Points.wei1[i,HomeTeam] <- Points.wei1[i,HomeTeam]+1; Points.wei1[i,AwayTeam] <- Points.wei1[i,AwayTeam]+1; Score.preds.wei1[n,"PredOC",i] <- 2
    }
    if(h.wei2>a.wei2) {Points.wei2[i,HomeTeam] <- Points.wei2[i,HomeTeam]+3; Score.preds.wei2[n,"PredOC",i] <- 1
    } else if(h.wei2<a.wei2) {Points.wei2[i,AwayTeam] <- Points.wei2[i,AwayTeam]+3; Score.preds.wei2[n,"PredOC",i] <- 3
    } else if(h.wei2==a.wei2) {Points.wei2[i,HomeTeam] <- Points.wei2[i,HomeTeam]+1; Points.wei2[i,AwayTeam] <- Points.wei2[i,AwayTeam]+1; Score.preds.wei2[n,"PredOC",i] <- 2
    }
    
  }
  
  # Once all the matches have been predicted:
  # Compute simulated ranking of each team in the i-th iteration under each model
  Sim.teams.dep <- names(sort(Points.dep[i,], decreasing = T)) # Team names in descending order of simulated points
  for (r in 1:20) { # Creates the predicted numerical rankings as was done with the media predictions
    Sim.rank.dep[r,i] <- which(Sim.teams.dep==Teams.ord[r])
  }
  Sim.teams.unw <- names(sort(Points.unw[i,], decreasing = T)) # Team names in descending order of simulated points
  for (r in 1:20) {
    Sim.rank.unw[r,i] <- which(Sim.teams.unw==Teams.ord[r])
  }
  Sim.teams.wei1 <- names(sort(Points.wei1[i,], decreasing = T)) # Team names in descending order of simulated points
  for (r in 1:20) {
    Sim.rank.wei1[r,i] <- which(Sim.teams.wei1==Teams.ord[r])
  }
  Sim.teams.wei2 <- names(sort(Points.wei2[i,], decreasing = T)) # Team names in descending order of simulated points
  for (r in 1:20) {
    Sim.rank.wei2[r,i] <- which(Sim.teams.wei2==Teams.ord[r])
  }
  
  # Calculate correlation (adjusted Spearman) between predicted rankings and observed for each model:
  correl.dep[i] <- AdjSpr(Sim.rank.dep[,i])
  correl.unw[i] <- AdjSpr(Sim.rank.unw[,i])
  correl.wei1[i] <- AdjSpr(Sim.rank.wei1[,i])
  correl.wei2[i] <- AdjSpr(Sim.rank.wei2[,i])
}

#### Comparison with media predictions ####

# Compute mean adjusted Spearman correlation over the 100 simulations for each simulated model
Comparison <- round(c(mean(correl.dep),mean(correl.unw),mean(correl.wei1),
                      mean(correl.wei2),AdjSpr(Rank.TS),AdjSpr(Rank.Gua),
                      AdjSpr(Rank.BBC)),digits = 3)
names(Comparison) <- c("Dep Pois","Unweighted DC","1st weighted DC","2nd weighted DC","talkSPORT","Guardian","BBC")
Comparison # Print the mean scores of our simulations alongside the journalist scores
c(sd(correl.dep),sd(correl.unw),sd(correl.wei1),sd(correl.wei2)) # Standard errors

#### Create table for showing predicted vs real rankings ####
Avg.pts.dep = Avg.pts.unw = Avg.pts.wei1 = Avg.pts.wei2 = rep(NA,20) # To store the average no. of points scored by each team over our simulations
names(Avg.pts.dep) = names(Avg.pts.unw) = names(Avg.pts.wei1) = names(Avg.pts.wei2) = team.names.PL # Attach team names
for (team in team.names.PL) { # Go through each team and find the average no. of simulated points under each model
  Avg.pts.dep[team] <- round(mean(Points.dep[,team]), digits = 2)
  Avg.pts.unw[team] <- round(mean(Points.unw[,team]), digits = 2)
  Avg.pts.wei1[team] <- round(mean(Points.wei1[,team]), digits = 2)
  Avg.pts.wei2[team] <- round(mean(Points.wei2[,team]), digits = 2)
} # We will need this to make an overall set of rankings out of our simulation results

# Order of teams based on average points under each model:
Avg.teams.dep <- names(sort(Avg.pts.dep,decreasing = T)) # Teams ordered by avg simulated points
Avg.teams.unw <- names(sort(Avg.pts.unw,decreasing = T)) # Teams ordered by avg simulated points
Avg.teams.wei1 <- names(sort(Avg.pts.wei1,decreasing = T)) # Teams ordered by avg simulated points
Avg.teams.wei2 <- names(sort(Avg.pts.wei2,decreasing = T)) # Teams ordered by avg simulated points

Avg.rank.dep = Avg.rank.unw = Avg.rank.wei1 = Avg.rank.wei2 = rep(NA,20) # To store average simulated rankings for each model
for (r in 1:20) { # Create the average predicted numerical rankings of each team under each model
  Avg.rank.dep[r] <- which(Avg.teams.dep==Teams.ord[r])
  Avg.rank.unw[r] <- which(Avg.teams.unw==Teams.ord[r])
  Avg.rank.wei1[r] <- which(Avg.teams.wei1==Teams.ord[r])
  Avg.rank.wei2[r] <- which(Avg.teams.wei2==Teams.ord[r])
}

# Display these simulated rankings alongside the observed and media-predicted rankings
Pred.comp <- data.frame("Team"=Teams.ord, # Teams in descending order of real-life ranking
                        "Actual"=1:20, # Real rankings
                        "BBC"=Rank.BBC, # BBC predictions
                        "Guardian"=Rank.Gua, # Guardian predictions
                        "talkSPORT"=Rank.TS, # talkSPORT predictions
                        "DP"=Avg.rank.dep, # Dependent Poisson predictions
                        "DP.error"=(1:20)-Avg.rank.dep, # Error w.r.t. real table
                        "UDC"=Avg.rank.unw, # Unweighted DC predictions
                        "UDC.error"=(1:20)-Avg.rank.unw,
                        "WDC1"=Avg.rank.wei1, # Weighted DC (1) predictions
                        "WDC1.error"=(1:20)-Avg.rank.wei1,
                        "WDC2"=Avg.rank.wei2, # Weighted DC (2) predictions
                        "WDC2.error"=(1:20)-Avg.rank.wei2)

AddPlusSigns <- function(vector){ # To add plus signs in front of positive errors in the above table
  ifelse(vector > 0, str_c("+", vector), vector)
} # Makes interpretation easier
# Apply to each error column:
Pred.comp$DP.error <- AddPlusSigns(Pred.comp$DP.error)
Pred.comp$UDC.error <- AddPlusSigns(Pred.comp$UDC.error)
Pred.comp$WDC1.error <- AddPlusSigns(Pred.comp$WDC1.error)
Pred.comp$WDC2.error <- AddPlusSigns(Pred.comp$WDC2.error)








