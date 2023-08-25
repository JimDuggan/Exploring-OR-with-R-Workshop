library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(igraph)
library(furrr)
library(future)


create_network_df<- function(df){
  network_df1 <- tibble(FromAgent=as.integer(df[,1]),
                        ToAgent=as.integer(df[,2]))
  network_df2 <- tibble(FromAgent=as.integer(df[,2]),
                        ToAgent=as.integer(df[,1]))
  
  network_df <- dplyr::bind_rows(network_df1, network_df2) %>%
    dplyr::arrange(FromAgent,ToAgent)
  network_df
}

network_to_list <- function(N,net){
  agent_net <- vector(mode="list",length=N)
  names(agent_net) <- as.character(1:N)
  contacts <- group_split(net,FromAgent)
  names(contacts) <- map_chr(contacts,~first(.x$FromAgent))
  for(nam in names(contacts)){
    agent_net[[nam]] <- contacts[[nam]]
  }
  agent_net
}

# Generates a random network
generate_random_network <- function(N=1000,
                                    edge_mult=2,
                                    seed=F,
                                    seedVal=100){
  if(seed) set.seed(seedVal)
  graph       <- sample_gnm(n=N,m=N*edge_mult)
  gr_df       <- data.frame(get.edgelist(graph))
  net_df      <- create_network_df(gr_df) 
  sim_network <- network_to_list(N,net_df)
  list(N=N,
       graph=graph,
       network=sim_network)
}

#-------------------------------------------------------------------------------
# Function to run a single simulation
#-------------------------------------------------------------------------------
run_sim <- function(run_id=1,
                    net,
                    adopters=c(1),
                    end_time=50,
                    prob_spread=0.10,
                    seed=FALSE,
                    seed_val=100){
  if(seed) set.seed(seed_val)
  
  agent_ids <- 1:net$N
  
  # Create a tibble for agent information, this is stored for each time step
  agents <- tibble(
    run_id              = rep(run_id,length(agent_ids)),
    sim_time            = rep(0L,length(agent_ids)),
    agent_id            = agent_ids,
    num_connections     = rep(0L,length(agent_ids)),
    pa_state            = rep(FALSE,length(agent_ids)),
    a_state             = rep(FALSE,length(agent_ids)),
    change_to_a         = rep(FALSE,length(agent_ids)),
    change_state_time   = rep(NA,length(agent_ids))
  )
  
  # Initialise all agent's state, including adopter(s)
  agents[,"num_connections"] <- as.integer(degree(net$graph))
  agents[agents$agent_id %in% adopters,"pa_state"]    <- FALSE
  agents[agents$agent_id %in% adopters,"a_state"]       <- TRUE
  agents[!(agents$agent_id %in% adopters),"pa_state"]   <- TRUE
  agents[!(agents$agent_id %in% adopters),"a_state"]    <- FALSE
  agents[agents$agent_id==adopters,"change_state_time"] <- 0L
  
  # Create transitions table to log state changes for agents
  transitions <- tibble(
    agent_id        = integer(),
    sim_time        = integer(),
    state_pa_to_a   = logical()
  )
  
  # Record state changes for initial adopters
  transitions <- dplyr::add_row(transitions,
                                sim_time=0L,
                                agent_id=as.integer(adopters),
                                state_pa_to_a=TRUE)
  
  time <- 1:end_time
  agents$run_id <- run_id
  
  # Initialise tibble that will contain all states over time
  trace_sim <- agents
  
  # Start the simulation loop
  for(t in time){
    agents$sim_time <- as.integer(t)
    
    # Get the list of potential adopters that have connections
    # Return value as a vector (for convenience)
    pa_list <- agents[agents$pa_state==TRUE & agents$num_connections > 0,
                      "agent_id",
                      drop=T]
    
    # If there are potential adopters, loop through these
    if(length(pa_list) > 0){
      
      for(a in pa_list){
        # Find all neighbours of agent a
        neighbours <- net$network[as.character(a)][[1]][,"ToAgent",drop=T]
        
        # Find which neighbours are adoptors (get agent ids as a vector)
        neigh_adopters <- agents[agents$agent_id %in% neighbours & 
                                   agents$a_state ==TRUE,
                                 "agent_id",
                                 drop=T]
        
        # Calculate the probability of adoption using Reed-Frost
        lambda <- 1 - (1 - prob_spread) ^ length(neigh_adopters)
        
        # Simulate random draw to decide if transition happens
        rn <- runif(1)
        if(rn < lambda ){
          # Mark agent for state change
          agents[agents$agent_id==a,"change_to_a"] <- TRUE
          # Record state change time
          agents[agents$agent_id==a,"change_state_time"] <- t
        }
      }
      
      # End of day --> Now Flip states for those that have changed from pa to a
      targets <- agents[agents$change_to_a==TRUE,"agent_id",drop=TRUE] 
      agents[agents$change_to_a==TRUE,"pa_state"]     <- FALSE
      agents[agents$change_to_a==TRUE,"a_state"]      <- TRUE
      agents[agents$change_to_a==TRUE,"change_to_a"]  <- FALSE
      
      # Log the flip event as a transition
      transitions <- dplyr::add_row(transitions,
                                    sim_time=as.integer(t),
                                    agent_id=as.integer(targets),
                                    state_pa_to_a=TRUE)
      
    }
    
    # Append agents states to overall list
    trace_sim <- dplyr::bind_rows(trace_sim,
                                  agents)
  } #End of simulation
  
  # Organise and return all results, including transition information
  agent_sim_full <- dplyr::left_join(trace_sim,
                                     transitions,
                                     by=c("sim_time","agent_id"))
}

#-------------------------------------------------------------------------------
# Run one simulation
#-------------------------------------------------------------------------------
net <- generate_random_network(1000,seed=T)
names(net)
table(degree(net$graph))

res <- run_sim(net = net,
               end_time = 80)
dplyr::glimpse(res)

res %>% dplyr::filter(agent_id %in% 1:10,state_pa_to_a==TRUE) %>%
  dplyr::select(run_id,agent_id,sim_time,state_pa_to_a)


ar <- res %>%
  dplyr::group_by(sim_time) %>%
  dplyr::summarise(Adoptions=sum(state_pa_to_a,na.rm = T)) %>%
  dplyr::ungroup()
ar

p1 <- ggplot(ar,aes(x=sim_time,y=Adoptions))+geom_point()+geom_line()
p1

states <- res %>% 
  dplyr::group_by(sim_time) %>% 
  dplyr::summarise(PA=sum(pa_state),A=sum(a_state)) %>%
  tidyr::pivot_longer(PA:A,
                      names_to = "State",
                      values_to = "Number")
p2 <- ggplot(states,aes(x=sim_time,y=Number,colour=State,fill=State))+geom_area()
p2

not_adopted <- res %>% 
  dplyr::filter(sim_time==80,pa_state==TRUE) %>%
  dplyr::pull(num_connections) %>%
  table()
not_adopted


#-------------------------------------------------------------------------------
# Run multiple simulations
#-------------------------------------------------------------------------------
net <- generate_random_network(1000,seed=T)
conns <- degree(net$graph)
inits <- c(Lowest=which(conns == 1)[1], 
           Highest=which(conns == max(conns))[1],
           Mean=which(conns == floor(mean(conns)))[1])
inits

plan(multisession)

NSim <- 9
sims  <- furrr::future_map2(1:NSim,
                            rep(inits,NSim/3),
                            ~run_sim(run_id = .x,
                                     net    = net,
                                     adopters = .y,
                                     end_time = 80),
                            .options = furrr_options(seed = T)) %>%
  dplyr::bind_rows()
sims

ar <- sims %>%
  dplyr::group_by(run_id,sim_time) %>%
  dplyr::summarise(Adoptions=sum(state_pa_to_a,na.rm = T)) %>%
  dplyr::mutate(run_desc=case_when(
    run_id %% 3 == 1 ~ "Lowest Connections",
    run_id %% 3 == 2 ~ "Highest Connections",
    run_id %% 3 == 0 ~ "Mean Connections")) %>%
  dplyr::ungroup()

p1 <- ggplot(ar,aes(x=sim_time,y=Adoptions,group=run_id,colour=run_desc))+
  geom_point()+geom_line()+facet_wrap(~run_desc,nrow = 3)
p1

quants <- ar %>% 
  dplyr::group_by(run_desc,sim_time) %>%
  dplyr::summarise(Q05=quantile(Adoptions,0.05),
                   Q95=quantile(Adoptions,0.95),
                   Mean=mean(Adoptions)) %>%
  dplyr::ungroup()

quants

p4 <- ggplot(quants,aes(x=sim_time,y=Mean,colour=run_desc,group=run_desc))+
  geom_ribbon(aes(ymin=Q05,ymax=Q95,fill=run_desc,group=run_desc),alpha=0.2)+
  geom_line(size=2)

p4




