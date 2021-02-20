# Author: ....

#' Runs one "monte carlo" simulation trial of the ABM.
#'
#' @param sim.params List of options governing simulation size and behavior:
#'    Value ranges are marked as [lower, upper)[default].
#'    n.candidates: Number of alternatives/candidates in the field [1,inf)[10]
#'    n.dims: Maximum number of political issues affecting utility [1, 32)[5]
#'    n.agents: Number of agents to simulate [1, inf)[200]
#'    n.heuristics: Number of distinct combinations of issues allowed [1, 2^n_dims][32]
#'    n.timesteps: Number of ticks to run the simulation [1, inf)[250]
#'    election.cycle: Ticks between elections. 0 cancels elections. [0, n_timesteps][0]
#'    conservatism: Agent motivation to preserve current way of thinking [0, 1][0]
#'    ego: Agent selfishness vs selflessness in utility calculation [0, 1][1]
#'    aversion: Agent aversion to complexity [0, 5)[1]
#'    org.aversion: Political org aversion to complexity (0, 5)[1]
#' @return A list of many results matrixes
#' @export
simulate <- function(sim.params) {

  sim.params <- FillDefaultSimParams(sim.params)
  ValidateSimParams(sim.params)

  #### Initialize Model Variables ####

  agent.state <- data.frame(prefs=I(AssignPrefs(sim.params)),
                            heuristic=AssignHeuristics(sim.params),
                            switching.cost=rep(0, sim.params$n.agents))
  candidate.state <- data.frame(stances=I(GenerateStances(sim.params)))
  candidate.state$utility <- rowSums(candidate.state)
  org.state <- data.frame(heuristic=I(GenerateHeuristics(sim.params)),
                          membership=tabulate(agent.state$heuristic, sim.params$n.heuristics))
  results <- AllocateResultsDF(sim.params)

  #### Simulation Main Loop ####

  for (t in 0:sim.params$n.timesteps) {
    #### Update Mental Model ####
    exp.util <- ComputeUtility(agent.state, candidate.state, org.state)
    if (election_cycle > 0 && mod(t, election_cycle) == 0) {
      #### Election ####
      ballots <- CastBallots(exp.util)
      vote.shares <- factor(ballots, levels=seq(sim.params$n.candidates))
      election.winner <- CountBallots(ballots, sim.params$n.candidates)
      results$candpop[t,] <- tabulate(vote.shares, sim.params$n.candidates)
      results$elected[t] <- election.winner
      # TODO: Option for no elections where agents only affect by own choice.
    }
    #### Evaluate mental model ####
    agent.complexity <- ComputeComplexity(agent_h)
  #   agent_r <- fit(cand_y, y_expected, winner, ballots, ego, n_dims) # n_agents by 1
  #   agent_p <- significance(agent_h, agent_b) # n_agents by 1
  #   agent_m <- desirability(agent_p, agent_r, agent_k, aversion) # n_agents by 1
  #   ### Update non-election graphs ###
  #   dims[t,] <- rowSums(agent_h)
  #   fits[t,] <- tabulate(cut(agent_r, fit_bins), length(fit_bins))
  #   simplicity[t,] <- tabulate(agent_k, n_dims)
  #   utility[t,] <- tabulate(cut(y_expected[winner,]/n_dims, util_bins), length(util_bins))
  #   ### Evaluate neighbor's models ###
  #   tmp_h <- encounter_neighbors(agent_h, n_agents)
  #   tmp_y <- model(agent_b, tmp_h, cand_x)
  #   tmp_k <- complexity(tmp_h)
  #   tmp_r <- fit(cand_y, tmp_y, winner, ballots, ego, n_dims)
  #   tmp_p <- significance(tmp_h, agent_b)
  #   tmp_m <- desirability(tmp_p, tmp_r, tmp_k, aversion)
  #   ### Change mental model ###
  #   delta <- similarity(agent_h, tmp_h, agent_b, n_dims)
  #   validate_that(sum(delta >= 0) == n_agents, sum(delta <= 1) == n_agents)
  #   will_update <- tmp_m * (1+delta) > agent_m * (1+gamma*stubbornness)
  #   stubbornness <- ossify(stubbornness, will_update, n_agents)
  #   agent_h[,will_update] <- tmp_h[,will_update]
  #   # Transcribe dimension combinations into orgs
  #   orgs <- heuristic2org(agent_h, n_dims)
  #   org[,t] <- orgs
  #   orgsvec <- tabulate(orgs, 2^n_dims)
  #   orgdens[,t] <- orgsvec[initial_org_indexes]
  }
  return(results)
}
