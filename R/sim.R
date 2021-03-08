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
sim <- function(sim.params) {

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

  for (t in 1:sim.params$n.timesteps) {
    #### Update Mental Model ####
    exp.util <- ComputeUtility(agent.state, candidate.state$stances, org.state)
    # if (sim.params$election.cycle > 0 && (t %% sim.params$election.cycle) == 0) {
      #### Election ####
      ballots <- CastBallots(exp.util)
      vote.shares <- factor(ballots, levels=seq(sim.params$n.candidates))
      election.winner <- CountBallots(ballots, sim.params$n.candidates)
      # TODO: Option for no elections where agents only affect by own choice.
    # }
    #### Evaluate mental model ####
    agent.state$cplx <- ComputeComplexity(agent.state, org.state)
    agent.state$fit <- ComputeFit(candidate.state$utility, exp.util,
                          election.winner, ballots, sim.params)
    agent.state$sig <- ComputeSignificance(agent.state, org.state, sim.params)
    agent.state$desr <- ComputeDesirability(agent.state, sim.params)
    #### Update Results ####
    #XXX Is growing df costly? Allocate up front?
    results$candpop <- rbind(results$candpop, tabulate(vote.shares, sim.params$n.candidates))
    results$elected <- cbind(results$elected, election.winner)
    results$issuepop < rbind(results$issuepop, ComputeIssuePop(agent.state, org.state, sim.params))
    results$orgpop <- rbind(results$orgpop, tabulate(agent.state$heuristic, sim.params$n.heuristics))
    results$modelfit <- rbind(results$modelfit, tabulate(cut(agent.state$fit, sim.params$nbins), sim.params$nbins))
    results$modelcplx <- rbind(results$modelcplx, tabulate(agent.state$cplx, sim.params$n.dims))
    results$modelutil <- rbind(results$modelutil, ComputePopExpUtil(exp.util, election.winner, sim.params))
    #### Evaluate neighbor's models ####
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
  }
  return(results)
}
