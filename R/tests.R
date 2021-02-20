# ### Evaluate mental model ###
# agent_k <- complexity(heuristics); # n_agents by 1
# agent_r <- fit(y_actual, y_expected, winner, ballots, ego, n_dims); # n_agents by 1
# agent_p <- significance(heuristics, betas); # n_agents by 1
# agent_m <- desirability(agent_p, agent_r, agent_k, aversion); # n_agents by 1
# ### Error checking ###
# validate_that(sum(y_expected >= 0) == n_agents*n_candidates, sum(y_expected <= n_dims) == n_agents*n_candidates);
# validate_that(sum(agent_r >= 0) == n_agents, sum(agent_r < 1) == n_agents);
# validate_that(sum(agent_k >= 1) == n_agents, sum(agent_k <= n_dims) == n_agents);
# validate_that(sum(agent_p >= 0) == n_agents, sum(agent_p <= n_dims) == n_agents);
# validate_that(sum(agent_m >= 0) == n_agents, sum(agent_m <= 1) == n_agents);
