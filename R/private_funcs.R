#' @author Eric Chandler <echandler@uchicago.edu>
#' @description Helper functions for simulation code.
#' @keywords internal
#' @noRd

library("assertthat")
library("gtools")

#### Initialization Functions ####

FillDefaultSimParams <- function(params) {
  # Fills missing simulation parameters with default values
  #
  # Args:
  #   params: List of parameter names to values
  if (!("n.candidates" %in% names(params))) {params$n.candidates <- 10}
  if (!("n.dims" %in% names(params))) {params$n.dims <- 5}
  if (!("n.agents" %in% names(params))) {params$n.agents <- 200}
  if (!("n.heuristics" %in% names(params))) {params$n.heuristics <- 32}
  if (!("n.timesteps" %in% names(params))) {params$n.timesteps <- 250}
  if (!("election.cycle" %in% names(params))) {params$election.cycle <- 0}
  if (!("conservatism" %in% names(params))) {params$conservatism <- 0}
  if (!("ego" %in% names(params))) {params$ego <- 1}
  if (!("aversion" %in% names(params))) {params$aversion <- 1}
  if (!("org.aversion" %in% names(params))) {params$org.aversion <- 1}
  return(params)
}

ValidateSimParams <- function(params){
  # Asserts that simulation parameters have reasonable self-consistent values
  #
  # Args:
  #   params: List of options regarding the size and behavior of simulation
  assertthat::assert_that(assertthat::is.scalar(params$n.candidates) && params$n.candidates >= 1)
  assertthat::assert_that(assertthat::is.scalar(params$n.dims) && params$n.dims >= 1)
  err_msg <- "n.dims is only supported up to the largest integer width = 32"
  assertthat::see_if(params$n.dims < 32, msg = err_msg)
  assertthat::assert_that(assertthat::is.scalar(params$n.agents) && params$n.agents >= 1)
  assertthat::assert_that(assertthat::is.scalar(params$n.heuristics) && params$n.heuristics >= 1)
  # See AssignHeuristics note on maximum number of heuristics
  assertthat::assert_that(params$n.heuristics <= 2^(params$n.dims))
  assertthat::assert_that(assertthat::is.scalar(params$n.timesteps) && params$n.timesteps >= 1)
  assertthat::assert_that(assertthat::is.scalar(params$election.cycle) && params$election.cycle >= 0)
  assertthat::assert_that(assertthat::is.scalar(params$aversion) &&params$ aversion >= 0)
  assertthat::assert_that(assertthat::is.scalar(params$conservatism))
  assertthat::assert_that(params$conservatism >= 0 && params$conservatism <= 1)
}

AssignPrefs <- function(sim.params) {
  # Gives every agent random preferences on each issue
  #
  # Args:
  #   sim.params: List of global simulation configuration options
  # Returns:
  #   Matrix of agents' private preferences broken down by issue
  betasvec <- rnorm(sim.params$n.agents*sim.params$n.dims, .5, .2);
  betasvec[betasvec < 0] = 0;
  betasvec[betasvec > 1] = 1;
  betasmat <- matrix(betasvec, sim.params$n.agents, sim.params$n.dims)
  betasdf <- as.data.frame(betasmat)
  colnames(betasdf) <- paste("d", 1:sim.params$n.dims, sep="")
  rownames(betasdf) <- paste("a", 1:sim.params$n.agents, sep="")
  return(betasdf);
}

GenerateHeuristics <- function(sim.params) {
  # Gives a finite set of heuristics to be shared and re-used during the sim
  #
  # Args:
  #   sim.params: List of global simulation configuration options
  # Returns:
  #   Data frame of heuristic weights
  alphas <- rep(sim.params$org.aversion, times=sim.params$n.dims)
  df <- as.data.frame(rdirichlet(sim.params$n.heuristics, alphas))
  colnames(df) <- paste("d", 1:sim.params$n.dims, sep="")
  rownames(df) <- paste("h", 1:sim.params$n.heuristics, sep="")
  return(df)
}

AssignHeuristics <- function(sim.params) {
  # Gives every agent a mental model, randomly from the set of available models
  #
  # Args:
  #   sim.params: List of global simulation configuration options
  # Returns:
  #   Vector of each agents' assigned mental model
  sample.int(sim.params$n.heuristics, size=sim.params$n.agents, replace=TRUE)
}

GenerateStances <- function(sim.params) {
  # Randomly situates every candidate inside an abstract ideological space.
  #
  # Args:
  #   sim.params: List of global simulation configuration options
  # Returns:
  #   Dataframe (n.candidates x n.dims) of values between 0 and 1
  stancesvec <- runif(sim.params$n.candidates*sim.params$n.dims)
  stancesdf <- as.data.frame(matrix(stancesvec, sim.params$n.candidates, sim.params$n.dims))
  colnames(stancesdf) <- paste("d", 1:sim.params$n.dims, sep="")
  rownames(stancesdf) <- paste("c", 1:sim.params$n.candidates, sep="")
  return(stancesdf)
}

AllocateResultsDF <- function(sim.params) {
  candidate.names <- paste("c",1:sim.params$n.candidates, sep="")
  issue.names <- paste("d",1:sim.params$n.dims, sep="")
  org.names <- paste("h",1:sim.params$n.heuristics, sep="")
  candpop <- data.frame(matrix(numeric(), 0, sim.params$n.candidates,
                                           dimnames=list(c(), candidate.names)))
  orgpop <- data.frame(matrix(numeric(), 0, sim.params$n.heuristics,
                                dimnames=list(c(), org.names)))
  issuepop <- data.frame(matrix(numeric(), 0, sim.params$n.dims,
                                dimnames=list(c(), issue.names)))
  elected <- data.frame(matrix(integer(), 0, sim.params$n.candidates,
                               dimnames=list(c(), candidate.names)))
  modelfit <- data.frame(matrix(numeric(), 0, 21, dimnames=list(c(), seq(0,1,length.out=21))))
  modelcplx <- data.frame(matrix(numeric(), 0, 21, dimnames=list(c(), seq(0,1,length.out=21))))
  modelutil <- data.frame(matrix(numeric(), 0, 21, dimnames=list(c(), seq(0,1,length.out=21))))
  return(data.frame(candpop=I(candpop), orgpop=I(orgpop), issuepop=I(issuepop),
                    elected=I(elected), modelfit=I(modelfit), modelcplx=I(modelcplx),
                    modelutil=I(modelutil)))
}

#### Heuristic Evaluation Functions ####

ComputeComplexity <- function(agent.state) {
  # Counts the dimensions actively employed in a mental model
  #
  # When model dimensions were binary, we simply counted the nonzero dims.
  # In this implementation dims are scalar. I'll use inverse sum of squares as a
  # proxy.T= The intuition being that the heuristics sum to 1, the sum of squares
  # is maximized when 100% weight is concentrated in one dimension, and minimized
  # when weights are split evenly across dimensions.
  #
  # Args:
  #   heuristics: Matrix of each agents' current mental model
  # Returns:
  #   Vector of complexity score of each heuristic
  rowsum(agent.state$heuristic * agent.state$heuristic)
}

ComputeFit <- function(y.actual, y.expected, winner, ballots, ego, n.dims) {
  # Measures mental models' skill in predicting actualized utility
  #
  # Args:
  #   y.actual: Vector of candidates' emitted utility
  #   y.expected: Matrix of agents' expected utility per candidate
  #   winner: Integer index of candidate who won election
  #   ballots: Integer vector of who every agent voted for
  #   ego: On a [0,1] scale, are agents completely selfish or altruistic
  #   n.dims: Number of dimensions/issues in abstract ideological space
  # Returns:
  #   Vector of each agents' current mental model's fit
}

ComputeDesirability <- function(p, r, k, a) {
  # Measures the benefit/cost ratio of different mental models
}

ComputeSignificance <- function(heuristics, prefs) {
  # Measures the "effect size" of mental models
  #
  # Args:
  #   heuristics: Matrix of each agents' current mental model
  #   prefs: Matrix of agents' private preferences broken down by issue
  # Returns:
  #   Vector of significance values
}

ComputeSimilarity <- function(old.heuristics, new.heuristics, prefs, n.dims) {
  # Measures the alignment between old and new set of heuristics
  #
  # Args:
  #   old.heuristics: Matrix of each agents' current mental model
  #   new.heuristics: Matrix of proposed new mental model per agent
  #   prefs: Matrix of agents' private preferences broken down by issue
  #   n.dims: Number of dimensions/issues in ideological space
  # Returns:
  #   Vector of alignment scores
}

ComputeUtility <- function(agent.state, candidate.state, org.state) {
  # Agents evaluate expected utility with respect to each candidate's stances
  #
  # Args:
  #   prefs: Matrix of agents' private preferences broken down by issue
  #   heuristics: Matrix of agents' private mental model
  #   x.observed: Matrix of each candidates' stance on each issue
  # Returns:
  #   Matrix of each agent's expected utility by candidate
  full.heuristic <- org.state$heuristic[agent.state$heuristic,]
  # XXX: This cast might be expensive
  return(data.matrix(agent.state$prefs * full.heuristic) %*% t(candidate.state$stances))
}


#### Agent Behaviors ####

SocialLearning <- function(heuristics, n.agents) {
  # Agents communicate socially and encounter other heuristics
  #
  # Args:
  #   heuristics: Matrix of each agents' current mental model
  #   n.agents: Number of agents in simulation
}

CastBallots <- function(y.expected) {
  # Agents vote for the candidate that maximize their expected utility
  #
  # Args:
  #   y.expected: Matrix (n.agents x n.candidates) of every agent's expected
  #     utility with respect to every candidate
  # Returns:
  #   Integer vector of who every agent voted for
  max.col(t(y_expected))
}

CountBallots <- function(ballots, n.candidates) {
  # Compute the election winner by plurality
  #
  # Args:
  #   ballots: Integer vector of who every agent voted for
  #   n.candidates: Total number of candidates in the election
  # Returns:
  #   Integer identifier of the winning candidate
  as.integer(levels(ballots)[which.max(tabulate(ballots, n.candidates))])
}

EntrenchViews <- function(rate, will.update, n.agents) {
  # Increase agents' cognative barrier to accepting novel information
  #
  # Args:
  #   rate: Scalar incremental increase in switching cost
  #   will.update: Boolean vector of which agents' heuristics are changing
  #   n.agents: Number of agents
}
