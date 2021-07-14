This repository contains the `code` and `latex` files for the master thesis. 

The simulation scenarios (based on Rojas-Perilla et al. 2020) in Julia can be found in the `code/utils/simulations` folder. Note that there is a separate file for the GB2 distribution. The models that generate the simulations are in `sim_models.jl` and the simulation process, which includes the sampling step from the generated population, in `simulations.jl`

The models that I have used in the simulation study so far are in `code/model/stan/simulations.`The model with the shift tranformation is `log_shift_model.stan`. When drawing from the posterior predictive distribution, we need two cases: no areas out-of-sample (`log_shift_y_pred.stan`) and some areas out of sample (`log_shift_pred_miss.stan`). 

In `code/notebooks` are the tests that I have done. Two are particularly important. `simulation_study` contains posterior predictive checks for the log-shifted model and also some comparisons to the EBP under transformations. In `skewed likelihoods` I try out a group of skewed likelihood with `brms`. The results can be found in the appendix of the thesis. Additional auxiliary scripts imported into the noteboks can be found in `config`, `ops` and `utils`.





