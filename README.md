# Modelling Zika adverse pregnancy outcomes

### Summary

This repository contains code to accompany the paper: 

Eggo RM, Kucharski AJ. Expected Duration of Adverse Pregnancy Outcomes after Zika Epidemic . _Emerg Infect Dis_ 2018;24:1

### Guide to files

`simulation_model/Main_model_ZIK.R` Main file to load and fit data then plot results - calls following source file:

> `simulation_model/data_functions.R` Fits statistical models and estimates population-level dynamics.

### Dependencies

The model needs collated data from the [CDC data repository of publicly available Zika data](https://github.com/cdcepi/zika). Specifically, the `get.microcephy()` function in `data_functions.R` will look for a forked copy of this repository in `~/Documents/zika-1`.


