# Dualfoil Battery Simulation Device
See [Cap from the ORNL-CEES team](https://github.com/ORNL-CEES/Cap "Github - ORNL-CEES/Cap") for the bigger picture

## Included in this repository ...
 +  a quick guide to understanding Dualfoil5 
 +  a python package that works with Pycap to realistically model batteries
 +  graphical representation of sample output data generated from Dualfoil5.2
 +  tests to verify the functionality of the `battery` package

## Basic Use
 1. Dualfoil5.1 is run through and managed by the `Dualfoil` object, which is found in `energy_storage_device.py` of the `battery` package
 2. `Dualfoil` can run individual legs of a simulation, return output values, and graph those values in 3D
 3. For more complicated simulations, a `Dualfoil` object can be passed into one of Pycap's experiments, such as `CyclicChargeDischarge`
