# Dualfoil Battery Simulation Device
See [Cap from the ORNL-CEES team](https://github.com/ORNL-CEES/Cap "Github - ORNL-CEES/Cap") for the bigger picture

## Included in this repository ...
 +  a quick guide to understanding Dualfoil5 
 +  a python package that works with Pycap to realistically model batteries
 +  graphical representation of sample output data generated from Dualfoil5.2
 +  tests to verify the functionality of the `battery` package

## Installation
 1. Make sure you have downloaded the latest non-beta version of [docker](https://www.docker.com/products/overview "Get Docker")
 2. Pull the Cap image `dalg24/cap` by copying and pasting the following command into your shell:
 ```
 docker pull dalg24/cap
 ```
 3. Now clone this repository with the following command:
 ```
 git clone https://github.com/iSchomer/Dualfoil_Storage_Device.git
 ```
 4. This should download a `Dualfoil_Storage_Device` directory. Change into it:
 ```
 cd Dualfoil_Storage_Device/
 ```
 5. Lastly, run a docker container with the pycap image:
 ```
 docker run --rm -it -p 8888:8888 -v $PWD:/notebooks dalg24/cap
 ```
 6. We use a Jupyter Notebook as our interface; To get there, open up a browser and type in the ip address of your machine into the web address bar.


## Basic Use
 + Dualfoil is an instance of pycap.EnergyStorageDevice
 + `Dualfoil` can run individual legs of a simulation, return output values, and graph those values in 3D
 + For more complicated simulations, a `Dualfoil` object can be passed into one of Pycap's experiments, such as `CyclicChargeDischarge`

 Here is an example of some basic operations:
 ```python
 from pycap import PropertyTree, Charge
 from battery import *
 fom energy_storage_device import Dualfoil

 # can run dualfoil sim manually 
 device = Dualfoil(path='docker/')
 print(device.get_voltage())
 # charge for 1 minute
 device.evolve_one_time_step_constant_current(60, 10.0)
 print(device.get_voltage())

 # can run with pycap.Charge
 # see help(pycap) for more details
 ptree = PropertyTree()
 ptree.put_double('time_step', 30.0)
 ptree.put_string('charge_mode', 'constant_current')
 ptree.put_double('charge_current', 10.0)
 ptree.put_string('charge_stop_at_1', 'voltage_greater_than')
 ptree.put_double('charge_voltage_limit', 4.6)
 ptree.put_bool('charge_voltage_finish', True)
 ptree.put_double('charge_voltage_finish_max_time',
                  120)  # 2 minutes after end time
 ptree.put_double('charge_voltage_finish_current_limit',
                  5.0)  # or no lower than 5 amps 
 
 from pycap import initialize_data, plot_data
 data = initialize_data()
 # run the simulation and plot the data
 cccv = pycap.Charge(ptree)
 cccv.run(device, data)
 plot_data(data)
 ```
