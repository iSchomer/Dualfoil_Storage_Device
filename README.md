# Dualfoil Battery Simulation Device
See [Cap from the ORNL-CEES team](https://github.com/ORNL-CEES/Cap "Github - ORNL-CEES/Cap") for the bigger picture.

## Included in this repository ...
 +  a quick guide to understanding Dualfoil5 
 +  a python package that works with Pycap to realistically model batteries
 +  graphical representation of sample output data generated from Dualfoil5.2
 +  tests to verify the functionality of the `battery` package

## Installation
 1. Pull the Cap image `dalg24/cap` by copying and pasting the following command into your shell:

 ```
 docker pull dalg24/cap
 ```

 2. Now clone this repository with the following command:

 ```
 git clone https://github.com/iSchomer/Dualfoil_Storage_Device.git
 ```

 3. This should download a `Dualfoil_Storage_Device` directory. Follow this and change into the `docker` directory.
 4. Make clean, and then use the command `make dualfoil` to ensure that the program compiles on your system.
 5. Lastly, run a docker container with the pycap image:

 ```
 docker run --rm -it -p 8888:8888 -v $PWD:/notebooks dalg24/cap
 ```

 6. Pycap and this code can be accessed by opening up a browser and going to `http://<ip_address>:8888` where `ip_address` is the IP address of the machine with the Docker daemon running.

Once you have opened into the Jupyter notebook, you will have access to pycap as well as all the code included in this repository.


## Basic Use
 + Dualfoil is an instance of pycap.EnergyStorageDevice.
 + `Dualfoil` can run individual legs of a simulation, return output values, and graph those values in 3D.
 + For more complicated simulations, a `Dualfoil` object can be passed into one of Pycap's experiments, such as `CyclicChargeDischarge`

 Here is an example of some basic operations:
 ```python
 from pycap import PropertyTree, Charge
 from battery import *
 fom energy_storage_device import Dualfoil

 # can run dualfoil sim manually 
 device = Dualfoil(path='docker/')
 print(device.get_voltage())
 # charge for 1 minute with a constant current of 10 amperes
 device.evolve_one_time_step_constant_current(60, 10.0)
 print(device.get_voltage())

 # can run with pycap.Charge
 # see help(pycap) for more details
 ptree = PropertyTree()
 # Set up a simulation that charges until it reaches 4.6V
 ptree.put_double('time_step', 30.0)
 ptree.put_string('charge_mode', 'constant_current')
 ptree.put_double('charge_current', 10.0)
 ptree.put_string('charge_stop_at_1', 'voltage_greater_than')
 ptree.put_double('charge_voltage_limit', 4.6)
 # sim will hold end voltage until one of the end conditions are met:
 # 2 minutes have passed OR current falls below 5 amperes
 ptree.put_bool('charge_voltage_finish', True)
 ptree.put_double('charge_voltage_finish_max_time', 120)
 ptree.put_double('charge_voltage_finish_current_limit', 5.0)
 
 from pycap import initialize_data, plot_data
 data = initialize_data()
 device.reset()
 # run the simulation and plot the data
 charge_with_voltage_finish = pycap.Charge(ptree)
 charge_with_voltage_finish.run(device, data)
 plot_data(data)
 ```
