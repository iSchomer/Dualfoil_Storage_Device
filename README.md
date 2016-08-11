# Dualfoil Battery Simulation Device
See [Cap from the ORNL-CEES team](https://github.com/ORNL-CEES/Cap "Github - ORNL-CEES/Cap") for the bigger picture.

## Included in this repository ...
 +  a quick guide to understanding Dualfoil5 
 +  a Python package that wraps Dualfoil's functionality into a usable and versatile class
 +  sample simulations generated from the Python package independently and within Cap
 +  tests to verify the functionality of the Python package

## Installation
 1. Pull the Cap image `dalg24/cap` from Docker with the following command:

 ```
 $ docker pull dalg24/cap
 ```

 2. Now clone this repository from Github to your system:

 ```
 $ git clone https://github.com/iSchomer/Dualfoil_Storage_Device.git
 ```

 3. This should download a `Dualfoil_Storage_Device` directory. Run a Docker container with the pycap image inside the cloned directory:

 ```
 $ docker run --rm -it -p 8888:8888 -v $PWD:/notebooks dalg24/cap
 ```

 4. Pycap and this code can be accessed by opening up a browser and going to `http://<ip_address>:8888` where `ip_address` is the IP address of the machine with the Docker daemon running.

 5. Lastly, once inside the Jupyter browser, compile the dualfoil executables:
 ```
 $ cd docker/dualfoil5-1
 $ make dualfoil
 $ cd ../dualfoil5-2
 $ make
 ```

Once you have completed these steps, you will have access to pycap as well as all the code included in this repository.


## Basic Use
`Dualfoil` is child of `pycap.EnergyStorageDevice`. It has functionality both independently and within Cap.

+ Within this Python package
   + Alter Dualfoil's input file
   + Run individual parts of a simulation through class methods
   + Parse output files and stores data in a dictionary
   + Plot Dualfoil-specific profile data in 3D
+ Within Cap
   + Access input and output using keywords
   + Run an `Experiment`, which combines base simulation types to simulate established electrochemical measurement techniques
   + Plot output for clear visualization of results

Here is an example of some basic operations:

 ```python
 # add battery module to path
 import sys
 sys.path.append('/notebooks')

 from battery import Dualfoil
 from pycap import PropertyTree, Charge
 from pycap import initialize_data, plot_data

 # can run dualfoil sim manually 
 device = Dualfoil(path='docker/dualfoil5-1')
 print(device.get_voltage())
 # charge for 1 minute with a constant current of 2 amperes
 time_step = 60
 current = 2.0
 device.evolve_one_time_step_constant_current(time_step, current)
 print(device.get_voltage())

 # can run with Charge from pycap
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
 
 data = initialize_data()
 device.reset()
 # run the simulation and plot the data
 charge = Charge(ptree)
 charge.run(device, data)
 plot_data(data)
 ```
