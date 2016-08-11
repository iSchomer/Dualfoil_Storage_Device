__all__ = ['Dualfoil']

import subprocess
from pycap import EnergyStorageDevice
import df_manip

class Dualfoil(EnergyStorageDevice):

    """
    Instance of EnergyStorageDevice, using dualfoil5.1 to simulate battery behavior
    
    Attributes
    ----------
    inbot : InputManager
        object that maintains input data
    outbot : OutputManager
        object that maintains output data
    file_path : str
        Full or relative path to dualfoil files
    file_name : str
        Name of the input file used by dualfoil
    """

    def __init__(self, path=None, input_name='dualfoil5.in', restart_capable=True):
        
        """
        NOTE: Altough the `path` parameter is optional,
              this variable must be included if dualfoil
              files are not in the same directory.
        
        Parameters
        ----------
        path : str, optional
            Full or relative path to dualfoil files
        input_name : str, optional
            Name of the input file used by dualfoil
        restart_capable : bool, optional
            Indicates whether user wants to use dualfoil 5.1
            (capable of performing restarts) or 5.2 (incapable of restarts)
        """

        if path is not None:
            if not path.endswith('/'):
                path += '/'
        self.file_path = path
        self.inbot = df_manip.InputManager(path, input_name, restart_capable)
        self.outbot = df_manip.OutputManager(path)

        self.fileName = input_name
        self.restart_capable = restart_capable

    # use when wanting to start a new simulation from scratch
    def reset(self):
        """
        Resets input and output managers
        Clears all output data lists
        """
        self.inbot.reset()
        self.outbot.reset()

    def run(self):
        """
        Runs dualfoil standard simulation and updates `restart`
        value accordingly.
        """

        cmd = './dualfoil'

        if self.file_path is None:
            directory = None
        else:
            directory = self.file_path
        # call subprocess, hiding error stream from user
        subprocess.call(cmd, cwd=directory, stderr=subprocess.PIPE)

    def run_impedance(self):
        """
        Runs dualfoil with an impedance mode.
        """
        # make sure dualfoil5.2 is being used
        if self.restart_capable:
            raise RuntimeError('This option is only compatible with dualfoil5.2')

        # change input file to run impedance mode
        self.inbot.add_impedance()
        self.run()

    def get_voltage(self):
        v = self.outbot.get_voltage()
        if v == -1:
            # no output
            # run a 0 second simulation to get the starting voltage
            self.evolve_one_time_step_constant_current(0, -5)
            v = self.outbot.get_voltage()
            self.reset()
        return v

    def get_current(self):
        c = self.outbot.get_current()
        if c == -1:
            # run a 0 second sim to get initial current
            self.evolve_one_time_step_constant_voltage(0, 0)
            c = self.outbot.get_current()
            self.reset()
        return c

    def get_time_step(self):
        """
        Get the dualfoil's last-used time step value
        
        Returns
        -------
        float
            time step in seconds (-1 if not applicable)
        """
        # this feature is only for 5.1
        if not self.restart_capable:
            raise RuntimeError('This feature is only compatable with dualfoil5.1')

        rstFile = open('%sdf_restart.dat' % self.file_path, 'r')
        tmp = rstFile.readline()
        tmp = tmp.lstrip().split()
        # get timestep in seconds
        ts = float(tmp[0])
        rstFile.close()
        return ts

    # main sim functions------------------------------------------------
    # time_step must be converted from seconds to minutes
    def evolve_one_time_step_constant_current(self, time_step, current):
        # pycap currents: - for discharge, + for charge
        # dualfoil currents: + for discharge, - for charge
        current = -current

        time_step = time_step / 60
        try:
            self.inbot.add_new_leg(current, time_step, 1, "constant current")
            self.run()
            self.outbot.update_output()
        except FileNotFoundError as err:
            # handles incorrect path
            err = str(err)
            err = err.split('] ')[-1]
            print("Error when executing dualfoil: ", err)
        except RuntimeError as err:
            print(err)
            raise

    def evolve_one_time_step_constant_voltage(self, time_step, voltage):
        time_step = time_step / 60
        try:
            self.inbot.add_new_leg(voltage, time_step, 0, "constant voltage")
            self.run()
            self.outbot.update_output()
        except FileNotFoundError as err:
            # handles incorrect path
            err = str(err)
            err = err.split('] ')[-1]
            print("Error when executing dualfoil: ", err)
        except RuntimeError as err:
            print(err)
            raise

    def evolve_one_time_step_constant_power(self, time_step, power):
        time_step = time_step / 60
        # must negate power to align with Cap:
        # - power decreases voltage
        # + power increases voltage
        # Dualfoil is opposite
        power = -power
        try:
            self.inbot.add_new_leg(power, time_step, -2, "constant power")
            self.run()
            self.outbot.update_output()
        except FileNotFoundError as err:
            # handles incorrect path
            err = str(err)
            err = err.split('] ')[-1]
            print("Error when executing dualfoil: ", err)
        except RuntimeError as err:
            print(err)
            raise

    def evolve_one_time_step_constant_load(self, time_step, load):
        time_step = time_step / 60
        try:
            self.inbot.add_new_leg(load, time_step, -3, "constant load")
            self.run()
            self.outbot.update_output()
        except FileNotFoundError as err:
            # handles incorrect path
            err = str(err)
            err = err.split('] ')[-1]
            print("Error when executing dualfoil: ", err)
        except RuntimeError as err:
            print(err)
            raise

    def evolve_to_voltage_constant_current(self, current, cutoff):
        """
        Impose the current and evolve until a cutoff voltage.
        
        Parameters
        ----------
        current : float
            the current in amperes
        cutoff : float
            the cutoff voltage in amperes
        """
        # pycap currents: - for discharge, + for charge
        # dualfoil currents: + for discharge, - for charge
        current = -current

        try:
            self.inbot.add_new_leg(current, cutoff, 2, "constant current")
            self.run()
            self.outbot.update_output()
        except FileNotFoundError as err:
            # handles incorrect path
            err = str(err)
            err = err.split('] ')[-1]
            print("Error when executing dualfoil: ", err)
        except RuntimeError as err:
            print(err)
            raise

    def evolve_one_time_step_linear_current(self, time_step, current,
                                            divisor=4):
        """
        Impose a linearly increasing current by dividing into substeps
        of constant current, and evolve in time.

        Only applicable to dualfoil5.1

        Parameters
        ----------
        time_step : float
            the time step in seconds
        current : float
            the final current in amperes
        divisor : int, optional
            the number of substeps to be taken to create the current increase
        """
        # make sure we have the correct dualfoil
        if not self.restart_capable:
            raise RuntimeError('This simulation option is only available for dualfoil5.1')

        # pycap currents: - for discharge, + for charge
        # dualfoil currents: + for discharge, - for charge
        current = -current

        time_step = time_step / 60
        total_time = time_step
        if self.inbot.from_restart:
            linear_current = self.get_current()
        else:
            linear_current = 0.0
        time_step = time_step / (divisor+1)
        run_time = time_step
        change = (current-linear_current) / divisor
        
        try:
            while run_time <= total_time:
                self.inbot.add_new_leg(linear_current, time_step, 1,
                                       "linear current")
                # get next timestep values and run
                run_time += time_step
                linear_current += change
                self.run()
                self.outbot.update_output()
        except FileNotFoundError as err:
            # handles incorrect path
            err = str(err)
            err = err.split('] ')[-1]
            print("Error when executing dualfoil: ", err)
        except RuntimeError as err:
            print(err)
            raise
        # indicate computation progress
        print('.', end='')

    def evolve_one_time_step_linear_voltage(self, time_step, voltage,
                                            divisor=4):
        """
        Impose a linearly increasing voltage by dividing into substeps
        of constant voltage, and evolve in time.

        Only applicable to dualfoil5.1

        Parameters
        ----------
        time_step : float
            the time step in seconds
        voltage : float
            the final voltage in volts
        divisor : int, optional
            the number of substeps to be taken to create the voltage increase
        """
        # make sure we have the correct dualfoil
        if not self.restart_capable:
            raise RuntimeError('This simulation option is only available for dualfoil5.1')

        time_step = time_step / 60
        total_time = time_step
        linear_voltage = self.get_voltage()
        time_step = time_step / (divisor+1)
        run_time = time_step
        change = (voltage-linear_voltage) / divisor

        try:
            while run_time <= total_time:
                self.inbot.add_new_leg(linear_voltage, time_step, 0,
                                       "linear voltage")
                # get next timestep values and run
                run_time += time_step
                linear_voltage += change
                self.run()
                self.outbot.update_output()
        except FileNotFoundError as err:
            # handles incorrect path
            err = str(err)
            err = err.split('] ')[-1]
            print("Error when executing dualfoil: ", err)
        except RuntimeError as err:
            print(err)
            raise
        # indicate computation progress
        print('.', end='')

    # start_point parameter added to the following 2 functions
    # because they cannot be extracted
    def evolve_one_time_step_linear_power(self, time_step, power,
                                          start_point=0.0, divisor=4):
        """
        Impose a linearly increasing power by dividing into substeps
        of constant power, and evolve in time.

        Only applicable to dualfoil5.1

        Parameters
        ----------
        time_step : float
            the time step in seconds
        power : float
            the final power in watts
        start_point : float, optional
            the initial power in watts
        divisor : int, optional
            the number of substeps to be taken to create the power increase
        """
        # make sure we have the correct dualfoil
        if not self.restart_capable:
            raise RuntimeError('This simulation option is only available for dualfoil5.1')

        # must negate power to align with Cap:
        # - power decreases voltage
        # + power increases voltage
        # Dualfoil is opposite
        power = -power

        time_step = time_step / 60
        total_time = time_step
        time_step = time_step / (divisor+1)
        run_time = time_step
        change = (power-start_point) / divisor
        linear_power = start_point

        
        try:
            while run_time <= total_time:
                self.inbot.add_new_leg(linear_power, time_step, -2, "linear power")
                # get next timestep values and run
                run_time += time_step
                linear_power += change
                self.run()
                self.outbot.update_output()
        except FileNotFoundError as err:
            # handles incorrect path
            err = str(err)
            err = err.split('] ')[-1]
            print("Error when executing dualfoil: ", err)
        except RuntimeError as err:
            print(err)
            raise
        # indicate computation progress
        print('.', end='')

    def evolve_one_time_step_linear_load(self, time_step, load,
                                         start_point=0.0, divisor=4):
        """
        Impose a linearly increasing load by dividing into substeps
        of constant load, and evolve in time.

        Only applicable to dualfoil5.1

        Parameters
        ----------
        time_step : float
            the time step in seconds
        load : float
            the final load in ohms
        start_point : float, optional
            the initial load in ohms
        divisor : int, optional
            the number of substeps to be taken to create the current increase
        """
        # make sure we have the correct dualfoil
        if not self.restart_capable:
            raise RuntimeError('This simulation option is only available for dualfoil5.1')

        time_step = time_step / 60
        total_time = time_step
        time_step = time_step / (divisor+1)
        run_time = time_step
        change = (load-start_point) / divisor
        linear_load = start_point

        
        try:
            while run_time <= total_time:
                self.inbot.add_new_leg(linear_load, time_step, -3, "linear load")
                # get next timestep values and run
                run_time += time_step
                linear_load += change
                self.run()
                self.outbot.update_output()
        except FileNotFoundError as err:
            # handles incorrect path
            err = str(err)
            err = err.split('] ')[-1]
            print("Error when executing dualfoil: ", err)
        except RuntimeError as err:
            print(err)
            raise
        # indicate computation progress
        print('.', end='')