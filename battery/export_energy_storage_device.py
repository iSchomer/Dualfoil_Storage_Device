__all__ = ['Dualfoil']

import subprocess
from pycap import EnergyStorageDevice
import df_manip
import df_grapher

class Dualfoil(EnergyStorageDevice):

    """
    Instance of EnergyStorageDevice, using dualfoil5.1 to simulate battery behavior
    
    Attributes
    ----------
    outbot : OutputManager
        object that maintains output data
    filePath : str
        Full or relative path to dualfoil files
    fileName : str
        Name of the input file used by dualfoil
    restart : bool
        Indicates whether the next leg or simulation will be from a restart
    """

    def __init__(self, Path='', Input='dualfoil5.in'):
        
        """
        Parameters
        ----------
        Path : str, optional
            Full or relative path to dualfoil files
        Input : str, optional
            Name of the input file used by dualfoil
        """

        if not Path.endswith('/') and len(Path) != 0:
            Path += '/'
        self.filePath = Path
        self.fileName = Input
        self.restart = False

        self.outbot = df_manip.OutputManager(path=self.filePath)

        # compile dualfoil
        if self.filePath == '':
            subprocess.call('make clean && make dualfoil', shell=True)
        else:
            subprocess.call('cd %s && make clean && make dualfoil'
                            % self.filePath, shell=True)

    # use when wanting to start a new simulation from scratch
    def reset(self):
        """
        Resets restart value
        Clears all output data lists
        """
        self.restart = False
        self.outbot.reset()

    def run(self):
        """
        Runs dualfoil and updates `restart` value accordingly
        """

        #update restart value
        if not self.restart:
            self.restart = True

        if self.filePath == '':
            subprocess.call('./dualfoil', shell=True)
        else:
            subprocess.call('cd %s && ./dualfoil' % self.filePath, shell=True)

    def set_filepath(self, path):
        """
        assigns the parameter to `filePath`
        
        Parameters
        ----------
        path : str
            Full or relative path to dualfoil files
        """
        if not path.endswith('/'):
            path += '/'
        self.filePath = path
        self.outbot.set_filepath(self.filePath)

    def get_voltage(self):
        v = self.outbot.get_voltage()
        if v == -1:
            # no output
            # run a 0 time simulation to get the starting voltage
            self.evolve_one_time_step_constant_current(0, -20)
            v = self.outbot.get_voltage()
            self.reset()
        return v

    def get_current(self):
        c = self.outbot.get_current()
        if c == -1:
            # run a 0 time sim to get initial current
            self.evolve_one_time_step_constant_voltage(0, 0)
            c = self.outbot.get_current()
            self.reset()
        return c

    def get_total_time(self):
        """
        Get the total dualfoil simulation runtime
        
        Returns
        -------
        float
            the total time in minutes
        """
        rstFile = open('%sdf_restart.dat' % self.filePath, 'r')
        tmp = rstFile.readline()
        tmp = tmp.lstrip().split()
        # get timestep in minutes
        ts = float(tmp[1]) / 60
        rstFile.close()
        return ts

    def get_time_step(self):
        """
        Get the dualfoil's last-used time step value
        
        Returns
        -------
        float
            time step in minutes
        """
        rstFile = open('%sdf_restart.dat' % self.filePath, 'r')
        tmp = rstFile.readline()
        tmp = tmp.lstrip().split()
        # get timestep in minutes
        ts = float(tmp[0]) / 60
        rstFile.close()
        return ts

    # main sim functions------------------------------------------------
    # time_step must be converted from seconds to minutes
    def evolve_one_time_step_constant_current(self, time_step, current):
        # pycap currents: - for discharge, + for charge
        # dualfoil currents: + for discharge, - for charge
        current = -current

        time_step = time_step / 60
        df_manip.add_new_leg(current, time_step, 1, "constant current",
                             path=self.filePath, restart=self.restart)
        self.run()
        self.outbot.update_output()

    def evolve_one_time_step_constant_voltage(self, time_step, voltage):
        time_step = time_step / 60
        df_manip.add_new_leg(voltage, time_step, 0, "constant voltage",
                             path=self.filePath, restart=self.restart)
        self.run()
        self.outbot.update_output()

    def evolve_one_time_step_constant_power(self, time_step, power):
        time_step = time_step / 60
        df_manip.add_new_leg(power, time_step, -2, "constant power",
                             path=self.filePath, restart=self.restart)
        self.run()
        self.outbot.update_output()

    def evolve_one_time_step_constant_load(self, time_step, load):
        time_step = time_step / 60
        df_manip.add_new_leg(load, time_step, -3, "constant load",
                             path=self.filePath, restart=self.restart)
        self.run()
        self.outbot.update_output()

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

        df_manip.add_new_leg(current, cutoff, 2, "constant current",
                             path=self.filePath, restart=self.restart)
        self.run()
        self.outbot.update_output()

    def evolve_one_time_step_linear_current(self, time_step, current,
                                            divisor=10):
        """
        Impose a linearly increasing current by dividing into substeps
        of constant current, and evolve in time.
        
        Parameters
        ----------
        time_step : float
            the time step in seconds
        current : float
            the final current in amperes
        divisor : int, optional
            the number of substeps to be taken to create the current increase
        """
        # pycap currents: - for discharge, + for charge
        # dualfoil currents: + for discharge, - for charge
        current = -current

        time_step = time_step / 60
        if self.restart:
            linCurr = self.get_current()
        else:
            linCurr = 0.0
        ts = time_step / (divisor+1)
        ttot = ts
        change = (current-linCurr) / divisor
        

        while ttot <= time_step:
            df_manip.add_new_leg(linCurr, ts, 1, "linear current",
                                 path=self.filePath, restart=self.restart)
            # get next timestep values
            ttot += ts
            linCurr += change
            self.run()
            self.outbot.update_output()

    def evolve_one_time_step_linear_voltage(self, time_step, voltage,
                                            divisor=10):
        """
        Impose a linearly increasing voltage by dividing into substeps
        of constant voltage, and evolve in time.
        
        Parameters
        ----------
        time_step : float
            the time step in seconds
        voltage : float
            the final voltage in volts
        divisor : int, optional
            the number of substeps to be taken to create the voltage increase
        """
        time_step = time_step / 60
        linearV = self.get_voltage()
        ts = time_step / (divisor+1)
        tott = ts
        change = (voltage-linearV) / divisor

        while tott <= time_step:
            df_manip.add_new_leg(linearV, ts, 0, "linear voltage",
                                 path=self.filePath, restart=self.restart)
            # get next timestep values
            tott += ts
            linearV += change
            self.run()
            self.outbot.update_output()

    # start_point parameter added to the following 2 functions
    # because they cannot be extracted
    def evolve_one_time_step_linear_power(self, time_step, power,
                                          start_point=0.0, divisor=10):
        """
        Impose a linearly increasing power by dividing into substeps
        of constant power, and evolve in time.
        
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
        time_step = time_step / 60
        ts = time_step / (divisor+1)
        tott = ts
        change = (power-start_point) / divisor
        linPow = start_point

        while tott <= time_step:
            df_manip.add_new_leg(linPow, ts, -2, "linear power",
                                 path=self.filePath, restart=self.restart)
            # get next timestep values
            tott += ts
            linPow += change
            self.run()
            self.outbot.update_output()

    def evolve_one_time_step_linear_load(self, time_step, load,
                                         start_point=0.0, divisor=10):
        """
        Impose a linearly increasing load by dividing into substeps
        of constant load, and evolve in time.
        
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
        time_step = time_step / 60
        ts = time_step / (divisor+1)
        tott = ts
        change = (load-start_point) / divisor
        linLoad = start_point

        while tott <= time_step:
            df_manip.add_new_leg(linLoad, ts, -3, "linear load",
                                 path=self.filePath, restart=self.restart)
            # get next timestep values
            tott += ts
            linLoad += change
            self.run()
            self.outbot.update_output()
