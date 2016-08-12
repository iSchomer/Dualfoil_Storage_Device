"""
Module for manipulating the input and output of dualfoil5.1
"""

__all__ = ['InputManager', 'extract_main_output',
           'extract_profiles', 'OutputManager',
           'get_total_time']

from datetime import datetime

# INPUT

class InputManager:

    """
    Used as a complimentative class for `Dualfoil`
    
    Manages the input file for dualfoil; used to start new simulations
    and continue from restarts.
    
    Attributes
    ----------
    file_name: str
        Name of the input file to be used by dualfoil
    file_path: str
        Full or relative path to dualfoil files
    from_restart: bool, optional
        indicates whether the next simulation will occur from a restart
    """

    def __init__(self, path, input_name='dualfoil5.in',
                 restart_capable=True):
        """
        Parameters
        ----------
        path : str
            Full or relative path to dualfoil files
        input_name : str, optional
            Name of the input file
        restart_capable : bool
            Indicates whether dualfoil5.1 (restart capable) or
            dualfoil5.2 (restart incapable) is being used
        """
        if path is None: 
            self.file_path = ''
        else:
            self.file_path = path
        self.file_name = input_name
        self.from_restart = False
        self.restart_capable = restart_capable

    def reset(self):
        self.from_restart = False

    def add_new_leg(self, run_value, stop_condition, mode, descr=None):
        """
        Appends a new leg to dualfoil's input file,
        which works with or without restart
        
        Format for a given experiment:
        "Specified (run_value) to/for a given (stop_condition)"
        The value of the mode will determine the meaning of `run_value` and `stop_condition`

        Parameters
        ----------
        run_value : float
            See below for the different meanings this value can take on
        stop_condition : float
            See below for the different meanings this value can take on
        mode : float
            defines 'mc(i)' for the input in dualfoil, which controls mode of operation
            Possible Values
            ---------------
            -3: specified load (ohms-m2) for a given time(min)
            -2: specified power (ohms-m2) for a given time (min)
            -1: specified tapered current (A) to a cutoff potential (V)
             0: specified voltage (V) for a give time (min) 
             1: specified current (A) for a given time (min)
             2: specified current (A) to a cutoff potential (V)    
        descr : str, optional
            comment that describes the function of the new leg
        """

        new_input = ''
        modified = False

        # BELOW: two variables used by both dualfoils in command line
        # PURPOSE: represent the range of voltages outside which 
        #   the battery should not continue running in
        low_voltage_cut = 1e-3
        high_voltage_cut = 14.0

        df_input = self.file_path + self.file_name
        with open('%s' % df_input, 'r') as file:

            line = file.readline()
            while line != '':
                # this section applies only to dualfoil5.1
                # doesn't alter 5.2 in any way though
                if self.from_restart:
                    # make sure from_restart is set to true
                    if line.find('.false.') != -1:
                        line = line.replace('.false.', '.true.')
                else:
                    # make sure from_restart is set to false
                    if line.find('.true.') != -1:
                        line = line.replace('.true.', '.false.')

                # take dualfoil out of isothermal mode
                if line.find('lht') != -1:
                    tmp = line.split()
                    if int(tmp[0]) != 0:
                        # replace the first number with '0',
                        # for regular heat generation mode
                        line = line.replace(str(tmp[0]), '0', 1)

                # make sure dualfoil is NOT in impedance mode
                if line.find('imp') != -1:
                    tmp = line.split()
                    if int(tmp[0]) != 0:
                        # replace the first number with '0' for off
                        line = line.replace(str(tmp[0]), '0', 1)
                    
                # assure that the number of current changes is 1
                # change exec line to our desired parameters
                # set tracker for next loopthru
                if line.find('lcurs') != -1 and not modified:
                    tmp = line.lstrip().split()
                    # also make sure lcurs is 1
                    if int(tmp[0]) != 1:
                        line = line.replace(str(tmp[0]), '1', 1)
                    new_input += line

                    # skip over all other command lines
                    while line != '\n':
                        line = file.readline()

                    # Next section is only for dualfoil5.1
                    if self.restart_capable:
                        # if next leg depends on time, we need the total time
                        if (mode != 2) and (mode != -1):
                            if self.from_restart:
                                # depends on time AND we are from restart. 
                                # need total in minutes for dualfoil
                                total_time = get_total_time(self.file_path) / 60 
                                stop_condition += total_time

                    line = (str(run_value) + ' ' + str(stop_condition) +
                            ' ' + str(mode) + ' ' + str(low_voltage_cut) +
                            ' ' + str(high_voltage_cut))
                    # BELOW: if using dualfoil5.2, another parameter exists
                    #   at end of command line: internal_resistance
                    # PURPOSE: 'internal resistance of foils, tabs, etc.'
                    if not self.restart_capable:
                        internal_resistance = 8e-4  # default value from download
                        line = line + ' ' + str(internal_resistance)

                    # check to see if optional description has been added
                    if descr is not None:
                        line = line + ' !' + descr + '\n\n'
                    else:
                        line = line + '\n\n'
                    # don't do this again even if 'lcurs' is in file again
                    modified = True

                # keep up the new file and read next line
                new_input += line
                line = file.readline()

        with open(df_input, 'w') as file:
            file.write(new_input)
            
        # if this is our first time, next will be from restart
        # when working in dualfoil5.1
        if not self.from_restart:
            self.from_restart = True
    
    def add_impedance(self):
        """
        Changes the input file for impedance mode
        
        Only compatible with dualfoil5.2
        """
        new_input = ''

        #
        # STEP 1: Adjust main input file
        #
        df_input = self.file_path + self.file_name
        with open('%s' % df_input, 'r') as file:

            line = file.readline()
            while line != '':
                # must set `lht` to 2
                # this puts dualfoil in isothermal mode
                if line.find('lht') != -1:
                    tmp = line.split()
                    if int(tmp[0]) != 2:
                        # replace the first number with '2'
                        line = line.replace(str(tmp[0]), '2', 1)

                # also must set `imp` to 1
                # this flag runs dualfoil in impedance mode
                if line.find('imp') != -1:
                    tmp = line.split()
                    if int(tmp[0]) != 1:
                        # replace the first number with '1'
                        line = line.replace(str(tmp[0]), '1', 1)

                # keep up the new file and read next line
                new_input += line
                line = file.readline()

        # rewrite the input file 
        with open(df_input, 'w') as file:
            file.write(new_input)
            
        #
        # STEP 2: adjust activation-energy input file
        # (impedance mode only works if activation energies of)
        # (      solid-state diffusions are equal to 0.       )
        #
        new_input = ''
        df_input = self.file_path + 'li-ion-ebar.in'
        with open('%s' % df_input, 'r') as file:
            for line in file.readlines():
                if line.find('solid state diffusion') != -1:
                    # replace the number with 0 if it isn't already
                    tmp = line.split()
                    if float(tmp[0]) != 0:
                        line = line.replace(str(tmp[0]), '0.0', 1)
                # keep up new input 
                new_input += line

        # rewrite activation-energy input file
        with open('%s' % df_input, 'w') as file:
            file.write(new_input)
        

def get_total_time(path=None, restart_capable=True):
    """
    Get the total dualfoil simulation time in minutes

    Only compatible with dualfoil5.1

    Parameters
    ----------
    path : str, optional
        Full or relative path to dualfoil files
    restart_capable : bool, optional
        Indicates whether dualfoil5.1 (restart capable) or
        dualfoil5.2 (restart incapable) is being used
    """

    if not restart_capable:
        print('This feature is only applicable when working with dualfoil5.1')
        return

    if path is None:
        restart_file = open('df_restart.dat', 'r')
    else:
        restart_file = open('%sdf_restart.dat' % path, 'r')
    tmp = restart_file.readline()
    tmp = tmp.lstrip().split()
    # get timestep in seconds
    time_step = float(tmp[1])
    restart_file.close()
    return time_step
        
# OUTPUT:
# for extracting and organizing data from dualfoil5.out

def extract_main_output(file='dualfoil5.out', path=None):
    """
    Gathers data from dualfoil5.out generated by dualfoil
    
    Parameters
    ----------
    file : str, optional
        main output file (most likely "Dualfoil5.out") generated by Dualfoil
    path : str, optional
        path to get to file, if not in current directory

    Returns
    -------
    dict
        contains each list of floats generated in main dualfoil output

        Keys
        ----
        time : list of float
            the time in seconds
        neg_util : list of float
            initial stoicheometric parameter for the negative electrolyte
        pos_util : list of float
            initial stoicheometric parameter for the positive electrolyte
        voltage : list of float
            the potential of the cell in volts
        open_circuit_potential : list of float
            the open-circuit potential in volts
        current : list of float
            the current in amperes
        temperature : list of float
            the temperature in Celcius
        heat_gen : list of float
            the generated heat in Watts/m^2
    """

    # first go through and find position where output starts in file
    x = 0
    previous = ''
    if path is not None:
        fpath = path + file
    else:
        fpath = file
    with open(fpath, 'r') as fin:
        data_list = []
        
        line = fin.readline()

        # find start of CSV portion of file
        while line.find('(min)') == -1:
            line = fin.readline()

        while line != '':
            # only take lines with convertable data
            if line.find(',') != -1:
                # halt if we get NaN
                if line.find('NaN') != -1:
                    raise RuntimeError('Dualfoil did not converge.')
                # make sure we are not taking in a copy
                if line != previous:
                    previous = line
                    line = line.rstrip('\n').rstrip(' ').lstrip(' ')
                    data_list.append(line)

            # read in the next line
            line = fin.readline()

    # keyword variable for all output (left to right)
    output = {'time': [], 'neg_util': [], 'pos_util': [],
              'voltage': [], 'open_circuit_potential': [],
              'current': [], 'temperature': [], 'heat_gen': []}

    for data in data_list:
        tmp = data.split(',')
        for i in tmp:
            i.lstrip(' ')
        output['time'].append(float(tmp[0]))
        output['neg_util'].append(float(tmp[1]))
        output['pos_util'].append(float(tmp[2]))
        output['voltage'].append(float(tmp[3]))
        output['open_circuit_potential'].append(float(tmp[4]))
        # CURRENT EXPLAINED
        # Dualfoil: + for discharge, - for charge
        # Pycap:    - for discharge, + for charge
        # multiply by -1 to account for this
        output['current'].append(float(tmp[5])*(-1))
        # sometimes dualfoil does not compute next two
        if (tmp[6] == ' ******'):
            tmp[6] = '0.00'
        output['temperature'].append(float(tmp[6]))
        if (tmp[7] == ' ******'):
            tmp[7] = '0.00'
        output['heat_gen'].append(float(tmp[7]))

    return output


def extract_profiles(file='profiles.out', path=None):

    """
    Gathers data from profiles.out generated by dualfoil.
    
    Parameters
    ----------
    file : str
        main output file (most likely "profiles.out") generated by Dualfoil
    path : str
        path to get to file, if not in current directory

    Returns
    -------
    dict
        Contains lists for each type of input
        *Note: 'time' is a list of floats; every other key is two-dimmensional
               ([number of timesteps] by [number of nodes across cell])
        
        Keys
        ----
        time : list of float
            the time stamp in seconds for each profile taken
        distance : list of list of float
            locations of nodes across the cell in microns
        electrolyte_conc : list of list of float
            concentration in mol/m^3
        solid_surface_conc : list of list of float
            concentration in mol/m^3
        liquid_potential : list of list of float
            the liquid potential in volts
        solid_potential : list of list of float
            the solid potential in volts
        liquid_current : list of list of float
            liquid current density in amperes/m^2
        j_main : list of list of float
            main liquid current density in amperes/m^2
        j_side_1 : list of list of float
            first side reaction liquid current density in amperes/m^2
        j_side_2 : list of list of float
            second side reaction liquid current density in amperes/m^2
        j_side_3 : list of list of float
            third side reaction liquid current density in amperes/m^2
    """

    if path is not None:
        fpath = path + file
    else:
        fpath = path

    profile_list = []
    with open(fpath, 'r') as fin:
        block = []

        #skip first line
        line = fin.readline()

        while line != '':
            line = fin.readline()
            tmp = line.rstrip('\n').rstrip(' ')
            # check for nonconvergence
            if tmp.find('NaN') != -1:
                raise RuntimeError('Dualfoil did not converge.')
            if tmp == '': # end of a block
                if len(block) > 0:
                    profile_list.append(block)
                    block = []
            else:
                block.append(tmp)

    # dict to contain all data for each time chunk
    output = {'distance':[], 'electrolyte_conc':[],
              'solid_surface_conc':[], 'liquid_potential':[],
              'solid_potential':[], 'liquid_current':[],
              'j_main':[], 'j_side_1':[], 'j_side_2':[],
              'j_side_3':[], 'time':[]}

    # add each row's data into appropriate list
    for profile in profile_list:
        # extract columns
        distance = []
        elec_conc = []
        sol_surf_conc = []
        liquid_potential = []
        solid_potential = []
        liquid_cur = []
        j_main = []
        j_side1 = []
        j_side2 = []
        j_side3 = []

        for row in profile[3:]:
            tmp = row.split(',')
            distance.append(float(tmp[0]))
            elec_conc.append(float(tmp[1]))
            sol_surf_conc.append(float(tmp[2]))
            liquid_potential.append(float(tmp[3]))
            solid_potential.append(float(tmp[4]))
            liquid_cur.append(float(tmp[5]))
            j_main.append(float(tmp[6]))
            j_side1.append(float(tmp[7]))
            j_side2.append(float(tmp[8]))
            j_side3.append(float(tmp[9]))

        # add each data list to its corresponding datatype
        output['distance'].append(distance)
        output['electrolyte_conc'].append(elec_conc)
        output['solid_surface_conc'].append(sol_surf_conc)
        output['liquid_potential'].append(liquid_potential)
        output['solid_potential'].append(solid_potential)
        output['liquid_current'].append(liquid_cur)
        output['j_main'].append(j_main)
        output['j_side_1'].append(j_side1)
        output['j_side_2'].append(j_side2)
        output['j_side_3'].append(j_side3)

        # extract time step and add to time list
        tmp = profile[2]
        time = float(tmp.lstrip('t = ').split(' ')[0])
        output['time'].append(time)

    return output

class OutputManager:
    """
    Used as a complimentative class for `Dualfoil`
    
    Maintains the output generated by dualfoil
    
    Attributes
    ----------
    file_path : str
        full or relative path to dualfoil files
    output : dict
        Contains lists of floats for each variable written
        to Dualfoil's main output file
        
        Keys
        ----
        time : list of float
            the time in seconds
        neg_util : list of float
            initial stoicheometric parameter for the negative electrolyte
        pos_util : list of float
            initial stoicheometric parameter for the positive electrolyte
        voltage : list of float
            the potential of the cell in volts
        open_circuit_potential : list of float
            the open-circuit potential in volts
        current : list of float
            the current in amperes
        temperature : list of float
            the temperature in Celcius
        heat_gen : list of float
            the generated heat in Watts/m^2
    profiles : dict
        Contains a one-dimmensional list of timesteps
        Contains two-dimmensional lists of variables written to
          'profiles.out,' with the following max sizes:
          variable[number of timesteps][nodes across cell]
        
        Keys
        ----
        time : list of float
            the time stamp in seconds for each profile taken
        distance : list of list of float
            locations of nodes across the cell in microns
        electrolyte_conc : list of list of float
            concentration in mol/m^3
        solid_surface_conc : list of list of float
            concentration in mol/m^3
        liquid_potential : list of list of float
            the liquid potential in volts
        solid_potential : list of list of float
            the solid potential in volts
        liquid_current : list of list of float
            liquid current density in amperes/m^2
        j_main : list of list of float
            main liquid current density in amperes/m^2
        j_side_1 : list of list of float
            first side reaction liquid current density in amperes/m^2
        j_side_2 : list of list of float
            second side reaction liquid current density in amperes/m^2
        j_side_3 : list of list of float
            third side reaction liquid current density in amperes/m^2
    """
    
    def __init__(self, path):
        """
        Initialize output list variables.
        
        Parameters
        ----------
        path : str
            Full or relative path to dualfoil files
        """

        if path is None: 
            self.file_path = ''
        else:
            self.file_path = path

        # main output and profiles dicts
        self.output = {'time':[], 'neg_util':[], 'pos_util':[],
                       'current':[], 'heat_gen':[], 'temperature':[],
                       'voltage':[], 'open_circuit_potential':[]}

        self.profiles = {'time':[], 'distance':[], 'electrolyte_conc':[],
                         'solid_surface_conc':[], 'liquid_potential':[],
                         'solid_potential':[], 'liquid_current':[],
                         'j_main':[], 'j_side_1':[], 'j_side_2':[], 'j_side_3':[]}
        
    def reset(self):
        """
        Clear the two dicts.
        """
        self.output = {'time':[], 'neg_util':[], 'pos_util':[],
                       'current':[], 'heat_gen':[], 'temperature':[],
                       'voltage':[], 'open_circuit_potential':[]}

        self.profiles = {'time':[], 'distance':[], 'electrolyte_conc':[],
                         'solid_surface_conc':[], 'liquid_potential':[],
                         'solid_potential':[], 'liquid_current':[],
                         'j_main':[], 'j_side_1':[], 'j_side_2':[], 'j_side_3':[]}

    def get_voltage(self):
        """
        Get the voltage, or return -1 if there is no output
        
        Returns
        -------
        float
            the voltage in volts
        """
        if len(self.output['voltage']) == 0:
            return -1
        else:
            return self.output['voltage'][-1]
        
    def get_current(self):
        """
        Get the current, or return -1 if there is no output
        
        Returns
        -------
        float
            the current in amperes
        """
        if len(self.output['current']) == 0:
            return -1
        else:
            return self.output['current'][-1]

    def update_output(self):
        """
        Append output data from `dualfoil5.out` and `profiles.out`
        into the appropriate keys.
        """
        # main output
        main_output = extract_main_output(path=self.file_path)
        for k in self.output.keys():
            self.output[k].extend(main_output[k])

        # profiles
        profiles = extract_profiles(path=self.file_path)
        for k in self.profiles.keys():
            self.profiles[k].extend(profiles[k])

    def write_main_output(self):
        """
        Organizational tool to display the main output from dualfoil 
        into a readable file: combinedOutput.out
        """

        # main output data
        with open('%scombinedOutput.out' % self.file_path, 'w') as out_file:
            date = str(datetime.now().isoformat())
            out_file.write(date)
            out_file.write('\n\nMain Output data\n\n')

            # In order to print in the same order as Dualfoil, manually sort
            # using a list of indices similar to numpy.argsort
            keys = sorted(self.output.keys()) # alphabetical

            # print entries in same order
            for j in range(len(self.output['time'])):
                sorter = [7, 2, 4, 5, 3, 0, 6, 1] # only works for
                                                  # alphabetical
                for i in sorter:
                    key = keys[i]
                    entry = str(self.output[key][j]).rjust(10)
                    entry += ','
                    out_file.write(entry)
                    if keys[i] is 'heat_gen':
                        out_file.write('\n')
    