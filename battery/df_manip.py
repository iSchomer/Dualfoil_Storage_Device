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
                    if int(tmp[0]) != 0:
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
        print('This feature is only applicable when working with dualfoil5.2')
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
        main output file (most likely "Dualfoil5.out") generated by Dualfoil5.1
    path : str, optional
        path to get to file, if not in current directory

    Returns
    -------
    time : list of float
        the time in seconds
    n_util, p_util : list of float
        initial stoicheometric parameter for the negative(n) and positive(p) electrolytes
    potential : list of float
        the potential of the cell in volts
    uocp : list of float
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

        for line in fin.readlines():
            if line.find('(min)') != -1:
                # found it! stop here
                break
            x += 1

    # now read lines again
    with open(fpath, 'r') as fin:

        for line in fin.readlines()[x+2:]:
            # only take lines with convertable data
            if line.find(',') != -1:
                # make sure we are not taking in a copy
                if line != previous:
                    previous = line
                    line = line.rstrip('\n').rstrip(' ').lstrip(' ')
                    data_list.append(line)

    # variable lists for each time
    time = []
    n_util = []
    p_util = []
    potential = []
    uocp = []
    current = []
    temperature = []
    heat_gen = []

    for data in data_list:
        tmp = data.split(',')
        for i in tmp:
            i.lstrip(' ')
        time.append(float(tmp[0]))
        n_util.append(float(tmp[1]))
        p_util.append(float(tmp[2]))
        potential.append(float(tmp[3]))
        uocp.append(float(tmp[4]))
        # CURRENT EXPLAINED
        # Dualfoil: + for discharge, - for charge
        # Pycap:    - for discharge, + for charge
        # multiply by -1 to account for this
        current.append(float(tmp[5])*(-1))
        temperature.append(float(tmp[6]))

        # for 5.1 code

        if (tmp[7] == ' ******'):
            tmp[7] = '0.00'
        heat_gen.append(float(tmp[7]))

    # return data in order it appears
    return time, n_util, p_util, potential, uocp, current, temperature, heat_gen


def extract_profiles(file='profiles.out', path=None):

    """
    Gathers data from profiles.out generated by dualfoil.
    
    Parameters
    ----------
    file : str
        main output file (most likely "Dualfoil5.out") generated by Dualfoil5.2
    path : str
        path to get to file, if not in current directory

    Returns
    -------
    time : list of floats
        the time in seconds each profile is taken
    distance : list of floats
        the distance accross the cell in microns
    elec_conc : list of list of floats
        the concentration of electrolyte in mol/cubic meters
    sol_surf : list of list of floats
        Potential in the electrolyte, neg. or pos. depending on location in cell
    liq_potential : list of list of floats
        the liquid potential in volts
    sol_potential : list of list of floats
        the solid potential in volts
    liq_cur : list of list of floats
        the liquid current density in amperes/square meters
    jmain : list of list of floats
        main liquid current density in amperes/square meters
    j_side1 : list of list of floats
        first side reaction liquid current density in amperes/square meters
    j_side2 : list of list of floats
        second side reaction liquid current density in amperes/square meters
    j_side3 : list of list of floats
        third side reaction liquid current density in amperes/square meters
    """

    if path is not None:
        fpath = path + file
    else:
        fpath = path
    with open(fpath, 'r') as fin:
        profile_list = []
        profile = []

        # ignore the first line
        for line in fin.readlines()[1:]:

            line = line.rstrip('\n').rstrip(' ')
            if line == '':
                if profile != []:
                    profile_list.append(profile)
                    profile = []
                continue
            # print(line)
            profile.append(line)

    # list of appropriate variable lists for each time chunk
    distance_list = []
    elec_conc_list = []
    sol_surf_conc_list = []
    liquid_potential_list = []
    solid_potential_list = []
    liquid_cur_list = []
    j_main_list = []
    j_side1_list = []
    j_side2_list = []
    j_side3_list = []
    time_list = []

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

        # add each data list to its corresponding vector
        distance_list.append(distance)
        elec_conc_list.append(elec_conc)
        sol_surf_conc_list.append(sol_surf_conc)
        liquid_potential_list.append(liquid_potential)
        solid_potential_list.append(solid_potential)
        liquid_cur_list.append(liquid_cur)
        j_main_list.append(j_main)
        j_side1_list.append(j_side1)
        j_side2_list.append(j_side2)
        j_side3_list.append(j_side3)

        # extract time step and add to time list
        tmp = profile[2]
        time = float(tmp.lstrip('t = ').split(' ')[0])
        time_list.append(time)

    # return data in order it appears
    return (time_list, distance_list, elec_conc_list,
            sol_surf_conc_list, liquid_potential_list,
            solid_potential_list, liquid_cur_list, j_main_list,
            j_side1_list, j_side2_list, j_side3_list)

class OutputManager:
    """
    Used as a complimentative class for `Dualfoil`
    
    Maintains the output generated by dualfoil
    
    Attributes
    ----------
    file_path : str
        Full or relative path to dualfoil files
    time : list of float
        the time in seconds
    n_util, p_util : list of float
        initial stoicheometric parameter for the negative(n) and positive(p) electrolytes
    potential : list of float
        the potential of the cell in volts
    uocp : list of float
        the open-circuit potential in volts
    current : list of float
        the current in amperes
    temperature : list of float
        the temperature in Celcius
    heat_gen : list of float
        the generated heat in Watts/m^2
    time_prof : list of floats
        the time in seconds each profile is taken
    distance_prof : list of floats
        the distance accross the cell in microns
    elec_conc_prof : list of list of floats
        the concentration of electrolyte in mol/cubic meters
    sol_surf_prof : list of list of floats
        Potential in the electrolyte, neg. or pos. depending on location in cell
    liq_potential_prof : list of list of floats
        the liquid potential in volts
    sol_potential_prof : list of list of floats
        the solid potential in volts
    liq_cur_prof : list of list of floats
        the liquid current density in amperes/square meters
    j_main_prof : list of list of floats
        main liquid current density
    j_side1_prof : list of list of floats
        first side reaction liquid current density
    j_side2_prof : list of list of floats
        second side reaction liquid current density
    j_side3_prof : list of list of floats
        third side reaction liquid current density
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

        # main output list variables
        self.time = []
        self.n_util = []
        self.p_util = []
        self.potential = []
        self.uocp = []
        self.current = []
        self.temperature = []
        self.heat_gen = []
        
        # profile list variables
        self.time_prof = []
        self.distance_prof = []
        self.elec_conc_prof = []
        self.sol_surf_conc_prof = []
        self.liq_pot_prof = []
        self.sol_pot_prof = []
        self.liq_cur_prof = []
        self.j_main_prof = []
        self.j_side1_prof = []
        self.j_side2_prof = []
        self.j_side3_prof = []
        
    def reset(self):
        """
        Clear all output lists.
        """
        self.time.clear()
        self.n_util.clear()
        self.p_util.clear()
        self.potential.clear()
        self.uocp.clear()
        self.current.clear()
        self.temperature.clear()
        self.heat_gen.clear()
        self.time_prof.clear()
        self.distance_prof.clear()
        self.elec_conc_prof.clear()
        self.sol_surf_conc_prof.clear()
        self.liq_pot_prof.clear()
        self.sol_pot_prof.clear()
        self.liq_cur_prof.clear()
        self.j_main_prof.clear()
        self.j_side1_prof.clear()
        self.j_side2_prof.clear()
        self.j_side3_prof.clear()

    def get_voltage(self):
        """
        Get the voltage, or return -1 if there is no output
        
        Returns
        -------
        float
            the voltage in volts
        """
        if len(self.potential) == 0:
            return -1
        else:
            return self.potential[-1]
        
    def get_current(self):
        """
        Get the current, or return -1 if there is no output
        
        Returns
        -------
        float
            the current in amperes
        """
        if len(self.current) == 0:
            return -1
        else:
            return self.current[-1]

    def update_output(self):
        """
        Append output data from `dualfoil5.out` and `profiles.out`
        into the appropriate data list.
        """
        # main output
        x = extract_main_output(path=self.file_path)
        self.time.extend(x[0])
        self.n_util.extend(x[1])
        self.p_util.extend(x[2])
        self.potential.extend(x[3])
        self.uocp.extend(x[4])
        self.current.extend(x[5])
        self.temperature.extend(x[6])
        self.heat_gen.extend(x[7])

        # profiles
        x = extract_profiles(path=self.file_path)
        self.time_prof.extend(x[0])
        self.distance_prof.extend(x[1])
        self.elec_conc_prof.extend(x[2])
        self.sol_surf_conc_prof.extend(x[3])
        self.liq_pot_prof.extend(x[4])
        self.sol_pot_prof.extend(x[5])
        self.liq_cur_prof.extend(x[6])
        self.j_main_prof.extend(x[7])
        self.j_side1_prof.extend(x[8])
        self.j_side2_prof.extend(x[9])
        self.j_side3_prof.extend(x[10])

    def write_main_output(self):
        """
        Organizational tool to display the main output from dualfoil 
        into a readable file: combinedOutput.out
        """

        output = [self.time, self.n_util, self.p_util, self.potential,
                  self.uocp, self.current, self.temperature, self.heat_gen]
        # main output data
        with open('%scombinedOutput.out' % self.file_path, 'w') as out_file:
            date = str(datetime.now().isoformat())
            out_file.write(date)
            out_file.write('\n\nMain Output data\n\n')
            out_file.write('     Time     N_util   P_util   Potential   Uocp       Curr      Temp    Heatgen\n')
            out_file.write('     (min)      x         y        (v)      (v)       (A/m2)      (C)     (W/m2)\n\n')
            for i in range(len(self.time)):
                for j in range(len(output)):
                    out_file.write(str(output[j][i]).rjust(9))
                    out_file.write(',')
                    if j == (len(output)-1):
                        out_file.write('\n')
    