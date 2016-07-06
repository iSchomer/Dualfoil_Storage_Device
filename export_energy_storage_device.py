import subprocess
from pycap import EnergyStorageDevice
import df_manip
import df_grapher

class Dualfoil(EnergyStorageDevice):

    def __init__(self, Path='', Input='dualfoil5.in', vcutL=1e-4, vcutH=10.0):
        self.vcutL = vcutL
        self.vcutH = vcutH
        if not Path.endswith('/'):
            Path += '/'
        self.filePath = Path
        self.fileName = Input
        self.restart = False

        # compile dualfoil
        if self.filePath == '':
            subprocess.call('make clean && make dualfoil', shell=True)
        else:
            subprocess.call('cd %s && make clean && make dualfoil'
                            % self.filePath, shell=True)

        # main output list variables
        self.time = []
        self.n_util = []
        self.p_util = []
        self.potential = []
        self.uocp = []
        self.curr = []
        self.temp = []
        self.heatgen = []
        self.legs = []
        
        # profile list variables
        self.time_prof = []
        self.distance_prof = []
        self.elec_conc_prof = []
        self.sol_surf_conc_prof = []
        self.liq_pot_prof = []
        self.sol_pot_prof = []
        self.liq_cur_prof = []
        self.jmain_prof = []
        self.jside1_prof = []
        self.jside2_prof = []
        self.jside3_prof = []

    # use when wanting to start a new simulation from scratch
    def reset(self):
        self.restart = False
        self.time.clear()
        self.n_util.clear()
        self.p_util.clear()
        self.potential.clear()
        self.uocp.clear()
        self.curr.clear()
        self.temp.clear()
        self.heatgen.clear()
        self.legs.clear()
        self.time_prof.clear()
        self.distance_prof.clear()
        self.elec_conc_prof.clear()
        self.sol_surf_conc_prof.clear()
        self.liq_pot_prof.clear()
        self.sol_pot_prof.clear()
        self.liq_cur_prof.clear()
        self.jmain_prof.clear()
        self.jside1_prof.clear()
        self.jside2_prof.clear()
        self.jside3_prof.clear()

    def run(self):
        #update restart value
        if not self.restart:
            self.restart = True

        if self.filePath == '':
            subprocess.call('./dualfoil', shell=True)
        else:
            subprocess.call('cd %s && ./dualfoil' % self.filePath, shell=True)

    def set_filepath(self, path):
        if not path.endswith('/'):
            path += '/'
        self.filePath = path

    def get_voltage(self):
        if len(self.potential) == 0:
            # run a 0 time simulation to get the starting voltage
            self.evolve_one_time_step_constant_current(0, -20)
            v = self.potential[-1]
            self.reset()
            return v
        else:
            return self.potential[-1]

    def get_current(self):
        if len(self.curr) == 0:
            # run a 0 time sim to get initial current
            self.evolve_one_time_step_constant_voltage(0, 0)
            c = self.curr[-1]
            self.reset()
            return c
        else:
            return self.curr[-1]

    def get_total_time(self):
        rstFile = open('%s:' % self.filePath, 'r')
        tmp = rstFile.readline()
        tmp = tmp.lstrip().split()
        # get timestep in minutes
        ts = float(tmp[1]) / 60
        return ts

    def get_time_step(self):
        rstFile = open('%sdf_restart.dat' % self.filePath, 'r')
        tmp = rstFile.readline()
        tmp = tmp.lstrip().split()
        # get timestep in minutes
        ts = float(tmp[0]) / 60
        return ts

    # update all list variables so that they represent last-run sim
    def update_output(self):
        # main output
        t, n, p, v, u, c, tp, hg = df_manip.extract_main_output(path=self.filePath)
        self.time += t
        self.n_util += n
        self.p_util += p
        self.potential += v
        self.uocp += u
        self.curr += c
        self.temp += tp
        self.heatgen += hg

        # profiles
        (T, D, EC, SSC, LP, SP, LC, 
         JM, JS1, JS2, JS3) = df_manip.extract_profiles(path=self.filePath)
        self.time_prof += T
        self.distance_prof = D
        self.elec_conc_prof += EC
        self.sol_surf_conc_prof += SSC
        self.liq_pot_prof += LP
        self.sol_pot_prof += SP
        self.liq_cur_prof += LC
        self.jmain_prof += JM
        self.jside1_prof += JS1
        self.jside2_prof += JS2
        self.jside3_prof += JS3

    # append list variables to the combined output file
    def write_main_output(self):
        output = [self.time, self.n_util, self.p_util, self.potential,
                  self.uocp, self.curr, self.temp, self.heatgen]
        subprocess.call('date > %scombinedOutput.out' % self.filePath,
                        shell=True)

        # main output data
        with open('%scombinedOutput.out' % self.filePath, 'a') as outFile:
            outFile.write('\nMain Output data\n\n')
            outFile.write('     Time     N_util   P_util   Potential   Uocp       Curr      Temp    Heatgen\n')
            outFile.write('     (min)      x         y        (v)      (v)       (A/m2)      (C)     (W/m2)\n\n')
            for i in range(len(self.time)):
                for j in range(len(output)):
                    outFile.write(str(output[j][i]).rjust(9))
                    outFile.write(',')
                    if j == (len(output)-1):
                        outFile.write('\n')

        # file for tracking simulation actions in a quickread format
        subprocess.call('date > %slegs.dat' % self.filePath, shell=True)
        with open('%slegs.dat' % self.filePath, 'a') as legsFile:
            for string in self.legs:
                string = string.rstrip('\n')
                string += '\n'  # only want one newline per command
                legsFile.write(string)

    # main user functions------------------------------------------------
    # time_step must be converted from seconds to minutes
    def evolve_one_time_step_constant_current(self, time_step, current,
                                              descr=''):
        # pycap: - for discharge, + for charge
        # dualfoil: + for discharge, - for charge
        current = -current

        time_step = time_step / 60
        df_manip.add_new_leg(current, time_step, 1, description=descr,
                             path=self.filePath, restart=self.restart)
        self.run()
        self.update_output()

    def evolve_one_time_step_constant_voltage(self, time_step, voltage,
                                              descr=''):
        time_step = time_step / 60
        df_manip.add_new_leg(voltage, time_step, 0, description=descr,
                             path=self.filePath, restart=self.restart)
        self.run()
        self.update_output()

    def evolve_one_time_step_constant_power(self, time_step, power,
                                            descr=''):
        time_step = time_step / 60
        df_manip.add_new_leg(power, time_step, -2, description=descr,
                             path=self.filePath, restart=self.restart)
        self.run()
        self.update_output()

    def evolve_one_time_step_constant_load(self, time_step, load,
                                           descr=''):
        time_step = time_step / 60
        df_manip.add_new_leg(load, time_step, -3, description=descr,
                             path=self.filePath, restart=self.restart)
        self.run()
        self.update_output()

    def evolve_to_voltage_constant_current(self, current, cutoff,
                                           descr=''):
        df_manip.add_new_leg(current, cutoff, 2, description=descr,
                             path=self.filePath, restart=self.restart)
        self.run()
        self.update_output()

    def evolve_one_time_step_linear_current(self, time_step, current,
                                            divisor=10, descr=''):
        time_step = time_step / 60
        if not self.restart:
            tmpcurr = 0
        else:
            tmpcurr = get_current()

        ts = time_step / divisor
        ttot = ts
        change = (current-tmpcurr) / divisor
        tmpcurr = change

        while ttot <= time_step:
            df_manip.add_new_leg(tmpcurr, ts, 1, description=descr,
                                 path=self.filePath, restart=self.restart)
            # get next timestep values
            ttot += ts
            tmpcurr += change
            self.run()
            self.update_output()

    def evolve_one_time_step_linear_voltage(self, time_step, voltage,
                                            divisor=10, descr=''):
        time_step = time_step / 60
        if not self.restart:
            tmpvolt = 0
        else:
            tmpvolt = get_current()

        ts = time_step / divisor
        tott = ts
        change = (voltage-tmpvolt) / divisor
        tmpvolt = change

        while tott <= time_step:
            df_manip.add_new_leg(tmpvolt, ts, 0, description=descr,
                                 path=self.filePath, restart=self.restart)
            # get next timestep values
            tott += ts
            tmpvolt += change
            self.run()
            self.update_output()

    # start_point parameter added to the following 2 functions
    # because they cannot be extracted
    def evolve_one_time_step_linear_power(self, time_step, power,
                                          start_point=0, divisor=10,
                                          descr=''):
        time_step = time_step / 60
        tmppower = start_point
        ts = time_step / divisor
        tott = ts
        change = (power-start_point) / divisor
        tmppower = change

        while ts <= time_step:
            df_manip.add_new_leg(tmppower, ts, 1, description=descr,
                                 path=self.filePath, restart=self.restart)
            # get next timestep values
            tott += ts
            tmppower += change
            self.run()
            self.update_output()

    def evolve_one_time_step_linear_load(self, time_step, load,
                                         start_point=0, divisor=10,
                                         descr=''):
        time_step = time_step / 60
        tmpload = start_point
        ts = time_step / divisor
        tott = ts
        change = (load-start_point) / divisor
        tmpload = change

        while ts <= time_step:
            df_manip.add_new_leg(tmpload, ts, 1, description=descr,
                                 path=self.filePath, restart=self.restart)
            # get next timestep values
            tott += ts
            tmpload += change
            self.run()
            self.update_output()
