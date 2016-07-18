import sys
sys.path.append('/notebooks')
from battery import *
from energy_storage_device import Dualfoil
from pycap import PropertyTree, Charge
from numpy import array as ar
from numpy import argsort

import unittest

path = '/notebooks/docker/'

class DualfoilTestCase(unittest.TestCase):
    def test_consistency_constant_evolve_functions(self):
        df = Dualfoil(path=path)

        # determine that what we want is what we get
        # constant evolve functions
        dt = 1.0  # seconds
        I = 5.0   # current
        df.evolve_one_time_step_constant_current(dt, I)
        self.assertAlmostEqual(df.get_current(), I)
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        df.reset()
        V = 4.5  # voltage
        df.evolve_one_time_step_constant_voltage(dt, V)
        self.assertAlmostEqual(df.get_voltage(), V)
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        df.reset()
        P = 15.0  # watts/m2

        # BELOW: represents leniency of error caused by
        # dualfoil, not the code being tested.
        # REASON FOR ERROR: Since dualfoil does not
        #   calculate power, it is calculated with current
        #   and voltage. Current is rounded off to the
        #   nearest hundreths in dualfoil, so the final 
        #   product between voltage and current will be off 
        #   by about that much.
        error = 1e-2 

        df.evolve_one_time_step_constant_power(dt, P)
        # Sign of current only indicates direction of flow
        # No such thing as negative power; use abs val
        power = abs(df.get_voltage() * df.get_current())
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        self.assertAlmostEqual(power, P, delta=error)
        df.reset()
        L = 0.7  # ohms-m2
        df.evolve_one_time_step_constant_load(dt, L)
        # Sign of current only indicates direction of flow
        # No such thing as negative load; use abs val
        load = abs(df.get_voltage() / df.get_current())
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        self.assertAlmostEqual(load, L, delta=error)
        df.reset()
        Vcut = 4.53   # cutoff voltage
        C = 10.0      # current (charge)
        error = 1e-4  # voltage has roundoff error after
                      #   the fourth decimal place
        df.evolve_to_voltage_constant_current(C, Vcut)
        self.assertAlmostEqual(df.get_voltage(), Vcut,
                               delta=error)
        df.reset()

    def test_consistency_linear_evolve_functions(self):
        df = Dualfoil(path=path)

        # affirm total time and final dependent value
        dt = 15.0  # seconds
        div = 4    # number of substeps
        c_fin = -10.0  # final current (charge)
        df.evolve_one_time_step_linear_current(dt, c_fin, div)
        self.assertAlmostEqual(df_manip.get_total_time(path), dt)
        self.assertAlmostEqual(df.get_current(), c_fin)
        df.reset()
        v_fin = 4.3  # final current (lower than initial)
        df.evolve_one_time_step_linear_voltage(dt, v_fin, div)
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        self.assertAlmostEqual(df.get_voltage(), v_fin)
        df.reset()
        p_fin = 16.0  # final power
        df.evolve_one_time_step_linear_power(dt, p_fin,
                                             divisor=div,
                                             start_point=8.0)

        # BELOW: relaxing the error requirement
        # REASON: Like above, power must be calculated with current
        #   and voltage, which both have roundoff error. However, 
        #   running a simulation with multiple restarts likely
        #   accumulates in roundoff errors to account for a larger
        #   error by comparison
        error = .05

        # power is positive
        power = abs(df.get_voltage() * df.get_current())
        self.assertAlmostEqual(power, p_fin, delta=error)
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        df.reset()
        l_fin = 12.0  # final load
        df.evolve_one_time_step_linear_load(dt, l_fin,
                                            divisor=div,
                                            start_point=8.0)
        
        # Below: relax the error further
        # REASON: same situation here as with power, but more
        #   significant when dividing by current because the number
        #   is small with proportionately large roundoff error
        error = 0.5
        # load is positive
        load = abs(df.get_voltage() / df.get_current())
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        self.assertAlmostEqual(load, l_fin, delta=error)
        df.reset()
        
    def test_consistency_pycap_simulation(self):
        # 
        # weak run test; simply ensures that Dualfoil object
        # can be run with pycap.Charge
        #
        df1 = Dualfoil(path=path)  # will use pycap
        df2 = Dualfoil(path=path)  # manual runs
        im = df_manip.InputManager(path=path)

        # testing a charge-to-hold-const-voltage
        # manual 
        # use InputManager to set the input file
        c = -12.0  # constant current
        im.add_new_leg(c, 5.0, 1)
        df1.run()
        df1.outbot.update_output()
        v = 4.54  # constant voltage
        im.add_new_leg(v, 5.0, 0)
        df1.run()
        df1.outbot.update_output()
        
        # pycap simulation
        # build a ptree input
        ptree = PropertyTree()
        ptree.put_double('time_step', 300.0)  # 5 minutes
        ptree.put_string('charge_mode', 'constant_current')
        ptree.put_double('charge_current', 12.0)
        ptree.put_string('charge_stop_at_1', 'voltage_greater_than')
        ptree.put_double('charge_voltage_limit', 4.54)
        ptree.put_bool('charge_voltage_finish', True)
        # hold end voltage after either 5 minutes have passed
        # OR current falls under 1 ampere
        ptree.put_double('charge_voltage_finish_max_time', 300.0)
        ptree.put_double('charge_voltage_finish_current_limit', 1.0)

        const_current_const_voltage = Charge(ptree)
        const_current_const_voltage.run(df2)

        # check the output lists of both devices
        o1 = df1.outbot
        o2 = df2.outbot
        o2.write_main_output()
        self.assertEqual(len(o1.time), len(o2.time))
        for i in range(len(o1.time)):
            self.assertAlmostEqual(o1.time[i], o2.time[i])
            # BELOW: relaxed delta for voltage
            # REASON: dualfoil cuts off its voltages at 5
            #   decimal places, meaning that this end-digit
            #   is subject to roundoff errors
            error = 1e-5
            self.assertAlmostEqual(o1.potential[i], o2.potential[i],
                                   delta=error)
            self.assertAlmostEqual(o1.current[i], o2.current[i])

    def test_accuracy_pycap_simulation(self):
        #
        # tests the accuracy of a pycap simulation against a 
        # straight run dualfoil sim with different timesteps
        #
        df1 = Dualfoil(path=path)  # manual runs
        df2 = Dualfoil(path=path)  # pycap simulation
        im = df_manip.InputManager(path=path)

        # testing a charge-to-hold-const-voltage
        # manual 
        # use InputManager to set the input file
        c = -10.0  # constant charge current
        # charge for 5 minutes straight
        im.add_new_leg(c, 5, 1)
        df1.run()
        df1.outbot.update_output()
        v = 4.539  # expected voltage after 5 minutes
        # hold constant voltage for 3 minutes straight
        im.add_new_leg(v, 3.0, 0)
        df1.run()
        df1.outbot.update_output()

        # pycap simulation
        # build a ptree input
        ptree = PropertyTree()
        ptree.put_double('time_step', 30.0)  # 30 second time step
        ptree.put_string('charge_mode', 'constant_current')
        ptree.put_double('charge_current', 10.0)
        ptree.put_string('charge_stop_at_1', 'voltage_greater_than')
        ptree.put_double('charge_voltage_limit', v)
        ptree.put_bool('charge_voltage_finish', True)
        # hold end voltage after either 3 minutes have passed
        # OR current falls under 1 ampere
        ptree.put_double('charge_voltage_finish_max_time', 180.0)
        ptree.put_double('charge_voltage_finish_current_limit', 1.0)

        const_current_const_voltage = Charge(ptree)
        const_current_const_voltage.run(df2)

        o1 = df1.outbot         # contains sim1 output
        o2 = df2.outbot         # contains sim2 output
        o2.write_main_output()

        # affirm we make it this far and have usable data
        self.assertTrue(len(o1.time) > 0)
        self.assertTrue(len(o2.time) > 0)
        # lengths of data should be different
        self.assertFalse(len(o1.time) == len(o2.time))

        # TEST LOGIC:
        #  -Merge the two outputs into one, sorted by
        #   increasing time stamps.
        #  -Compare the consistency of the two simulations
        #   by checking for smooth changes within the curves
        #   of the combined output lists
        o1.time.extend(o2.time)
        time = ar(o1.time)  # nparray
        o1.potential.extend(o2.potential)
        voltage = ar(o1.potential)  # nparray
        o1.current.extend(o2.current)
        current = ar(o1.current)  # np array
        # create a dictionary with the combined output lists
        output = {'time': time,
                  'voltage': voltage,
                  'current': current
                 }
        # sort based on time, keeping the three types aligned
        key = argsort(output['time'])
        # for using the key to sort the list
        tmp = {'time': [], 'voltage': [], 'current': []}
        for i in key:
            tmp['time'].append(output['time'][i])
            tmp['voltage'].append(output['voltage'][i])
            tmp['current'].append(output['current'][i])
        # reassign ordered set to `output` as nparrays
        output['time'] = ar(tmp['time'])
        output['voltage'] = ar(tmp['voltage'])
        output['current'] = ar(tmp['current'])

        # BELOW: first 20 seconds are identical time stamps;
        #     skip these to avoid errors from incorrect sorting
        # REASON FOR ERROR: Dualfoil only prints time data as 
        #     precice as minutes to three decimal places. So when
        #     the following is generated....
        #       Manual Run         |       Pycap Simulation
        #  (min)     (V)     (amp) |  (min)     (V)     (amp)
        #  .001   4.52345    10.0  |  .001   4.52345    10.0
        #  .001   4.52349    10.0  |  .001   4.52349    10.0
        #           ...                       ...
        #     ...python's `sorted()` function has no way of 
        #     distinguishing entries; it instead returns this:
        # [ 
        #   (.001, 4.52345, 10.0),
        #   (.001, 4.52349, 10.0),  <- these two should
        #   (.001, 4.52345, 10.0),  <-   be switched
        #   (.001, 4.52349, 10.0)
        # ] 
        # SOLUTION: consistency test affirms that the exact same
        #     time step will produce same current and voltage, so
        #     skip ahead to first instance where time stamps will 
        #     be out of alignment
        i = 0
        while output['time'][i] <= 0.4:  # 24 seconds
            i = i + 1
        index_limit = len(output['time'])-1  
        
        # go through and affirm smoothness of curve
        while i < index_limit:
            # Check if time values are the same to 3 decimal places.
            # If so, current and voltage are not guarunteed 
            #   to also be exactly the same, but should be close
            if output['time'][i] == output['time'][i-1]:
                # affirm that current is virtually the same
                self.assertAlmostEqual(output['current'][i],
                                       output['current'][i-1])
                # BELOW: delta is eased slightly
                # REASON: `sorted()` can't tell which entry came
                #   first from same time-stamp if from different
                #   simulations; allow for this with error
                error = 3e-5
                self.assertAlmostEqual(output['voltage'][i],
                                       output['voltage'][i-1],
                                       delta=error)
            else:  
                # Time values are different
                # Check to affirm that the variable NOT being held 
                # constant is steadily increasing / decreasing

                # First part happens in first 4 minutes
                if output['time'][i] <= 5.0:  # part 1, const currrent
                    # current should be equal
                    self.assertEqual(output['current'][i],
                                     output['current'][i-1])
                    # voltage should not have decreased
                    self.assertTrue(output['voltage'][i],
                                    output['voltage'][i-1])
                else:  # part 2, const voltage                
                    # current should be getting less negative
                    self.assertTrue(output['current'][i] <=
                                    output['current'][i-1])
                    # voltage should decrease, then stay at 4.54
                    if output['voltage'][i-1] == 4.54:
                        self.assertEqual(output['voltage'][i],
                                         output['voltage'][i-1])
                    else:
                        self.assertTrue(output['voltage'][i] <=
                                        output['voltage'][i-1])
            # update index
            i = i + 1
    
    def test_compatibilty_other_dualfoil(self):
        # PURPOSE: test to see if Dualfoil object works with the
        #   version of dualfoil without restard (5.2)
        new_path = '/notebooks/docker/dualfoil5-2/'
        df = Dualfoil(path=new_path, input_name='li-ion.in',
                      restart_capable=False)

        # Go through evolve tests. Affirm the following:
        #   1. Variable held constant stays constant
        #   2. Dependent variable changes in an expected way

        v = df.get_voltage()
        # make sure voltage works by ensuring it does not 
        # equal one of the indicators of failure
        self.assertTrue(v != 0)
        self.assertTrue(v != -1)

        # 1. constant current (charge)
        dt = 15.0  # 15 sec
        c = 5.0   # 5 amperes
        df.evolve_one_time_step_constant_current(dt, c)
        v_fin = df.get_voltage()
        c_fin = df.get_current()
        # affirm success
        # current is constant
        self.assertEqual(c, c_fin)
        # charging means potential is increasing
        self.assertTrue(v_fin > v)

        # 2. constant voltage
        df.reset()
        v = df.get_voltage()  # volts
        # (no way to get an accurate initial current)
        df.evolve_one_time_step_constant_voltage(dt, v)
        v_fin = df.get_voltage()
        c_fin = df.get_current()
        # affirm success
        # current should be negative and not zero
        self.assertTrue(c_fin < 0)
        # voltage is constant
        self.assertEqual(v, v_fin)

        # 3. constant load
        df.reset()
        load = 0.7  # ohms-m2
        df.evolve_one_time_step_constant_load(dt, load)
        v_fin = df.get_voltage()
        c_fin = df.get_current()
        # load should be positive
        load_fin = abs(v_fin / c_fin)
        # load should be close to equal, but not quite...
        # REASON: roundoff error of current and voltage
        # SOLUTION: create room for error with `error`
        error = 2e-2
        # affirm success
        self.assertAlmostEqual(load, load_fin, delta=error)
        
        # 4. constant power
        df.reset()
        p = 15.0  # ohms-m2
        df.evolve_one_time_step_constant_power(dt, p)
        v_fin = df.get_voltage()
        c_fin = df.get_current()
        # power is always positive
        p_fin = abs(v_fin * c_fin)
        # affirm success
        self.assertAlmostEqual(p, p_fin, delta=error)

    def test_impedance(self):
        # test the impedance mode of dualfoil5.2
        new_path = '/notebooks/docker/dualfoil5-2/'
        df = Dualfoil(path=new_path, input_name='li-ion.in',
                      restart_capable=False)
        df.run_impedance()

        # list variables for impedance mode
        omega = []   # rad/s
        z_real = []  # ohm*cm2
        z_imag = []  # ohm*cm2

        # extract the output, determine that data make sense
        with open('%sdualfoil5.out' % new_path, 'r') as fout:
            # read until the beginning of the data
            line = fout.readline()
            while line.find('(Rad/s)') == -1:
                line = fout.readline()

            # extract the output
            while line != '':
                if line.find(',') != -1:
                    tmp = line.split(',')
                    # indication of failure: entry is 'NaN'
                    omega.append(float(tmp[0]))
                    z_real.append(float(tmp[1]))
                    z_imag.append(float(tmp[2]))
                line = fout.readline()

        # indication of failure: an entry that is not nonzero
        for i in range(0, len(omega)-1):
            self.assertNotEqual(omega[i], 0)
            self.assertNotEqual(z_real[i], 0)
            self.assertNotEqual(z_imag[i], 0)

if __name__ == '__main__':
    unittest.main()
