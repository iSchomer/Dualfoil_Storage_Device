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
        error = 1e-2  # represents leniency of error caused
                      # by dualfoil, not the code being tested
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
        Vcut = 4.3    # cutoff voltage
        C = -10.0     # current (discharge)
        error = 1e-4  # this dualfoil simulation is more accurate
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
        # since power is not reported by dualfoil, must
        # calculate with given values; leads to high error caused
        # by significant roundoff when working with small currents;
        # relax the error requirement
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
        # load is positive
        # same situation here as with power, but more significant
        # when dividing by small current with roundoff
        # relax the error requirement
        error = 0.5
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
        c = -10.0  # constant current
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
        ptree.put_double('charge_current', 10.0)
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
        self.assertEqual(len(o1.time), len(o2.time))
        for i in range(len(o1.time)):
            self.assertAlmostEqual(o1.time[i], o2.time[i])
            # relaxed delta because this is the last printed
            # decimal place in output; avoids roundoff errors
            error = 1e-5
            self.assertAlmostEqual(o1.potential[i], o2.potential[i],
                                   delta=error)
            self.assertAlmostEqual(o1.current[i], o2.current[i])

    def test_accuracy_pycap_simulation(self):
        #
        # tests the accuracy of a pycap simulation against a 
        # straight run dualfoil sim with different timesteps
        #
        df1 = Dualfoil(path=path)  # will use pycap
        df2 = Dualfoil(path=path)  # manual runs
        im = df_manip.InputManager(path=path)

        # testing a charge-to-hold-const-voltage
        # manual 
        # use InputManager to set the input file
        c = -10.0  # constant charge current
        # charge for 4 minutes straight
        im.add_new_leg(c, 4.0, 1)
        df1.run()
        df1.outbot.update_output()
        v = 4.54  # constant voltage
        # hold constant voltage for 3 minutes straight
        im.add_new_leg(v, 3.0, 0)
        df1.run()
        df1.outbot.update_output()

        # pycap simulation
        # build a ptree input
        ptree = PropertyTree()
        ptree.put_double('time_step', 20.0)  # 20 second time step
        ptree.put_string('charge_mode', 'constant_current')
        ptree.put_double('charge_current', 10.0)
        ptree.put_string('charge_stop_at_1', 'voltage_greater_than')
        ptree.put_double('charge_voltage_limit', 4.54)
        ptree.put_bool('charge_voltage_finish', True)
        # hold end voltage after either 3 minutes have passed
        # OR current falls under 1 ampere
        ptree.put_double('charge_voltage_finish_max_time', 180.0)
        ptree.put_double('charge_voltage_finish_current_limit', 1.0)

        const_current_const_voltage = Charge(ptree)
        const_current_const_voltage.run(df2)

        o1 = df1.outbot         # contains sim1 output
        o2 = df2.outbot         # contains sim2 output

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
        while output['time'][i] < (1/3):  # 20 seconds
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
                # same with voltage, but delta is eased slightly
                # because sorted() can't tell which entry came first
                # from same time-stamp if from different simulations
                error = 2e-5
                self.assertAlmostEqual(output['voltage'][i],
                                       output['voltage'][i-1],
                                       delta=error)
            else:  
                # Time values are different
                # Check to affirm that the variable NOT being held 
                # constant is steadily increasing / decreasing

                # First part happens in first 4 minutes
                if output['time'][i] <= 4.0:  # part 1, const currrent
                    # current should be equal
                    self.assertEqual(output['current'][i],
                                     output['current'][i-1])
                    # voltage should not have decreased
                    self.assertTrue(output['voltage'][i] >=
                                    output['voltage'][i-1])
                else:  # part 2, const voltage                
                    # current should be decreasing or staying the same
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

if __name__ == '__main__':
    unittest.main()
