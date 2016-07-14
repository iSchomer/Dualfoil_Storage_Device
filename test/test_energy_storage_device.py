import sys
sys.path.append('/notebooks')
from battery import *
from energy_storage_device import Dualfoil
from pycap import PropertyTree, Charge

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
        self.assertAlmostEqual(df.get_current(), I*-1)
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
        df.evolve_one_time_step_constant_power(dt, P)
        power = df.get_voltage() * df.get_current()
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        self.assertAlmostEqual(power, P, delta=1e-2)
        df.reset()
        L = 0.7  # ohms-m2
        df.evolve_one_time_step_constant_load(dt, L)
        load = df.get_voltage() / df.get_current()
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        self.assertAlmostEqual(load, L, delta=1e-2)
        df.reset()
        Vcut = 4.3  # cutoff voltage
        C = -10.0   # current (discharge)
        df.evolve_to_voltage_constant_current(C, Vcut)
        self.assertAlmostEqual(df.get_voltage(), Vcut,
                               delta=1e-4)
        df.reset()

    def test_consistency_linear_evolve_functions(self):
        df = Dualfoil(path=path)

        # affirm total time and final dependent value
        dt = 15.0  # seconds
        div = 4    # number of substeps
        Cfin = -10.0  # final current (charge)
        df.evolve_one_time_step_linear_current(dt, Cfin, div)
        self.assertAlmostEqual(df_manip.get_total_time(path), dt)
        self.assertAlmostEqual(df.get_current(), Cfin*-1)
        df.reset()
        Vfin = 4.3  # final current (lower than initial)
        df.evolve_one_time_step_linear_voltage(dt, Vfin, div)
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        self.assertAlmostEqual(df.get_voltage(), Vfin)
        df.reset()
        Pfin = 16.0  # final power
        df.evolve_one_time_step_linear_power(dt, Pfin,
                                             divisor=div,
                                             start_point=8.0)
        power = df.get_voltage() * df.get_current()
        self.assertAlmostEqual(power, Pfin, delta=.05)
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        df.reset()
        Lfin = 12.0  # final load
        df.evolve_one_time_step_linear_load(dt, Lfin,
                                            divisor=div,
                                            start_point=8.0)
        load = df.get_voltage() / df.get_current()
        self.assertAlmostEqual(df_manip.get_total_time(path),
                               dt)
        self.assertAlmostEqual(load, Lfin, delta=0.5)
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
            # decimal place in output
            self.assertAlmostEqual(o1.potential[i], o2.potential[i],
                                   delta=1e-5)
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

        o1 = df1.outbot
        o2 = df2.outbot
        o2.write_main_output()

        # affirm we make it this far and have usable data
        self.assertTrue(len(o1.time) > 0)
        self.assertTrue(len(o2.time) > 0)
        # lengths of data should be different
        self.assertFalse(len(o1.time) == len(o2.time))

        # Create one combined list of tuples with form 
        #         (time, current, voltage).
        # Compare the consistency of the two simulation ouputs
        # by checking smooth changes within the curves of the 
        # combined output lists
        output = []
        for i in range(len(o1.time)):
            tuple = (o1.time[i], o1.current[i], o1.potential[i])
            output.append(tuple)
        for i in range(len(o1.time)):
            tuple = (o2.time[i], o2.current[i], o2.potential[i])
            output.append(tuple)
        # sort the output based on time values
        output = sorted(output, key=lambda output: output[0])
        # Delete any identical tuples from the list
        # These would be where the two simulations aligned.
        # Not relevant to this test; consistency test affirms
        # that identical time stamps will produce identical output
        j = 0
        while j < len(output)-2:
            if (output[j][0] == output[j+1][0] and
                output[j][1] == output[j+1][1] and
                output[j][2] == output[j+1][2]):
                output.pop(j)
            j = j + 1

        # find start point; first 20 seconds are identical time stamps;
        # skip these to avoid errors from incorrect sorting
        index = 0
        while output[index][0] < (1/3):  # 20 seconds
            index = index + 1
        limit = len(output)-2
        
        # go through and affirm smoothness of curve
        while index < limit:
            # if time values are the same
            if output[index][0] == output[index-1][0]:
                # affirm that current is virtually the same
                self.assertAlmostEqual(output[index][1],
                                       output[index-1][1])
                # same with voltage, but delta is eased slightly
                # because sorted() can't tell which entry came first
                # from same time-stamp if from different simulations
                self.assertAlmostEqual(output[index][2],
                                       output[index-1][2],
                                       delta = 2e-5)
            else:  # time values are different
                if output[index][0] <= 4.0:  # part 1, const currrent
                    # current should be equal
                    self.assertEqual(output[index-1][1],
                                     output[index][1])
                    # voltage should not have decreased
                    self.assertTrue(output[index][2] >=
                                    output[index-1][2])
                else:  # part 2, const voltage                
                    # current should be increasing or staying the same
                    self.assertTrue(output[index][1] >=
                                    output[index-1][1])
                    # voltage should decrease, then stay at 4.54
                    if output[index-1][2] == 4.54:
                        self.assertEqual(output[index-1][2], output[index][2])
                    else:
                        self.assertTrue(output[index][2] <= output[index-1][2])
            # update index
            index = index + 1

if __name__ == '__main__':
    unittest.main()
