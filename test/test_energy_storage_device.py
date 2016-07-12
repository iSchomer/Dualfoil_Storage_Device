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
        
    def test_pycap_simulation(self):
        # 
        # weak run test; simply ensures that Dualfoil object
        # can be run with pycap.Charge
        #
        D1 = Dualfoil(path=path)  # will use pycap
        D2 = Dualfoil(path=path)  # manual runs
        im = df_manip.InputManager(path=path)

        # testing a charge-to-hold-const-voltage
        # manual 
        # use df_manip to set the input file
        c = -10.0  # constant current
        im.add_new_leg(c, 5.0, 1)
        D1.run()
        D1.outbot.update_output()
        v = 4.54  # constant voltage
        im.add_new_leg(v, 5.0, 0)
        D1.run()
        D1.outbot.update_output()

        # auto
        # build a ptree input
        ptree = PropertyTree()
        ptree.put_double('time_step', 300.0)  # 5 minutes
        ptree.put_string('charge_mode', 'constant_current')
        ptree.put_double('charge_current', 10.0)
        ptree.put_string('charge_stop_at_1', 'voltage_greater_than')
        ptree.put_double('charge_voltage_limit', 4.54)
        ptree.put_bool('charge_voltage_finish', True)
        ptree.put_double('charge_voltage_finish_max_time', 300.0)
        ptree.put_double('charge_voltage_finish_current_limit', 1.0)

        cccv = Charge(ptree)
        cccv.run(D2)
        D2.outbot.write_main_output()

        # check the output lists of both devices
        o1 = D1.outbot
        o2 = D2.outbot
        self.assertEqual(len(o1.time), len(o2.time))
        for i in range(len(o1.time)):
            self.assertAlmostEqual(o1.time[i], o2.time[i])
            # relaxed delta because this is the last printed
            # decimal place in output
            self.assertAlmostEqual(o1.potential[i], o2.potential[i],
                                   delta=1e-5)
            self.assertAlmostEqual(o1.curr[i], o2.curr[i])

if __name__ == '__main__':
    unittest.main()
