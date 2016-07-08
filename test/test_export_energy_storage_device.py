import sys
sys.path.append('/notebooks')
from battery import *
from export_energy_storage_device import Dualfoil

import unittest

path = '/notebooks/docker'

class DualfoilTestCase(unittest.TestCase):
    def test_constant_evolve_functions(self):
        df = Dualfoil()
        df.set_filepath(path)
        self.assertTrue(df.filePath == '/notebooks/docker/')

        # determine that what we want is what we get
        # constant evolve functions
        dt = 1.0  # seconds
        I = 5.0   # current
        df.evolve_one_time_step_constant_current(dt, I)
        self.assertAlmostEqual(abs(df.get_current()), abs(I))
        self.assertAlmostEqual(df.get_total_time(), dt / 60)
        df.reset()
        V = 4.5  # voltage
        df.evolve_one_time_step_constant_voltage(dt, V)
        self.assertAlmostEqual(df.get_voltage(), V)
        self.assertAlmostEqual(df.get_total_time(), dt / 60)
        df.reset()
        P = 1.0  # watts/m2
        df.evolve_one_time_step_constant_power(dt, P)
        self.assertAlmostEqual(df.get_total_time(), dt / 60)
        df.reset()
        L = 1.0  # ohms-m2
        df.evolve_one_time_step_constant_power(dt, P)
        self.assertAlmostEqual(df.get_total_time(), dt / 60)
        df.reset()
        Vcut = 4.3  # cutoff voltage
        C = -10.0   # current (discharge)
        df.evolve_to_voltage_constant_current(C, Vcut)
        self.assertAlmostEqual(df.get_voltage(), Vcut,
                               delta=1e-4)
        df.reset()

    def test_linear_evolve_functions(self):
        df = Dualfoil(Path=path)

        # affirm total time and final dependent value
        dt = 15.0  # seconds
        div = 4    # number of substeps
        Cfin = -10.0  # final current (charge)
        df.evolve_one_time_step_linear_current(dt, Cfin, div)
        self.assertAlmostEqual(df.get_total_time(), dt / 60)
        self.assertAlmostEqual(abs(df.get_current()), abs(Cfin))
        df.reset()
        Vfin = 4.3  # final current (lower than initial)
        df.evolve_one_time_step_linear_voltage(dt, Vfin, div)
        self.assertAlmostEqual(df.get_total_time(), dt / 60)
        self.assertAlmostEqual(df.get_voltage(), Vfin)
        df.reset()
        Pfin = 4.0  # final power
        df.evolve_one_time_step_linear_power(dt, Pfin,
                                             divisor=div)
        self.assertAlmostEqual(df.get_total_time(), dt / 60)
        df.reset()
        Lfin = 12.0  # final load
        df.evolve_one_time_step_linear_load(dt, Lfin,
                                            divisor=div,
                                            start_point=8.0)
        df.outbot.write_main_output()
        self.assertAlmostEqual(df.get_total_time(), dt / 60)
        df.reset()


if __name__ == '__main__':
    unittest.main()
