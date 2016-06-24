
# coding: utf-8

# In[8]:

import unittest
import subprocess
import io_manip

class testOutputs(unittest.TestCase):
    
    #NOTE: will have to change path directories
    
    def setUp(self):
        #do straight test first and gather output
        subprocess.call('cd /Users/ips/dualfoil/ && ./dualfoil', shell=True)
        t1, n1, p1, v1, u1, c1, tm1, h1 = io_manip.extract_main_output('/Users/ips/dualfoil/dualfoil5.out')
        self.ar1 = [t1, n1, p1, v1, u1, c1, tm1, h1]
        
        #then do restarted run, using input file
        subprocess.call('python main.py < sampleTest.txt', shell=True)
        t2, n2, p2, v2, u2, c2, tm2, h2 = io_manip.extract_main_output('/Users/ips/dualfoil/combinedOutput.out')
        self.ar2 = [t2, n2, p2, v2, u2, c2, tm2, h2]
           
    def test_values(self):
        self.assertEqual(len(self.ar1), len(self.ar2))
        self.assertEqual(len(self.ar1[0]), len(self.ar2[0]))
        for i in range(len(self.ar1)):
            for j in range(len(self.ar1[0])):
                self.assertAlmostEqual(self.ar1[i][j], self.ar2[i][j], delta=(self.ar1[i][j]/50000))

        

if __name__ == '__main__':
    unittest.main()
