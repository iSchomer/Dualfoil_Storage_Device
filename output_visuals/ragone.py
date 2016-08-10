# Copyright (c) 2016, the Cap authors.
#
# This file is subject to the Modified BSD License and may not be distributed
# without copyright and license information. Please refer to the file LICENSE
# for the text and further information on this license.
#
# NOTE : this is a modified version of ragone_plot.py, part of  the Cap project
# from ORNL-CEES.

from numpy import trapz, count_nonzero, array, append, power, argsort
from matplotlib import pyplot
from copy import copy
from pycap import (PropertyTree, Charge, Discharge, initialize_data,
                   save_data, Experiment, Observer)
from time import time

__all__ = ['plot_ragone', 'RagoneAnalysis']

def plot_ragone(data, figure=None, ls='r-o'):
    power = data['power']
    energy = data['energy']
    plot_linewidth = 3
    label_fontsize = 30
    tick_fontsize = 20
    if figure:
        pyplot.figure(figure.number)
    else:
        pyplot.figure(figsize=(16, 12))
    pyplot.plot(power, energy, ls, lw=plot_linewidth)
    pyplot.xscale('log')
    pyplot.yscale('log')
    pyplot.xlabel(r'$\mathrm{Power\  [W]}$', fontsize=label_fontsize)
    pyplot.ylabel(r'$\mathrm{Energy\ [J]}$', fontsize=label_fontsize)
    pyplot.gca().get_xaxis().set_tick_params(labelsize=tick_fontsize)
    pyplot.gca().get_yaxis().set_tick_params(labelsize=tick_fontsize)

def run_discharge(device, ptree):
    data = initialize_data()

    # (re)charge the device
    initial_voltage = ptree.get_double('initial_voltage')

    charge_database = PropertyTree()
    charge_database.put_string('charge_mode', 'constant_current')
    charge_database.put_double('charge_current', 10.0)
    charge_database.put_string('charge_stop_at_1', 'voltage_greater_than')
    charge_database.put_double('charge_voltage_limit', initial_voltage)
    charge_database.put_bool('charge_voltage_finish', True)
    charge_database.put_double('charge_voltage_finish_current_limit', 1e-2)
    charge_database.put_double('charge_voltage_finish_max_time', 600)
    charge_database.put_double('charge_rest_time', 0)
    charge_database.put_double('time_step', 10.0)

    charge = Charge(charge_database)
    start = time()
    charge.run(device, data)
    end = time()
    # used for tracking time of this substep
    print('Charge: %s min' % ((end-start) / 60))

    data['time'] -= data['time'][-1]

    # discharge at constant power
    discharge_power = ptree.get_double('discharge_power')
    final_voltage = ptree.get_double('final_voltage')
    time_step = ptree.get_double('time_step')

    discharge_database = PropertyTree()
    discharge_database.put_string('discharge_mode', 'constant_power')
    discharge_database.put_double('discharge_power', discharge_power)
    discharge_database.put_string('discharge_stop_at_1', 'voltage_less_than')
    discharge_database.put_double('discharge_voltage_limit', final_voltage)
    discharge_database.put_double('discharge_rest_time', 10 * time_step)
    discharge_database.put_double('time_step', time_step)

    discharge = Discharge(discharge_database)
    start = time()
    discharge.run(device, data)
    end = time()
    # used for tracking time of this substep
    print('Discharge: %s min' % ((end-start) / 60))

    return data

def examine_discharge(data):
    time = data['time']
    current = data['current']
    voltage = data['voltage']
    power = current[:] * voltage[:]
    mask = time[:] <= 0
    energy_in = trapz(power[mask], time[mask])
    mask = time[:] >= 0
    energy_out = trapz(power[mask], time[mask])

    return [energy_in, energy_out]

class RagoneAnalysis(Experiment):
    '''Record a Ragone plot
    Performs a series of discharge at various rates and neasures the energy.
    Attributes
    ----------
    _discharge_power_lower_limit : float
    _discharge_upper_lower_limit : float
    _steps_per_decade : int
    _ptree : PropertyTree
    _data : dict
        Stores power and energy as numpy.array(s) of floating point numbers.
    See Also
    --------
    RagonePlot
    '''
    def __new__(cls, *args, **kwargs):
        return object.__new__(RagoneAnalysis)

    def __init__(self, ptree):
        Experiment.__init__(self)
        self._discharge_power_lower_limit = ptree.get_double(
            'discharge_power_lower_limit')
        self._discharge_power_upper_limit = ptree.get_double(
            'discharge_power_upper_limit')
        self._steps_per_decade = ptree.get_int('steps_per_decade')
        self._min_steps_per_discharge = ptree.get_int(
            'min_steps_per_discharge')
        self._max_steps_per_discharge = ptree.get_int(
            'max_steps_per_discharge')
        self._time_step_initial_guess = ptree.get_double('time_step')
        self._ptree = copy(ptree)
        self.reset()

    def reset(self):
        self._ptree.put_double('time_step', self._time_step_initial_guess)
        self._data = {
            'energy': array([], dtype=float),
            'power': array([], dtype=float)
        }

    def run(self, device, fout=None):
        discharge_power = self._discharge_power_lower_limit
        while discharge_power <= self._discharge_power_upper_limit:
            # print discharge_power
            self._ptree.put_double('discharge_power', discharge_power)
            try:
                # this loop control the number of time steps in the discharge
                for measurement in ['first', 'second']:
                    data = run_discharge(device, self._ptree)
                    if fout:
                        path = 'ragone_chart_data'
                        path += '/power=' + str(discharge_power) + 'W'
                        path += '/' + measurement
                        save_data(data, path, fout)
                    energy_in, energy_out = examine_discharge(data)
                    steps = count_nonzero(data['time'] > 0)
                    if steps >= self._min_steps_per_discharge:
                        break
                    else:
                        time_step = data['time'][-1]
                        time_step /= self._max_steps_per_discharge
                        self._ptree.put_double('time_step', time_step)

            except RuntimeError:
                print('Failed to discharge at {0} watt'.format(
                    discharge_power))
                break
            self._data['energy'] = append(self._data['energy'],
                                          -energy_out)
            self._data['power'] = append(self._data['power'],
                                         discharge_power)
            discharge_power *= power(10.0, 1.0 / self._steps_per_decade)
            self.notify()
Experiment._builders['RagoneAnalysis'] = RagoneAnalysis