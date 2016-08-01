__all__ = ['graph_main_output_3D', 'graph_profiles_3D']

from matplotlib import cm
from mpl_toolkits.mplot3d import axes3d
from matplotlib.ticker import LinearLocator, FormatStrFormatter
import matplotlib.pyplot as plt
import numpy as np

def graph_main_output_3D(x, y, z, labelX=None, labelY=None,
                         labelZ=None, interactive=False):
    
    """
    Graphs a 3D line of main output data
    
    Parameters
    ----------
    x, y, z : list
        output data found in dualfoil5.out
    labelX, labelY, labelZ : str, optional
        label for each corresponding axis
    interactive : bool, optional
        flag to turn on or off an interactive display
    """

    fig = plt.figure(figsize=(10,8))
    ax = fig.gca(projection='3d', xlabel=labelX,
                 ylabel=labelY, zlabel=labelZ)

    ax.plot(x, y, z)

    ax.zaxis.set_major_locator(LinearLocator(8))
    ax.zaxis.set_major_formatter(FormatStrFormatter('%.01f'))

    if interactive:
        for angle in range(0, 360):
            ax.view_init(30, angle)
            plt.draw()
    else:
        ax.view_init(30, 225)
    plt.show()

def graph_profiles_3D(x, y, z, clarity=5,
                      labelX='Distance across cell',
                      labelY='Time', labelZ=None,
                      interactive=False):

    """
    Graphs profile output data to a surface
    
    Parameters
    ----------
    x : list
        recommended to use 'distance' data from profiles.out
    y : list
        recommended to use 'time' data from profiles.out
    Z : list
        dependent variable to be mapped across distance over time
    clarity : int, optional
        degree of plot accuracy; 1 for highest accuracy
    labelX, labelY, labelZ : str, optional
        label for each corresponding axis
    interactive : bool
        flag to turn on of off an interactive display
    """
    
    fig = plt.figure(figsize=(10,8))
    ax = fig.gca(projection='3d', xlabel=labelX,
                 ylabel=labelY, zlabel=labelZ)

    #all parameters must be 2D arrays; x&y mesh together
    x = np.array(x)
    y = np.array(y)
    x, y = np.meshgrid(x, y, sparse=True)
    for l in range(len(z)):
        z[l] = np.array(z[l])
    z = np.array(z)
    surf = ax.plot_surface(x, y, z, rstride=clarity, cstride=clarity,
                           linewidth=0, cmap=cm.plasma)
    fig.colorbar(surf, shrink=0.5, aspect=5)

    if interactive:
        for angle in range(0,360):
            ax.view_init(30, angle)
            plt.draw()
    else:
        ax.view_init(30, -30)
    plt.show()