# Example from (pypi.org/project/PyGnuplot)
import PyGnuplot as gp
import numpy as np
import os.path
from os import path
import sys
import time

cwd = os.getcwd()
pathcheck = cwd+"/myfigure.png"
datapath = cwd+"/tmp.dat"

if path.exists(pathcheck):
    os.remove(pathcheck)
if path.exists(datapath):
    os.remove(datapath)

X = np.arange(10)
Y = np.sin(X/(2*np.pi))
Z = Y**2.0

gp.default_term=''
gp.s([X, Y, Z])
gp.c('set term png')
gp.c('set output "myfigure.png"')
gp.c('plot "tmp.dat" u 1:2 w lp t "Sin", "" using 1:3 w lp t "Sin2"')

time.sleep(1.0)

print('Checking Path: ',pathcheck)

if path.exists(pathcheck):
    print('<DartMeasurementFile name="GnuPlotTest" type="image/png">')
    print(pathcheck)
    print('</DartMeasurementFile>')
    sys.exit(0)
else:
    print('Path did not exist.')

sys.exit(1)

