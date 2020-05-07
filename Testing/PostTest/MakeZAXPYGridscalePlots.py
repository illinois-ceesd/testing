import PyGnuplot as gp
# import numpy as np
import os.path
from os import path
import sys
import time
# import parjuke
import pyjuke as juke
import ABaTe as abate
# import subprocess
import uuid

localrev = juke.getrandkey(juke.localrev)
cwd = os.getcwd()
imagepath = cwd + "/zaxpy_gridscale_" + localrev + ".png"
datapath = cwd + "/zaxpy_gridscale_timing"
testname = "zaxpy_gridscale_plot"
tmpdatapath = cwd + "/zaxpy_gridscale_data.txt"

if path.exists(datapath) is False:
    print(testname + ": Datapath(" + datapath + ") did not exist.")
    sys.exit(1)

if path.exists(imagepath):
    os.remove(imagepath)
if path.exists(tmpdatapath):
    os.remove(tmpdatapath)

os.system("touch "+tmpdatapath)
gridsize = 16
while gridsize < 2048:
    os.system('grep "zaxpy3\-' + str(gridsize) +
              '" ' + datapath + ' >> ' + tmpdatapath)
    gridsize = 2*gridsize


gp.default_term = ''
gp.c('set term png')
gp.c('set boxwidth 0.5')
gp.c('set style fill solid')
gp.c('set title "Loopy/ZAXPY grid scaling"')
gp.c('set output "' + imagepath + '"')
gp.c('set ylabel "time(s)"')
gp.c('set xlabel "gridsize"')
gp.c('plot "' + tmpdatapath + '" u 0:3:xtic(1) w boxes t ""')

time.sleep(1.0)

print(testname + ': Checking Image Path: ', imagepath)


if path.exists(imagepath):
    abate.output_dart_measurement_file(imagepath)
#    print('<DartMeasurementFile name="GrugeMPITiming" type="image/png">')
#    print(imagepath)
#    print('</DartMeasurementFile>')
    sys.exit(0)
else:
    print(testname + ': Failed to create image.')
    sys.exit(1)
