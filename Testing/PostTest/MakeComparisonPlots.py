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
testname = "make_stream_plot"
machinename = juke.buildhost

imagepath = cwd + "/loopy.c.bw.comparison_" + localrev + ".png"

zaxpydatapath = cwd + "/zaxpy_gridscale_timing"
testname = "comparison_plot"
tmpdatapath = cwd + "/zaxpy_gridscale_data.txt"

streamdatapath = cwd + "/stream_data.txt"

if path.exists(zaxpydatapath) is False:
    print(testname + ": Datapath(" + zaxpydatapath + ") did not exist.")
    sys.exit(1)

if path.exists(streamdatapath) is False:
    print(testname + ": Datapath(" + streamdatapath + ") did not exist.")
    sys.exit(1)

if path.exists(imagepath):
    os.remove(imagepath)
if path.exists(tmpdatapath):
    os.remove(tmpdatapath)

os.system("touch "+tmpdatapath)
gridsize = 16
while gridsize < 1024:
    numbytes = pow(gridsize,3) * 24.0 / (pow(1024,2))
    os.system('printf "' + str(numbytes) + ' " >> ' + tmpdatapath)
    os.system('grep "zaxpy3\-' + str(gridsize) +
              '" ' + datapath + ' >> ' + tmpdatapath)
    gridsize = 2*gridsize


gp.default_term = ''
#gp.c('set ylabel font "Helvetica, 16"')
#gp.c('set xlabel font "Helvetica, 16"')
#gp.c('set key font "Helvetica, 12"')
#gp.c('set tic font "Helvetica, 12"')
gp.c('set logscale x 2')
gp.c('set logscale y 2')
gp.c('set term png')
gp.c('set boxwidth 0.5')
gp.c('set style fill solid')
gp.c('set title "ZAXPY Loopy vs C on ' + machinename + '"')
gp.c('set output "' + imagepath + '"')
gp.c('set ylabel "Bandwidth (GB/s)"')
gp.c('set xlabel "DataSize (MB)"')
plot_command = 'plot "' + tmpdatapath +
'" u 1:(($1/1024.0)/($4/10.0)) w lp t "Loopy/ZAXPY BW"' +
',"" u ($1/(1024.0*1024.0)):($2/1024.0) w lp t "Stream BW"'
gp.c(plot_command)

time.sleep(1.0)

print(testname + ': Checking Image Path: ', imagepath)


if path.exists(imagepath):
    abate.output_dart_measurement_file(imagepath)
    sys.exit(0)
else:
    print(testname + ': Failed to create image.')
    sys.exit(1)
