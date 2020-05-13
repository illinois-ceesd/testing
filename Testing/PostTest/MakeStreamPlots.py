import PyGnuplot as gp
import os.path
from os import path
import sys
import time
import pyjuke as juke
import ABaTe as abate
import uuid

localrev = juke.getrandkey(juke.localrev)
cwd = os.getcwd()
datapath = cwd + "/stream_data.txt"
testname = "make_stream_plot"
machinename = juke.buildhost
imagepath = cwd + "/stream_" + machinename + "_bw_" + localrev + ".png"

if path.exists(datapath) is False:
    print(testname + ": Datapath(" + datapath + ") did not exist.")
    sys.exit(1)

if path.exists(imagepath):
    os.remove(imagepath)


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
gp.c('set title "Stream on ' + machinename + '"')
gp.c('set output "' + imagepath + '"')
gp.c('set ylabel "Bandwidth (GB/s)"')
gp.c('set xlabel "DataSize (MB)"')
gp.c('plot "' + datapath + '" u ($1/(1024.0*1024.0)):($2/1024.0) w lp t "Stream BW"')

time.sleep(1.0)

print(testname + ': Checking Image Path: ', imagepath)


if path.exists(imagepath):
    abate.output_dart_measurement_file(imagepath)
    sys.exit(0)
else:
    print(testname + ': Failed to create image.')
    sys.exit(1)
