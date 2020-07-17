import PyGnuplot as gp
import os.path
from os import path
import sys
import time
import teesd
import abate
# import subprocess
# import uuid

localrev = teesd.getrandkey(teesd.localrev)
cwd = os.getcwd()
imagepath = cwd + "/zaxpy_gridscale_" + localrev + ".png"
datapath = cwd + "/zaxpy_gridscale_timing"
testname = "zaxpy_gridscale_plot"
tmpdatapath = cwd + "/zaxpy_gridscale_data.txt"
machinename = teesd.buildhost

if path.exists(datapath) is False:
    print(testname + ": Datapath(" + datapath + ") did not exist.")
    sys.exit(1)

if path.exists(imagepath):
    os.remove(imagepath)
if path.exists(tmpdatapath):
    os.remove(tmpdatapath)

os.system("touch "+tmpdatapath)
gridsize = 16
while gridsize < 1024:
    numbytes = pow(gridsize, 3) * 24.0 / (pow(1024, 2))
    os.system('printf "' + str(numbytes) + ' " >> ' + tmpdatapath)
    os.system(r'grep "zaxpy3\-' + str(gridsize) +
              '" ' + datapath + ' >> ' + tmpdatapath)
    gridsize = 2*gridsize

gp.default_term = ''
# gp.c('set ylabel font "Helvetica, 16"')
# gp.c('set xlabel font "Helvetica, 16"')
# gp.c('set key font "Helvetica, 12"')
# gp.c('set tic font "Helvetica, 12"')
gp.c('set logscale x 2')
gp.c('set logscale y 2')
gp.c('set term png')
gp.c('set boxwidth 0.5')
gp.c('set style fill solid')
gp.c('set title "Loopy/ZAXPY on ' + machinename + '"')
gp.c('set output "' + imagepath + '"')
gp.c('set ylabel "Bandwidth (GB/s)"')
gp.c('set xlabel "DataSize (MB)"')
gp.c('plot "' + tmpdatapath +
     '" u 1:(($1/1024.0)/($4/10.0)) w lp t "Loopy/ZAXPY BW"')

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
