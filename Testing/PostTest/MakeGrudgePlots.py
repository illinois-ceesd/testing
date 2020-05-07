import PyGnuplot as gp
# import numpy as np
import os.path
from os import path
import sys
import time
# import parjuke
import pyjuke as juke
import ABaTe as abate
import uuid

cwd = os.getcwd()
localrev = juke.getrandkey(juke.localrev)

imagepath = cwd + "/grudgempi_partimes_" + localrev + ".png"
datapath = cwd + "/GrudgeMPI_ParTimes_4"
testname = "MakeImages"

if path.exists(datapath) is False:
    print(testname+": Datapath("+datapath+") did not exist.")
    sys.exit(1)

if path.exists(imagepath):
    os.remove(imagepath)

gp.default_term = ''
gp.c('set term png')
gp.c('set boxwidth 0.5')
gp.c('set style fill solid')
gp.c('set title "Grudge Timing"')
gp.c('set output "' + imagepath + '"')
gp.c('set ylabel "time(s)"')
gp.c('set xlabel "routine"')
gp.c('plot "' + datapath + '" u 0:3:xtic(1) w boxes t ""')

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
