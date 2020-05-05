import xmltodict
import pyjuke as juke
import sys

path_to_data_file = juke.sourcepath+"/Testing/Data/XML/xmltodict.xml"
filedesc = open(path_to_data_file,"r")
mydict = xmltodict.parse(filedesc.read())

print(mydict)

if(mydict['juke'] != 'Pass'):
    sys.exit(1)
sys.exit(0)
