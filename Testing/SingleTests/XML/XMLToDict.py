import xmltodict
import teesd
import sys

path_to_data_file = teesd.sourcepath+"/Testing/Data/XML/xmltodict.xml"
filedesc = open(path_to_data_file, "r")
mydict = xmltodict.parse(filedesc.read())

print(mydict)

if(mydict['teesd'] != 'Pass'):
    sys.exit(1)
sys.exit(0)
