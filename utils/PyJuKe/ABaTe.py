def output_dart_measurement_file(path_to_file,
                                 name="MeasurementFile",
                                 filetype="image/png"):
    print('<DartMeasurementFile name="'+name+'" type="'+filetype+'">')
    print(path_to_file)
    print('</DartMeasurementFile>')

def output_dart_measurement(measurementData,name="measurement",
                            mdattype="numeric/double",
                            encoding="none",compression="none"):
    print('<DartMeasurement name="'+name+'" type="'+mdattype+'"'+
          ' encoding="'+encoding+'" compression="'+compression+'">')
    print(measurementData)
    print('</DartMeasurement>')
