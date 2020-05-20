import pyopencl as cl

testname = 'HavePyOpenCL'

print(testname+': PyOpenCL version: ' + cl.VERSION_TEXT)
print(testname+': OpenCL header version: ' +
      '.'.join(map(str, cl.get_cl_header_version())))
print(testname+': ')

# Get installed platforms (SDKs)
print(testname+': - Installed platforms (SDKs) and available devices:')
platforms = cl.get_platforms()

for plat in platforms:
    indent = ''

    # Get and print platform info
    print(testname + ': ' + indent +
          '{} ({})'.format(plat.name, plat.vendor))
    indent = '\t'
    print(testname + ': ' + indent + 'Version: ' + plat.version)
    print(testname + ': ' + indent + 'Profile: ' + plat.profile)
    print(testname + ': ' + indent + 'Extensions: ' +
          str(plat.extensions.strip().split(' ')))

    # Get and print device info
    devices = plat.get_devices(cl.device_type.ALL)

    print(testname + ': ' + indent + 'Available devices: ')
    if not devices:
        print(testname + ': ' + indent + '\tNone')

    for dev in devices:
        indent = '\t\t'
        print(testname + ': ' + indent +
              '{} ({})'.format(dev.name, dev.vendor))

        indent = '\t\t\t'
        flags = [('Version', dev.version),
                 ('Type', cl.device_type.to_string(dev.type)),
                 ('Extensions', str(dev.extensions.strip().split(' '))),
                 ('Memory (global)', str(dev.global_mem_size)),
                 ('Memory (local)', str(dev.local_mem_size)),
                 ('Address bits', str(dev.address_bits)),
                 ('Max work item dims', str(dev.max_work_item_dimensions)),
                 ('Max work group size', str(dev.max_work_group_size)),
                 ('Max compute units', str(dev.max_compute_units)),
                 ('Driver version', dev.driver_version),
                 ('Image support', str(bool(dev.image_support))),
                 ('Little endian', str(bool(dev.endian_little))),
                 ('Device available', str(bool(dev.available))),
                 ('Compiler available', str(bool(dev.compiler_available)))]

        [print(testname + ': ' + indent +
               '{0:<25}{1:<10}'.format(name + ':', flag)) for name, flag in flags]

        # Device version string has the following syntax, extract the number like this
        # OpenCL<space><major_version.minor_version><space><vendor-specific information>
        version_number = float(dev.version.split(' ')[1])

    print(testname + ': ')

