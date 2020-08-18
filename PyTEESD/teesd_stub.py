import platform
import os
import stat

EXETYPE_UNKNOWN = -1
EXETYPE_PYTHON = 0
EXETYPE_SHELL = 1


def python_path_environment(path):
    environment = f'export PYTHONPATH="{path}:${{PYTHONPATH}}"'
    return environment


def teesd_environment(systemtype):
    environment = 'export PYOPENCL_CTX='
    ctx_string = "''"
    #    if systemtype == 'quartz':
    #        ctx_string = "''"
    environment += ctx_string
    return environment


def open_shell_script(scriptfilename):
    outscript = open(scriptfilename, "w")
    print("#!/bin/bash --login\n\n", file=outscript)
    print("conda activate euler.env", file=outscript)
    return outscript


def close_shell_script(outscript):
    scriptfilename = outscript.name
    outscript.close()
    os.chmod(scriptfilename, stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO)


def get_test_exetypes(path, testlist):
    exetypes = {}
    for testname in testlist:
        nameroot = path+'/'+testname
        if os.path.isfile(nameroot+".py"):
            exetypes[testname] = EXETYPE_PYTHON
        elif os.path.isfile(nameroot+".sh"):
            exetypes[testname] = EXETYPE_SHELL
        else:
            print(f'WARNING: Unknown exe type for test({testname})'
                  f'at path({path}).')
            exetypes[testname] = EXETYPE_UNKNOWN


def get_sysname():
    sysname_long = "workstation"
    sysname_short = "workstation"
    nodename = platform.node()
    systemid = nodename.find("quartz")
    if systemid >= 0:
        sysname_short = "quartz"
        sysname_long = "Quartz@LLNL"
    systemid = nodename.find("lassen")
    if systemid >= 0:
        sysname_short = "lassen"
        sysname_long = "Lassen@LLNL"
    return [sysname_short, sysname_long]


def get_parallel_spawner_command(sysname_short, nproc=1):
    spawner_command = ''
    if nproc <= 0:
        return spawner_command
    spawner_command = f'mpiexec -n {nproc}'
    if sysname_short == 'quartz':
        spawner_command = f'srun -n {nproc}'
    elif sysname_short == 'lassen':
        spawner_command = f'jsrun -p {nproc}'
    return spawner_command


def get_node_allocation_command(sysname_short, nnodes=1, nproc=1, timelim=30,
                                queuename='pdebug'):
    alloc_command = ''
    if nnodes <= 0:
        nnodes = 1
    if nproc <= 0:
        nproc = 1
    if sysname_short == 'quartz':
        alloc_command = f'salloc -N {nnodes} -t {timelim} -p{queuename}'
    if sysname_short == 'lassen':
        alloc_command = f'lalloc {nnodes} -W {timelim} -q {queuename}'
    return alloc_command


def getrandkey(keyseed='', keylen=8):
    import uuid
    numkey = keylen - len(keyseed)
    if numkey > 0:
        key = str(uuid.uuid4())
        key = key[0:numkey]
        keyseed += key
    key = keyseed
    key = key[0:keylen]
    return key


def generate_suite_runner(suitename, suitepath, outputpath,
                          nnodes=1, nproc=1, pycom="python"):

    programname = "generate_suite_runner"
    sysname_short, sysname_long = get_sysname()

    scriptshortname = suitename
    scriptname = scriptshortname + ".sh"
    scriptfilename = outputpath + "/" + scriptname
    spawnername = scriptshortname + "_spawner.sh"
    spawnerpath = outputpath + "/" + spawnername
    resultsfilename = outputpath + "/" + suitename + "_results.txt"
    testlistfile = suitepath + "/testlist.txt"

    print(programname+": Generating runner for suite(", suitename, ")")
    print(programname+": System Name(", sysname_long, ")")
    print(programname+": scriptfilename  = ", scriptfilename)
    print(programname+": resultsfilename = ", resultsfilename)

    pathroot = suitepath+"/"

    spawner_command = get_parallel_spawner_command(sysname_short, nproc=nproc)
    nodealloc_command = get_node_allocation_command(sysname_short, nnodes=nnodes,
                                                    nproc=nproc, timelim=30,
                                                    queuename='pdebug')
    print(f'{programname}: (spawner_command)  = ({spawner_command})')
    print(f'{programname}: (nodealloc_command) = ({nodealloc_command})')

    # Generate the spawner script
    outscript = open_shell_script(spawnerpath)
    print('\n\n', file=outscript)
    print(python_path_environment(teesdpath), file=outscript)
    print(teesd_environment(sysname_short), file=outscript)
    print(f'\n\nnumProcs="{nproc}"', file=outscript)
    print("if [ ! -z ${1} ]; then numProcs=${1}; fi", file=outscript)
    print(f'printf "{scriptshortname}: Running testing suite: ({suitename})\\n"',
          file=outscript)
    print(f'rm -f {resultsfilename}', file=outscript)
    print(f'for testname in $(cat {testlistfile})\n', file=outscript)
    print("do\n", file=outscript)
    print(f'  printf "{scriptshortname}: Running test (${{testname}})...\\n"',
          file=outscript)
    print(f'  printf "{scriptshortname}: Command: {spawner_command} "',
          file=outscript)
    print(f'  testfilenameroot={pathroot}${{testname}}', file=outscript)
    print('  if [ -f "${testfilenameroot}.py" ]; then', file=outscript)
    print(f'    printf " {pycom} ${{testfilenameroot}}.py "', file=outscript)
    print('  elif [ -f "${testfilenameroot}.sh" ]; then', file=outscript)
    print('    printf " ${testfilenameroot}.sh "', file=outscript)
    print('  elif [ -f "${testfilenameroot}" ]; then', file=outscript)
    print('    printf " ${testfilenameroot} "', file=outscript)
    print('  else', file=outscript)
    print('    printf " (NO VALID TEST FILE FOUND!!)\\n"', file=outscript)
    print('  fi', file=outscript)
    print(f'  printf " {resultsfilename}\\n"', file=outscript)
    print('  date', file=outscript)
    print('  if [ -f "${testfilenameroot}.py" ]; then', file=outscript)
    print(f'    {spawner_command} {pycom} ${{testfilenameroot}}.py'
          f' {resultsfilename}', file=outscript)
    print('  elif [ -f "${testfilenameroot}.sh" ]; then', file=outscript)
    print(f'    {spawner_command} ${{testfilenameroot}}.sh'
          f' {resultsfilename}', file=outscript)
    print('  elif [ -f "${testfilenameroot}" ]; then', file=outscript)
    print(f'    {spawner_command} ${{testfilenameroot}} {resultsfilename}',
          file=outscript)
    print('  else', file=outscript)
    print(f'    printf "{scriptshortname}: Skipping non-existent test:'
          f' ${{testfilameroot}}\\n"',
          file=outscript)
    print('  fi', file=outscript)
    print('  date', file=outscript)
    print('done', file=outscript)
    close_shell_script(outscript)

    # Generate the suite runner script
    outscript2 = open_shell_script(scriptfilename)
    print(f'{nodealloc_command} {spawnerpath}', file=outscript2)
    print("errorCode=$?", file=outscript2)
    print("date", file=outscript2)
    print("exit $errorCode", file=outscript2)
    close_shell_script(outscript2)
