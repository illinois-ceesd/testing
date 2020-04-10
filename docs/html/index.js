var index =
[
    [ "Introduction", "index.html#plascom2_intro", null ],
    [ "Documentation Layout", "index.html#layout", null ],
    [ "Guides", "index.html#xpacc_guides", null ],
    [ "Getting Started with PlasCom2", "getting_started.html", [
      [ "Quickstart", "getting_started.html#quickstart", null ],
      [ "Getting the code", "getting_started.html#obtain", null ],
      [ "Building PlasCom2", "getting_started.html#build", [
        [ "Prerequisites", "getting_started.html#prereq", null ],
        [ "Configuration and Compiling", "getting_started.html#configbuild", null ]
      ] ],
      [ "Testing", "getting_started.html#test", null ]
    ] ],
    [ "User Reference", "user_reference.html", [
      [ "Basic Usage", "user_reference.html#basic", [
        [ "Inline Help", "user_reference.html#inlinehelp", null ],
        [ "Typical Usage", "user_reference.html#typicalusage", null ]
      ] ],
      [ "Configuration File", "user_reference.html#configurationfile", [
        [ "PlasCom2 Section", "user_reference.html#configplascom2", null ],
        [ "Geometry Section", "user_reference.html#configgeometry", [
          [ "Grid Region Configuration", "user_reference.html#configgridboundaries", null ],
          [ "Grid Generator Configuration", "user_reference.html#configgridgen", null ],
          [ "Geometry Processing Configuration", "user_reference.html#configgeomproc", null ]
        ] ],
        [ "Domain Section", "user_reference.html#configdomain", [
          [ "Data Dictionary Stuff", "user_reference.html#configdata", null ],
          [ "Boundary Condition Stuff", "user_reference.html#configbc", null ],
          [ "Solution Initialization Stuff", "user_reference.html#configsoln", null ]
        ] ]
      ] ]
    ] ],
    [ "Theory", "theory.html", [
      [ "Fluid Dynamics", "theory.html#navierStokes", [
        [ "Conservation equations", "theory.html#conserve", null ],
        [ "Viscous stress constitutive relation", "theory.html#viscous", null ],
        [ "Heat flux constitutive relation", "theory.html#heat", null ]
      ] ],
      [ "Transport Coefficient Models", "theory.html#trans", [
        [ "Power Law", "theory.html#transModelPower", null ]
      ] ],
      [ "Equations of state", "theory.html#eos", [
        [ "Calorically perfect ideal gas", "theory.html#calIdealGas", null ]
      ] ],
      [ "Non-dimensionalization", "theory.html#nondimen", [
        [ "Non-dimensional equation of state", "theory.html#nonDimenEOS", null ]
      ] ],
      [ "Curvilinear coordinate systems", "theory.html#gridMetrics", [
        [ "Forms of the viscous terms", "theory.html#viscousForms", [
          [ "Strong form of the viscous terms in xi-coordinates", "theory.html#strongVisc", null ]
        ] ]
      ] ]
    ] ],
    [ "Numerics", "numerics.html", [
      [ "Differentiation", "numerics.html#difference", null ],
      [ "Computation of the metrics", "numerics.html#meterics", null ]
    ] ],
    [ "Testing Reference", "testing_reference.html", [
      [ "Running Tests", "testing_reference.html#running", null ],
      [ "Adding Tests", "testing_reference.html#addtest", [
        [ "Add a Test Function", "testing_reference.html#addfunc", null ],
        [ "Add a Testing Function Prototype", "testing_reference.html#addproto", null ],
        [ "Add new Testing Function to Testing Object", "testing_reference.html#addcall", [
          [ "Function Wrapper", "testing_reference.html#wrapper", null ],
          [ "Call in Process method", "testing_reference.html#processcall", null ],
          [ "Call by Name", "testing_reference.html#callbyname", null ]
        ] ]
      ] ],
      [ "Adding Suites", "testing_reference.html#addsuite", [
        [ "Adding a new testing suite", "testing_reference.html#newsuite", null ],
        [ "Add new suite to suite lists", "testing_reference.html#addlist", null ]
      ] ],
      [ "Additional Topics", "testing_reference.html#additional", null ],
      [ "Anatomy of PlasCom2 Testing", "testing_reference.html#anatomy", [
        [ "Code Parser", "testing_reference.html#parser", null ],
        [ "PlasCom2 Testing Object", "testing_reference.html#object", null ],
        [ "Test Drivers", "testing_reference.html#drivers", null ],
        [ "Testing Functions", "testing_reference.html#functions", null ],
        [ "Test Suites", "testing_reference.html#suites", null ],
        [ "Tests", "testing_reference.html#tests", null ],
        [ "Platform Drivers", "testing_reference.html#platforms", null ],
        [ "CMake/CTest", "testing_reference.html#cmake", null ]
      ] ]
    ] ],
    [ "Developer Reference", "developer_reference.html", [
      [ "Configuration Object", "developer_reference.html#config", [
        [ "Configuration Files", "developer_reference.html#configfile", [
          [ "Composition", "developer_reference.html#configcomposition", null ],
          [ "Inheritance", "developer_reference.html#configinheritance", null ]
        ] ],
        [ "Configuration Keys", "developer_reference.html#configkey", null ],
        [ "Configuration Values", "developer_reference.html#configvalue", null ]
      ] ],
      [ "State Object", "developer_reference.html#state", [
        [ "Field MetaData", "developer_reference.html#metadata", null ],
        [ "Data Dictionary", "developer_reference.html#dictionary", null ]
      ] ],
      [ "Interval Object", "developer_reference.html#interval", [
        [ "Creating interval objects", "developer_reference.html#creation", null ],
        [ "Detecting overlap or collision of intervals", "developer_reference.html#overlap", null ],
        [ "Relative translation of intervals", "developer_reference.html#translation", null ],
        [ "Finding buffer indices for sub-intervals", "developer_reference.html#indices", null ],
        [ "Mapping coordinates and indices for buffers and intervals", "developer_reference.html#mapping", null ]
      ] ]
    ] ],
    [ "WENO", "weno.html", [
      [ "Euler equations", "weno.html#gov", null ],
      [ "Finite difference WENO", "weno.html#numerics", [
        [ "WENO reconstruction operator", "weno.html#reconstruction", null ],
        [ "Freestream preservation on curvilinear grids", "weno.html#freestream", null ]
      ] ],
      [ "Boundary treatment using the characteristic wave relations", "weno.html#nscbc", [
        [ "Characteristic space", "weno.html#charspace", null ],
        [ "Characteristic space in terms of primitive variables", "weno.html#charprim", null ],
        [ "Overall implementation procedure", "weno.html#ovimp", null ],
        [ "Applying boundary conditions on \\f$\\mathbf{L}^*\\f$", "weno.html#bcbc", null ]
      ] ],
      [ "Right and left eigenvectors", "weno.html#eigenvectors", null ],
      [ "Roe average", "weno.html#Roeaverage", null ]
    ] ],
    [ "How To Set Up Testing for an XPACC development using ABATE", "testingproject_guide.html", [
      [ "Introduction", "testingproject_guide.html#testing_intro", null ],
      [ "Testing with IX", "testingproject_guide.html#testing_constructs_sec", [
        [ "IX Testing Constructs", "testingproject_guide.html#ix_testing_code", null ],
        [ "IX Testing Support Utilities", "testingproject_guide.html#ix_testing_util", null ],
        [ "Putting it all together", "testingproject_guide.html#putting_it_together", null ]
      ] ],
      [ "Testing in the XPACC Project Template", "testingproject_guide.html#ixpt_testing", [
        [ "Serial Test Examples", "testingproject_guide.html#ixpt_serial_tests", null ],
        [ "Parallel Test Examples", "testingproject_guide.html#ixpt_parallel_tests", null ],
        [ "Direct Test Example", "testingproject_guide.html#direct_tests", null ],
        [ "Reusing the Examples", "testingproject_guide.html#direct_use", null ],
        [ "Creating a \"gold standard\" test", "testingproject_guide.html#gold_standard", null ]
      ] ],
      [ "Automated Testing", "testingproject_guide.html#autotesting_sec", null ]
    ] ],
    [ "Doxygen Documentation Guide", "xpacc_documentation_guide.html", [
      [ "Documenting Code", "xpacc_documentation_guide.html#code_sec", [
        [ "File documentation", "xpacc_documentation_guide.html#files_sec", null ],
        [ "Class documentation", "xpacc_documentation_guide.html#class_sec", [
          [ "Member Data", "xpacc_documentation_guide.html#member_data_sec", null ],
          [ "Methods (including stand-alone non-member functions)", "xpacc_documentation_guide.html#member_methods", null ]
        ] ],
        [ "Program documentation", "xpacc_documentation_guide.html#program_sec", null ],
        [ "Other code constructs", "xpacc_documentation_guide.html#other_sec", null ],
        [ "Miscellaneous commands", "xpacc_documentation_guide.html#misc_com", null ]
      ] ],
      [ "Formats for Code Comments", "xpacc_documentation_guide.html#comment_formats", null ],
      [ "Tips for converting Latex to Doxygen", "xpacc_documentation_guide.html#conversion", [
        [ "Enabling latex style commands in Doxygen", "xpacc_documentation_guide.html#latexCommands", null ],
        [ "Converting formulas", "xpacc_documentation_guide.html#convertFormulas", null ]
      ] ]
    ] ],
    [ "XPACC Coding Guide for C/C++ in PlasCom2", "xpacc_coding_guide.html", [
      [ "Coding Style", "xpacc_coding_guide.html#General", null ],
      [ "PlasCom2-specific", "xpacc_coding_guide.html#PlasCom2-specific", null ]
    ] ]
];