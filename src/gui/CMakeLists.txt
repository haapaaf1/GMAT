# $Id$
# 
# GMAT: General Mission Analysis Tool.
# 
# CMAKE script file for GMAT GUI
# This file must be installed in the src/gui directory
#
# Author: Joris T. Olympio
#  
# DO NOT MODIFY THIS FILE UNLESS YOU KNOW WHAT YOU ARE DOING!
#
PROJECT(GMAT_gui)


MESSAGE("==============================")
MESSAGE("GMAT GUI building setup " ${VERSION})


# ====================================================================
SET(GUITARGET "GmatGui")

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    #TARGETS = $(GUITARGET) macstacksize macrez macosx
    #CLEANTARGET = archclean cleanmacosx
else()
    #TARGETS = $(GUITARGET)
    #CLEANTARGET = archclean
endif()


# ====================================================================
# DEVIL package

if (USE_DEVIL AND UNIX)
    MESSAGE("-- Building with DeVIL")

    if (NOT DevIL_DIR)
        #SET(DevIL_INCLUDE_DIR "/usr/local/include/IL")
        find_package(DevIL)

        if(NOT IL_FOUND)
            MESSAGE("** Disabling USE_DEVIL")
            SET(USE_DEVIL FALSE)
        endif()
    else()
        # check directory
        if(EXISTS "${DevIL_DIR}" AND IS_DIRECTORY "${DevIL_DIR}")
            SET(IL_INCLUDE_DIR ${DevIL_DIR})
        else()
            #MESSAGE(FATAL_ERROR "DEVIL NOT FOUND")
            MESSAGE("** DEVIL NOT FOUND in directory " ${DevIL_DIR})
            MESSAGE("** Disabling USE_DEVIL")
            SET(USE_DEVIL FALSE)
        endif()
    endif ()
    

    if (USE_DEVIL)
        MESSAGE("-- DevIL FOUND")
        MESSAGE("-- DevIL_DIR=" ${IL_INCLUDE_DIR})
        LINK_DIRECTORIES(${IL_INCLUDE_DIR})
        LINK_LIBRARIES(${IL_LIBRARIES})
        LINK_LIBRARIES(${ILU_LIBRARIES})
        LINK_LIBRARIES(${ILUT_LIBRARIES})
        INCLUDE_DIRECTORIES(${IL_INCLUDE_DIR})
    else()
        ADD_DEFINITIONS("-DSKIP_DEVIL")
    endif()
endif()


# ====================================================================
# SPICE
if (USE_SPICE)
	INCLUDE_DIRECTORIES("${CSPICE_DIR}/include")
    SET(SPICE_SRCS "spacecraft/SpicePanel.cpp")
endif()

#	rm -f app/AboutDialog.o
# ====================================================================
# other libraries
LINK_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}/../base/lib")
LINK_LIBRARIES("libGmatBase")

# Link WX Contrib Libraries (STC, NetUtils)
SET(WXCONTRIB_LIB_DIR  "${WXWIDGET_DIR}/lib/vc_dll")
LINK_LIBRARIES("${WXCONTRIB_LIB_DIR}/wxmsw28_netutils.lib")
LINK_LIBRARIES("${WXCONTRIB_LIB_DIR}/wxmsw28_stc.lib")

# need some opengl libs
if(WIN32)
    LINK_LIBRARIES("glu32")
    LINK_LIBRARIES("opengl32")
endif()

ADD_DEFINITIONS("-D_DYNAMICLINK")
ADD_DEFINITIONS("-DWXUSINGDLL")
ADD_DEFINITIONS("-D_MBCS")
ADD_DEFINITIONS("-D__USE_STC_EDITOR__")
ADD_DEFINITIONS("-D__ENABLE_EMAIL__")
ADD_DEFINITIONS("-D__USE_SPICE__")


# ====================================================================
# include directories for header files
INCLUDE_DIRECTORIES("../base/include")
INCLUDE_DIRECTORIES("../base/asset")
INCLUDE_DIRECTORIES("../base/attitude")
INCLUDE_DIRECTORIES("../base/burn")
INCLUDE_DIRECTORIES("../base/command")
INCLUDE_DIRECTORIES("../base/configs")
INCLUDE_DIRECTORIES("../base/coordsystem")
INCLUDE_DIRECTORIES("../base/executive")
INCLUDE_DIRECTORIES("../base/factory")
INCLUDE_DIRECTORIES("../base/forcemodel")
INCLUDE_DIRECTORIES("../base/foundation")
INCLUDE_DIRECTORIES("../base/function")
INCLUDE_DIRECTORIES("../base/hardware")
INCLUDE_DIRECTORIES("../base/interface")
INCLUDE_DIRECTORIES("../base/interpolator")
INCLUDE_DIRECTORIES("../base/interpreter")
INCLUDE_DIRECTORIES("../base/math")
INCLUDE_DIRECTORIES("../base/parameter")
INCLUDE_DIRECTORIES("../base/plugin")
INCLUDE_DIRECTORIES("../base/propagator")
INCLUDE_DIRECTORIES("../base/solarsys")
INCLUDE_DIRECTORIES("../base/solver")
INCLUDE_DIRECTORIES("../base/spacecraft")
INCLUDE_DIRECTORIES("../base/stopcond")
INCLUDE_DIRECTORIES("../base/subscriber")
INCLUDE_DIRECTORIES("../base/util")
INCLUDE_DIRECTORIES("../base/forcemodel")
INCLUDE_DIRECTORIES("../base/forcemodel/harmonic")
INCLUDE_DIRECTORIES("../base/event")
INCLUDE_DIRECTORIES("include")
INCLUDE_DIRECTORIES("include/bitmaps")
INCLUDE_DIRECTORIES("include/il")
INCLUDE_DIRECTORIES("app")
INCLUDE_DIRECTORIES("asset")
INCLUDE_DIRECTORIES("burn")
INCLUDE_DIRECTORIES("command")
INCLUDE_DIRECTORIES("controllogic")
INCLUDE_DIRECTORIES("coordsystem")
INCLUDE_DIRECTORIES("forcemodel")
INCLUDE_DIRECTORIES("foundation")
INCLUDE_DIRECTORIES("function")
INCLUDE_DIRECTORIES("hardware")
INCLUDE_DIRECTORIES("mission")
INCLUDE_DIRECTORIES("output")
INCLUDE_DIRECTORIES("propagator")
INCLUDE_DIRECTORIES("rendering")
INCLUDE_DIRECTORIES("solarsys")
INCLUDE_DIRECTORIES("solver")
INCLUDE_DIRECTORIES("spacecraft")
INCLUDE_DIRECTORIES("subscriber")
INCLUDE_DIRECTORIES("view")
INCLUDE_DIRECTORIES("./event")
INCLUDE_DIRECTORIES(${WXWIDGET_DIR}/include/)
INCLUDE_DIRECTORIES(${WXWIDGET_DIR}/include/msvc/)
INCLUDE_DIRECTORIES(${WXWIDGET_DIR}/contrib/include)

# ====================================================================
# source files
SET(GUI_SRCS 
    app/AboutDialog.cpp
    app/GmatApp.cpp
    app/GmatAppData.cpp
    app/CompareFilesDialog.cpp
    app/CompareTextDialog.cpp
    app/GmatConnection.cpp
    app/GmatMainFrame.cpp
    app/GmatMenuBar.cpp
    app/GmatToolBar.cpp
    app/GuiInterpreter.cpp
    app/GuiMessageReceiver.cpp
    app/GmatNotebook.cpp
    app/GmatServer.cpp
    app/GmatTreeItemData.cpp
    app/GuiPlotReceiver.cpp
    app/InteractiveMatlabDialog.cpp
    app/MissionTreeItemData.cpp
    app/RunScriptFolderDialog.cpp
    app/ResourceTree.cpp
    app/ScriptPanel.cpp
    app/SetPathDialog.cpp
    app/TextEphemFileDialog.cpp
    app/WelcomePanel.cpp
    asset/GroundStationPanel.cpp
    burn/FiniteBurnSetupPanel.cpp
    burn/ImpulsiveBurnSetupPanel.cpp
    command/AchievePanel.cpp
    command/AssignmentPanel.cpp
    command/BeginFiniteBurnPanel.cpp
    command/CallFunctionPanel.cpp
    command/EndFiniteBurnPanel.cpp
    command/GmatCommandPanel.cpp
	command/ManageObjectPanel.cpp
    command/ManeuverPanel.cpp
    command/MinimizePanel.cpp
    command/NonlinearConstraintPanel.cpp
    command/OptimizePanel.cpp
    command/PropagatePanel.cpp
    command/ReportPanel.cpp
    command/ScriptEventPanel.cpp
    command/TargetPanel.cpp
    command/TogglePanel.cpp
    command/VaryPanel.cpp
    controllogic/ForPanel.cpp
    controllogic/ConditionPanel.cpp
    coordsystem/CoordPanel.cpp
    coordsystem/CoordSysCreateDialog.cpp
    coordsystem/CoordSystemConfigPanel.cpp
    forcemodel/DragInputsDialog.cpp
    foundation/ArraySetupDialog.cpp
	foundation/ArraySetupPanel.cpp
    foundation/GmatDialog.cpp
    foundation/GmatMdiChildFrame.cpp
    foundation/GmatPanel.cpp
    foundation/GmatBaseSetupPanel.cpp
    foundation/GmatSavePanel.cpp
    foundation/GmatStaticBoxSizer.cpp
    foundation/GuiItemManager.cpp
    foundation/MultiPathSetupPanel.cpp
    foundation/ParameterCreateDialog.cpp
    foundation/ParameterSelectDialog.cpp
    foundation/ParameterSetupPanel.cpp
    foundation/ShowScriptDialog.cpp
    foundation/ShowSummaryDialog.cpp
    foundation/SinglePathSetupPanel.cpp
    foundation/UserInputValidator.cpp
    function/FunctionSetupPanel.cpp
    function/MatlabFunctionSetupPanel.cpp
    hardware/BurnThrusterPanel.cpp
    hardware/ThrusterCoefficientDialog.cpp
    hardware/ThrusterConfigPanel.cpp
    mission/DecoratedTree.cpp
    mission/MissionTree.cpp
	mission/MissionTreeToolbar.cpp
	mission/TreeViewOptionDialog.cpp
	mission/UndockedMissionPanel.cpp
    output/CompareReportPanel.cpp
	output/EventFilePanel.cpp
    output/OutputTree.cpp
    output/ReportFilePanel.cpp
    propagator/PropagationConfigPanel.cpp
    propagator/PropagatorSelectDialog.cpp
    rendering/Camera.cpp
    rendering/GLStars.cpp
    rendering/GmatOpenGLSupport.cpp
    rendering/Light.cpp
    rendering/Load3ds.cpp
    rendering/LoadPOV.cpp
    rendering/ModelManager.cpp
    rendering/ModelObject.cpp
    rendering/Rendering.cpp
    solarsys/BarycenterPanel.cpp
    solarsys/CelesBodySelectDialog.cpp
    solarsys/CelestialBodyOrbitPanel.cpp
    solarsys/CelestialBodyOrientationPanel.cpp
    solarsys/CelestialBodyPanel.cpp
    solarsys/CelestialBodyPropertiesPanel.cpp
    solarsys/LibrationPointPanel.cpp
    solarsys/SolarSystemWindow.cpp
    solarsys/UniversePanel.cpp
    solver/DCSetupPanel.cpp
    solver/SQPSetupPanel.cpp
    solver/SolverCreatePanel.cpp
    solver/SolverGoalsPanel.cpp
    solver/SolverSetupPanel.cpp
    solver/SolverVariablesPanel.cpp
    spacecraft/AttitudePanel.cpp
    spacecraft/BallisticsMassPanel.cpp
    spacecraft/FormationSetupPanel.cpp
	spacecraft/OrbitDesignerDialog.cpp
    spacecraft/OrbitPanel.cpp
	spacecraft/OrbitSummaryDialog.cpp
    spacecraft/SpacecraftPanel.cpp
    spacecraft/SpaceObjectSelectDialog.cpp
    spacecraft/TankPanel.cpp
    spacecraft/ThrusterPanel.cpp
    spacecraft/VisualModelCanvas.cpp
    spacecraft/VisualModelPanel.cpp
    subscriber/EphemerisFilePanel.cpp
	subscriber/GroundTrackCanvas.cpp
    subscriber/MdiChild3DViewFrame.cpp
	subscriber/MdiChildGroundTrackFrame.cpp
    subscriber/MdiChildViewFrame.cpp
    subscriber/MdiChildTsFrame.cpp
    subscriber/MdiGlPlotData.cpp
    subscriber/MdiTsPlotData.cpp
	subscriber/OrbitViewCanvas.cpp
    subscriber/OrbitViewPanel.cpp
    subscriber/ReportFileSetupPanel.cpp
    subscriber/SubscriberSetupPanel.cpp
    subscriber/ViewCanvas.cpp
    subscriber/TsPlotCanvas.cpp
    subscriber/TsPlotXYCanvas.cpp
    subscriber/TsPlotCurve.cpp
    subscriber/TsPlotOptionsDialog.cpp
    subscriber/XyPlotSetupPanel.cpp
	view/Editor.cpp
	view/EditorPanel.cpp
	view/EditorPreferences.cpp
	view/EditorPrintout.cpp
	view/findReplaceDialog.cpp
    view/ViewTextDialog.cpp
    view/ViewTextFrame.cpp
    ${STC_EDITOR_SRCS}
    ${SPICE_SRCS}
)


# ====================================================================
# TODO not sure I understand what are those mac specifics for... 
# it should no compromise building though...

if(APPLE)
#macrez:
#	echo "linking Carbon and wx resource files ......."
#	$(REZ) -d WXUSINGDLL -o $(GUITARGET)
#	cp -f $(GUITARGET) ../../application/bin/.

#macosx: 
#	echo "building Mac application bundle ......."
#	/Developer/Tools/SetFile -a C $(TOP_DIR)/application/bin/$(GUITARGET)
#	mkdir -p $(CONTENTS_DIR)
#	mkdir -p $(MACOS_DIR)
#	mkdir -p $(RES_DIR)
#	mkdir -p $(LIBS_DIR)
#	sed -e "s/IDENTIFIER/`echo GMAT | sed -e 's,\.\./,,g' | \
#	sed -e 's,/,.,g'`/" -e "s/EXECUTABLE/RunGMAT/" $(GMAT_INFO) >$(MAC_PKG)
#	echo -n "APPL????" > $(MAC_PKGINFO)
#	ln -f $(TOP_DIR)/application/bin/$(GUITARGET) $(MAC_APP)
#	ln -f $(TOP_DIR)/src/gui/RunGMAT $(MAC_SCRIPT_DIR)/RunGMAT
#	cp -f $(GMAT_ICONS) $(MAC_ICONS) 
#	./$(INSTALL_LIBS_INTO_BUNDLE) GMAT ../base/lib $(BASELIBNAME) $(PCRE_LIB_LOC)

#macstacksize:
#	echo "setting stack size needed for SPICE"
#	$(SPICE_STACKSIZE)
endif()

# ====================================================================
# add the icon, windows only.
if(WIN32)
    SET(GMAT_ICON_RC  "${CMAKE_CURRENT_SOURCE_DIR}/${GMAT_ICON_DIR}/${GMAT_ICON_RC}")
    SET(GMAT_ICON_O  "${CMAKE_CURRENT_SOURCE_DIR}/${GMAT_ICON_DIR}/${GMAT_ICON_O}")
    SET(GUI_SRCS "${GUI_SRCS};${GMAT_ICON_RC}")
elseif(APPLE)
# we can add an icon for mac/gmat

endif()

# ====================================================================
# compilation

# add the install targets
SET(EXECUTABLE_OUTPUT_PATH "../../bin")

LINK_DIRECTORIES("${WXWIDGET_DIR}/lib/vc_dll")

target_link_libraries(${GUITARGET} ${wxWidgets_LIBRARIES})

ADD_EXECUTABLE(${GUITARGET} ${GUI_SRCS})

SET_TARGET_PROPERTIES(${GUITARGET} PROPERTIES TARGET_VERSION_MAJOR "${VERSION}" LINK_FLAGS "/SUBSYSTEM:windows")

# ====================================================================
# installation (make install)
SET(CMAKE_INSTALL_PREFIX "./")  # default is usr/local

if  (SHARED_BASE)
	INSTALL(TARGETS ${TargetName} DESTINATION "${CMAKE_CURRENT_SOURCE_DIR}/../../../../application/bin")
endif()