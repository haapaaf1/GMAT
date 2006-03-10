//$Header$
//------------------------------------------------------------------------------
//                              GuiItemManager
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2004/02/06
//
/**
 * Declares GuiItemManager class. This class creates varisous GUI components.
 * It is a singleton - only one instance of this class can be created.
 */
//------------------------------------------------------------------------------

#ifndef GuiItemManager_hpp
#define GuiItemManager_hpp

#include "gmatwxdefs.hpp"
#include "GuiInterpreter.hpp"

#include <wx/sizer.h>
#include <wx/control.h>
#include <wx/textctrl.h>
#include <wx/combobox.h>
#include <wx/checkbox.h>
#include <wx/notebook.h>
#include <wx/button.h>
#include <wx/grid.h>
#include <wx/radiobut.h>
#include <wx/string.h>

class GuiItemManager
{
public:

   enum ShowParamOption
   {
      SHOW_REPORTABLE, // Real, Array, String
      SHOW_PLOTTABLE,  // Real
   };
   
   // for SpacePoint
   static const int MAX_SPACE_POINT = 60;  // CELES_POINT + SPACE_OBJECT
   static const int MAX_CELES_POINT = 20;  // CELES_BODY + CAL_POINT
   static const int MAX_CELES_BODY = 20;
   static const int MAX_CAL_POINT = 20;
   static const int MAX_SPACE_OBJECT = 40; // FORMATION + SPACECRAFT
   static const int MAX_FORMATION = 10;
   static const int MAX_SPACECRAFT = 30;
   
   // for Parameter
   static const int MAX_SC_PROPERTY = 100; // Max Spacecraft
   static const int MAX_IB_PROPERTY = 10;  // Max ImpulsiveBurn
   static const int MAX_PROPERTY = 110;
   static const int MAX_USER_VAR = 20;
   static const int MAX_USER_STRING = 20;
   static const int MAX_USER_ARRAY = 20;
   static const int MAX_USER_PARAM = 40;
   static const int MAX_PLOT_PARAM = 140;

   // Other
   static const int MAX_COORD_SYS = 20;
   static const int MAX_BURN = 10;
   static const int MAX_HARDWARE = 100;  //waw: Changed from 10 to 100
   static const int MAX_FUNCTION = 10;
   
   static GuiItemManager* GetInstance();
   
   void UpdateAll();
   void UpdateCelestialPoint();
   void UpdateFormation();
   void UpdateSpacecraft();
   void UpdateBurn();
   void UpdateParameter();
   void UpdateSolarSystem();
   void UpdateCoordSystem();
   void UpdateHardware();
   void UpdateFunction();
   
   void UnregisterListBox(const wxString &type, wxListBox *lb,
                          wxArrayString *excList = NULL);
   void UnregisterComboBox(const wxString &type, wxComboBox *cb);
   
   int GetNumSpacecraft()
      { return theNumSpacecraft; }
   
   int GetNumFuelTank()
      { return theNumFuelTank; }
   
   int GetNumThruster()
      { return theNumThruster; }
   
   int GetNumConfigBody()
      { return theNumCelesBody; }
   
   int GetNumCoordSystem()
      { return theNumCoordSys; }
   
   int GetNumFunction()
      { return theNumFunction; }
   
   int GetNumPlottableParameter()
      { return theNumPlottableParam; }
   
   int GetNumSystemParameter()
      { return theNumSystemParam; }
   
   int GetNumUserVariable()
      { return theNumUserVariable; }
   
   int GetNumUserString()
      { return theNumUserString; }
   
   int GetNumUserArray()
      { return theNumUserArray; }
   
   int GetNumUserParameter()
      { return theNumUserParam; }
   
   wxString* GetPlottableParameterList()
      { return thePlottableParamList; }
   
   wxString* GetSystemParameterList()
      { return theSystemParamList; }
   
   wxString* GetUserVariableList()
      { return theUserVariableList; }
   
   wxString* GetUserStringList()
      { return theUserStringList; }
   
   wxString* GetUserParameterList()
      { return theUserParamList; }
   
   wxString* GetCoordSysList()
      { return theCoordSysList; }
   
   wxString* GetConfigBodyList()
      { return theCelesBodyList; }
   
   int GetNumProperty(const wxString &objType);
   wxString* GetPropertyList(const wxString &objType);

   // ComboBox
   wxComboBox* GetSpacecraftComboBox(wxWindow *parent, wxWindowID id,
                                     const wxSize &size);
   
   wxComboBox* GetBurnComboBox(wxWindow *parent, wxWindowID id,
                               const wxSize &size);
   
   wxComboBox* GetCoordSysComboBox(wxWindow *parent, wxWindowID id,
                                   const wxSize &size);
   
   wxComboBox* GetConfigBodyComboBox(wxWindow *parent, wxWindowID id,
                                     const wxSize &size);
   
   wxComboBox* GetFunctionComboBox(wxWindow *parent, wxWindowID id,
                                   const wxSize &size);

   wxComboBox* GetSpacePointComboBox(wxWindow *parent, wxWindowID id, 
                                     const wxSize &size, bool addVector = false);
   
   wxComboBox* GetCelestialPointComboBox(wxWindow *parent, wxWindowID id, 
                                         const wxSize &size, bool addVector = false);
   
   wxComboBox* GetUserVariableComboBox(wxWindow *parent, wxWindowID id,
                                       const wxSize &size);
   
   wxComboBox* GetFuelTankComboBox(wxWindow *parent, wxWindowID id,
                                   const wxSize &size);
   
   wxComboBox* GetThrusterComboBox(wxWindow *parent, wxWindowID id,
                                   const wxSize &size);
   
   // ListBox
   wxListBox* GetSpacePointListBox(wxWindow *parent, wxWindowID id, 
                                   const wxSize &size, bool addVector = false);
   
   wxListBox* GetCelestialPointListBox(wxWindow *parent, wxWindowID id,
                                       const wxSize &size,
                                       wxArrayString &excList);
   
   wxListBox* GetSpaceObjectListBox(wxWindow *parent, wxWindowID id,
                                    const wxSize &size,
                                    wxArrayString *excList = NULL,
                                    bool includeFormation = true);
   
   wxListBox* GetSpacecraftListBox(wxWindow *parent, wxWindowID id,
                                   const wxSize &size,
                                   wxArrayString *excList = NULL);
   
   wxListBox* GetFormationListBox(wxWindow *parent, wxWindowID id,
                                  const wxSize &size,
                                  wxArrayString &sosToExclude);
   
   wxListBox* GetPropertyListBox(wxWindow *parent, wxWindowID id,
                                 const wxSize &size,
                                 const wxString &objType, int showOption = SHOW_PLOTTABLE);
   
   wxListBox* GetPlottableParameterListBox(wxWindow *parent, wxWindowID id,
                                           const wxSize &size,
                                           const wxString &nameToExclude = "");
   
   wxListBox* GetAllUserParameterListBox(wxWindow *parent, wxWindowID id,
                                         const wxSize &size, bool showArray);
   
   wxListBox* GetUserVariableListBox(wxWindow *parent, wxWindowID id,
                                     const wxSize &size,
                                     const wxString &nameToExclude = "");
   
   wxListBox* GetUserStringListBox(wxWindow *parent, wxWindowID id,
                                   const wxSize &size,
                                   const wxString &nameToExclude = "");
   
   wxListBox* GetUserArrayListBox(wxWindow *parent, wxWindowID id,
                                  const wxSize &size,
                                  const wxString &nameToExclude = "");
   
   wxListBox* GetUserParameterListBox(wxWindow *parent, wxWindowID id,
                                      const wxSize &size);
   
   wxListBox* GetConfigBodyListBox(wxWindow *parent, wxWindowID id,
                                   const wxSize &size,
                                   wxArrayString &excList);
   wxListBox* GetFuelTankListBox(wxWindow *parent, wxWindowID id,
                                 const wxSize &size,
                                 wxArrayString *excList = NULL);
   wxListBox* GetThrusterListBox(wxWindow *parent, wxWindowID id,
                                 const wxSize &size,
                                 wxArrayString *excList);
   
   // BoxSizer
   wxBoxSizer*
   CreateParameterSizer(wxWindow *parent,
                        wxListBox **userParamListBox, wxWindowID userParamListBoxId,
                        wxButton **createVarButton, wxWindowID createVarButtonId,
                        wxComboBox **objectComboBox, wxWindowID objectComboBoxId,
                        wxListBox **propertyListBox, wxWindowID propertyListBoxId,
                        wxComboBox **coordSysComboBox, wxWindowID coordSysComboBoxId,
                        wxComboBox **originComboBox, wxWindowID originComboBoxId,
                        wxStaticText **coordSysLabel, wxBoxSizer **coordSysBoxSizer,
                        int showOption = SHOW_PLOTTABLE, bool showArray = false,
                        const wxString &onwer = "Spacecraft");
   wxBoxSizer*
   CreateUserVarSizer(wxWindow *parent,
                      wxListBox **userParamListBox, wxWindowID userParamListBoxId,
                      wxButton **createVarButton, wxWindowID createVarButtonId,
                      int showOption = SHOW_REPORTABLE, bool showArray = false);
   
private:
   
   GuiItemManager();
   virtual ~GuiItemManager();
   GuiItemManager(const GuiItemManager&);
   GuiItemManager& operator=(const GuiItemManager&);
   
   void UpdatePropertyList();
   void UpdateParameterList();

   void UpdateSpacecraftList();
   void UpdateFormationList();
   void UpdateSpaceObjectList();
   void UpdateCelestialBodyList();
   void UpdateCelestialPointList();
   void UpdateSpacePointList();
   void UpdateBurnList();
   void UpdateCoordSystemList();
   void UpdateHardwareList();
   void UpdateFunctionList();
   
   static GuiItemManager *theInstance;
   GuiInterpreter *theGuiInterpreter;
   SolarSystem *theSolarSystem;
   
   std::vector<wxListBox*> mSpaceObjectLBList;
   std::vector<wxListBox*> mSpacecraftLBList;
   std::vector<wxListBox*> mFuelTankLBList;
   std::vector<wxListBox*> mThrusterLBList;
   
   std::vector<wxComboBox*> mSpacePointCBList;
   std::vector<wxComboBox*> mSpacecraftCBList;
   std::vector<wxComboBox*> mBurnCBList;
   std::vector<wxComboBox*> mCoordSysCBList;
   std::vector<wxComboBox*> mFunctionCBList;
   std::vector<wxComboBox*> mFuelTankCBList;
   std::vector<wxComboBox*> mThrusterCBList;
   
   std::vector<wxArrayString*> mSpaceObjectExcList;
   std::vector<wxArrayString*> mSpacecraftExcList;
   std::vector<wxArrayString*> mFuelTankExcList;
   std::vector<wxArrayString*> mThrusterExcList;
   
   int theNumScProperty;
   int theNumImpBurnProperty;
   int theNumSpaceObject;
   int theNumFormation;
   int theNumSpacecraft;
   int theNumBurn;
   int theNumCoordSys;
   int theNumFunction;
   int theNumFuelTank;
   int theNumThruster;
   int theNumPlottableParam;
   int theNumSystemParam;
   int theNumUserVariable;
   int theNumUserString;
   int theNumUserArray;
   int theNumUserParam;
   int theNumCelesBody;
   int theNumCelesPoint;
   int theNumCalPoint;
   int theNumSpacePoint;
   
   wxString theSpacePointList[MAX_SPACE_POINT];
   wxString theCelesPointList[MAX_CELES_POINT];
   wxString theCelesBodyList[MAX_CELES_BODY];
   wxString theCalPointList[MAX_CAL_POINT];
   
   wxString theSpaceObjectList[MAX_SPACE_OBJECT];
   wxString theFormationList[MAX_FORMATION];
   wxString theSpacecraftList[MAX_SPACECRAFT];
   wxString theBurnList[MAX_BURN];
   wxString theCoordSysList[MAX_COORD_SYS];
   wxString theFunctionList[MAX_FUNCTION];
   wxString theFuelTankList[MAX_HARDWARE];
   wxString theThrusterList[MAX_HARDWARE];
   
   wxString theScPropertyList[MAX_SC_PROPERTY];
   wxString theImpBurnPropertyList[MAX_IB_PROPERTY];
   wxString thePlottableParamList[MAX_PLOT_PARAM];
   wxString theSystemParamList[MAX_PROPERTY];
   wxString theUserVariableList[MAX_USER_VAR];
   wxString theUserStringList[MAX_USER_STRING];
   wxString theUserArrayList[MAX_USER_ARRAY];
   wxString theUserParamList[MAX_USER_PARAM];
   
};

#endif // GuiItemManager_hpp
