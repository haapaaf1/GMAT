//$Header$
//------------------------------------------------------------------------------
//                                  CelestialBodyOrbitPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Wendy C. Shoan
// Created: 2009.01.26
//
/**
 * This is the panel for the Properties tab on the notebook on the CelestialBody
 * Panel.
 *
 */
//------------------------------------------------------------------------------

#include "CelestialBodyOrbitPanel.hpp"
#include "SolarSystem.hpp"
#include "GmatBaseException.hpp"
#include "GmatAppData.hpp"
#include "MessageInterface.hpp"
#include "StringUtil.hpp"
#include "A1Mjd.hpp"
#include <fstream>

//#define DEBUG_CB_ORBIT_PANEL

// event tables for wxMac/Widgets
BEGIN_EVENT_TABLE(CelestialBodyOrbitPanel, wxPanel)
   EVT_COMBOBOX(ID_COMBO_BOX_EPHEM_SOURCE, CelestialBodyOrbitPanel::OnEphemSourceComboBoxChange)
   EVT_TEXT(ID_TEXT_CTRL_EPHEM_FILE, CelestialBodyOrbitPanel::OnEphemFileTextCtrlChange)
   EVT_BUTTON(ID_BROWSE_BUTTON_EPHEM_FILE, CelestialBodyOrbitPanel::OnEphemFileBrowseButton)
   EVT_TEXT(ID_TEXT_CTRL_NAIF_ID, CelestialBodyOrbitPanel::OnNaifIdTextCtrlChange)
   EVT_COMBOBOX(ID_COMBO_BOX_CENTRAL_BODY, CelestialBodyOrbitPanel::OnCentralBodyComboBoxChange)
   EVT_TEXT(ID_TEXT_CTRL_INITIAL_EPOCH, CelestialBodyOrbitPanel::OnEpochTextCtrlChange)
   EVT_TEXT(ID_TEXT_CTRL_SMA, CelestialBodyOrbitPanel::OnSMATextCtrlChange)
   EVT_TEXT(ID_TEXT_CTRL_ECC, CelestialBodyOrbitPanel::OnECCTextCtrlChange)
   EVT_TEXT(ID_TEXT_CTRL_INC, CelestialBodyOrbitPanel::OnINCTextCtrlChange)
   EVT_TEXT(ID_TEXT_CTRL_RAAN, CelestialBodyOrbitPanel::OnRAANTextCtrlChange)
   EVT_TEXT(ID_TEXT_CTRL_AOP, CelestialBodyOrbitPanel::OnAOPTextCtrlChange)
   EVT_TEXT(ID_TEXT_CTRL_TA, CelestialBodyOrbitPanel::OnTATextCtrlChange)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------

CelestialBodyOrbitPanel::CelestialBodyOrbitPanel(GmatPanel *cbPanel, 
                              wxWindow *parent, CelestialBody *body) :
   wxPanel          (parent),
   dataChanged      (false),
   canClose         (true),
   theBody          (body),
   naifID           (-99),
   initialEpoch     (0.0),
   SMA              (0.0),
   ECC              (0.0),
   INC              (0.0),
   RAAN             (0.0),
   AOP              (0.0),
   TA               (0.0),
   ephemSrcChanged  (false),
   ephemFileChanged (false),
   naifIDChanged    (false),
   cBodyChanged     (false),
   epochChanged     (false),
   SMAChanged       (false),
   ECCChanged       (false),
   INCChanged       (false),
   RAANChanged      (false),
   AOPChanged       (false),
   TAChanged        (false),
   userDef          (false),
   isSun            (false),
   theCBPanel       (cbPanel)
{
   guiManager     = GuiItemManager::GetInstance();
   guiInterpreter = GmatAppData::Instance()->GetGuiInterpreter();
   ss             = guiInterpreter->GetSolarSystemInUse();
   
   Create();
}

CelestialBodyOrbitPanel::~CelestialBodyOrbitPanel()
{
   guiManager->UnregisterComboBox("CelestialBody", centralBodyComboBox);
}

void CelestialBodyOrbitPanel::SaveData()
{
   std::string strval;
   Real        tmpval;
   Integer     tmpint;
   bool        realsOK   = true;
   bool        intsOK    = true;
   bool        stringsOK = true;
   
   
   // don't do anything if no data has been changed.
   // note that dataChanged will be true if the user modified any combo box or
   // text ctrl, whether or not he/she actually changed the value; we want to only
   // send the values to the object if something was really changed, to avoid
   // the hasBeenModified flag being set to true erroneously
   if (!dataChanged) return;
   
   bool reallyChanged = false;
   canClose    = true;
   
   if (ephemSrcChanged)
   {
      strval = ephemSourceComboBox->GetValue();
//      theBody->SetStringParameter(theBody->GetParameterID("PosVelSource"), strval); 
      if (strval != ephemSrc) reallyChanged = true;
      ephemSrc = strval;
   }
   if (ephemFileChanged)
   {
      strval = ephemFileTextCtrl->GetValue();
      #ifdef DEBUG_CB_ORBIT_PANEL
         MessageInterface::ShowMessage("ephemFileChanged is true : %s\n",
               strval.c_str());       
      #endif
      std::ifstream filename(strval.c_str());
      
      if (!filename)
      {
         std::string errmsg = "File \"" + strval;
         errmsg += "\" does not exist.\n";
         MessageInterface::PopupMessage(Gmat::ERROR_, errmsg);
         stringsOK = false;
      }
      else
      {
         if (strval != ephemFile) reallyChanged = true;
         ephemFile = strval;
         filename.close();
      } 
   }
   if (userDef && naifIDChanged)
   {
      strval = naifIDTextCtrl->GetValue();
      if (!theCBPanel->CheckInteger(tmpint, strval, "NAIF ID", "Integer Number"))
         intsOK = false;
      else 
      {
         if (tmpint != naifID) reallyChanged = true;
         naifID = tmpint;
      }
   }
   if (cBodyChanged)
   {
      strval = centralBodyComboBox->GetValue();
      theBody->SetStringParameter(theBody->GetParameterID("CentralBody"), strval); 
      if (strval != centralBody) reallyChanged = true;
      centralBody = strval;
   }
   
   if (!isSun)
   {
      if (epochChanged)
      {  // this probably needs more checking .......
         strval = initialEpochTextCtrl->GetValue();
         if (!theCBPanel->CheckReal(tmpval, strval, "Initial Two Body Epoch", "Real Number"))
            realsOK = false;
         else 
         {
            if (tmpval != initialEpoch) reallyChanged = true;
            initialEpoch = tmpval;
         }
      }
      if (SMAChanged) 
      {
         strval = SMATextCtrl->GetValue(); 
         if (!theCBPanel->CheckReal(tmpval, strval, "Initial SMA", "Real Number"))
            realsOK = false;
         else 
         {
            if (tmpval != SMA) reallyChanged = true;
            SMA = tmpval;
         }
      }
      if (ECCChanged)  
      {
         strval = ECCTextCtrl->GetValue(); 
         if (!theCBPanel->CheckReal(tmpval, strval, "Initial ECC", "Real Number"))
            realsOK = false;
         else 
         {
            if (tmpval != ECC) reallyChanged = true;
            ECC = tmpval;
         }
      }
      if (INCChanged)  
      {
         strval = INCTextCtrl->GetValue(); 
         if (!theCBPanel->CheckReal(tmpval, strval, "Initial INC", "Real Number"))
            realsOK = false;
         else 
         {
            if (tmpval != INC) reallyChanged = true;
            INC = tmpval;
         }
     }
      if (RAANChanged) 
      {
         strval = RAANTextCtrl->GetValue();
         if (!theCBPanel->CheckReal(tmpval, strval, "Initial RAAN", "Real Number"))
            realsOK = false;
         else 
         {
            if (tmpval != RAAN) reallyChanged = true;
            RAAN = tmpval;
         }
      }
      if (AOPChanged)  
      {
         strval = AOPTextCtrl->GetValue(); 
         if (!theCBPanel->CheckReal(tmpval, strval, "Initial AOP", "Real Number"))
            realsOK = false;
         else 
         {
            if (tmpval != AOP) reallyChanged = true;
            AOP = tmpval;
         }
      }
      if (TAChanged)  
      {
         strval = TATextCtrl->GetValue();
         if (!theCBPanel->CheckReal(tmpval, strval, "Initial TA", "Real Number"))
            realsOK = false;
         else 
         {
            if (tmpval != TA) reallyChanged = true;
            TA = tmpval;
         }
      }
   }

   
   if (!realsOK)
   {
      std::string errmsg = "Please enter valid Real values before saving data.\n";
      MessageInterface::PopupMessage(Gmat::ERROR_, errmsg);
   }
   if (!intsOK)
   {
      std::string errmsg = "Please enter valid Integer values before saving data.\n";
      MessageInterface::PopupMessage(Gmat::ERROR_, errmsg);
   }

   if (realsOK && intsOK && stringsOK && reallyChanged)
   {
      theBody->SetStringParameter(theBody->GetParameterID("PosVelSource"), ephemSrc); 
      theBody->SetStringParameter(theBody->GetParameterID("SourceFilename"), ephemFile);
      if (userDef) theBody->SetIntegerParameter(theBody->GetParameterID("NAIFId"), naifID);
      if (!isSun)
      {
         A1Mjd a1Epoch(initialEpoch);
         theBody->SetTwoBodyEpoch(a1Epoch);
         Rvector6 elements(SMA, ECC, INC, RAAN, AOP, TA);
         theBody->SetTwoBodyElements(elements);
      }
   }
   else
      canClose = false;
   
   dataChanged = false;
   ResetChangeFlags(true);
}

void CelestialBodyOrbitPanel::LoadData()
{
   try
   {
//      StringArray ephemSourceList = theBody->GetEphemSourceList();
//      for (unsigned int ii = 0; ii < ephemSourceList.size(); ii++)
//         ephemSourceComboBox-Append((ephemSourceList.at[ii]).c_str());
      ephemSrc   = theBody->GetStringParameter(theBody->GetParameterID("PosVelSource"));
      ephemSourceComboBox->SetValue(ephemSrc.c_str());
      
      ephemFile  = theBody->GetSourceFileName();
      ephemFileTextCtrl->SetValue(ephemFile.c_str());
      
      centralBody = theBody->GetCentralBody();
      centralBodyComboBox->SetValue(centralBody.c_str());
      
      if (userDef)
      {
         naifID = theBody->GetIntegerParameter(theBody->GetParameterID("NAIFId"));
         naifIDStringWX = guiManager->ToWxString(naifID);
         naifIDTextCtrl->SetValue(naifIDStringWX);
         
      }
      if (ephemSrc == "TwoBodyPropagation") 
      {
         ephemFileTextCtrl->Disable();
         ephemFileBrowseButton->Disable();
         if (userDef) naifIDTextCtrl->Disable();
      }
      
      if (!isSun)
      {
         initialEpoch = (theBody->GetTwoBodyEpoch()).Get();
         initialEpochStringWX = guiManager->ToWxString(initialEpoch);
         initialEpochTextCtrl->SetValue(initialEpochStringWX);
         
         Rvector6 elements = theBody->GetTwoBodyElements();
         SMA  = elements[0];
         ECC  = elements[1];
         INC  = elements[2];
         RAAN = elements[3];
         AOP  = elements[4];
         TA   = elements[5];
         SMAStringWX = guiManager->ToWxString(elements[0]);
         SMATextCtrl->SetValue(SMAStringWX);
         ECCStringWX = guiManager->ToWxString(elements[1]);
         ECCTextCtrl->SetValue(ECCStringWX);
         INCStringWX = guiManager->ToWxString(elements[2]);
         INCTextCtrl->SetValue(INCStringWX);
         RAANStringWX = guiManager->ToWxString(elements[3]);
         RAANTextCtrl->SetValue(RAANStringWX);
         AOPStringWX = guiManager->ToWxString(elements[4]);
         AOPTextCtrl->SetValue(AOPStringWX);
         TAStringWX = guiManager->ToWxString(elements[5]);
         TATextCtrl->SetValue(TAStringWX);
      }

      ResetChangeFlags();
      
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage());
   }
}



//------------------------------------------------------------------------------
// private methods
//------------------------------------------------------------------------------
void CelestialBodyOrbitPanel::Create()
{
   int bSize     = 2;
   if (theBody->GetName() == SolarSystem::SUN_NAME)  isSun = true;
   else                                              isSun = false;
   userDef = theBody->IsUserDefined();
   
   GmatStaticBoxSizer  *boxSizer1 = new GmatStaticBoxSizer(wxVERTICAL, this, "Ephemeris Data");
   
   // empty the temporary value strings
   ephemSourceStringWX       = "";
   ephemFileStringWX      = "";
   naifIDStringWX         = "";
   centralBodyStringWX    = "";
   initialEpochStringWX   = "";
   SMAStringWX            = "";
   ECCStringWX            = "";
   INCStringWX            = "";
   RAANStringWX           = "";
   AOPStringWX            = "";
   TAStringWX             = "";
   
   // ephem source combo box
   sourceArray              = theBody->GetEphemSourceList();
   unsigned int ephemListSz = sourceArray.size();
   sourceArrayWX            = new wxString[ephemListSz];
   for (unsigned int jj = 0; jj < ephemListSz; jj++)
      sourceArrayWX[jj] = wxT(sourceArray[jj].c_str());
   ephemSourceStaticText = new wxStaticText(this, ID_TEXT, wxT("Ephemeris Source"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
   ephemSourceComboBox   = new wxComboBox(this, ID_COMBO_BOX_EPHEM_SOURCE, wxT(sourceArrayWX[0]),
                           wxDefaultPosition, wxDefaultSize, ephemListSz, sourceArrayWX,
                           wxCB_DROPDOWN|wxCB_READONLY);
   // ephem file
   ephemFileStaticText    =  new wxStaticText(this, ID_TEXT, wxT("Ephemeris File"),
                             wxDefaultPosition, wxSize(-1,-1), 0);
   ephemFileTextCtrl      = new wxTextCtrl(this, ID_TEXT_CTRL_EPHEM_FILE, wxT(""),
                            wxDefaultPosition, wxSize(200,-1), 0);
   ephemFileBrowseButton  = new wxButton(this, ID_BROWSE_BUTTON_EPHEM_FILE, wxT("Browse"),
                            wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
   
   if (userDef)
   {
      // naif ID for user-defined bodies
      naifIDStaticText   = new wxStaticText(this, ID_TEXT,wxT("NAIF ID"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
      naifIDTextCtrl     = new wxTextCtrl(this, ID_TEXT_CTRL_NAIF_ID, wxT(""),
                           wxDefaultPosition, wxSize(80, -1), 0);
   }
  
   // central body
//   centralBodyArray      = ss->GetBodiesInUse();
//   unsigned int bodiesSz = centralBodyArray.size();
//   centralBodyArrayWX = new wxString[bodiesSz];
//   for (unsigned int ii = 0; ii < bodiesSz; ii++)
//      centralBodyArrayWX[ii] = wxT(centralBodyArray[ii].c_str());
   centralBodyStaticText = new wxStaticText(this, ID_TEXT, wxT("Central Body"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
//   centralBodyComboBox   = new wxComboBox(this, ID_COMBO_BOX_CENTRAL_BODY, wxT(centralBodyArrayWX[0]),
//                           wxDefaultPosition, wxDefaultSize, bodiesSz, centralBodyArrayWX,
//                           wxCB_DROPDOWN|wxCB_READONLY);
   centralBodyComboBox = guiManager->GetCelestialBodyComboBox(this, ID_COMBO_BOX_CENTRAL_BODY, 
                                                              wxSize(80,-1));
   
   if (!isSun)
   {
      // initial epoch
      initialEpochStaticText = new wxStaticText(this, ID_TEXT, wxT("Initial A1 Epoch"),
                               wxDefaultPosition, wxSize(-1,-1), 0);
      initialEpochTextCtrl   = new wxTextCtrl(this, ID_TEXT_CTRL_INITIAL_EPOCH, wxT(""),
                               wxDefaultPosition, wxSize(140, -1),0);
      
      // SMA
      SMAStaticText      = new wxStaticText(this, ID_TEXT, wxT("SMA"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
      SMATextCtrl        = new wxTextCtrl(this, ID_TEXT_CTRL_SMA, wxT(""),
                           wxDefaultPosition, wxSize(140, -1),0);
      SMAUnitsStaticText = new wxStaticText(this, ID_TEXT, wxT("km"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
      
      // ECC
      ECCStaticText      = new wxStaticText(this, ID_TEXT, wxT("ECC"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
      ECCTextCtrl        = new wxTextCtrl(this, ID_TEXT_CTRL_ECC, wxT(""),
                           wxDefaultPosition, wxSize(140, -1),0);
      ECCUnitsStaticText = new wxStaticText(this, ID_TEXT, wxT(""),
                           wxDefaultPosition, wxSize(-1,-1), 0);
      
      // INC
      INCStaticText      = new wxStaticText(this, ID_TEXT, wxT("INC"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
      INCTextCtrl        = new wxTextCtrl(this, ID_TEXT_CTRL_INC, wxT(""),
                           wxDefaultPosition, wxSize(140, -1),0);
      INCUnitsStaticText = new wxStaticText(this, ID_TEXT, wxT("deg"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
      
      // RAAN
      RAANStaticText      = new wxStaticText(this, ID_TEXT, wxT("RAAN"),
                            wxDefaultPosition, wxSize(-1,-1), 0);
      RAANTextCtrl        = new wxTextCtrl(this, ID_TEXT_CTRL_RAAN, wxT(""),
                            wxDefaultPosition, wxSize(140, -1),0);
      RAANUnitsStaticText = new wxStaticText(this, ID_TEXT, wxT("deg"),
                            wxDefaultPosition, wxSize(-1,-1), 0);
      
      // AOP
      AOPStaticText      = new wxStaticText(this, ID_TEXT, wxT("AOP"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
      AOPTextCtrl        = new wxTextCtrl(this, ID_TEXT_CTRL_AOP, wxT(""),
                           wxDefaultPosition, wxSize(140, -1),0);
      AOPUnitsStaticText = new wxStaticText(this, ID_TEXT, wxT("deg"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
      
      // TA
      TAStaticText      = new wxStaticText(this, ID_TEXT, wxT("TA"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
      TATextCtrl        = new wxTextCtrl(this, ID_TEXT_CTRL_TA, wxT(""),
                           wxDefaultPosition, wxSize(140, -1),0);
      TAUnitsStaticText = new wxStaticText(this, ID_TEXT, wxT("deg"),
                           wxDefaultPosition, wxSize(-1,-1), 0);
   
   }
   
// sizers
   
   wxFlexGridSizer *orbitDataFlexGridSizer = new wxFlexGridSizer(3,0,0);
   orbitDataFlexGridSizer->Add(ephemSourceStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
   orbitDataFlexGridSizer->Add(ephemSourceComboBox,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
   orbitDataFlexGridSizer->Add(20,20,0,wxGROW|wxALIGN_LEFT|wxALL, bSize);
   
   orbitDataFlexGridSizer->Add(ephemFileStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
   orbitDataFlexGridSizer->Add(ephemFileTextCtrl,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
   orbitDataFlexGridSizer->Add(ephemFileBrowseButton,0, wxALIGN_CENTRE|wxALL, bSize);

   if (userDef)
   {
      orbitDataFlexGridSizer->Add(naifIDStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      orbitDataFlexGridSizer->Add(naifIDTextCtrl,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      orbitDataFlexGridSizer->Add(20,20,0,wxGROW|wxALIGN_LEFT|wxALL, bSize);
   }
   
   orbitDataFlexGridSizer->Add(centralBodyStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
   orbitDataFlexGridSizer->Add(centralBodyComboBox,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
   orbitDataFlexGridSizer->Add(20,20,0,wxGROW|wxALIGN_LEFT|wxALL, bSize);

   wxFlexGridSizer *initialStateFlexGridSizer = NULL;
   if (!isSun)
   {
      initialStateFlexGridSizer = new wxFlexGridSizer(3,0,0);
      initialStateFlexGridSizer->Add(initialEpochStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(initialEpochTextCtrl,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(20,20,0,wxGROW|wxALIGN_LEFT|wxALL, bSize);
      
      initialStateFlexGridSizer->Add(SMAStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(SMATextCtrl,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(SMAUnitsStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   
      initialStateFlexGridSizer->Add(ECCStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(ECCTextCtrl,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(ECCUnitsStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   
      initialStateFlexGridSizer->Add(INCStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(INCTextCtrl,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(INCUnitsStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   
      initialStateFlexGridSizer->Add(RAANStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(RAANTextCtrl,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(RAANUnitsStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   
      initialStateFlexGridSizer->Add(AOPStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(AOPTextCtrl,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(AOPUnitsStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   
      initialStateFlexGridSizer->Add(TAStaticText,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(TATextCtrl,0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
      initialStateFlexGridSizer->Add(TAUnitsStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   }
   
   boxSizer1->Add(orbitDataFlexGridSizer, 0, wxGROW|wxALIGN_LEFT|wxALL, bSize);
   
   mainBoxSizer    = new GmatStaticBoxSizer(wxHORIZONTAL, this, "");
   mainBoxSizer->Add(boxSizer1, 0, wxGROW|wxALIGN_LEFT|wxALL, bSize); 
   if (!isSun)
   {
      GmatStaticBoxSizer  *boxSizer2 = new GmatStaticBoxSizer(wxVERTICAL, this, "Initial TwoBody State");
      boxSizer2->Add(initialStateFlexGridSizer, 0, wxGROW|wxALIGN_CENTER|wxALL, bSize);
      mainBoxSizer->Add(boxSizer2, 0, wxGROW|wxALIGN_CENTRE|wxALL, bSize); 
   }
   
   // disable ephem source, ephem file, and central body for default bodies
   if (!userDef)
   {
      ephemSourceComboBox->Disable();
      ephemFileTextCtrl->Disable();
      ephemFileBrowseButton->Disable();
//      centralBodyComboBox->Disable();
   }
   centralBodyComboBox->Disable(); // do not allow user to change central body (maybe make this
                                   // static text somewhere ...)
   
   this->SetAutoLayout(true);
   this->SetSizer(mainBoxSizer);
   mainBoxSizer->Fit(this);
   mainBoxSizer->SetSizeHints(this);
}

void CelestialBodyOrbitPanel::ResetChangeFlags(bool discardMods)
{
   ephemSrcChanged  = false;
   ephemFileChanged = false;
   naifIDChanged    = false;
   cBodyChanged     = false;
   epochChanged     = false;
   SMAChanged       = false;
   ECCChanged       = false;
   INCChanged       = false;
   RAANChanged      = false;
   AOPChanged       = false;
   TAChanged        = false;
   
   if (discardMods)
   {
//      ephemSourceComboBox->DiscardEdits();
      ephemFileTextCtrl->DiscardEdits();
      if (userDef) naifIDTextCtrl->DiscardEdits();
//      centralBodyComboBox->DiscardEdits();
      if (!isSun)
      {
         initialEpochTextCtrl->DiscardEdits();
         SMATextCtrl->DiscardEdits();
         ECCTextCtrl->DiscardEdits();
         INCTextCtrl->DiscardEdits();
         RAANTextCtrl->DiscardEdits();
         AOPTextCtrl->DiscardEdits();
         TATextCtrl->DiscardEdits();
      }
   }
}

//Event Handling
void CelestialBodyOrbitPanel::OnEphemSourceComboBoxChange(wxCommandEvent &event)
{
   std::string newEphemSrc = (ephemSourceComboBox->GetStringSelection()).c_str();
   if (newEphemSrc == ephemSrc) return;
   ephemSrcChanged = true;
   dataChanged     = true;
   ephemSrc        = newEphemSrc;
   theCBPanel->EnableUpdate(true);
   if ((!userDef) || (newEphemSrc == "TwoBodyPropagation"))
   {
      ephemFileTextCtrl->Disable();
      ephemFileBrowseButton->Disable();
   }
   else
   {
      ephemFileTextCtrl->Enable();
      ephemFileBrowseButton->Enable();
   }
   if (userDef)
   {
      if (newEphemSrc == "SPICE")  naifIDTextCtrl->Enable();
      else                         naifIDTextCtrl->Disable();
   }
}

void CelestialBodyOrbitPanel::OnEphemFileTextCtrlChange(wxCommandEvent &event)
{
   if (ephemFileTextCtrl->IsModified())
   {
      ephemFileChanged  = true;
      dataChanged       = true;
      theCBPanel->EnableUpdate(true);
   }
}

void CelestialBodyOrbitPanel::OnEphemFileBrowseButton(wxCommandEvent &event)
{
   wxString oldFile = ephemFileTextCtrl->GetValue();
   wxFileDialog dialog(this, _T("Choose a file"), _T(""), _T(""), _T("*.*"));
   if (dialog.ShowModal() == wxID_OK)
   {
      wxString fileName = (dialog.GetPath()).c_str();
      if (!fileName.IsSameAs(oldFile))
      {
         ephemFileTextCtrl->SetValue(fileName);
         ephemFileChanged = true;
         dataChanged      = true;
         theCBPanel->EnableUpdate(true);
      }
   }
}

void CelestialBodyOrbitPanel::OnNaifIdTextCtrlChange(wxCommandEvent &event)
{
   if (naifIDTextCtrl->IsModified())
   {
      naifIDChanged   = true;
      dataChanged     = true;
      theCBPanel->EnableUpdate(true);
   }
}

void CelestialBodyOrbitPanel::OnCentralBodyComboBoxChange(wxCommandEvent &event)
{
   std::string newCentralBody = (centralBodyComboBox->GetStringSelection()).c_str();
   if (newCentralBody == centralBody) return;
   cBodyChanged       = true;
   dataChanged        = true;
   centralBody        = newCentralBody;
   theCBPanel->EnableUpdate(true);
}

void CelestialBodyOrbitPanel::OnEpochTextCtrlChange(wxCommandEvent &event)
{
   if (initialEpochTextCtrl->IsModified())
   {
      epochChanged    = true;
      dataChanged     = true;
      theCBPanel->EnableUpdate(true);
   }
}

void CelestialBodyOrbitPanel::OnSMATextCtrlChange(wxCommandEvent &event)
{
   if (SMATextCtrl->IsModified())
   {
      SMAChanged      = true;
      dataChanged     = true;
      theCBPanel->EnableUpdate(true);
   }
}

void CelestialBodyOrbitPanel::OnECCTextCtrlChange(wxCommandEvent &event)
{
   if (ECCTextCtrl->IsModified())
   {
      ECCChanged      = true;
      dataChanged     = true;
      theCBPanel->EnableUpdate(true);
   }
}

void CelestialBodyOrbitPanel::OnINCTextCtrlChange(wxCommandEvent &event)
{
   if (INCTextCtrl->IsModified())
   {
      INCChanged      = true;
      dataChanged     = true;
      theCBPanel->EnableUpdate(true);
   }
}

void CelestialBodyOrbitPanel::OnRAANTextCtrlChange(wxCommandEvent &event)
{
   if (RAANTextCtrl->IsModified())
   {
      RAANChanged     = true;
      dataChanged     = true;
      theCBPanel->EnableUpdate(true);
   }
}

void CelestialBodyOrbitPanel::OnAOPTextCtrlChange(wxCommandEvent &event)
{
   if (AOPTextCtrl->IsModified())
   {
      AOPChanged      = true;
      dataChanged     = true;
      theCBPanel->EnableUpdate(true);
   }
}

void CelestialBodyOrbitPanel::OnTATextCtrlChange(wxCommandEvent &event)
{
   if (TATextCtrl->IsModified())
   {
      TAChanged       = true;
      dataChanged     = true;
      theCBPanel->EnableUpdate(true);
   }
}
