//$Header$
//------------------------------------------------------------------------------
//                                  CelestialBodyPropertiesPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Wendy C. Shoan
// Created: 2009.01.14
//
/**
 * This is the panel for the Properties tab on the notebook on the CelestialBody
 * Panel.
 *
 */
//------------------------------------------------------------------------------

#include "CelestialBodyPropertiesPanel.hpp"
#include "GmatBaseException.hpp"
#include "GmatAppData.hpp"
#include "MessageInterface.hpp"
#include "StringUtil.hpp"
#include <fstream>

//#define DEBUG_CB_PROP_PANEL

// event tables for wxMac/Widgets
BEGIN_EVENT_TABLE(CelestialBodyPropertiesPanel, wxPanel)
   EVT_BUTTON(ID_BUTTON_BROWSE, CelestialBodyPropertiesPanel::OnBrowseButton)
   EVT_TEXT(ID_TEXT_CTRL_MU, CelestialBodyPropertiesPanel::OnMuTextCtrlChange)
   EVT_TEXT(ID_TEXT_CTRL_EQRAD, CelestialBodyPropertiesPanel::OnEqRadTextCtrlChange)
   EVT_TEXT(ID_TEXT_CTRL_FLAT, CelestialBodyPropertiesPanel::OnFlatTextCtrlChange)
   EVT_TEXT(ID_TEXT_CTRL_TEXTURE, CelestialBodyPropertiesPanel::OnTextureTextCtrlChange)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------

CelestialBodyPropertiesPanel::CelestialBodyPropertiesPanel(GmatPanel *cbPanel, 
                              wxWindow *parent, CelestialBody *body) :
   wxPanel        (parent),
   dataChanged    (false),
   canClose       (true),
   theBody        (body),
   mu             (0.0),
   eqRad          (0.0),
   flat           (0.0),
   textureMap     (""),
   muChanged      (false),
   eqRadChanged   (false),
   flatChanged    (false),
   textureChanged (false),
   theCBPanel     (cbPanel)
{
   guiManager     = GuiItemManager::GetInstance();
//   guiInterpreter = GmatAppData::Instance()->GetGuiInterpreter();
   
   Create();
}

CelestialBodyPropertiesPanel::~CelestialBodyPropertiesPanel()
{
   // nothing to do ... la la la la la ...
}

void CelestialBodyPropertiesPanel::SaveData()
{
   std::string strval;
   Real        tmpval;
   bool        realsOK   = true;
   bool        stringsOK = true;
   
   
   // don't do anything if no data has been changed.
   // note that dataChanged will be true if the user modified any combo box or
   // text ctrl, whether or not he/she actually changed the value; we want to only
   // send the values to the object if something was really changed, to avoid
   // the hasBeenModified flag being set to true erroneously
   if (!dataChanged) return;
   
   bool reallyChanged = false;
   canClose    = true;
   
   if (muChanged)
   {
      strval = muTextCtrl->GetValue();
      if (!theCBPanel->CheckReal(tmpval, strval, "Mu", "Positive Real Number",
            false, true, true, false))
         realsOK = false;
      else 
      {
         if (tmpval != mu) reallyChanged = true;
         mu = tmpval;
      }
   }
   if (eqRadChanged)
   {
      strval = eqRadTextCtrl->GetValue();
      if (!theCBPanel->CheckReal(tmpval, strval, "Equatorial Radius", "Positive Real Number",
            false, true, true, false))
         realsOK = false;
      else 
      {
         if (tmpval != eqRad) reallyChanged = true;
         eqRad = tmpval;
      }
   }
   if (flatChanged)
   {
      strval = flatTextCtrl->GetValue();
      if (!theCBPanel->CheckReal(tmpval, strval, "Flattening Coefficient", "Non-negative Real Number",
            false, true, true, true))
         realsOK = false;
      else 
      {
         if (tmpval != flat) reallyChanged = true;
         flat = tmpval;
      }
   }
   if (!realsOK)
   {
      std::string errmsg = "Please enter valid Real values before saving data.\n";
      MessageInterface::PopupMessage(Gmat::ERROR_, errmsg);
   }

   if (textureChanged)
   {
      strval = textureTextCtrl->GetValue();
      #ifdef DEBUG_CB_PROP_PANEL
         MessageInterface::ShowMessage("textureChanged is true : %s\n",
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
         if (strval != textureMap) reallyChanged = true;
         textureMap = strval;
         filename.close();
      } 
   }

   if (realsOK && stringsOK && reallyChanged)
   {
      #ifdef DEBUG_CB_PROP_PANEL
         MessageInterface::ShowMessage("Reals and Strings are OK - setting them\n");
         MessageInterface::ShowMessage(
               "mu = %12.4f, eqRad = %12.4f, flat = %12.4f, textureMap = %s\n",
               mu, eqRad, flat, textureMap.c_str());       
      #endif
      theBody->SetGravitationalConstant(mu);
      theBody->SetEquatorialRadius(eqRad);
      theBody->SetFlattening(flat);
      theBody->SetStringParameter(theBody->GetParameterID("TextureMapFileName"), 
                                  textureMap);
   }
   else
      canClose = false;
   
   dataChanged = false;
   ResetChangeFlags(true);
}

void CelestialBodyPropertiesPanel::LoadData()
{
   try
   {
      mu         = theBody->GetGravitationalConstant();
      muString   = guiManager->ToWxString(mu);
      muTextCtrl->SetValue(muString);
      
      eqRad       = theBody->GetEquatorialRadius();
      eqRadString = guiManager->ToWxString(eqRad);
      eqRadTextCtrl->SetValue(eqRadString);
      
      flat       = theBody->GetFlattening();
      flatString = guiManager->ToWxString(flat);
      flatTextCtrl->SetValue(flatString);
      
      textureMap = theBody->GetStringParameter(theBody->GetParameterID("TextureMapFileName"));
      textureTextCtrl->SetValue(textureMap.c_str());
      
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
void CelestialBodyPropertiesPanel::Create()
{
   int bSize     = 2;
   
   // empty the temporary value strings
   muString      = "";
   eqRadString   = "";
   flatString    = "";
   textureString = "";
   
   // mu
   muStaticText      = new wxStaticText(this, ID_TEXT, wxT("Mu"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   muTextCtrl        = new wxTextCtrl(this, ID_TEXT_CTRL_MU, wxT(""),
                       wxDefaultPosition, wxSize(150, -1),0);
   muUnitsStaticText = new wxStaticText(this, ID_TEXT, wxT("km^3/sec^2"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   // eq. radius
   eqRadStaticText      = new wxStaticText(this, ID_TEXT, wxT("Equatorial Radius"),
                          wxDefaultPosition, wxSize(-1,-1), 0);
   eqRadTextCtrl        = new wxTextCtrl(this, ID_TEXT_CTRL_EQRAD, wxT(""),
                          wxDefaultPosition, wxSize(150, -1),0);
   eqRadUnitsStaticText = new wxStaticText(this, ID_TEXT, wxT("km"),
                          wxDefaultPosition, wxSize(-1,-1), 0);
   // flattening
   flatStaticText      = new wxStaticText(this, ID_TEXT, wxT("Flattening"),
                         wxDefaultPosition, wxSize(-1,-1), 0);
   flatTextCtrl        = new wxTextCtrl(this, ID_TEXT_CTRL_FLAT, wxT(""),
                         wxDefaultPosition, wxSize(150, -1),0);
   flatUnitsStaticText = new wxStaticText(this, ID_TEXT, wxT(""), // unitless
                         wxDefaultPosition, wxSize(-1,-1), 0);
   // texture map
   textureStaticText = new wxStaticText(this, ID_TEXT, wxT("Texture Map File"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   textureTextCtrl   = new wxTextCtrl(this, ID_TEXT_CTRL_TEXTURE, wxT(""),
                       wxDefaultPosition, wxSize(300,-1), 0);
   browseButton      = new wxButton(this, ID_BUTTON_BROWSE, wxT("Browse"),
                       wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
   
   
   wxFlexGridSizer *cbPropGridSizer = new wxFlexGridSizer(4,0,0);
   cbPropGridSizer->Add(muStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(muTextCtrl,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(muUnitsStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(20,20,0,wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(eqRadStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(eqRadTextCtrl,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(eqRadUnitsStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(20,20,0,wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(flatStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(flatTextCtrl,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(flatUnitsStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(20,20,0,wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(20,20,0,wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(20,20,0,wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer->Add(20,20,0,wxALIGN_LEFT|wxALL, bSize);
   
   wxFlexGridSizer *cbPropGridSizer2 = new wxFlexGridSizer(3,0,0);
   cbPropGridSizer2->Add(textureStaticText,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer2->Add(textureTextCtrl,0, wxALIGN_LEFT|wxALL, bSize);
   cbPropGridSizer2->Add(browseButton,0, wxALIGN_CENTRE|wxALL, bSize);
   
   pageSizer    = new GmatStaticBoxSizer(wxVERTICAL, this, "");
   
   pageSizer->Add(cbPropGridSizer, 0, wxALIGN_CENTRE|wxALL, bSize); 
   pageSizer->Add(cbPropGridSizer2, 0, wxALIGN_CENTRE|wxALL, bSize); 
   
   this->SetAutoLayout(true);
   this->SetSizer(pageSizer);
   pageSizer->Fit(this);
   //pageSizer->SetSizerHints(this);
}

void CelestialBodyPropertiesPanel::ResetChangeFlags(bool discardMods)
{
   muChanged      = false;
   eqRadChanged   = false;
   flatChanged    = false;
   textureChanged = false;
   if (discardMods)
   {
      muTextCtrl->DiscardEdits();
      eqRadTextCtrl->DiscardEdits();
      flatTextCtrl->DiscardEdits();
      textureTextCtrl->DiscardEdits();
   }
   dataChanged = false;
}

//Event Handling
void CelestialBodyPropertiesPanel::OnMuTextCtrlChange(wxCommandEvent &event)
{
   if (muTextCtrl->IsModified())
   {
      muChanged   = true;
      dataChanged = true;
      theCBPanel->EnableUpdate(true);
   }
}

void CelestialBodyPropertiesPanel::OnEqRadTextCtrlChange(wxCommandEvent &event)
{
   if (eqRadTextCtrl->IsModified())
   {
      eqRadChanged   = true;
      dataChanged    = true;
      theCBPanel->EnableUpdate(true);
   }

}

void CelestialBodyPropertiesPanel::OnFlatTextCtrlChange(wxCommandEvent &event)
{
   if (flatTextCtrl->IsModified())
   {
      flatChanged   = true;
      dataChanged   = true;
      theCBPanel->EnableUpdate(true);
   }

}

void CelestialBodyPropertiesPanel::OnTextureTextCtrlChange(wxCommandEvent &event)
{
   if (textureTextCtrl->IsModified())
   {
      textureChanged  = true;
      dataChanged     = true;
      theCBPanel->EnableUpdate(true);
   }

}


void CelestialBodyPropertiesPanel::OnBrowseButton(wxCommandEvent &event)
{
   wxString oldTexture = textureTextCtrl->GetValue();
   wxFileDialog dialog(this, _T("Choose a file"), _T(""), _T(""), _T("*.*"));
   if (dialog.ShowModal() == wxID_OK)
   {
      wxString fileName = (dialog.GetPath()).c_str();
      if (!fileName.IsSameAs(oldTexture))
      {
         textureTextCtrl->SetValue(fileName);
         textureChanged = true;
         theCBPanel->EnableUpdate(true);
         dataChanged = true;
      }
   }
}

wxString CelestialBodyPropertiesPanel::ToString(Real rval)
{
   return guiManager->ToWxString(rval);
}



   