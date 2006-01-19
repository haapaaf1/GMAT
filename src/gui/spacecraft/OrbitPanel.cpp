//$Header$
//------------------------------------------------------------------------------
//                           OrbitPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P.
//
//
// Author: Allison Greene
// Created: 2004/04/08
/**
 * This class contains information needed to setup users spacecraft orbit
 * through GUI
 * 
 */
//------------------------------------------------------------------------------
#include "OrbitPanel.hpp"
#include "MessageInterface.hpp"
#include <sstream>

//#define DEBUG_ORBIT_PANEL 1
//#define DEBUG_ORBIT_PANEL_CONVERT 1
//#define DEBUG_ORBIT_PANEL_SAVE 1

//------------------------------
// event tables for wxWindows
//------------------------------
BEGIN_EVENT_TABLE(OrbitPanel, wxPanel)
   EVT_COMBOBOX(ID_COMBOBOX, OrbitPanel::OnComboBoxChange)
   EVT_TEXT(ID_TEXTCTRL, OrbitPanel::OnTextChange)
END_EVENT_TABLE()

//------------------------------
// public methods
//------------------------------
//------------------------------------------------------------------------------
// OrbitPanel(wxWindow *parent)
//------------------------------------------------------------------------------
/**
 * Constructs OrbitPanel object.
 *
 * @param <parent> input parent.
 *
 * @note Creates the Universe GUI
 */
//------------------------------------------------------------------------------
OrbitPanel::OrbitPanel(wxWindow *parent,
                        Spacecraft *spacecraft,
                        SolarSystem *solarsystem,
                        wxButton *theApplyButton) : 
   wxPanel           (parent),
   theApplyButton    (theApplyButton)
{
   // initalize data members
   theGuiInterpreter = GmatAppData::GetGuiInterpreter();
   theGuiManager = GuiItemManager::GetInstance();

   theSpacecraft = spacecraft;
   theSolarSystem = solarsystem;

   mIsCoordSysChanged = false;
   mIsStateChanged = false;
   mIsStateTypeChanged = false;
   mIsEpochChanged = false;
   canClose = true;

   Create();
}


//------------------------------------------------------------------------------
// ~OrbitPanel()
//------------------------------------------------------------------------------
OrbitPanel::~OrbitPanel()
{
   theGuiManager->UnregisterComboBox("CoordinateSystem", mCoordSysComboBox);
}


//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
/**
 * @note Gets the values from theSpacecraft and puts them in the text fields
 */
//------------------------------------------------------------------------------
void OrbitPanel::LoadData()
{
   #if DEBUG_ORBIT_PANEL
      MessageInterface::ShowMessage("In OrbitPanel::LoadData() \n");
   #endif

   mInternalCoord = theGuiInterpreter->GetInternalCoordinateSystem();

   if (mInternalCoord == NULL)
   {
      MessageInterface::ShowMessage
         ("   mInternalCoord is NULL.\n");
   }
   else
   {
      #if DEBUG_ORBIT_PANEL
      MessageInterface::ShowMessage
         ("   mInternalCoord=%s, addr=%d\n",
          mInternalCoord->GetName().c_str(), mInternalCoord);
      #endif
   }
   
   // load data from the core engine
   try
   {
      Rvector6 outState, displayState;

      StringArray reps = TimeConverterUtil::GetValidTimeRepresentations();

      // load the epoch format - hard coded for now
//      wxString epochFormatList[] =
//      {
//         wxT("TAIModJulian"),
//         wxT("UTCModJulian"),
//         wxT("TAIGregorian"),
//         wxT("UTCGregorian")
//      };
//
//      for (unsigned int i = 0; i<4; i++)
//         epochFormatComboBox->Append(wxString(epochFormatList[i].c_str()));

      for (unsigned int i = 0; i < reps.size(); i++)
         epochFormatComboBox->Append(reps[i].c_str());
      
      // load the epoch
      std::string epochFormat = theSpacecraft->GetStringParameter("DateFormat"); //GetDisplayDateFormat();
      mEpoch = theSpacecraft->GetRealParameter("A1Epoch");
      epochFormatComboBox->SetValue(wxT(epochFormat.c_str()));
      fromEpochFormat = epochFormat;
      
      std::string epochStr = theSpacecraft->GetStringParameter("Epoch"); //GetDisplayEpoch();
      epochValue->SetValue(wxT(epochStr.c_str()));
      
      // load the coordinate system
      std::string coordSystemStr =
         theSpacecraft->GetRefObjectName(Gmat::COORDINATE_SYSTEM);
      mCoordSysComboBox->SetValue(coordSystemStr.c_str());
      //mFromCoordStr = coordSystemStr.c_str();
      mFromCoordStr = coordSystemStr;
      
      mOutCoord = (CoordinateSystem*)theGuiInterpreter->
         GetCoordinateSystem(coordSystemStr);
      mFromCoord = mOutCoord;
      
      if (mOutCoord == NULL)
      {
         MessageInterface::ShowMessage
            ("   The Spacecraft CoordinateSystem is NULL.\n");
      }
      else
      {
         #if DEBUG_ORBIT_PANEL
         MessageInterface::ShowMessage("   mOutCoord=%s, addr=%d\n",
                                       mOutCoord->GetName().c_str(), mOutCoord);
         #endif
      }
      
      // get the origin for the output coordinate system
      std::string originName = mOutCoord->GetStringParameter("Origin");
      SpacePoint *origin = (SpacePoint*)theGuiInterpreter->GetConfiguredItem(originName);

      #if DEBUG_ORBIT_PANEL
      MessageInterface::ShowMessage
         ("   origin=%s, addr=%d\n", originName.c_str(), origin);
      #endif
      
      // load the state type - hard coded for now
      std::string stType;
      
      if (origin->IsOfType(Gmat::CELESTIAL_BODY))
      {
         wxString stateTypeList[] =
         {
            wxT("Cartesian"),
            wxT("Keplerian"),
            wxT("ModifiedKeplerian"),
            wxT("SphericalAZFPA"),
            wxT("SphericalRADEC")
         };

         for (unsigned int i = 0; i<5; i++)
            stateTypeComboBox->Append(wxString(stateTypeList[i].c_str()));

         stType = theSpacecraft->GetDisplayCoordType();
      }
      else
      {
         wxString stateTypeList[] =
         {
            wxT("Cartesian"),
            wxT("SphericalAZFPA"),
            wxT("SphericalRADEC")
         };

         for (unsigned int i = 0; i<3; i++)
            stateTypeComboBox->Append(wxString(stateTypeList[i].c_str()));

         stType = "Cartesian";
      }
      
      stateTypeComboBox->SetValue(wxT(stType.c_str()));
      //mFromStateTypeStr = stType.c_str();
      mFromStateTypeStr = stType;;

      // anomaly types - hard coded for now
      wxString anomalyList[] =
      {
         wxT("Mean Anomaly"),
         wxT("True Anomaly"),
         wxT("Eccentric Anomaly")
      };

      for (unsigned int i = 0; i<3; i++)
         anomalyComboBox->Append(wxString(anomalyList[i].c_str()));
      
      // Get Spacecraft initial state and display
      mCartState = theSpacecraft->GetState(stType);
      mTempCartState = mCartState;
      mOutState = mCartState;
      DisplayState();
      SetLabelsUnits(stType);
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage
         ("OrbitPanel:LoadData() error occurred!\n%s\n", e.GetMessage().c_str());
   }
}


//------------------------------------------------------------------------------
// void SaveData()
//------------------------------------------------------------------------------
/**
 * @note Gets the values from the text fields and puts them in theSpacecraft
 */
//------------------------------------------------------------------------------
void OrbitPanel::SaveData()
{
   #if DEBUG_ORBIT_PANEL_SAVE
   MessageInterface::ShowMessage
      ("OrbitPanel::SaveData() entered\n   mCartState=%s\n   mTempCartState=%s\n   "
       "mOutState=%s\n", mCartState.ToString().c_str(), mTempCartState.ToString().c_str(),
       mOutState.ToString().c_str());
   MessageInterface::ShowMessage("===> mIsCoordSysChanged=%d\n", mIsCoordSysChanged);
   MessageInterface::ShowMessage("===> mIsStateTypeChanged=%d\n", mIsStateTypeChanged);
   MessageInterface::ShowMessage("===> mIsStateChanged=%d\n", mIsStateChanged);
   #endif
   
   Rvector6 outState, cartState, displayState;
   
   canClose = true;
   
   // save epoch format
   wxString epochFormatStr = epochFormatComboBox->GetStringSelection();
   theSpacecraft->SetDisplayDateFormat(epochFormatStr.c_str());
   
   // save epoch
   wxString epochStr = epochValue->GetValue();
   theSpacecraft->SetDisplayEpoch(epochStr.c_str());
   
   // save coordinate system
   wxString coordSystemStr = mCoordSysComboBox->GetStringSelection();
   theSpacecraft->SetRefObjectName(Gmat::COORDINATE_SYSTEM, coordSystemStr.c_str());

   // if coordinate system is NULL, set it to avoid crash when show script(loj: 11/28/05)
   if (theSpacecraft->GetRefObject(Gmat::COORDINATE_SYSTEM, "") == NULL)
      theSpacecraft->SetRefObject(mOutCoord, Gmat::COORDINATE_SYSTEM);
   if (theSpacecraft->GetInternalCoordSystem() == NULL)
      theSpacecraft->SetInternalCoordSystem(mInternalCoord);
   
   // save state type
   wxString stateTypeStr = stateTypeComboBox->GetStringSelection();
   theSpacecraft->SetDisplayCoordType(std::string (stateTypeStr.c_str()));
   /// @todo: need to correct in spacecraft code because this should
   /// only save display state type

   // save orbital elements    
   displayState[0] = atof(textCtrl1->GetValue());
   displayState[1] = atof(textCtrl2->GetValue());
   displayState[2] = atof(textCtrl3->GetValue());
   displayState[3] = atof(textCtrl4->GetValue());
   displayState[4] = atof(textCtrl5->GetValue());
   displayState[5] = atof(textCtrl6->GetValue()); 
   
   if (strcmp(stateTypeStr.c_str(), "Keplerian") == 0)
   {
      if(displayState[1] < 0.0)
      {
         MessageInterface::PopupMessage
            (Gmat::WARNING_, "ECC must be greater than or equal to zero.");
         
         textCtrl2->SetValue(ToString(mOutState[1]));
         canClose = false;
         return;
      }
      else if((displayState[0] > 0.0) && (displayState[1] > 1.0))
      {
         MessageInterface::PopupMessage
            //(Gmat::WARNING_, "SMA > 0 and ECC > 1, both can not be");
            (Gmat::WARNING_, "SMA cannot be positive and ECC greater than 1.");
         textCtrl1->SetValue(ToString(mOutState[0]));
         textCtrl2->SetValue(ToString(mOutState[1]));
         canClose = false;
         return;
      }
      else if((displayState[0] < 0.0) && (displayState[1] < 1.0))
      {
         MessageInterface::PopupMessage
            //(Gmat::WARNING_, "SMA < 0 and ECC < 1, both can not be");
            (Gmat::WARNING_, "SMA cannot be negative and ECC less than 1.");
         textCtrl1->SetValue(ToString(mOutState[0]));
         textCtrl2->SetValue(ToString(mOutState[1]));
         canClose = false;
         return;
      }          
   }

   if (strcmp(stateTypeStr.c_str(), "ModifiedKeplerian") == 0)
   {
      if(displayState[0] <= 0.0)
      {
         MessageInterface::PopupMessage
            (Gmat::WARNING_, "RadPer must be greater than zero.");
         canClose = false;
         return;
      }

      if(displayState[1] == 0.0)
      {
         MessageInterface::PopupMessage
            (Gmat::WARNING_, "RadApo can not be equal to zero.");
         canClose = false;
         return;
      }
   }

   if ( (strcmp(stateTypeStr.c_str(), "SphericalAZFPA") == 0) || 
        (strcmp(stateTypeStr.c_str(), "SphericalRADEC") == 0) )
   {
      if (displayState[0] <= 0.0)
      {
         MessageInterface::PopupMessage
            (Gmat::WARNING_, "RMAG must be greater than zero.");
         canClose = false;
         return;
      }
   }

//   // Check to make sure that the Keplerian values 
//   // are acceptable for the spacecraft
//   if (strcmp(stateTypeStr.c_str(), "Keplerian") == 0)
//   {
//      if(displayState[1] < 0.0)
//      {
//         displayState[1] *= -1.0;
//         MessageInterface::PopupMessage
//         (Gmat::WARNING_, "ECC < 0, so multipled ECC by -1\n");
//      }
//      else if((displayState[0] > 0.0) && (displayState[1] > 1.0))
//      {
//         displayState[0] *= -1.0;
//         MessageInterface::PopupMessage
//         (Gmat::WARNING_, "SMA > 0 and ECC > 1, so multipled SMA by -1\n");
//      }
//      else if((displayState[0] < 0.0) && (displayState[1] < 1.0))
//      {
//         displayState[0] *= -1.0;
//         MessageInterface::PopupMessage
//         (Gmat::WARNING_, "SMA < 0 and ECC < 1, so multipled SMA by -1\n");
//      }          
//   }    

   
   // save cooridnate system name if changed
   if (mIsCoordSysChanged)
   {
      mIsCoordSysChanged = false;
      theSpacecraft->SetStringParameter
         ("CoordinateSystem",
          mCoordSysComboBox->GetStringSelection().c_str());
   }
   
   // save state type name if changed
   
   if (mIsStateTypeChanged)
   {
      mIsStateTypeChanged = false;
      wxString stateTypeStr = stateTypeComboBox->GetStringSelection();
      
      #if DEBUG_ORBIT_PANEL
      MessageInterface::ShowMessage
         ("OrbitPanel::SaveData() Saving stateType=%s\n", stateTypeStr.c_str());
      #endif
      
      theSpacecraft->SetStringParameter("StateType", stateTypeStr.c_str());
   }
   
   // Check to see if state needs to be converted to Cartesian before save
   if (mIsStateChanged)
   {
      mIsStateChanged = false;
      
      // turn off modified flag
      textCtrl1->DiscardEdits();
      textCtrl2->DiscardEdits();
      textCtrl3->DiscardEdits();
      textCtrl4->DiscardEdits();
      textCtrl5->DiscardEdits();
      textCtrl6->DiscardEdits();
      
      #if DEBUG_ORBIT_PANEL
         MessageInterface::ShowMessage
            ("OrbitPanel::SaveData() text changed. displayState =\n   %s\n",
             displayState.ToString().c_str());
      #endif
         
      outState = displayState;
      
      // Make sure the saved state is cartesian
      if (strcmp(stateTypeStr.c_str(), "Cartesian") == 0)
      {
         // already cartesian state
         cartState = displayState;
      }
      else
      {
         // convert to cartesian state
         //loj: 11/28/05
         //cartState = ConvertState(mInternalCoord, displayState, 
         //                         stateTypeStr.c_str(), "Cartesian");
         cartState = ConvertState(mOutCoord, displayState, 
                                  stateTypeStr.c_str(), "Cartesian");
      }
      
      // Make sure the saved state is earth mean J2000 equatorial
      if (strcmp(coordSystemStr.c_str(), "EarthMJ2000Eq") == 0)
      {
         // already earth mean J2000 equatorial
         outState = cartState;
      }         
      else
      {
         // convert to Internal CoordinateSystem (Earth Mean J2000 Equatorial)
         mCoordConverter.Convert(A1Mjd(mEpoch), cartState, mOutCoord, outState,
                                 mInternalCoord);
      }
      
      // save outState
      mCartState = outState;       
      mTempCartState = outState;
      
      // save to spacecraft
      theSpacecraft->SetState(mCartState);
   }
   else
   {
      mCartState = mTempCartState;
      //outState = mCartState;
   }
   
   #if DEBUG_ORBIT_PANEL
      MessageInterface::ShowMessage( "epoch format      = %s\n",epochFormatStr.c_str() );
      MessageInterface::ShowMessage( "epoch             = %s\n",epochStr.c_str() );
      MessageInterface::ShowMessage( "coordinate system = %s\n",coordSystemStr.c_str() );
      MessageInterface::ShowMessage( "state type        = %s\n",stateTypeStr.c_str() );
      MessageInterface::ShowMessage( "anomaly type      = %s\n",anomaly.GetType().c_str() );
      MessageInterface::ShowMessage( "mCartState        = %s\n",mCartState.ToString().c_str() );
   #endif
      
   #if DEBUG_ORBIT_PANEL_SAVE
      MessageInterface::ShowMessage
         ("OrbitPanel::SaveData() exiting\n   mCartState=%s\n   mTempCartState=%s\n   "
          "mOutState=%s\n", mCartState.ToString().c_str(), mTempCartState.ToString().c_str(),
          mOutState.ToString().c_str());
   MessageInterface::ShowMessage("===> mIsStateChanged=%d\n", mIsStateChanged);
   #endif

}


//-------------------------------
// private methods
//-------------------------------

//------------------------------------------------------------------------------
// void Create()
//------------------------------------------------------------------------------
/**
 *
 * @note Creates the page for orbit information
 */
//------------------------------------------------------------------------------
void OrbitPanel::Create()
{
   #if DEBUG_ORBIT_PANEL
      MessageInterface::ShowMessage("In OrbitPanel::Create() \n");
   #endif

   Integer bsize = 3; // border size

   wxString emptyList[] = {};

   //------------- create sizers -------------
   // sizer for orbit tab
   wxBoxSizer *orbitSizer = new wxBoxSizer(wxHORIZONTAL);
      
   //flex grid sizer for the epoch format, coordinate system and state type
   wxFlexGridSizer *pageSizer = new wxFlexGridSizer(5, 2, bsize, bsize);
   
   // static box for the elements
   wxStaticBox *elementBox =
      new wxStaticBox(this, ID_STATIC_ELEMENT, wxT("Elements"));
   wxStaticBoxSizer *elementSizer =
      new wxStaticBoxSizer(elementBox, wxVERTICAL);
   //-----------------------------------------

   //----------------- epoch -----------------
   // label for epoch
   wxStaticText *epochStaticText = new wxStaticText( this, ID_TEXT,
      wxT("Epoch"), wxDefaultPosition, wxDefaultSize, 0 );
   
   // combo box for the epoch format
   epochFormatComboBox = new wxComboBox( this, ID_COMBOBOX, wxT(""),
      wxDefaultPosition, wxSize(150,-1), 0, emptyList, wxCB_DROPDOWN | wxCB_READONLY );

   // textfield for the epoch value
   epochValue = new wxTextCtrl( this, ID_TEXTCTRL, wxT(""),
      wxDefaultPosition, wxSize(150,-1), 0 );
   //-----------------------------------------

   //----------- coordinate system -----------
   // label for coordinate system
   wxStaticText *coordSysStaticText = new wxStaticText( this, ID_TEXT,
      wxT("Coordinate System"), wxDefaultPosition, wxDefaultSize, 0 );

   //Get CordinateSystem ComboBox from the GuiItemManager.
   mCoordSysComboBox =
      theGuiManager->GetCoordSysComboBox(this, ID_COMBOBOX, wxSize(150,-1));
   //-----------------------------------------

   //-------------- state type ---------------
   // label for state type
   wxStaticText *stateTypeStaticText = new wxStaticText( this, ID_TEXT,
      wxT("State Type"), wxDefaultPosition, wxDefaultSize, 0 );

   // combo box for the state
   stateTypeComboBox = new wxComboBox( this, ID_COMBOBOX, wxT(""),
      wxDefaultPosition, wxSize(150,-1), 0, emptyList, wxCB_DROPDOWN | wxCB_READONLY);
   //-----------------------------------------

   //---------------- anomaly ----------------
   // label for anomaly type
   anomalyStaticText = new wxStaticText( this, ID_TEXT,
      wxT("Anomaly Type "), wxDefaultPosition, wxDefaultSize, 0 );

   // combo box for the anomaly type
   anomalyComboBox = new wxComboBox( this, ID_COMBOBOX, wxT(""),
      wxDefaultPosition, wxSize(125,-1), 0, emptyList, wxCB_DROPDOWN | wxCB_READONLY );
   //-----------------------------------------
   
   // add to page sizer
   pageSizer->Add( epochStaticText, 0, wxALIGN_LEFT | wxALL, bsize );
   pageSizer->Add( 20, 20, 0, wxALIGN_LEFT | wxALL, bsize );
   pageSizer->Add( epochFormatComboBox, 0, wxALIGN_LEFT | wxALL, bsize );
   pageSizer->Add( epochValue, 0, wxALIGN_LEFT | wxALL, bsize );
   pageSizer->Add( coordSysStaticText, 0, wxALIGN_LEFT | wxALL, bsize );
   pageSizer->Add( mCoordSysComboBox, 0, wxALIGN_LEFT | wxALL, bsize );
   pageSizer->Add( stateTypeStaticText, 0, wxALIGN_LEFT | wxALL, bsize );
   pageSizer->Add( stateTypeComboBox, 0, wxALIGN_LEFT | wxALL, bsize );
   pageSizer->Add( anomalyStaticText, 0, wxALIGN_LEFT | wxALL, bsize );
   pageSizer->Add( anomalyComboBox, 0, wxALIGN_LEFT | wxALL, bsize );

   // panel that has the labels and text fields for the elements
   // adds default descriptors/labels
   AddElements(this);
   elementSizer->Add(elementsPanel, 0, wxALIGN_CENTER, bsize);

   orbitSizer->Add( pageSizer, 1, wxGROW|wxALIGN_CENTER, bsize );
   orbitSizer->Add( elementSizer, 1, wxGROW|wxALIGN_CENTER, bsize );

   //theSpacecraft->SetDisplay(true);

   this->SetSizer( orbitSizer );
}

//------------------------------------------------------------------------------
// void AddElements(wxWindow *parent)
//------------------------------------------------------------------------------
/**
 * @param <parent> input parent.
 *
 * @note Creates the default objects to put in the element static box
 */
//------------------------------------------------------------------------------
void OrbitPanel::AddElements(wxWindow *parent)
{
    elementsPanel = new wxPanel(parent);
    wxGridSizer *item0 = new wxGridSizer( 1, 0, 0 );

    wxFlexGridSizer *item3 = new wxFlexGridSizer( 6, 3, 0, 0 );
    item3->AddGrowableCol( 0 );
    item3->AddGrowableCol( 1 );
    item3->AddGrowableCol( 2 );

    description1 = new wxStaticText( elementsPanel, ID_TEXT, 
                    wxT("Descriptor1     "), wxDefaultPosition, 
                    wxDefaultSize, 0 );
    item3->Add( description1, 0, wxALIGN_CENTER|wxALL, 5 );
    textCtrl1 = new wxTextCtrl( elementsPanel, ID_TEXTCTRL, 
                    wxT(""), wxDefaultPosition, wxSize(150,-1), 0 );
    item3->Add( textCtrl1, 0, wxALIGN_CENTER|wxALL, 5 );
    label1 = new wxStaticText( elementsPanel, ID_TEXT, wxT("Label1"), 
                    wxDefaultPosition, wxDefaultSize, 0 );
    item3->Add( label1, 0, wxALIGN_CENTER|wxALL, 5 );

    description2 = new wxStaticText( elementsPanel, ID_TEXT, 
                    wxT("Descriptor2    "), wxDefaultPosition, 
                    wxDefaultSize, 0 );
    item3->Add( description2, 0, wxALIGN_CENTER|wxALL, 5 );
    textCtrl2 = new wxTextCtrl( elementsPanel, ID_TEXTCTRL, wxT(""), 
                    wxDefaultPosition, wxSize(150,-1), 0 );
    item3->Add( textCtrl2, 0, wxALIGN_CENTER|wxALL, 5 );
    label2 = new wxStaticText( elementsPanel, ID_TEXT, wxT("Label2"), 
                    wxDefaultPosition, wxDefaultSize, 0 );
    item3->Add( label2, 0, wxALIGN_CENTER|wxALL, 5 );
    
    description3 = new wxStaticText( elementsPanel, ID_TEXT, 
                    wxT("Descriptor3    "), wxDefaultPosition, 
                    wxDefaultSize, 0 );
    item3->Add( description3, 0, wxALIGN_CENTER|wxALL, 5 );
    textCtrl3 = new wxTextCtrl( elementsPanel, ID_TEXTCTRL, wxT(""), 
                   wxDefaultPosition, wxSize(150,-1), 0 );
    item3->Add( textCtrl3, 0, wxALIGN_CENTER|wxALL, 5 );
    label3 = new wxStaticText( elementsPanel, ID_TEXT, wxT("Label3"), 
                   wxDefaultPosition, wxDefaultSize, 0 );
    item3->Add( label3, 0, wxALIGN_CENTER|wxALL, 5 );
    
    description4 = new wxStaticText( elementsPanel, ID_TEXT, 
                   wxT("Descriptor4    "), wxDefaultPosition, 
                   wxDefaultSize, 0 );
    item3->Add( description4, 0, wxALIGN_CENTER|wxALL, 5 );
    textCtrl4 = new wxTextCtrl( elementsPanel, ID_TEXTCTRL, wxT(""), 
                   wxDefaultPosition, wxSize(150,-1), 0 );
    item3->Add( textCtrl4, 0, wxALIGN_CENTER|wxALL, 5 );
    label4 = new wxStaticText( elementsPanel, ID_TEXT, wxT("Label4"), 
                   wxDefaultPosition, wxDefaultSize, 0 );
    item3->Add( label4, 0, wxALIGN_CENTER|wxALL, 5 );
    
    description5 = new wxStaticText( elementsPanel, ID_TEXT, 
                   wxT("Descriptor5    "), wxDefaultPosition, 
                   wxDefaultSize, 0 );
    item3->Add( description5, 0, wxALIGN_CENTER|wxALL, 5 );
    textCtrl5 = new wxTextCtrl( elementsPanel, ID_TEXTCTRL, wxT(""), 
                   wxDefaultPosition, wxSize(150,-1), 0 );
    item3->Add( textCtrl5, 0, wxALIGN_CENTER|wxALL, 5 );
    label5 = new wxStaticText( elementsPanel, ID_TEXT, wxT("Label5"), 
                   wxDefaultPosition, wxDefaultSize, 0 );
    item3->Add( label5, 0, wxALIGN_CENTER|wxALL, 5 );
    
    description6 = new wxStaticText( elementsPanel, ID_TEXT, 
                  wxT("Descriptor6    "), wxDefaultPosition, 
                  wxDefaultSize, 0 );
    item3->Add( description6, 0, wxALIGN_CENTER|wxALL, 5 );
    textCtrl6 = new wxTextCtrl( elementsPanel, ID_TEXTCTRL, wxT(""),
                  wxDefaultPosition, wxSize(150,-1), 0 );
    item3->Add( textCtrl6, 0, wxALIGN_CENTER|wxALL, 5 );
    label6 = new wxStaticText( elementsPanel, ID_TEXT, wxT("Label6"), 
                  wxDefaultPosition, wxDefaultSize, 0 );
    item3->Add( label6, 0, wxALIGN_CENTER|wxALL, 5 );

    item0->Add( item3, 0, wxGROW|wxALL|wxALIGN_CENTER, 5 );

    elementsPanel->SetAutoLayout( TRUE );
    elementsPanel->SetSizer( item0 );

    item0->Fit( elementsPanel );
    item0->SetSizeHints( elementsPanel );
}


//------------------------------------------------------------------------------
// void OnComboBoxChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
/**
 * @note Activates the Apply button when text is changed
 */
//------------------------------------------------------------------------------
void OrbitPanel::OnComboBoxChange(wxCommandEvent& event)
{
   wxString coordSysStr = mCoordSysComboBox->GetStringSelection();
   wxString stateTypeStr = stateTypeComboBox->GetStringSelection();

   #if DEBUG_ORBIT_PANEL
   MessageInterface::ShowMessage
      ("OrbitPanel::OnComboBoxChange() mFromCoordStr=%s, coordSysStr=%s\n"
       "   mFromStateTypeStr=%s, stateTypeStr=%s\n", mFromCoordStr.c_str(),
       coordSysStr.c_str(), mFromStateTypeStr.c_str(), stateTypeStr.c_str());
   #endif
   

   // -------------------- epoch format change --------------------
   if (event.GetEventObject() == epochFormatComboBox)
   {
      std::string toEpochFormat = epochFormatComboBox->GetStringSelection().c_str();    
      std::string epochStr = epochValue->GetValue().c_str();
      theSpacecraft->SetDateFormat(toEpochFormat);
      epochValue->SetValue(theSpacecraft->GetStringParameter("Epoch").c_str());
      fromEpochFormat = toEpochFormat;
   }

   // -------------------- coordinate system or state type change --------------------
   if (event.GetEventObject() == mCoordSysComboBox ||
       event.GetEventObject() == stateTypeComboBox)
   {
      if (event.GetEventObject() == stateTypeComboBox)
         mIsStateTypeChanged = true;
      
      mOutCoord = theGuiInterpreter->
         GetCoordinateSystem(mCoordSysComboBox->GetStringSelection().c_str());
      
      if (mIsEpochChanged)
         UpdateEpoch();
      
      DisplayState();
      
      if (event.GetEventObject() == mCoordSysComboBox)
         mFromCoordStr = mCoordSysComboBox->GetStringSelection().c_str();
      
      if (event.GetEventObject() == stateTypeComboBox)      
         mFromStateTypeStr = stateTypeComboBox->GetStringSelection().c_str();
      
      mFromCoord = mOutCoord;
   }
   
   // -------------------- anomaly type change --------------------
   else if (event.GetEventObject() == anomalyComboBox)
   {
      std::string anomalyType;
    
      wxString description, stateValue;
       
      int anomalySelected = anomalyComboBox->GetSelection();
    
      if (anomalySelected == 0) 
         anomalyType = "MA";
      else if (anomalySelected == 1) 
         anomalyType = "TA";
      else if (anomalySelected == 2)
         anomalyType = "EA";
      
      int anomalyID = theSpacecraft->GetParameterID("AnomalyType");
      
      description.Printf("%s", anomalyType.c_str());
      description6->SetLabel(description);
      
      theSpacecraft->SetStringParameter(anomalyID, anomalyType);
      
      // this->anomaly = theSpacecraft->anomaly;
      this->anomaly = theSpacecraft->GetAnomaly();   // Joey added it
      
      wxString element;
      std::stringstream buffer;
      buffer.precision(18);
      Real an = theSpacecraft->GetRealParameter(anomalyType);
      buffer << an;
      element.Printf ("%s",buffer.str().c_str());
      textCtrl6->SetValue (element);
      buffer.str(std::string());
      
      #if DEBUG_ORBIT_PANEL
         MessageInterface::ShowMessage( "anomaly id = %d\n",anomalyID );
         MessageInterface::ShowMessage( "anomaly type = %s\n",anomalyType.c_str() );
         MessageInterface::ShowMessage( "s/c anomaly type = %s\n",anomaly.GetType().c_str() );
      #endif
   }
   
   if (theApplyButton != NULL)
      theApplyButton->Enable();
}


//------------------------------------------------------------------------------
// void OnTextChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
/**
 * @note Activates the Apply button when text is changed
 */
//------------------------------------------------------------------------------
void OrbitPanel::OnTextChange(wxCommandEvent& event)
{
   wxObject *obj = event.GetEventObject();
   
   if (obj == textCtrl1 || obj == textCtrl2 || obj == textCtrl3 ||
       obj == textCtrl4 || obj == textCtrl5 || obj == textCtrl6 )
   {
      if ( (textCtrl1->IsModified()) || (textCtrl2->IsModified()) || 
           (textCtrl3->IsModified()) || (textCtrl4->IsModified()) ||
           (textCtrl5->IsModified()) || (textCtrl6->IsModified()) )
      {
         mIsStateChanged = true;
      }
   }
   else if (obj == epochValue && epochValue->IsModified())
   {
      mIsEpochChanged = true;
   }
   
   if (theApplyButton != NULL)
      theApplyButton->Enable();
}


//------------------------------------------------------------------------------
// void OrbitPanel::SetLabelsUnits(const std::string &stateType)
//------------------------------------------------------------------------------
/**
 * @param <stateType> input state type for display.
 *
 * @note Sets the labels and units for the state.
 */
//------------------------------------------------------------------------------
void OrbitPanel::SetLabelsUnits(const std::string &stateType)
{
   Integer baseLabel = theSpacecraft->GetParameterID("Element1"),
           baseUnit  = theSpacecraft->GetParameterID("Element1Units");
           
   std::string st = theSpacecraft->GetStringParameter("StateType");
   theSpacecraft->SetStringParameter("StateType", stateType);
   
   description1->SetLabel(theSpacecraft->GetParameterText(baseLabel).c_str());
   label1->SetLabel(theSpacecraft->GetStringParameter(baseUnit).c_str());

   description2->SetLabel(theSpacecraft->GetParameterText(baseLabel+1).c_str());
   label2->SetLabel(theSpacecraft->GetStringParameter(baseUnit+1).c_str());

   description3->SetLabel(theSpacecraft->GetParameterText(baseLabel+2).c_str());
   label3->SetLabel(theSpacecraft->GetStringParameter(baseUnit+2).c_str());

   description4->SetLabel(theSpacecraft->GetParameterText(baseLabel+3).c_str());
   label4->SetLabel(theSpacecraft->GetStringParameter(baseUnit+3).c_str());

   description5->SetLabel(theSpacecraft->GetParameterText(baseLabel+4).c_str());
   label5->SetLabel(theSpacecraft->GetStringParameter(baseUnit+4).c_str());

   description6->SetLabel(theSpacecraft->GetParameterText(baseLabel+5).c_str());
   label6->SetLabel(theSpacecraft->GetStringParameter(baseUnit+5).c_str());

   if ( (strcmp(stateType.c_str(), "Keplerian") == 0) || 
        (strcmp(stateType.c_str(), "ModifiedKeplerian") == 0) )
   {
      int anomalyID = theSpacecraft->GetParameterID("AnomalyType");
      wxString description = (theSpacecraft->GetStringParameter(anomalyID)).c_str();

      // set the labels for the anomaly
      anomalyStaticText->Show(true);
      anomalyComboBox->Show(true);
      
      if (strcmp(description.c_str(), "MA") == 0) 
         anomalyComboBox->SetSelection(0);
      else if (strcmp(description.c_str(), "TA") == 0) 
         anomalyComboBox->SetSelection(1);
      else if (strcmp(description.c_str(), "EA") == 0)
         anomalyComboBox->SetSelection(2);
   }
   else
   {
      anomalyStaticText->Show(false);
      anomalyComboBox->Show(false);
   }

   theSpacecraft->SetStringParameter("StateType", st);
}

//------------------------------------------------------------------------------
// void InitializeCoordinateSystem()
//------------------------------------------------------------------------------
/**
 * @note Activates the Apply button when epoch combobox is changed
 */
//------------------------------------------------------------------------------
void OrbitPanel::InitializeCoordinateSystem(CoordinateSystem *cs)
{
   cs->SetSolarSystem(theSolarSystem); //Assume you have the SolarSystem pointer
   
   SpacePoint *sp;
   std::string spName;

   // Set the Origin for the coordinate system
   spName = cs->GetStringParameter("Origin");
   sp = (SpacePoint*) theGuiInterpreter->GetConfiguredItem(spName);
   
   if (sp == NULL)
      throw GmatBaseException("Cannot find SpacePoint named \"" +
                              spName + "\" used for the coordinate system " +
                              cs->GetName() + " origin");
   
   cs->SetRefObject(sp, Gmat::SPACE_POINT, spName);

   // Set the J2000Body for the coordinate system
   spName = cs->GetStringParameter("J2000Body");
   sp = (SpacePoint*) theGuiInterpreter->GetConfiguredItem(spName);
   
   if (sp == NULL)
      throw GmatBaseException("Cannot find SpacePoint named \"" +
         spName + "\" used for the coordinate system " +
         cs->GetName() + " J2000 body");
   
   cs->SetRefObject(sp, Gmat::SPACE_POINT, spName);
   
   cs->Initialize();
}


//------------------------------------------------------------------------------
// void UpdateEpoch()
//------------------------------------------------------------------------------
void OrbitPanel::UpdateEpoch()
{
//   wxString coordSysStr = mCoordSysComboBox->GetStringSelection().c_str();
//   wxString toEpochFormat = epochFormatComboBox->GetStringSelection();
//   wxString epochStr = epochValue->GetValue();
//   std::string newEpoch = epochStr.c_str();
//   
//   if (epochStr != "TAIModJulian")
//   {
//      // convert to TAIModJulian
//      newEpoch = timeConverter.Convert(epochStr.c_str(), fromEpochFormat,
//                                       "TAIModJulian");
//   }
//   
//   mEpoch = atof(newEpoch.c_str());

   std::string newEpoch = epochValue->GetValue().c_str();
   std::string toEpochFormat = 
      epochFormatComboBox->GetStringSelection().c_str();
      
   if (toEpochFormat != fromEpochFormat)
   {
      theSpacecraft->SetEpoch(newEpoch);
      theSpacecraft->SetStringParameter("DateFormat", toEpochFormat);
      //epochValue->SetValue(theSpacecraft->GetStringParameter("Epoch"));
      epochValue->SetValue(theSpacecraft->GetStringParameter("Epoch").c_str());
      mEpoch = theSpacecraft->GetEpoch();
   }   
   

   Rvector6 outState;
   
   if (mFromStateTypeStr != "Cartesian")
   {
      // convert to Cartesian
      stateConverter.SetMu(mFromCoord);
      outState = stateConverter.Convert(mOutState, mFromStateTypeStr.c_str(),
                                        "Cartesian", anomaly);
      mTempCartState = outState;
   }
   
   if (mFromCoordStr != "EarthMJ2000Eq")
   {
      mCoordConverter.Convert(A1Mjd(mEpoch), mTempCartState, mFromCoord,
                              mTempCartState, mInternalCoord);
      
      Rmatrix33 rmat = mCoordConverter.GetLastRotationMatrix();
      //MessageInterface::ShowMessage
      //   ("===> Spacecraft::UpdateEpoch() rmat=%s\n", rmat.ToString().c_str());
   }
   
   
   #if DEBUG_ORBIT_PANEL
   MessageInterface::ShowMessage
      ("Spacecraft::UpdateEpoch() new mEpoch=%f, mOutState=\n   %s\n"
       "mTempCartState=\n   %s\n", mEpoch, mOutState.ToString().c_str(),
       mTempCartState.ToString().c_str());
   #endif
   
   mIsEpochChanged = false;
   
   // turn off modified flag
   epochValue->DiscardEdits();
}


//------------------------------------------------------------------------------
// void DisplayState()
//------------------------------------------------------------------------------
void OrbitPanel::DisplayState()
{
   std::string coordSysStr = mCoordSysComboBox->GetStringSelection().c_str();
   std::string stateTypeStr = stateTypeComboBox->GetStringSelection().c_str();

   #if DEBUG_ORBIT_PANEL
   MessageInterface::ShowMessage
      ("OrbitPanel::DisplayState() coordSysStr=%s, stateTypeStr=%s, mEpoch=%f\n",
       coordSysStr.c_str(), stateTypeStr.c_str(), mEpoch);
   #endif

   
   #if DEBUG_ORBIT_PANEL_CONVERT
   MessageInterface::ShowMessage
      ("OrbitPanel::DisplayState() --- before conversion, mOutState=\n   %s\n",
       mOutState.ToString().c_str());
   MessageInterface::ShowMessage("===> mIsStateChanged=%d\n", mIsStateChanged);
   #endif
   
   Rvector6 outState;
   
   if (mIsStateChanged)
   {
      Rvector6 inState;
      
      inState[0] = atof(textCtrl1->GetValue());
      inState[1] = atof(textCtrl2->GetValue());
      inState[2] = atof(textCtrl3->GetValue());
      inState[3] = atof(textCtrl4->GetValue());
      inState[4] = atof(textCtrl5->GetValue());
      inState[5] = atof(textCtrl6->GetValue());
      outState = inState;
      
      
      // first convert to desired state type
      stateConverter.SetMu(mOutCoord);
      outState = stateConverter.Convert(inState, mFromStateTypeStr,
                                        stateTypeStr, anomaly);
      #if DEBUG_ORBIT_PANEL_CONVERT
      MessageInterface::ShowMessage("===> First, convert to %s\n", stateTypeStr.c_str());
      MessageInterface::ShowMessage("===> inState=%s\n", inState.ToString().c_str());
      MessageInterface::ShowMessage("===> outState=%s\n", outState.ToString().c_str());
      #endif
      
      // next convert to desired coordinate system
      if (coordSysStr != mFromCoordStr)
      {
         //loj: 11/28/05
         //mCoordConverter.Convert(A1Mjd(mEpoch), outState, mInternalCoord,
         //                        outState, mOutCoord);
         mCoordConverter.Convert(A1Mjd(mEpoch), outState, mFromCoord,
                                 outState, mOutCoord);
         
         #if DEBUG_ORBIT_PANEL_CONVERT
         MessageInterface::ShowMessage("===> Second, convert to %s\n", coordSysStr.c_str());
         MessageInterface::ShowMessage("===> outState=%s\n", outState.ToString().c_str());
         #endif
      }
   }
   else
   {
      if (coordSysStr == "EarthMJ2000Eq" &&
          stateTypeStr == "Cartesian")
      {
         #if DEBUG_ORBIT_PANEL_CONVERT
         MessageInterface::ShowMessage("===> No conversion necessary\n");
         #endif
         
         outState = mTempCartState;
      }
      else if (coordSysStr != "EarthMJ2000Eq" &&
               stateTypeStr == "Cartesian")
      {
         #if DEBUG_ORBIT_PANEL_CONVERT
         MessageInterface::ShowMessage("===> Just convert to %s\n", coordSysStr.c_str());
         #endif
         
         // just convert to desired coordinate system
         mCoordConverter.Convert(A1Mjd(mEpoch), mTempCartState, mInternalCoord,
                                 outState, mOutCoord);
      }
      else if (coordSysStr == "EarthMJ2000Eq" &&
               stateTypeStr != "Cartesian")
      {
         #if DEBUG_ORBIT_PANEL_CONVERT
         MessageInterface::ShowMessage("===> Just convert to %s\n", stateTypeStr.c_str());
         #endif
         
         // just convert to desired state type
         stateConverter.SetMu(mOutCoord);
         outState = stateConverter.Convert(mTempCartState, "Cartesian",
                                           stateTypeStr, anomaly);
      }
      else
      {         
         // first convert to desired coordinate system
         mCoordConverter.Convert(A1Mjd(mEpoch), mTempCartState, mInternalCoord,
                                 outState, mOutCoord);
         
         #if DEBUG_ORBIT_PANEL_CONVERT
         MessageInterface::ShowMessage("===> First, convert to outCS\n");
         MessageInterface::ShowMessage("===> outState=%s\n", outState.ToString().c_str());
         #endif
         
         // next convert to desired state type
         stateConverter.SetMu(mOutCoord);
         outState = stateConverter.Convert(outState, "Cartesian",
                                           stateTypeStr, anomaly);
         #if DEBUG_ORBIT_PANEL_CONVERT
         MessageInterface::ShowMessage("===> Second, convert to %s\n", stateTypeStr.c_str());
         MessageInterface::ShowMessage("===> outState=%s\n", outState.ToString().c_str());
         #endif         
      }
   }

   mOutState = outState;
   
   #if DEBUG_ORBIT_PANEL_CONVERT
   MessageInterface::ShowMessage
      ("OrbitPanel::DisplayState() ---  after converstion, mOutState=\n   %s\n",
       mOutState.ToString().c_str());
   #endif

   textCtrl1->SetValue(ToString(outState[0]));
   textCtrl2->SetValue(ToString(outState[1]));
   textCtrl3->SetValue(ToString(outState[2]));
   textCtrl4->SetValue(ToString(outState[3]));
   textCtrl5->SetValue(ToString(outState[4]));
   textCtrl6->SetValue(ToString(outState[5]));
   
   // labels for elements, anomaly and units
   SetLabelsUnits(stateTypeStr);
}


//------------------------------------------------------------------------------
// wxString ToString(Real rval)
//------------------------------------------------------------------------------
wxString OrbitPanel::ToString(Real rval)
{
   wxString element;
   std::stringstream buffer;
   buffer.precision(18);
   
   buffer << rval;
   element.Printf ("%s",buffer.str().c_str());
   return element;
}


//------------------------------------------------------------------------------
// Rvector6 ConvertState(CoordinateSystem *cs)
//------------------------------------------------------------------------------
Rvector6 OrbitPanel::ConvertState(CoordinateSystem *cs, const Rvector6 &state, 
                                  const std::string &fromElementType, 
                                  const std::string &toElementType)
{
   Rvector6 newState;
   newState.Set(state[0],state[1],state[2],state[3],state[4],state[5]); 
   
   try
   {
      stateConverter.SetMu(cs);
      newState = stateConverter.Convert(state, fromElementType, toElementType, 
                                        anomaly);
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage
         ("OrbitPanel:ConvertState() error occurred!\n%s\n", e.GetMessage().c_str());
   }
   
   return newState;
}
