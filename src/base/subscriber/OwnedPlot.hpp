//$Id$
//------------------------------------------------------------------------------
//                                  OwnedPlot
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel Conway, based on OwnedPlot Subscriber
// Created: 2009/10/01
//
/**
 * Declares OwnedPlot class.
 */
//------------------------------------------------------------------------------
#ifndef OwnedPlot_hpp
#define OwnedPlot_hpp

#include "GmatBase.hpp"
#include "Parameter.hpp"

class OwnedPlot : public GmatBase
{
public:
   OwnedPlot(const std::string &name, Parameter *xParam = NULL,
          Parameter *firstYParam = NULL, const std::string &plotTitle = "",
          const std::string &xAxisTitle = "", const std::string &yAxisTitle = "");
   OwnedPlot(const OwnedPlot &orig);
   OwnedPlot& operator=(const OwnedPlot& orig);
   virtual ~OwnedPlot(void);
   
   // methods inherited from Subscriber
   virtual bool         Initialize();
   
   // methods inherited from GmatBase
   virtual GmatBase*    Clone() const;
   virtual void         Copy(const GmatBase* orig);
   
   virtual bool         SetName(const std::string &who,
                                const std::string &oldName = "");
   
   virtual bool         TakeAction(const std::string &action,  
                                   const std::string &actionData = "");
   
   virtual bool         RenameRefObject(const Gmat::ObjectType type,
                                        const std::string &oldName,
                                        const std::string &newName);
   
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   virtual bool         IsParameterReadOnly(const Integer id) const;
   
   virtual Integer      GetIntegerParameter(const Integer id) const;
   virtual Integer      GetIntegerParameter(const std::string &label) const;
   virtual Integer      SetIntegerParameter(const Integer id,
                                            const Integer value);
   virtual Integer      SetIntegerParameter(const std::string &label,
                                            const Integer value);
   
   virtual bool         GetBooleanParameter(const Integer id) const;
   virtual bool         GetBooleanParameter(const std::string &label) const;
   virtual bool         SetBooleanParameter(const Integer id,
                                            const bool value);
   virtual bool         SetBooleanParameter(const std::string &label,
                                            const bool value);
   
   virtual std::string  GetOnOffParameter(const Integer id) const;
   virtual bool         SetOnOffParameter(const Integer id, 
                                          const std::string &value);
   virtual std::string  GetOnOffParameter(const std::string &label) const;
   virtual bool         SetOnOffParameter(const std::string &label, 
                                          const std::string &value);
   
   virtual std::string  GetStringParameter(const Integer id) const;
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);
   
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value,
                                           const Integer index);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value,
                                           const Integer index);
   
   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const;
   virtual const StringArray&
                        GetStringArrayParameter(const std::string &label) const;
   
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
   
   virtual const ObjectTypeArray&
                        GetRefObjectTypeArray();
   virtual const StringArray&
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   
   bool                 Activate();
   bool                 Deactivate();

   // Methods used to access the plot
   virtual void         SetData(std::vector<RealArray*> &dataBlast);
   virtual void         SetCurveData(const Integer forCurve, RealArray *xData,
                              RealArray *yData);

   virtual bool         MarkPoint(Integer whichOne = -1, Integer forCurve = -1);

   virtual Integer      SetUsedDataID(Integer id, Integer forCurve = -1);
   virtual void         SetUsedObjectID(Integer id);
   virtual Integer      UsesData(Integer id);
   virtual Integer      UsesObject(Integer id);

protected:

   bool SetXParameter(const std::string &paramName);
   bool AddYParameter(const std::string &paramName, Integer index);
   void BuildPlotTitle();
   bool ClearYParameters();
   bool RemoveYParameter(const std::string &name);
   bool ResetYParameters();
   bool PenUp();
   bool PenDown();
   bool RescaleData();
   
   void DeletePlotCurves();
   
   Parameter *mXParam;
   std::vector<Parameter*> mYParams;
   
   Integer mNumXParams;
   Integer mNumYParams;
   
   std::string mXParamName;
   StringArray mYParamNames;
   StringArray mAllParamNames;
   IntegerArray curveDataIDs;
   
   std::string mOldName;
   std::string mPlotTitle;
   std::string mXAxisTitle;
   std::string mYAxisTitle;
   std::string mDrawGrid;
   bool mIsOwnedPlotWindowSet;
   
   Integer mDataCollectFrequency;
   Integer mUpdatePlotFrequency;
   
   Integer mNumDataPoints;
   Integer mNumCollected;
   
   bool useLines;
   Integer lineWidth;
   bool useMarkers;
   Integer markerSize;

   IntegerArray supportedData;
   IntegerArray supportedObjects;

   // Imported Solver pieces
   bool                 active;
   bool                 isEndOfReceive;
   bool                 isEndOfRun;
   bool                 isInitialized;
   std::string          mSolverIterations;
   Gmat::RunState       runstate;

   enum
   {
      IND_VAR = GmatBaseParamCount,
      ADD,
      PLOT_TITLE,
      X_AXIS_TITLE,
      Y_AXIS_TITLE,
      DRAW_GRID,
      DATA_COLLECT_FREQUENCY,
      UPDATE_PLOT_FREQUENCY,
      SHOW_PLOT,
      USE_LINES,
      LINE_WIDTH,
      USE_MARKERS,
      MARKER_SIZE,
      OwnedPlotParamCount
   };
   
   static const Gmat::ParameterType
      PARAMETER_TYPE[OwnedPlotParamCount - GmatBaseParamCount];
   static const std::string
      PARAMETER_TEXT[OwnedPlotParamCount - GmatBaseParamCount];

   // methods inherited from Subscriber
   virtual bool Distribute(Integer len);
   virtual bool Distribute(const Real * dat, Integer len);

};

#endif
