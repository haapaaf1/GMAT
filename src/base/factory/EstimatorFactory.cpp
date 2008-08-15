//$Header$
//------------------------------------------------------------------------------
//                            EstimatorFactory
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/20
//
/**
 *  Implementation code for the EstimatorFactory class, responsible 
 *  for creating estimator objects.
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "EstimatorFactory.hpp"
#include "MessageInterface.hpp"  // temporary

// Here are the supported leaf classes
#include "Estimator.hpp"
#include "BatchLeastSquares.hpp"

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  Estimator* CreateEstimator(const std::string &ofType, const std::string &withName)
//------------------------------------------------------------------------------
/**
 * This method creates and returns an object of the requested Estimator class. 
 *
 * @param <ofType> type of Estimator object to create and return.
 * @param <withName> the name for the newly-created Estimator object.
 * 
 * @return A pointer to the created object.
 */
//------------------------------------------------------------------------------
Estimator* EstimatorFactory::CreateEstimator(const std::string &ofType,
                                    const std::string &withName)
{
MessageInterface::ShowMessage("EstimatorFactory is creating a %s named %s\n",
      ofType.c_str(), withName.c_str());
   if (ofType == "BatchLeastSquares")
      return new BatchLeastSquares(withName);
   //else if (ofType == "SequentialLeastSquares")
   //   return new SequentialLeastSquares(withName);
   //else if (ofType == "SimpleLinearKalmanFilter")
   //   return new SimpleLinearKalmanFilter(withName);
   //else if (ofType == "LinearKalmanFilter")
   //   return new LinearKalmanFilter(withName);
   //else if (ofType == "ExtendedKalmanFilter")
   //   return new ExtendedKalmanFilter(withName);
   //else if (ofType == "UnscentedKalmanFilter")
   //   return new UnscentedKalmanFilter(withName);
   //else if (ofType == "ExtendedConsiderFilter")
   //   return new ExtendedConsiderFilter(withName);
   //else if (ofType == "UnscentedConsiderFilter")
   //   return new UnscentedConsiderFilter(withName);
   //else if (ofType == "ExtendedNeglectFilter")
   //   return new ExtendedNeglectFilter(withName);
   //else if (ofType == "UnscentedNeglectFilter")
   //   return new UnscentedNeglectFilter(withName);
   //else if (ofType == "CentralDifferenceKalmanFilter")
   //   return new CentralDifferenceKalmanFilter(withName);
   //else if (ofType == "SquareRootInformationFilter")
   //   return new SquareRootInformationFilter(withName);

   //   #if defined __USE_MATLAB__
   //   if (ofType == "FminconOptimizer")
   //      return new FminconOptimizer(withName);
   //   #endif


   return NULL;
}


//------------------------------------------------------------------------------
//  EstimatorFactory()
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class EstimatorFactory. 
 * (default constructor)
 */
//------------------------------------------------------------------------------
EstimatorFactory::EstimatorFactory() :
   Factory     (Gmat::SOLVER)
//   Factory     (Gmat::ESTIMATOR)
{
   if (creatables.empty())
   {

      creatables.push_back("BatchLeastSquares");
      //creatables.push_back("SequentialLeastSquares");
      //creatables.push_back("SimpleLinearKalmanFilter");
      //creatables.push_back("LinearKalmanFilter");
      //creatables.push_back("ExtendedKalmanFilter");
      //creatables.push_back("UnscentedKalmanFilter");
      //creatables.push_back("ExtendedConsiderFilter");
      //creatables.push_back("UnscentedConsiderFilter");
      //creatables.push_back("ExtendedNeglectFilter");
      //creatables.push_back("UnscentedNeglectFilter");
      //creatables.push_back("CentralDifferenceKalmanFilter");
      //creatables.push_back("SquareRootInformationFilter");
      
      //#if defined __USE_MATLAB__
      //creatables.push_back("FminconOptimizer");
      //#endif
      
   }
}

//------------------------------------------------------------------------------
//  EstimatorFactory(StringArray createList)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class EstimatorFactory.
 *
 * @param <createList> list of creatable estimator objects
 *
 */
//------------------------------------------------------------------------------
EstimatorFactory::EstimatorFactory(StringArray createList) :
Factory(createList, Gmat::ESTIMATOR)
{
}


//------------------------------------------------------------------------------
//  EstimatorFactory(const EstimatorFactory& fact)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class EstimatorFactory.  (copy constructor)
 *
 * @param <fact> the factory object to copy to "this" factory.
 */
//------------------------------------------------------------------------------
EstimatorFactory::EstimatorFactory(const EstimatorFactory& fact) :
    Factory     (fact)
{
   if (creatables.empty())
   {
      creatables.push_back("BatchLeastSquares");
      //creatables.push_back("SequentialLeastSquares");
      //creatables.push_back("SimpleLinearKalmanFilter");
      //creatables.push_back("LinearKalmanFilter");
      //creatables.push_back("ExtendedKalmanFilter");
      //creatables.push_back("UnscentedKalmanFilter");
      //creatables.push_back("ExtendedConsiderFilter");
      //creatables.push_back("UnscentedConsiderFilter");
      //creatables.push_back("ExtendedNeglectFilter");
      //creatables.push_back("UnscentedNeglectFilter");
      //creatables.push_back("CentralDifferenceKalmanFilter");
      //creatables.push_back("SquareRootInformationFilter");
      
      //#if defined __USE_MATLAB__
      //creatables.push_back("FminconOptimizer");
      //#endif
      

   }
}


//------------------------------------------------------------------------------
//  CommandFactory& operator= (const CommandFactory& fact)
//------------------------------------------------------------------------------
/**
 * EstimatorFactory operator for the EstimatorFactory base class.
 *
 * @param <fact> the EstimatorFactory object that is copied.
 *
 * @return "this" EstimatorFactory with data set to match the input factory (fact).
 */
//------------------------------------------------------------------------------
EstimatorFactory& EstimatorFactory::operator=(const EstimatorFactory& fact)
{
   Factory::operator=(fact);
   return *this;
}
    

//------------------------------------------------------------------------------
// ~EstimatorFactory()
//------------------------------------------------------------------------------
/**
 * Destructor for the EstimatorFactory base class.
 */
//------------------------------------------------------------------------------
EstimatorFactory::~EstimatorFactory()
{
}

//---------------------------------
//  protected methods
//---------------------------------

//---------------------------------
//  private methods
//---------------------------------

