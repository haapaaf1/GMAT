/*
 * CGmatIF.h
 *
 * C wrapper interface for the C++ GmatInterface class and supporting classes.
 *
 *
 *
 *  Created on: Jan 20, 2011
 *      Author: abrown  Allen Brown, Emergent Space Technologies, Inc. for
 *		NASA Goddard Space Flight Center under
 *		NASA NASA Prime Contract NNG10CP02C, Task Order 5
 */

#ifndef CGMATIF_H_
#define CGMATIF_H_

#ifdef __cplusplus
extern "C" {
#endif

/* Windows DLL decoration: */
#ifdef _WIN32
	#ifdef _WINDLL
		#define C_EXPORT __declspec (dllexport)
	#endif
#endif
#ifndef C_EXPORT
   #define C_EXPORT
#endif

/*
 * The length of the state vector.
 * TBD: This size is hardcoded to 6 for convenience but will this hold up
 * for actual use?
 */
#define CGMATSTATESZ 6 /* position, velocity */

/*
 * forward declaration of GMAT internal or interface C++ classes,
 * (not used by clients of this interface)
 */
C_EXPORT typedef struct ODEModel ODEModel;
C_EXPORT typedef struct GmatState GmatState;

/*
 * COdeModel - a C "ODEModel" instance.   Clients should not create this struct,
 * let the CBuildODEs() do it for you.  Use deleteCOdeModel(COdeModel*) when done.
 *
 * We needed an opaque holder that hides the C++ classes from the C interface.
 * However, we also needed a place to hold state data for this mockup interface since
 * we don't have a real GMAT behind us.  (GMAT usually holds onto all of its states, but
 * this mockup doesn't have that functionality.)
 *
 * There is probably a better way than this opaque type to keep the C++ classes leaking
 * over into C land yet exposing a handle to an ODEModel.  Perhaps a vector or map
 * with only integer handles are exposed on the C interface?  This can be pursued with
 * the interface work with the real GMAT.
 */
C_EXPORT typedef struct {
	ODEModel* theOdes; /* populated and manipulated by this interface only! */
	GmatState* state; /* do not directly access or manipulate this! */
} COdeModel;

/*
 * Returns a null-terminated C string that describes the last error encountered by
 * any function in this interface.  If no problem was recorded then the returned
 * pointer will be NULL.  Clients should not modify or free this char array.
 */
C_EXPORT const char* getLastMessage();


/*
 * C Interface to:
 * ODEModel* GmatInterface::BuildODEs()
 *
 * Input: COdeModel* ode = the ode model
 * Returns nonzero if there is a failure, zero if success.
 */
C_EXPORT int StartGmat();

C_EXPORT int LoadScript(const char* scriptName);
C_EXPORT int RunScript();
C_EXPORT int LoadAndRunScript(const char* scriptName);
C_EXPORT int FindOdeModel(const char* modelName);
C_EXPORT int GetStateSize();
C_EXPORT const char *GetStateDescription();
C_EXPORT int CountObjects();
C_EXPORT const char *GetObjectName(int which);
C_EXPORT const char *GetRunSummary();
C_EXPORT double *GetState();
C_EXPORT const double *GetDerivativesForState(double epoch, double state[], 
         int stateDim, double dt, int order, int *pdim);
C_EXPORT int SetState(double epoch, double state[], int stateDim);
C_EXPORT const double *GetDerivatives(double dt, int order, int *pdim);


/*
 * C Interface to:
 * void ODEModel::SetState(GmatState *gms)
 *
 * Inputs:
 * 		COdeModel* ode = the ode model
 * 		double epoch = the epoch
 * 		const double state[] = the vehicle state for the ode model
 * 		unsigned int dim = the dimension of the state
 *
 * Returns nonzero if there is a failure, zero if success.
 *
 * NOTE: the expected state dimension is CGMATSTATESZ.
 */
//C_EXPORT unsigned int CSetODEState(COdeModel* ode, double epoch, double state[], int dim);

/*
 * C Interface to query the current state of the COdeModel* ode.
 *
 * Input:
 * 		COdeModel* ode = the ode model
 * Input/Output:
 * 		unsigned int *pdim = the dimensions of state (not NULL), updated by this method
 * Outputs:
 * 		double* the state data (pointer to array of doubles)
 *
 * Returns zero for *pdim and a NULL pointer if there is a failure.
 *
 * NOTE: Clients should not deallocate the returned pointer.
 */
//C_EXPORT const double* CGetODEState(COdeModel* ode, int *pdim);

/*
 * C Interface to:
 * bool ODEModel::GetDerivatives(Real * state, Real dt, Integer order,
 *    const Integer id);
 *
 * Input:
 * 		COdeModel* ode = the ode model
 * 		dt = TODO document this (from GMAT PhysicalModel)
 * 		order = TODO document this (from GMAT PhysicalModel)
 * Input/Output:
 * 		unsigned int *pdim = the dimensions of derivs (not NULL), updated by this method
 * Outputs:
 * 		double* = the derivative data (pointer to array of doubles)
 *
 * Returns zero for *pdim and a NULL pointer if there is a failure.
 *
 * NOTE: Clients should not deallocate the returned pointer memory.
 */
//C_EXPORT const double* CGetDerivatives(COdeModel* ode, double dt, int order, int *pdim);


#ifdef __cplusplus
}
#endif

#endif /* CGMATIF_H_ */
