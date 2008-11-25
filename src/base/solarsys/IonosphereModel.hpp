/* 
 * File:   IonosphereModel.hpp
 * Author: mwilkins
 *
 * Created on November 20, 2008, 10:33 AM
 */

#ifndef _IONOSPHEREMODEL_HPP
#define	_IONOSPHEREMODEL_HPP

    enum IONO_MODEL_REPS {
	DEFAULT_ID = 0,
	IRI90_ID,
	IRI95_ID,
	IRI01_ID,
	IRI07_ID,
	PRISM_ID,
	IFM_ID,
	CITFM_ID,
	SAMI2_ID,
	SAMI3_ID,
	GTIM_ID,
	FLIP_ID,
	USU_ID,
	CTIP_ID,
	TIMEGCM_ID,
	EndIonoModelReps
    };

       static const std::string IONOSPHERE_MODEL_DESCRIPTIONS[EndIonoModelReps];


#endif	/* _IONOSPHEREMODEL_HPP */

