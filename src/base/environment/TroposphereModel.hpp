/* 
 * File:   Troposphere.hpp
 * Author: mwilkins
 *
 * Created on November 20, 2008, 10:34 AM
 */

#ifndef _TROPOSPHERE_HPP
#define	_TROPOSPHERE_HPP

    enum TROPO_MODEL_REPS {
	DEFAULT_ID = 0,
	IFADIS_ID,
	NIELL_ID,
	HOPFIELD_MODIFIED_ID,
	HOPFILED_SIMPLIFIED_ID,
	SAASTOMOINEN_ID,
	DIFFERENTIAL_REFRACTION_ID,
	MARINI_ID,
	EndTropoModelReps
    };

    static const std::string TROPOSPHERE_MODEL_DESCRIPTIONS[EndTropoModelReps];


#endif	/* _TROPOSPHERE_HPP */

