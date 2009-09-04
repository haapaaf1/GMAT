#include "CCSDSTDMObtype.hpp"


//---------------------------------------------------------------------------
//  bool IsParameterRequired(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is required by the data format.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSTDMObtype::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndTDMDataReps)
	return CCSDS_TDM_IS_REQUIRED[id];
    else
	return false;
}