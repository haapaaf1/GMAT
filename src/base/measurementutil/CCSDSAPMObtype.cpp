#include "CCSDSAPMObtype.hpp"

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
bool CCSDSAPMObtype::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndAPMDataReps)
	return CCSDS_APM_IS_REQUIRED[id];
    else
	return false;
}