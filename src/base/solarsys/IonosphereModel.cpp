#include "IonosphereModel.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string MeasurementModel::IONOSPHERE_MODEL_DESCRIPTIONS[EndIonoModelReps] =
{
    "International Reference Ionosphere 1990 (IRI90)",
    "International Reference Ionosphere 1995 (IRI95)",
    "International Reference Ionosphere 2001 (IRI01)",
    "International Reference Ionosphere 2007 (IRI07)",
    "Parameterized Real-time Ionospheric Specification Model (PRISM)",
    "Ionospheric Forecast Model (IFM)",
    "Coupled Ionosphere-Thermosphere Forecast Model (CITFM)",
    "Sami2 is Another Model of the Ionosphere (SAMI2)",
    "Sami3 is Another Model of the Ionosphere (SAMI3)",
    "Global Theoretical Ionospheric Model (GTIM)",
    "Field Line Interhemispheric Plasma Model (FLIP)",
    "USU model of the global ionosphere (USU)",
    "A Coupled Thermosphere-Ionosphere-Plasmasphere Model (CTIP)",
    "Thermosphere-Ionosphere-Mesosphere-Electrodynamic-General Circulation Model (TIME-GCM)"
};

