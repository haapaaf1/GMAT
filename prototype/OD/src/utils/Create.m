function obj = Create(type,name)

global theSandbox

%  seems like there should be a better way to do this.  However, you can
%  only call the class function from within a constructor method.  So you
%  have to find another way to avoid all of this hard coding.

if strcmp(type,'Spacecraft')
    obj = Spacecraft;
elseif strcmp(type,'GroundStationMeasurement')
    obj = GroundStationMeasurement;
elseif strcmp(type,'GroundStationRange')
    obj = GroundStationRange;
elseif strcmp(type,'GroundStationRangeRate')
    obj = GroundStationRangeRate;
elseif strcmp(type,'BatchEstimator')
    obj = BatchEstimator;
elseif strcmp(type,'GroundStation')
    obj = GroundStation;
elseif strcmp(type,'Propagator');
    obj = Propagator;
elseif strcmp(type,'MeasurementSimulator');
    obj = MeasurementSimulator;
elseif strcmp(type,'GroundStationRaDec');
    obj = GroundStationRaDec;  
elseif strcmp(type,'MeasurementModel');
    obj = MeasurementModel;   
elseif strcmp(type,'ObservationDataFile');
    obj = ObservationDataFile;
elseif strcmp(type,'MeasurementGroup');
    obj = MeasurementGroup;
end

theSandbox.AddObject(obj,name);
