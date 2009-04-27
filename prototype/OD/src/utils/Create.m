function obj = Create(type,name)

global theSandbox

if strcmp(type,'Spacecraft')
    obj = Spacecraft;
elseif strcmp(type,'GroundStationMeasurement')
    obj = GroundStationMeasurement;
elseif strcmp(type,'GroundStationRange')
    obj = GroundStationRange;
elseif strcmp(type,'BatchEstimator')
    obj = BatchEstimator;
elseif strcmp(type,'GroundStation')
    obj = GroundStation;
elseif strcmp(type,'Propagator');
    obj = Propagator;
end

theSandbox.AddObject(obj,name);
