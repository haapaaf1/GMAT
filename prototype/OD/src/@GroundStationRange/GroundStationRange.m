classdef GroundStationRange < GroundStationMeasurement

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------
    %----- Set the private properties
    properties  (SetAccess = 'private')
        partialsMap
        computedMeas
        lengthMeas = 1;
    end

    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------
    %----- Set the methods
    methods
        
        %----- Constructor and copy constructor
        function obj2 = GroundStationRange(obj)

            if nargin ~= 0
                fns = fieldnames(obj);
                for i = 1:length(fns)
                    obj2.(fns{i}) = obj.(fns{i});
                end
            end

        end % Spacecraft
             
        %----- Get the data type Id, given the string representation.
        function Id = GetDataTypeId(Sat,name);
           
            switch name
                case 'Range'
                    Id = 1001;
                case 'STM'
                    Id = 1002;
                otherwise
                    Id = [];
            end

        end % GetParamId
        
        %----- CheckFeasiblity
        function [isFeasible] = CheckFeasibility(Meas)
                         
            %  Convert station location to inertial system
            jd         = Meas.Spacecraft.Epoch + 2430000;
            stationLoc = Meas.GroundStation.InertialState(jd);
            rangevec   = [Meas.Spacecraft.X Meas.Spacecraft.Y Meas.Spacecraft.Z]' - stationLoc;
            isFeasible = dot(rangevec,stationLoc)/norm(rangevec)/norm(stationLoc);

        end % CheckFeasiblity
                
        %----- Evaluate measurements
        function [y,dydx] = Evaluate(Meas)
                         
            %  Convert station location to inertial system
            jd         = Meas.Spacecraft.Epoch + 2430000;
            stationLoc = Meas.GroundStation.InertialState(jd);
            rangevec   = [Meas.Spacecraft.X Meas.Spacecraft.Y Meas.Spacecraft.Z]' - stationLoc;
            range      = norm(rangevec);
            y          = range + Meas.Bias;
            dydx       = rangevec'/range;

        end % Evaluate
        
    end % methods

end % classdef