%===== Modification History
% 04/27/09 D. Conway.  Initial Version, based on GroundStationRange

classdef GroundStationRangeRate < GroundStationMeasurement

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------
    %----- Set the private properties
    properties  (SetAccess = 'private')
        partialsMap
        computedMeas
    end

    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------
    %----- Set the methods
    methods
        
        %----- Constructor and copy constructor
        function obj2 = GroundStationRangeRate(obj)

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
                case 'RangeRate'
                    Id = 1003;
%                 case 'STM'
%                     Id = 1002;
                otherwise
                    Id = [];
            end

        end % GetParamId
                
        %----- Evaluate measurements
        function [y,dydx] = Evaluate(Meas)
                         
            %  Convert station location to inertial system
            jd         = Meas.Spacecraft.Epoch + 2430000;
            [stationLoc,stationVel] = Meas.GroundStation.InertialState(jd);
            rangevec   = [Meas.Spacecraft.X Meas.Spacecraft.Y Meas.Spacecraft.Z]' - stationLoc;
            range      = norm(rangevec);

            velocity   = [Meas.Spacecraft.VX Meas.Spacecraft.VY Meas.Spacecraft.VZ]' - stationVel;
            speed      = norm(velocity);
            
            rate       = rangevec' * velocity / range;
            
            y    = rate + Meas.Bias;
            dydx = 0.0; % rangevec'/range;

        end % Evaluate
        
    end % methods

end % classdef
