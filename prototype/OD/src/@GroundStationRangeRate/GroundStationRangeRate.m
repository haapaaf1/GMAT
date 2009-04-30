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

            % Will need range as part of the calc
            rangeVec   = [Meas.Spacecraft.X Meas.Spacecraft.Y Meas.Spacecraft.Z]' - stationLoc;
            range      = norm(rangeVec);

            % Get relative velocity of sat w.r.t. GS
            relVel = [Meas.Spacecraft.VX Meas.Spacecraft.VY Meas.Spacecraft.VZ]' - stationVel;
            
            % Calculate the range rate
            rate       = rangevec' * relVel / range;
            y    = rate + Meas.Bias;

            % And the derivatives
            dotprod = rangeVec' * relVel;
            range3 = range^3;
            
            % Note: Derivative w.r.t. sat coordinates.  Derivative w.r.t.
            % groundstation coordinates has opposite sign.
            dydx(1:3) = relVel / range - dotProd * rangeVec / range3;
            dydx(4:6) = rangeVec / range;

        end % Evaluate
        
    end % methods

end % classdef
