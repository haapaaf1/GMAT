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
        lengthMeas = 1;
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
                
        %----- CheckFeasiblity
        function [isFeasible] = CheckFeasibility(Meas)
                         
            %  Convert station location to inertial system
            jd         = Meas.Spacecraft.Epoch + 2430000;
            stationLoc = Meas.GroundStation.InertialState(jd);
            rangevec   = [Meas.Spacecraft.X Meas.Spacecraft.Y Meas.Spacecraft.Z]' - stationLoc;
            isFeasible = dot(rangevec,stationLoc)/norm(rangevec)/norm(stationLoc);

        end % CheckFeasiblity
                
        %----- Evaluate measurements
        function [y] = Evaluate(Meas)
                         
            %  Convert station location to inertial system
            jd         = Meas.Spacecraft.Epoch + 2430000;
            [stationLoc,stationVel] = Meas.GroundStation.InertialState(jd);

            % Will need range as part of the calc
            rangeVec   = [Meas.Spacecraft.X Meas.Spacecraft.Y Meas.Spacecraft.Z]' - stationLoc;
            range      = norm(rangeVec);

            % Get relative velocity of sat w.r.t. GS
            relVel = [Meas.Spacecraft.VX Meas.Spacecraft.VY Meas.Spacecraft.VZ]' - stationVel;
            
            % Calculate the range rate
            rate       = rangeVec' * relVel / range;
            y    = rate + Meas.Bias;

        end % Evaluate
        
        %----- GetPartials
        function [dydx] = GetPartial(Meas,participantIndex,Id)
                         
            %  Convert station location to inertial system
            jd         = Meas.Spacecraft.Epoch + 2430000;
            [stationLoc,stationVel] = Meas.GroundStation.InertialState(jd);

            % Will need range as part of the calc
            rangeVec   = [Meas.Spacecraft.X Meas.Spacecraft.Y Meas.Spacecraft.Z]' - stationLoc;
            range      = norm(rangeVec);

            % Get relative velocity of sat w.r.t. GS
            relVel = [Meas.Spacecraft.VX Meas.Spacecraft.VY Meas.Spacecraft.VZ]' - stationVel;
            
            % Calculate the range rate
            rate   = rangeVec' * relVel / range;


            % And the derivatives
            dotProd = rangeVec' * relVel;
            range3  = range^3;
            
            % Note: Derivative w.r.t. sat coordinates.  Derivative w.r.t.
            % groundstation coordinates has opposite sign.

            %  Check which participant the parial is associated with.  Then
            %  determine the partial according to state Id.
            dydx = [];
            if participantIndex == 1;      %  The Spacecraft
                if Id == 201;
                   dydx(1,1:3) = (relVel / range - dotProd * rangeVec / range3)';
                   dydx(1,4:6) = rangeVec' / range;
                end
            elseif participantIndex == 2;  %  The Groundstation
                if Id == 301
                   dydx(1,1:3) = -(relVel / range - dotProd * rangeVec / range3)';
                end
            elseif participantIndex == 3;  %  The measurement object
                if Id == 401
                    dydx = 1;
                end
            end
            
        end % GetPartials
        
    end % methods

end % classdef
