classdef GroundStationRange < GroundStationMeasurement

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------
    %----- Set the private properties
    properties  (SetAccess = 'private')
        y          = 0;
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
        
        
        %----- GetPartials
        function [dydx] = GetPartial(Meas,participantIndex,Id)
                         
            %  Convert station location to inertial system
            jd         = Meas.Spacecraft.Epoch + 2430000;
            stationLoc = Meas.GroundStation.InertialState(jd);
            rangevec   = [Meas.Spacecraft.X Meas.Spacecraft.Y Meas.Spacecraft.Z]' - stationLoc;
            range      = norm(rangevec);

            %  Check which participant the parial is associated with.  Then
            %  determine the partial according to state Id.
            dydx = [];
            if participantIndex == 1;      %  The Spacecraft
                if Id == 201;
                    dydx = [rangevec'/range zeros(1,3)];
                end
            elseif participantIndex == 2;  %  The Groundstation
                if Id == 301
                    dydx = -rangevec'/range;
                end
            elseif participantIndex == 3;  %  The measurement object
                if Id == 401
                    dydx = 1;
                end
            end
            
        end % GetPartials
        
    end % methods

end % classdef