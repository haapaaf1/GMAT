classdef GroundStationRaDec < GroundStationMeasurement

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------
    %----- Set the private properties
    properties  (SetAccess = 'private')
        partialsMap
        computedMeas
        lengthMeas = 2
    end

    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------
    %----- Set the methods
    methods
        
        %----- Constructor and copy constructor
        function obj2 = GroundStationRaDec(obj)

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
                
        %----- Evaluate measurement
        function [y,dydx] = Evaluate(Meas)
                         
            %  Convert station location to inertial system
            jd         = Meas.Spacecraft.Epoch + 2430000;
            stationLoc = Meas.GroundStation.InertialState(jd);
            satloc        = [Meas.Spacecraft.X Meas.Spacecraft.Y Meas.Spacecraft.Z]';
            
            % compute apparent measurements:  Ra and Dec
            rangeApparent = satloc - stationLoc;
            range_mag     = norm(rangeApparent);
            decTop = asin(rangeApparent(3,1)/range_mag);
            denom  = sqrt(rangeApparent(1,1)^2+rangeApparent(2,1)^2);
            raTop  = atan2((rangeApparent(2,1)/denom),(rangeApparent(1,1)/denom));
            raTop  = mod(raTop,2*pi); % RA is between 0 and 360 degrees
            y      = [raTop;decTop];

            % compute apparent measurement partials
            xhat = [1 0 0]';
            yhat = [0 1 0]';
            zhat = [0 0 1]';
            ddecdsatpos = 1/sqrt(1 - (rangeApparent(3,1)/range_mag)^2)*...
                         (zhat'/range_mag - rangeApparent(3,1)/range_mag^3*rangeApparent');
            dradsatpos  = 1/ (1 + (rangeApparent(2,1)/rangeApparent(1,1))^2)*...
                         (yhat'/rangeApparent(1,1) - xhat'*rangeApparent(2,1)/rangeApparent(1,1)^2);
            dydx = [dradsatpos;ddecdsatpos];

        end % Evaluate

    end % methods

end % classdef