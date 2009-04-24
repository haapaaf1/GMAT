classdef GroundStationRange < GroundStationMeasurement

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
        
        %----- GetState
        function x = GetState(Meas,Id);

            switch Id
                case 401
                    x = [Meas.RangeMeas.Bias]';
                otherwise
                    x = [];
            end

        end % GetState

        %----- SetState
        function Meas = SetState(Meas,Id,x);

            switch Id
                case 401
                    Meas.RangeMeas.Bias = x(1);
                otherwise
                    disp(['State Id ' num2str(x) ' is not a supported set state in GroundStationMeasurement::SetState'])
            end

        end % SetState

        %----- Initialization
        function GSMeas = Initialize(GSMeas,Sandbox)

        end %----- function Intialize
        
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
                
        %----- Evaluate measurements
        function [y,dydx,isFeasible] = Evaluate(Meas,dataIndex,Participants,partialsMap)
            
            %  
            Sat         = Participants{1,1};
            Station     = Participants{1,2};
            dataType    = Meas.dataTypes(dataIndex);
            
            if dataType     == 1001;
                [y,dydx,isFeasible] = Meas.EvaluateRange(Sat,Station);
            elseif dataType == 1002;
                [y,dydx,isFeasible] = Meas.EvaluateRADec(Sat,Station);
            else
                stop
            end
              
        end
        
        %----- Evaluate measurements
        function [y,dydx,isFeasible] = Simulate(Meas,dataType,Participants)
            
            %  
            Sat         = Participants{1,1};
            Station     = Participants{1,2};
            
            if dataType     == 1001;
                [y,dydx,isFeasible] = Meas.EvaluateRange(Sat,Station);
            elseif dataType == 1002;
                [y,dydx,isFeasible] = Meas.EvaluateRADec(Sat,Station);
            else
                stop
            end
              
        end
        
        function [y,dydx,isFeasible] = EvaluateRange(Meas,Sat,Station)

            %  Convert station location to inertial system
            jd         = Sat.Epoch + 2430000;
            stationLoc = Station.InertialState(jd);
            rangevec   = [Sat.X Sat.Y Sat.Z]' - stationLoc;
            range      = norm(rangevec);

            %  Calculate measurement and partial if feasible
           % if dot(rangevec,stationLoc) > 0
                isFeasible = 1;
                y    = range + Meas.RangeMeas.Bias;
                dydx = rangevec'/range;
           % else
           %     isFeasible = 1;
           %     y    = range + Meas.RangeMeas.Bias;
           %     dydx = rangevec'/range;
%          %       isFeasible = 0;
%                 y   = [];
%                 dydx = [];
           % end
            
            return
            Partials = [101 201 301];

            for i = 1:size(Partials,2)

                switch Partials(i)

                    case 101
                        %  Derivative w/r/t spacecraft position
                        dydx.d101 = [rangevec'/range; zeros(3,1)];
                    case 201
                        %  Derivative w/r/t ground station location
                        dydx.d201 = -rangevec'/range;
                    case 301
                        %  Derivative w/r/t measurement bias
                        dydx.d301 = 1;
                end

            end

        end
        

   
        function [y,dydx] = EvaluateRADec(Meas,Sat,Station)

            global eciPos  OWLT  speedoflight

            jd            = Sat.Epoch + 2430000;

            %===========================
            %--- The function values
            %===========================
            rangevec = [Sat.X;Sat.Y;Sat.Z] - Station.InertialState(jd);
            range     = norm(rangevec);

            % compute apparent Declination
            decTop = asin(rangevec(3,1)/range);

            % compute apparent Right Ascension
            foo = sqrt(rangevec(1,1)^2+rangevec(2,1)^2);
            raTop = atan2((rangevec(2,1)/foo),(rangevec(1,1)/foo));
            raTop = mod(raTop,2*pi); % RA is between 0 and 360 degrees

            y = [raTop;decTop];
            
            %=========================
            %--- The partials
            %===========================
            xhat = [1 0 0]';
            yhat = [0 1 0]';
            zhat = [0 0 1]';
            ddecdsatpos = 1/sqrt(1 - (rangevec(3,1)/range)^2)*...
                          (zhat'/range - rangevec(3,1)/range^3*rangevec');
            dradsatpos  = 1/ (1 + (rangevec(2,1)/rangevec(1,1))^2)*...
                            (yhat'/rangevec(1,1) - xhat'*rangevec(2,1)/rangevec(1,1)^2);
            dydx = -[ddecdsatpos;dradsatpos];

            Partials = [101 201 301];

            for i = 1:size(Partials,2)

                switch Partials(i)

                    case 101
                        %  Derivative w/r/t spacecraft position
                        ddecdsatpos = 1/sqrt(1 - (rangevec(3,1)/range)^2)*...
                            (zhat'/range - rangevec(3,1)/range^3*rangevec');
                        dradsatpos  = 1/ (1 + (rangevec(2,1)/rangevec(1,1))^2)*...
                            (yhat'/rangevec(1,1) - xhat'*rangevec(2,1)/rangevec(1,1)^2);
                        dydx.d101 = -[ddecdsatpos;dradsatpos];

                    case 201
                        %  Derivative w/r/t ground station location
                        ddecdsatpos = 1/sqrt(1 - (rangevec(3,1)/range)^2)*...
                            (zhat'/range - rangevec(3,1)/range^3*rangevec');
                        dradsatpos  = 1/ (1 + (rangevec(2,1)/rangevec(1,1))^2)*...
                            (yhat'/rangevec(1,1) - xhat'*rangevec(2,1)/rangevec(1,1)^2);
                        dydx = [ddecdsatpos;dradsatpos];
                        dydx.d201 = - [ddecdsatpos;dradsatpos];;

                    case 301
                        %  Derivative w/r/t measurement bias
                        dydx.d301 = [1;1];
                end

            end

        end

    end % methods

end % classdef