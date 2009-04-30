
%  04/16/09 S. Hughes.  Updated to handle infeasbile measurements.

classdef GroundStationMeasurement < Measurement

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------

    %----- Set the public properties
    properties  (SetAccess = 'public')

        RangeMeas
        RangeRateMeas
        AzimuthMeas
        ElevationMeas
        RightAscMeas
        DeclinationMeas
        dataTypeIndeces

    end

    %----- Set the private properties
    properties  (SetAccess = 'private')
        Measurements
        dataTypes
        y           = 0;
        partialsMap = {};
    end

    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------

    %----- Set the methods
    methods

        function Meas = GroundStationMeasurement(Meas)

            % Error Model for RangeRateMeas
            RangeMeas.StochasticModel  = 'WhiteNoise';
            RangeMeas.Bias             = 0;
            RangeMeas.BiasStdDev       = 0;
            RangeMeas.NoiseStdDev      = 0;
            RangeMeas.TimeConstant     = 0;
            Meas.RangeMeas             = RangeMeas;

            % Error Model for RangeRateMeas
            RangeRateMeas.StochasticModel  = 'WhiteNoise';
            RangeRateMeas.Bias             = 0;
            RangeRateMeas.BiasStdDev       = 0.5;
            RangeRateMeas.NoiseStdDev      = 0;
            RangeRateMeas.TimeConstant     = 0;
            Meas.RangeRateMeas             = RangeRateMeas;

            %-----  Use Random Walk Model for Azimuth Measurements
            AzimuthMeas.StochasticModel = 'WhiteNoise';
            AzimuthMeas.Bias            = 0;
            AzimuthMeas.BiasStdDev      = .01;
            AzimuthMeas.NoiseStdDev     = 0;
            AzimuthMeas.TimeConstant    = 0;
            Meas.AzimuthMeas            = AzimuthMeas;

            %-----  Use White Noise Model for Azimuth Measurements
            ElevationMeas.StochasticModel = 'WhiteNoise';
            ElevationMeas.Bias            = 0;
            ElevationMeas.NoiseStdDev     = 0.02;
            ElevationMeas.NoiseStdDev     = 0;
            ElevationMeas.TimeConstant    = 0;
            Meas.ElevationMeas            = ElevationMeas;

            %-----  Use Random Walk Model for Azimuth Measurements
            RightAscMeas.StochasticModel  = 'WhiteNoise';
            RightAscMeas.Bias             = 0;
            RightAscMeas.BiasStdDev       = .01;
            RightAscMeas.NoiseStdDev      = 0;
            RightAscMeas.TimeConstant     = 0;
            Meas.RightAscMeas             = RightAscMeas;

            %-----  Use White Noise Model for Azimuth Measurements
            DeclinationMeas.StochasticModel = 'WhiteNoise';
            DeclinationMeas.Bias            = 0;
            DeclinationMeas.NoiseStdDev     = 0.02;
            DeclinationMeas.NoiseStdDev     = 0;
            DeclinationMeas.TimeConstant    = 0;
            Meas.DeclinationMeas            = DeclinationMeas;

        end % GroundStationMeasurement

        function Id = GetParamId(Sat,name);

            switch name
                case 'RangeMeas.Bias'
                    Id = 401;
                otherwise
                    Id = '';
            end

        end % GetParamId

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
        
        %----- GetSTM
        function STM = GetSTM(Meas,Id);

            switch Id
                case 401
                    STM = 1;
                otherwise
                    STM = [];
            end

        end % GetSTM
        
        %----- Get id for dynamics model
        function Id = GetDynamicsId(Sat,paramId)
            
            switch paramId
                case 401
                    Id = 203;
                otherwise
                    Id = [];
            end
            
        end

        %----- Initialization
        function GSMeas = Initialize(GSMeas,Sandbox)

            %==============================================================
            % -- Read the measurement file 
            % -- Get handles for all participants for each data type.
            % -- Set up the allData structure which contains all data types
            %==============================================================
            
            %----- Read the file:  Currently only support Matlab .mat file with
            %  required data format.  Later include file reader here.
            load(GSMeas.Filename);
            
            %----- KLUDGE WHILE REWORKING THIS COMPONENT
            GSMeas.Measurements.Obs    = MeasData{1}.Obs;
            GSMeas.Measurements.Epochs = MeasData{1}.Epochs;
            
            %----- Loop over all data types to get handles for participants
            %      and to add data for each type to the allData structure.
            GSMeas.numDataTypes = size(MeasData,2);
            totalnumObs         = 1;
            for i = 1:GSMeas.numDataTypes
               
                %----- Concatenate Types, Epochs, Obs
                numcurrObs = size(MeasData{i}.Obs,1);
                low  = totalnumObs;
                high = totalnumObs+numcurrObs-1;
                DataTypes(low:high,1) = ones(numcurrObs,1)*MeasData{i}.DataType;
                Epochs(low:high,1)    = MeasData{i}.Epochs;
                Obs(low:high,1)       = MeasData{i}.Obs;
                typeIndex(low:high,1) = ones(numcurrObs,1)*i;
                
                %----- Find the participants and save their handles for later
                numObj = size(Sandbox.ObjectHandles,2);
                counter = 1;
                Participant1 = {};
                Participant2 = {};
                while counter <= numObj |  ( isempty(Participant1) & isempty(Participant2) )

                    currObj = Sandbox.ObjectHandles{counter};
                    if strcmp(class(currObj),'Spacecraft')
                        if currObj.Id == MeasData{i}.SatId;
                            Participant1 = Sandbox.ObjectHandles{counter};
                        end
                    end

                    if strcmp(class(currObj),'GroundStation')
                        if currObj.Id == MeasData{i}.SensorId;
                            Participant2 = Sandbox.ObjectHandles{counter};
                        end
                    end

                    counter = counter + 1;

                end

                if  isempty(Participant1) | isempty(Participant2)
                    display('SatId or SensorId from input file not found in List of Objects')
                    stop
                end
                
                GSMeas.Participants{i}{1} = Participant1;
                GSMeas.Participants{i}{2} = Participant2;

                GSMeas.dataTypes(i)       = GSMeas.GetDataTypeId(GSMeas.AddDataType{i}{1});
                totalnumObs = totalnumObs + numcurrObs;

            end %  for i = 1:GSMeas.numDataTypes 
            
            GSMeas.Obs       = Obs;
            GSMeas.Epochs    = Epochs;
            GSMeas.dataTypeIndeces = typeIndex;

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
            
%            return
%           OLD CODE THAT WILL BE USEFUL FOR PARTIALS MAP
%             Partials = [101 201 301];
% 
%             for i = 1:size(Partials,2)
% 
%                 switch Partials(i)
% 
%                     case 101
%                         %  Derivative w/r/t spacecraft position
%                         dydx.d101 = [rangevec'/range; zeros(3,1)];
%                     case 201
%                         %  Derivative w/r/t ground station location
%                         dydx.d201 = -rangevec'/range;
%                     case 301
%                         %  Derivative w/r/t measurement bias
%                         dydx.d301 = 1;
%                 end
% 
%             end

        end

    end % methods

end % classdef