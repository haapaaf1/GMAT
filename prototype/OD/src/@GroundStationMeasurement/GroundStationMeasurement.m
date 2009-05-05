
%  04/16/09 S. Hughes.  Updated to handle infeasbile measurements.

classdef GroundStationMeasurement < Measurement

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------

    %----- Set the private properties
    properties  (SetAccess = 'protected')
        dataTypes
        Spacecraft
        GroundStation
        ObjectDependencies = [];   
        thisObject;                
    end

    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------

    %----- Set the methods
    methods

        function Meas = GroundStationMeasurement(Meas)

        end % GroundStationMeasurement

        function Id = GetParamId(Sat,name);

            switch name
                case 'Bias'
                    Id = 401;
                otherwise
                    Id = '';
            end

        end % GetParamId
        
        %----- GetState
        function x = GetState(Meas,Id);

            switch Id
                case 401
                    x = [Meas.Bias]';
                otherwise
                    x = [];
            end

        end % GetState

        %----- SetState
        function Meas = SetCovariance(Meas,Id,x);

            switch Id
                case 401
                    Meas.BiasCovariance = x;
                otherwise
                    disp(['State Id ' num2str(Id) ' is not a supported set state in GroundStationMeasurement::SetState'])
            end

        end % SetState

        %----- GetState
        function x = GetCovariance(Meas,Id);

            switch Id
                case 401
                    x = [Meas.BiasCovariance]';
                otherwise
                    x = [];
            end

        end % GetState

        %----- SetState
        function Meas = SetState(Meas,Id,x);

            switch Id
                case 401
                    Meas.Bias = x(1);
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
            
        end % GetDynamicsId

        %----- Initialization
        function GSMeas = Initialize(GSMeas,Sandbox)

            %==============================================================
            % -- Read the measurement file 
            % -- Get handles for all participants for each data type.
            % -- Set up the allData structure which contains all data types
            %==============================================================
            
            %----- Read the file:  Currently only support Matlab.mat file with
            %  required data format.  Later include file reader here.
            load(GSMeas.Filename);
                       
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
                Obs(low:high,:)       = MeasData{i}.Obs;
                typeIndex(low:high,1) = ones(numcurrObs,1)*i;
                
                %----- Find the participants and save their handles for later
                numObj       = size(Sandbox.ObjectHandles,2);
                counter      = 1;
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
                
                if strcmp(class(Participant1),'Spacecraft')
                   GSMeas.Spacecraft    = Participant1;
                   GSMeas.GroundStation = Participant2;
                else
                   GSMeas.Spacecraft    = Participant2;
                   GSMeas.GroundStation = Participant1;
                end
                
                totalnumObs = totalnumObs + numcurrObs;

            end %  for i = 1:GSMeas.numDataTypes 
            
            GSMeas.Obs       = Obs;
            GSMeas.Epochs    = Epochs;

        end %----- function Intialize
        
        %----- Initialization
        function GSMeas = InitializeforSimulation(GSMeas,Sandbox)

            %==============================================================
            % -- Get handles for all participants for each data type.
            %==============================================================         
            %----- Loop over all data types to get handles for participants

            %  Fix to loop over the number of data types
            %Fix this loop index KLUDGE
            numDataTypes = size(GSMeas.AddDataType,2);
            GSMeas.thisObject = GSMeas;
            for i = 1:numDataTypes
                
                objNames = GSMeas.AddDataType{i};
                          
                %----- Find the handles to the participants 
                Participant1 = Sandbox.GetHandle(objNames{2});
                Participant2 = Sandbox.GetHandle(objNames{3});
                if  isempty(Participant1) | isempty(Participant2)
                    display('SatId or SensorId from input file not found in List of Objects')
                    stop
                end
                
                if strcmp(class(Participant1),'Spacecraft')
                   GSMeas.Spacecraft    = Participant1;
                   GSMeas.GroundStation = Participant2;
                else
                   GSMeas.Spacecraft    = Participant2;
                   GSMeas.GroundStation = Participant1;
                end
              
            end %  for i = 1:GSMeas.numDataTypes 
            
        end %----- function Intialize  
        
        %  GetParticipant Id
        function Id = SetObjectDependencies(GSMeas,ObjectList)
            
            numObj = size(ObjectList,2);
            GSMeas.ObjectDependencies = zeros(1,numObj);
            for i = 1:numObj 
                Participant = ObjectList{1,i};
                if isequal(Participant,GSMeas.Spacecraft)
                    GSMeas.ObjectDependencies(1,i) = 1;
                elseif isequal(Participant,GSMeas.GroundStation)
                    GSMeas.ObjectDependencies(1,i)= 2;
                elseif isequal(Participant,GSMeas.thisObject)
                    GSMeas.ObjectDependencies(1,i) = 3;
                end
            end

        end  %  Get ParticpantId
        
        function SetThisPointer(GSMeas)
            GSMeas.thisObject = GSMeas;
        end  % SetThisPointer

    end % methods

end % classdef