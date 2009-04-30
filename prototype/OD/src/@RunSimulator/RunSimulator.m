
%  04/16/09 S. Hughes.  Updated to handle infeasbile measurements.

classdef RunSimulator < handle

    % Set the public properties
    properties  (SetAccess = 'public')

        Simulator

    end

    % Set the methods
    methods
        
        %------------------------------------------------------------------
        %-----  Initialize
        %------------------------------------------------------------------
         function obj = RunSimulator(Simulator)
             
            global theSandbox

            %----- Add the estimator
            obj.Simulator = theSandbox.GetHandle(Simulator);
            theSandbox.AddCommand(obj);

        end
        
        %------------------------------------------------------------------
        %-----  Initialize
        %------------------------------------------------------------------

        function obj = Initialize(obj,theSandbox)

            %----- Initialize the estimator
            %obj.Simulator.Initialize(theSandbox);

        end

        %------------------------------------------------------------------
        %----- Prepare to Propagate
        %------------------------------------------------------------------

        function RunSim = PreparetoPropagate(RunSim)

            %---- KLUDGE TO GET EPOCH FOR PROPAGATOR
            RunSim.Simulator.Propagator.Epoch =RunSim.Simulator.Propagator.PSM.Objects{1}.Epoch;

            %Prop.PSV = PSV;
            RunSim.Simulator.Propagator.PSV   = RunSim.Simulator.Propagator.PSM.GetStates();

        end % Prepare to Propagate

        %------------------------------------------------------------------
        %----- Execute:  Solve the problem.
        %------------------------------------------------------------------

        function Execute(RunSim)

            global theSandbox

            %----- Extract attached objects and variables to shorten later code
            Prop        = RunSim.Simulator.Propagator;
            Simulator   = RunSim.Simulator;
            measManager = Simulator.MeasManager;
            Epochs      = Simulator.SimulationEpochs;
            numEpochs   = size(Epochs,1);
            RunSim      = RunSim.PreparetoPropagate();    % Update PSV

            %----- The Simulator Loop
            count = 0;
            for i = 1:numEpochs
                
                %  Prop all participants to next epoch
                Prop = Prop.SteptoEpoch(Epochs(i));
                isFeasible = measManager.measHandles{1}.CheckFeasibility;
                if isFeasible > 0
                   count = count + 1;
                   measVector = measManager.measHandles{1}.Evaluate();
                   feasObs(count,1:measManager.measHandles{1}.lengthMeas) = measVector';
                   feasEpochs(count,1) = Epochs(i);
                end
                
            end
            
            MeasData{1}.Epochs = feasEpochs;
            MeasData{1}.Obs    = feasObs;
            MeasData{1}.DataType = 101;
            MeasData{1}.SatId = 21639;
            MeasData{1}.SensorId = 222;
            save(Simulator.Filename, 'MeasData');
           
        end % Execute
    end
end