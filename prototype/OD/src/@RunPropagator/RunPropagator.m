
%  04/16/09 S. Hughes.  Updated to handle infeasbile measurements.

classdef RunPropagator < handle

    % Set the public properties
    properties  (SetAccess = 'public')

        Propagator

    end

    % Set the methods
    methods
        
        %------------------------------------------------------------------
        %-----  Initialize
        %------------------------------------------------------------------
         function obj = RunPropagator(Propagator)
             
            global theSandbox

            %----- Add the estimator
            obj.Propagator = theSandbox.GetHandle(Propagator);
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
            Propagator.Epoch = Propagator.PSM.Objects{1}.Epoch;

            %Prop.PSV = PSV;
            Propagator.PSV   = Propagator.PSM.GetStates();

        end % Prepare to Propagate

        %------------------------------------------------------------------
        %----- Execute:  Solve the problem.
        %------------------------------------------------------------------

        function Execute(RunSim)

            global theSandbox
            Prop = Prop.SteptoEpoch(Epochs(i));
            %----- Extract attached objects and variables to shorten later code
            Prop        = RunSim.Simulator.Propagator;
            Simulator   = RunSim.Simulator;
            measManager = Simulator.MeasManager;
            Epochs      = Simulator.SimulationEpochs;
            numEpochs   = size(Epochs,1);
            RunSim      = RunSim.PreparetoPropagate();    % Update PSV

           
        end % Execute
    end
end