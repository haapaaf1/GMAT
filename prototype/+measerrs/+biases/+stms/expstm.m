classdef expstm < measerrs.biases.stms.stm
    % Class for biases with exponential STM's.
    properties
        timeConstant;
    end % properties
    methods
        function es = expstm(tau)
            es.timeConstant = tau;
        end % function
        function Phi = stateTransitionMatrix(es,dt)
            Phi = exp(-dt/es.timeConstant);
        end % function
    end % methods
end % classdef