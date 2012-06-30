classdef rampstm < measerrs.biases.stms.constm
    % Class for STM's of biases with rectilinear drifts.
    methods
        function Phi = stateTransitionMatrix(~,dt)
            Phi(2,2) = stateTransitionMatrix@measerrs.biases.stms.constm;
            Phi(1,1:2) = [1 dt];
        end % function
    end % methods
end % classdef