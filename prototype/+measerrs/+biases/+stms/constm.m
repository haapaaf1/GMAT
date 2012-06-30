classdef constm < measerrs.biases.stms.stm
    % Class for identity state transition matrices.
    methods
        function Phi = stateTransitionMatrix(~,~)
            Phi = 1;
        end % function
    end % methods
end % classdef