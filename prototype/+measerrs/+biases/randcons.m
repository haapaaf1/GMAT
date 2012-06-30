classdef randcons < measerrs.biases.bias
    % Class for measurement biases that are random constants.
    % Note that this class just aliases the base class of the biases
    % subpackage.
    methods
        function rk = randcons(P)
            rk = rk@measerrs.biases.bias(P);
        end % function
    end % methods
end % classdef