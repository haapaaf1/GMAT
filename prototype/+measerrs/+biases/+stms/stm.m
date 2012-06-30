classdef stm < handle & hgsetget
    % Base class for state transition matrix subpackage of bias subpackage.
    methods(Abstract)
        Phi = stateTransitionMatrix(obj,dt)
    end % methods(Abstract)
end % classdef