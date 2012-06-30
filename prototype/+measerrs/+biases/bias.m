classdef bias < handle & hgsetget
    % Base Class for Biases Subpackage.
    properties
        stateCovariance;
        stateRealization;
        biasStatePartial;
    end % properties
    properties (SetAccess = protected, GetAccess = protected)
        stm;
        pnoise;
    end % properties (SetAccess = protected, GetAccess = protected)
    methods
        function b = bias(P)
            b.stateCovariance = P;
            b.stateRealization = measerrs.covsmpl(P);
            b.biasStatePartial = 1;
            b.stm = measerrs.biases.stms.constm;
            b.pnoise = measerrs.biases.pnoises.noisefree;
        end % function
        function Phi = stateTransitionMatrix(b,dt)
            Phi = b.stm.stateTransitionMatrix(dt);
        end % function
        function Qd = processNoiseCovariance(b,dt)
            Qd = b.pnoise.processNoiseCovariance(dt);
        end % function
        function wd = processNoiseRealization(b,dt)
            wd = b.pnoise.processNoiseRealization(dt);
        end % function
    end % methods
end % classdef