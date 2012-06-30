classdef pnsingle < measerrs.biases.pnoises.pnoise
    % Base class for single-input process noise models.
    properties
        processNoiseIntensity;
    end % properties
    methods
        function pns = pnsingle(Q)
            pns = pns@measerrs.biases.pnoises.pnoise;
            pns.processNoiseIntensity = Q;
        end % function
    end % methods
end % classdef