classdef pnmulti < measerrs.biases.pnoises.pnoise
    % Base class for multi-input process noise models.
    properties
        biaspn;
        driftpn;
    end % properties
    methods
        function pnm = pnmulti(biasobj,driftobj)
            pnm.biaspn = biasobj;
            pnm.driftpn = driftobj;
        end % function
    end % methods
end % classdef