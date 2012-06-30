classdef randwalk < measerrs.biases.bias
    % Class for random walk measurement biases.
    methods
        function rw = randwalk(P,Q)
            rw = rw@measerrs.biases.bias(P);
            rw.pnoise = measerrs.biases.pnoises.rwalkpn(Q);
        end % function
    end % methods
end % classdef