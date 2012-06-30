classdef rwrrpn < measerrs.biases.pnoises.pnmulti
    % Class for process noise for coupled random walk & random run processes.
    methods
        function rwrpn = rwrrpn(q1,q2)
            biaspn = measerrs.biases.pnoises.rwalkpn(q1);
            driftpn = measerrs.biases.pnoises.rrunpn(q2);
            rwrpn = rwrpn@measerrs.biases.pnoises.pnmulti(biaspn,driftpn);
        end % function
        function Qd = processNoiseCovariance(rwrpn,dt)
            Qd1 = rwrpn.biaspn.processNoiseCovariance(dt);
            Qd2 = rwrpn.driftpn.processNoiseCovariance(dt);
            Qd = Qd2;
            Qd(1,1) = Qd(1,1) + Qd1;
        end % function
    end % methods
end % classdef