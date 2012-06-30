classdef sogmpn < measerrs.biases.pnoises.pnsingle
    % Class for process noise for SOGM biases.
    properties
        naturalFrequency;
        dampingRatio;
    end % properties
    methods
        function spn = sogmpn(Q,wn,zeta)
            spn = spn@measerrs.biases.pnoises.pnsingle(Q);
            spn.naturalFrequency = wn;
            spn.dampingRatio = zeta;
        end % function
        function Qd = processNoiseCovariance(spn,dt)
            q = spn.processNoiseIntensity;
            z = spn.dampingRatio;
            wn = spn.naturalFrequency;
            if z == 1
                a = exp(-2*wn*dt);
                Qd(2,2) = q/(4*wn) * (1 - a*(1 - 2*wn*dt + 2*wn^2*dt^2));
                Qd(1,1) = q/(4*wn^3)*(1 - a*(1 + 2*wn*dt + 2*wn^2*dt^2));
                Qd(1,2) = q*dt^2/2*a;
            else
                wd = wn*sqrt(1-z^2);
                b = z*wn;
                a = 1/wd^2*exp(-2*b*dt);
                if z < 1
                    cwt = cos(wd*dt);
                    swt = sin(wd*dt);
                else
                    cwt = cosh(wd*dt);
                    swt = sinh(wd*dt);
                end
                Qd(2,2) = q/(4*b) * ...
                    (1 - a*(wd^2+2*b^2*swt^2 - 2*b*wd*swt*cwt) );
                Qd(1,1) = q/(4*b*wn^2) * ...
                    (1 - a*(wd^2+2*b^2*swt^2 + 2*b*wd*swt*cwt) );
                Qd(1,2) = q/2*a*swt^2;
            end
            Qd(2,1) = Qd(1,2);
            spn.processNoiseIntegral = Qd;
            spn.integrationTime = dt;
        end % function
    end % methods
end % classdef