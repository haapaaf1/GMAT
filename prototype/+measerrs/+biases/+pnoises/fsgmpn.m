classdef fsgmpn < measerrs.biases.pnoises.pnmulti
    % Class for process noise for Coupled FOGM and SOGM processes.
    methods
        function fspn = fsgmpn(q1,q2,tau,wn,zeta)
            biaspn = measerrs.biases.pnoises.fogmpn(q1,tau);
            driftpn = measerrs.biases.pnoises.sogmpn(q2,wn,zeta);
            fspn = fspn@measerrs.biases.pnoises.pnmulti(biaspn,driftpn);
        end % function
        function Qd = processNoiseCovariance(fspn,dt)
            q1 = fspn.biaspn.processNoiseIntensity;
            tau = fspn.biaspn.timeConstant;
            q2 = fspn.driftpn.processNoiseIntensity;
            wn = fspn.driftpn.naturalFrequency;
            zeta = fspn.driftpn.dampingRatio;
            
            % Taesul's code:
            beta = 1/tau;
            wd = sqrt(wn^2 - wn^2*zeta^2);
            a = -(1/2)*(beta + 2*zeta*wn) ;                a2 = a*a ;
            b = sqrt(wd^2 + beta*zeta*wn - (1/4)*beta^2);  b2 = b*b ;
            c = -beta/2 +zeta*wn ;                         c2 = c*c ;
            f = wn*wn ;    f2 = f*f ;                      ab2 = a2 + b2 ;
            if b2 <= 0
                error('Invalid parameters')
            end
            e2at = exp(2*a*dt);   s2bt = sin(2*b*dt);  c2bt = cos(2*b*dt);
            em1 = (e2at - 1)/(4*a) ;   fac1 = 1/(4*ab2) ;
            
            term1 = fac1*e2at.*s2bt*(b2-c2+2*a*c)/b ;
            term2 = fac1*(e2at.*c2bt-1)*(a*b2-a*c2-2*b2*c)/b2 ;
            term3 = fac1*(e2at.*(b*s2bt + a*c2bt) - a) ;
            
            Qd11q1 = q1*(em1*(1 + c2/b2)+ term1 + term2) ;
            Qd11q2 = (q2/b2)*(em1 - term3) ;
            
            Qd(1,1,:) = Qd11q1 + Qd11q2 ;
            
            term4 = fac1*e2at.*s2bt*(b2-c2-2*a*c)/b  ;
            term5 = fac1*(e2at.*c2bt-1)*(a*b2-a*c2+2*b2*c)/b2 ;
            term6 = fac1*(e2at.*(b*s2bt + a*c2bt) - a) ;
            
            Qd22q2 = q2*(em1*(1+c2/b2) + term4 + term5) ;
            Qd22q1 = (q1/b2)*f2*(em1 - term6) ;
            Qd(2,2,:) = Qd22q2 + Qd22q1 ;
            
            term7 = fac1*(e2at.*((a*b+b*c)*s2bt+(a*c-b2)*c2bt)-(a*c-b2)) ;
            term8 = fac1*(e2at.*((b*c-a*b)*s2bt+(a*c-b2)*c2bt)-(a*c-b2))  ;
            
            Qd12q2 =   (q2/b2)*(-c*em1 + term7) ;
            Qd12q1 = (q1*f/b2)*(-c*em1 + term8) ;
            Qd(1,2,:) = Qd12q2 + Qd12q1 ;
            Qd(2,1,:) = Qd(1,2,:) ;
            
            fspn.processNoiseIntegral = Qd;
            fspn.integrationTime = dt;
        end % function
    end % methods
end % classdef