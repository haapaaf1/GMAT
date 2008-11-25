C IRIF12.FOR ------------------------------------- OCTOBER 1991    
C**************************************************************  
c changes from IRIFU9 to IRIF10:
c       SOCO for solar zenith angle 
c	ACOS and ASIN argument forced to be within -1 / +1
c	EPSTEIN functions corrected for large arguments
C**************************************************************  
c changes from IRIF10 to IRIF11: 
c	LAY subroutines introduced
c       TEBA corrected for 1400 km
C**************************************************************  
c changes from IRIF11 to IRIF12:
C       Neutral temperature subroutines now in CIRA86.FOR 
C       TEDER changed
C       All names with 6 or more characters replaced 
C	10/29/91 XEN: 10^ in loop, instead of at the end
C
C**************************************************************  
C********** INTERNATIONAL REFERENCE IONOSPHERE ****************  
C**************************************************************  
C****************  FUNCTIONS,SUBROUTINES  *********************
C**************************************************************
C** NE: 	XE1,DXE1N,XE2,XE3,XE4,XE5,XE6,XE
C** TE/TI: 	TEBA,SPHARM,ELTE,TEDE,TI,TEDER
C** NI:		RPID,RDHHE,RDNO,KOEFP1,KOEFP2,KOEFP3,SUFE
C** PEAKS:	F2OUT,HMF2ED,FOF1ED,FOEEDI,XMDED,GAMMA1
C** MAG. FIELD: GGM,FIELDG
C** FUNCTIONS: 	REGFA1,TAL
C** TIME:	SOCO,HPOL,MODA
C** INTERPOL.:	B0POL
C** EPSTEIN:	RLAY,D1LAY,D2LAY,EPTR,EPST,EPSTEP,EPLA
C** LAY:	XE2TO5,XEN,ROGUL,VALGUL,LNGLSN,LSKNM,INILAY
C** NI-new:	IONCOM, RPDA
C**************************************************************  
C  
C**************************************************************  
C***  -------------------ADDRESSES------------------------  ***
C***  I  PROF. K. RAWER             DR. D. BILITZA       I  ***
C***  I  HERRENSTR. 43              GSFC CODE 933        I  ***
C***  I  7801 MARCH 1               GREENBELT MD 20771   I  ***
C***  I  F.R.G.                     USA                  I  ***
C***  ----------------------------------------------------  ***
C**************************************************************  
C**************************************************************  
C        
C*************************************************************   
C*************** ELECTRON DENSITY ****************************   
C*************************************************************   
C
C
      FUNCTION XE1(H)    
c----------------------------------------------------------------
C REPRESENTING ELECTRON DENSITY(M-3) IN THE TOPSIDE IONOSPHERE   
C (H=HMF2....1000 KM) BY HARMONIZED BENT-MODEL ADMITTING 
C VARIABILITY OFGLOBAL PARAMETER ETA,ZETA,BETA,DELTA WITH        
C GEOM. LATITUDE, SMOOTHED SOLAR FLUX AND CRITICAL FREQUENCY     
C (SEE MAIN PROGRAM).    
C [REF.:K.RAWER,S.RAMAKRISHNAN,1978]     
c----------------------------------------------------------------
      COMMON	/BLOCK1/	HMF2,XNMF2,HMF1
     &		/BLO10/		BETA,ETA,DELTA,ZETA
     &		/ARGEXP/	ARGMAX
                    
      DXDH = (1000.-HMF2)/700.
	x0 = 300. - delta
      xmx0 = (H-HMF2)/DXDH
         x = xmx0 + x0
	eptr1 = eptr(x,beta,394.5) - eptr(x0,beta,394.5)
	eptr2 = eptr(x,100.,300.0) - eptr(x0,100.,300.0) 

      y = BETA * ETA * eptr1 + ZETA * (100. * eptr2 - xmx0)

 	Y = y * dxdh
	if(abs(Y).gt.argmax) Y = sign(argmax,Y)
      XE1 = XNMF2 * EXP(-Y)                             
      RETURN          
      END             
C 
C
      FUNCTION DXE1N(H)                            
C LOGARITHMIC DERIVATIVE OF FUNCTION XE1 (KM-1).   
      COMMON	/BLOCK1/	HMF2,XNMF2,HMF1
     &		/BLO10/		BETA,ETA,DELTA,ZETA                    

	x0 = 300. - delta
      X=(H-HMF2)/(1000.0-HMF2)*700.0 + x0
	epst2 = epst(x,100.0,300.0)
	epst1 = epst(x,beta ,394.5)
      DXE1N = - ETA * epst1 + ZETA * (1. - epst2)             
      RETURN          
      END             
C
C
      REAL FUNCTION XE2(H)                         
C ELECTRON DENSITY FOR THE BOTTOMSIDE F-REGION (HMF1...HMF2).                   
      COMMON	/BLOCK1/HMF2,XNMF2,HMF1
     &		/BLOCK2/B0,B1,C1	/ARGEXP/ARGMAX
      X=(HMF2-H)/B0
	z=x**b1
	if(z.gt.argmax) z=argmax
      XE2=XNMF2*EXP(-z)/COSH(X)                 
      RETURN          
      END             
C
C
      REAL FUNCTION XE3(H)                         
C ELECTRON DENSITY FOR THE F1-LAYER (HZ.....HMF1). 
      COMMON	/BLOCK1/	HMF2,XNMF2,HMF1
     &		/BLOCK2/	B0,B1,C1
      XE3=XE2(H)+XNMF2*C1*SQRT(ABS(HMF1-H)/B0)      
      RETURN          
      END             
C
C
      REAL FUNCTION XE4(H)                         
C ELECTRON DENSITY FOR THE INDERMEDIUM REGION (HEF..HZ).                        
      COMMON	/BLOCK3/	HZ,T,HST,STR
     &		/BLOCK4/	HME,XNME,HEF
      IF(HST.LT.0.) GOTO 100                       
      XE4=XE3(HZ+T/2.0-SIGN(1.0,T)*SQRT(T*(HZ-H+T/4.0)))                        
      RETURN          
100   XE4=XNME+T*(H-HEF)                            
      RETURN          
      END             
C
C
      REAL FUNCTION XE5(H)                         
C ELECTRON DENSITY FOR THE E AND VALLEY REGION (HME..HEF).   
      LOGICAL NIGHT   
      COMMON	/BLOCK4/	HME,XNME,HEF
     &  	/BLOCK5/	NIGHT,E(4)                    
      T3=H-HME        
      T1=T3*T3*(E(1)+T3*(E(2)+T3*(E(3)+T3*E(4))))  
      IF(NIGHT) GOTO 100                           
      XE5=XNME*(1+T1)  
      RETURN          
100   XE5=XNME*EXP(T1)                              
      RETURN          
      END             
C
C
      REAL FUNCTION XE6(H)                         
C ELECTRON DENSITY FOR THE D REGION (HA...HME).    
      COMMON	/BLOCK4/	HME,XNME,HEF
     &		/BLOCK6/	HMD,XNMD,HDX
     &		/BLOCK7/	D1,XKK,FP30,FP3U,FP1,FP2    
      IF(H.GT.HDX) GOTO 100                        
      Z=H-HMD         
      FP3=FP3U        
      IF(Z.GT.0.0) FP3=FP30                        
      XE6=XNMD*EXP(Z*(FP1+Z*(FP2+Z*FP3)))           
      RETURN          
100   Z=HME-H         
      XE6=XNME*EXP(-D1*Z**XKK)
      RETURN          
      END             
C
C
      REAL FUNCTION XE(H)                          
C ELECTRON DENSITY BEETWEEN HA(KM) AND 1000 KM     
C SUMMARIZING PROCEDURES  NE1....6;                
      COMMON	/BLOCK1/HMF2,XNMF2,HMF1		
     &		/BLOCK3/HZ,T,HST,STR
     &  	/BLOCK4/HME,XNME,HEF
      IF(H.LT.HMF2) GOTO 100                       
      XE=XE1(H)     
      RETURN          
100   IF(H.LT.HMF1) GOTO 300                       
      XE=XE2(H)       
      RETURN          
300   IF(H.LT.HZ) GOTO 400                         
      XE=XE3(H)       
      RETURN          
400   IF(H.LT.HEF) GOTO 500                        
      XE=XE4(H)       
      RETURN          
500   IF(H.LT.HME) GOTO 600                        
      XE=XE5(H)       
      RETURN          
600   XE=XE6(H)       
      RETURN          
      END             
C                     
C**********************************************************                     
C***************** ELECTRON TEMPERATURE ********************                    
C**********************************************************                     
C
      SUBROUTINE TEBA(DIPL,SLT,NS,TE) 
C CALCULATES ELECTRON TEMPERATURES TE(1) TO TE(4) AT ALTITUDES
C 300, 400, 1400 AND 3000 KM FOR DIP-LATITUDE DIPL/DEG AND 
C LOCAL SOLAR TIME SLT/H USING THE BRACE-THEIS-MODELS (J. ATMOS.
C TERR. PHYS. 43, 1317, 1981); NS IS SEASON IN NORTHERN
C HEMISOHERE: IS=1 SPRING, IS=2 SUMMER ....
C ALSO CALCULATED ARE THE TEMPERATURES AT 400 KM ALTITUDE FOR
C MIDNIGHT (TE(5)) AND NOON (TE(6)).   
      DIMENSION C(4,2,81),A(82),TE(6)
      COMMON/CONST/UMR/KRADAT/ C
	IF(NS.LT.3) THEN
	   IS=NS
	ELSE IF(NS.GT.3) THEN
	   IS=2
	   DIPL=-DIPL
	ELSE
	   IS=1
	ENDIF
      COLAT=UMR*(90.-DIPL)                    
      AZ=.2618*SLT    
      CALL SPHARM(A,8,8,COLAT,AZ)
	IF(IS.EQ.2) THEN
	   KEND=3
	ELSE
	   KEND=4
	ENDIF                  
      DO 2 K=1,KEND      
      STE=0.          
      DO 1 I=1,81     
1       STE=STE+A(I)*C(K,IS,I)                       
2     TE(K)=10.**STE
	IF(IS.EQ.2) THEN
	   DIPL=-DIPL
	   COLAT=UMR*(90.-DIPL)                    
           CALL SPHARM(A,8,8,COLAT,AZ)
           STE=0.          
           DO 11 I=1,81     
11            STE=STE+A(I)*C(4,2,I)                       
           TE(4)=10.**STE
    	ENDIF

C---------- TEMPERATURE AT 400KM AT MIDNIGHT AND NOON
      DO 4 J=1,2      
        STE=0.          
        AZ=.2618*(J-1)*12.                           
        CALL SPHARM(A,8,8,COLAT,AZ)                  
        DO 3 I=1,81     
3         STE=STE+A(I)*C(2,IS,I)                       
4       TE(J+4)=10.**STE                             
      RETURN          
      END             
C
      SUBROUTINE SPHARM(C,L,M,COLAT,AZ)            
C CALCULATES THE COEFFICIENTS OF THE SPHERICAL HARMONIC                         
C EXPANSION THAT WAS USED FOR THE BRACE-THEIS-MODELS.                           
      DIMENSION C(82)                              
      C(1)=1.         
      K=2             
      X=COS(COLAT)    
      C(K)=X          
      K=K+1           
      DO 10 I=2,L     
      C(K)=((2*I-1)*X*C(K-1)-(I-1)*C(K-2))/I       
10    K=K+1           
      Y=SIN(COLAT)    
      DO 20 MT=1,M    
      CAZ=COS(MT*AZ)  
      SAZ=SIN(MT*AZ)  
      C(K)=Y**MT      
      K=K+1           
      IF(MT.EQ.L) GOTO 16                          
      C(K)=C(K-1)*X*(2*MT+1)                       
      K=K+1           
      IF((MT+1).EQ.L) GOTO 16                      
      DO 15 I=2+MT,L  
      C(K)=((2*I-1)*X*C(K-1)-(I+MT-1)*C(K-2))/(I-MT)                            
15    K=K+1           
16    N=L-MT+1        
      DO 18 I=1,N     
      C(K)=C(K-N)*CAZ                              
      C(K-N)=C(K-N)*SAZ                            
18    K=K+1           
20    CONTINUE        
      RETURN          
      END             
C
C
      REAL FUNCTION ELTE(H)
c----------------------------------------------------------------
C ELECTRON TEMPERATURE PROFILE BASED ON THE TEMPERATURES AT 120                 
C HMAX,300,400,600,1400,3000 KM ALTITUDE. INBETWEEN CONSTANT                    
C GRADIENT IS ASSUMED. ARGMAX IS MAXIMUM ARGUMENT ALLOWED FOR
C EXP-FUNCTION.
c----------------------------------------------------------------
      COMMON /BLOTE/AH(7),ATE1,ST(6),D(5)
C
      SUM=ATE1+ST(1)*(H-AH(1))                     
      DO 1 I=1,5
	aa = eptr(h    ,d(i),ah(i+1))
	bb = eptr(ah(1),d(i),ah(i+1))
1     SUM=SUM+(ST(I+1)-ST(I))*(AA-BB)*D(I)                
      ELTE=SUM        
      RETURN          
      END             
C
C
      FUNCTION TEDE(H,DEN,COV)                     
C ELECTRON TEMEPERATURE MODEL AFTER BRACE,THEIS .  
C FOR NEG. COV THE MEAN COV-INDEX (3 SOLAR ROT.) IS EXPECTED.                   
C DEN IS THE ELECTRON DENSITY IN M-3.              
      Y=1051.+(17.01*H-2746.)*                     
     &EXP(-5.122E-4*H+(6.094E-12-3.353E-14*H)*DEN) 
      ACOV=ABS(COV)   
      YC=1.+(.117+2.02E-3*ACOV)/(1.+EXP(-(ACOV-102.5)/5.))                      
      IF(COV.LT.0.)   
     &YC=1.+(.123+1.69E-3*ACOV)/(1.+EXP(-(ACOV-115.)/10.))                      
      TEDE=Y*YC       
      RETURN          
      END             
C
C                     
C*************************************************************                  
C**************** ION TEMPERATURE ****************************
C*************************************************************                  
C
C
      REAL FUNCTION TI(H)
c----------------------------------------------------------------
C ION TEMPERATURE FOR HEIGHTS NOT GREATER 1000 KM AND NOT LESS HS               
C EXPLANATION SEE FUNCTION RPID.                   
c----------------------------------------------------------------
      REAL 		MM
      COMMON  /BLOCK8/  HS,TNHS,XSM(4),MM(5),G(4),M

      SUM=MM(1)*(H-HS)+TNHS                        
      DO 100 I=1,M-1  
	aa = eptr(h ,g(i),xsm(i))
	bb = eptr(hs,g(i),xsm(i))
100   	SUM=SUM+(MM(I+1)-MM(I))*(AA-BB)*G(I)                
      TI=SUM          
      RETURN          
      END             
C
C
      REAL FUNCTION TEDER(H)                       
C THIS FUNCTION ALONG WITH PROCEDURE REGFA1 ALLOWS TO FIND                      
C THE  HEIGHT ABOVE WHICH TN BEGINS TO BE DIFFERENT FROM TI                     
      COMMON	/BLOTN/XSM1,TEX,TLBD,SIG
      TNH = TN(H,TEX,TLBD,SIG)                        
      DTDX = DTNDH(H,TEX,TLBD,SIG)                        
      TEDER = DTDX * ( XSM1 - H ) + TNH                    
      RETURN          
      END             
C
C                     
C*************************************************************                  
C************* ION RELATIVE PRECENTAGE DENSITY *****************                
C*************************************************************                  
C
C
      REAL FUNCTION RPID (H, H0, N0, M, ST, ID, XS)
c------------------------------------------------------------------
C D.BILITZA,1977,THIS ANALYTIC FUNCTION IS USED TO REPRESENT THE                
C RELATIVE PRECENTAGE DENSITY OF ATOMAR AND MOLECULAR OXYGEN IONS.              
C THE M+1 HEIGHT GRADIENTS ST(M+1) ARE CONNECTED WITH EPSTEIN-                  
C STEP-FUNCTIONS AT THE STEP HEIGHTS XS(M) WITH TRANSITION                      
C THICKNESSES ID(M). RPID(H0,H0,N0,....)=N0.       
C ARGMAX is the highest allowed argument for EXP in your system.
c------------------------------------------------------------------
      REAL 		N0         
      DIMENSION 	ID(4), ST(5), XS(4)                
      COMMON  /ARGEXP/	ARGMAX

      SUM=(H-H0)*ST(1)                             
      DO 100  I=1,M   
	      XI=ID(I)
		aa = eptr(h ,xi,xs(i))
		bb = eptr(h0,xi,xs(i))
100	      SUM=SUM+(ST(I+1)-ST(I))*(AA-BB)*XI 
      IF(ABS(SUM).LT.ARGMAX) then
	SM=EXP(SUM)
      else IF(SUM.Gt.0.0) then
	SM=EXP(ARGMAX)
      else
	SM=0.0
      endif
      RPID= n0 * SM        
      RETURN          
      END             
C
c
      SUBROUTINE RDHHE (H,HB,RDOH,RDO2H,RNO,PEHE,RDH,RDHE)                      
C BILITZA,FEB.82,H+ AND HE+ RELATIVE PERECENTAGE DENSITY BELOW                  
C 1000 KM. THE O+ AND O2+ REL. PER. DENSITIES SHOULD BE GIVEN                   
C (RDOH,RDO2H). HB IS THE ALTITUDE OF MAXIMAL O+ DENSITY. PEHE                  
C IS THE PRECENTAGE OF HE+ IONS COMPARED TO ALL LIGHT IONS.                     
C RNO IS THE RATIO OF NO+ TO O2+DENSITY AT H=HB.   
      RDHE=0.0        
      RDH=0.0         
      IF(H.LE.HB) GOTO 100                         
      REST=100.0-RDOH-RDO2H-RNO*RDO2H              
      RDH=REST*(1.-PEHE/100.)                      
      RDHE=REST*PEHE/100.                          
100   RETURN          
      END             
C
C
      REAL FUNCTION RDNO(H,HB,RDO2H,RDOH,RNO)      
C D.BILITZA, 1978. NO+ RELATIVE PERCENTAGE DENSITY ABOVE 100KM.                 
C FOR MORE INFORMATION SEE SUBROUTINE RDHHE.       
      IF (H.GT.HB) GOTO 200                        
      RDNO=100.0-RDO2H-RDOH                        
      RETURN          
200   RDNO=RNO*RDO2H  
      RETURN          
      END
C
C
      SUBROUTINE  KOEFP1(PG1O)                     
C THIEMANN,1979,COEFFICIENTS PG1O FOR CALCULATING  O+ PROFILES                  
C BELOW THE F2-MAXIMUM. CHOSEN TO APPROACH DANILOV-                             
C SEMENOV'S COMPILATION.                           
      DIMENSION PG1O(80)                           
      REAL FELD (80)  
      DATA FELD/-11.0,-11.0,4.0,-11.0,0.08018,     
     &0.13027,0.04216,0.25  ,-0.00686,0.00999,     
     &5.113,0.1 ,170.0,180.0,0.1175,0.15,-11.0,    
     &1.0 ,2.0,-11.0,0.069,0.161,0.254,0.18,0.0161,                             
     &0.0216,0.03014,0.1,152.0,167.0,0.04916,      
     &0.17,-11.0,2.0,2.0,-11.0,0.072,0.092,0.014,0.21,                          
     &0.01389,0.03863,0.05762,0.12,165.0,168.0,0.008,                           
     &0.258,-11.0,1.0,3.0,-11.0,0.091,0.088,       
     &0.008,0.34,0.0067,0.0195,0.04,0.1,158.0,172.0,                            
     &0.01,0.24,-11.0,2.0,3.0, -11.0,0.083,0.102,  
     &0.045,0.03,0.00127,0.01,0.05,0.09,167.0,185.0,                            
     &0.015,0.18/     
      K=0             
      DO 10 I=1,80    
      K=K+1           
10    PG1O(K)=FELD(I)                              
      RETURN          
      END             
C
C
      SUBROUTINE KOEFP2(PG2O)                      
C THIEMANN,1979,COEFFICIENTS FOR CALCULATION OF O+ PROFILES                     
C ABOVE THE F2-MAXIMUM (DUMBS,SPENNER:AEROS-COMPILATION)                        
      DIMENSION PG2O(32)                           
      REAL FELD(32)   
      DATA FELD/1.0,-11.0,-11.0,1.0,695.0,-.000781,                             
     &-.00264,2177.0,1.0,-11.0,-11.0,2.0,570.0,    
     &-.002,-.0052,1040.0,2.0,-11.0,-11.0,1.0,695.0,                            
     &-.000786,-.00165,3367.0,2.0,-11.0,-11.0,2.0, 
     &575.0,-.00126,-.00524,1380.0/                
      K=0             
      DO 10 I=1,32    
      K=K+1           
10    PG2O(K)=FELD(I)                              
      RETURN          
      END             
C
C
      SUBROUTINE  KOEFP3(PG3O)                     
C THIEMANN,1979,COEFFICIENTS FOR CALCULATING O2+ PROFILES.                      
C CHOSEN AS TO APPROACH DANILOV-SEMENOV'S COMPILATION.                          
      DIMENSION PG3O(80)                           
      REAL FELD(80)   
      DATA FELD/-11.0,1.0,2.0,-11.0,160.0,31.0,130.0,                           
     &-10.0,198.0,0.0,0.05922,-0.07983,            
     &-0.00397,0.00085,-0.00313,0.0,-11.0,2.0,2.0,-11.0,                        
     &140.0,30.0,130.0,-10.0,                      
     &190.0,0.0,0.05107,-0.07964,0.00097,-0.01118,-0.02614,                     
     &-0.09537,       
     &-11.0,1.0,3.0,-11.0,140.0,37.0,125.0,0.0,182.0,                           
     &0.0,0.0307,-0.04968,-0.00248,                
     &-0.02451,-0.00313,0.0,-11.0,2.0,3.0,-11.0,   
     &140.0,37.0,125.0,0.0,170.0,0.0,              
     &0.02806,-0.04716,0.00066,-0.02763,-0.02247,-0.01919,                      
     &-11.0,-11.0,4.0,-11.0,140.0,45.0,136.0,-9.0, 
     &181.0,-26.0,0.02994,-0.04879,                
     &-0.01396,0.00089,-0.09929,0.05589/           
      K=0             
      DO 10 I=1,80    
      K=K+1           
10    PG3O(K)=FELD(I)                              
      RETURN          
      END             
C
C
      SUBROUTINE SUFE (FIELD,RFE,M,FE)             
C SELECTS THE REQUIRED ION DENSITY PARAMETER SET.
C THE INPUT FIELD INCLUDES DIFFERENT SETS OF DIMENSION M EACH                
C CARACTERISED BY 4 HEADER NUMBERS. RFE(4) SHOULD CONTAIN THE                   
C CHOSEN HEADER NUMBERS.FE(M) IS THE CORRESPONDING SET.                         
      DIMENSION RFE(4),FE(12),FIELD(80),EFE(4)     
      K=0             
100   DO 101 I=1,4    
      K=K+1           
101   EFE(I)=FIELD(K)                              
      DO 111 I=1,M    
      K=K+1           
111   FE(I)=FIELD(K)  
      DO 120 I=1,4    
      IF((EFE(I).GT.-10.0).AND.(RFE(I).NE.EFE(I))) GOTO 100                     
120   CONTINUE        
      RETURN          
      END             
C
C                     
C*************************************************************                  
C************* PEAK VALUES ELECTRON DENSITY ******************                  
C*************************************************************                  
C
C
      SUBROUTINE F2OUT(XMODIP,XLATI,XLONGI,FF0,XM0,UT,
     &                              FOF2,XM3000)
C CALCULATES FOF2/MHZ AND M3000 USING THE CCIR-MAPS.                            
C INPUT: MODIFIED DIP LATITUDE XMODIP, GEOG. LATITUDE XLATI,                    
C LONGITUDE XLONGI (ALL IN DEG.), SMOOTHED SUNSPOT NUMBER R,
C MONTH AND UNIVERSAL TIME UT (DEC. HOURS).                    
C D.BILITZA,JULY 85.  
      DIMENSION FF0(988),XM0(441)                       
      INTEGER QM(7),QF(9)                          
      DATA QF/11,11,8,4,1,0,0,0,0/,QM/6,7,5,2,1,0,0/
      FOF2=GAMMA1(XMODIP,XLATI,XLONGI,UT,6,QF,9,76,13,988,FF0)                  
      XM3000=GAMMA1(XMODIP,XLATI,XLONGI,UT,4,QM,7,49,9,441,XM0)                 
      RETURN          
      END             
C
C
      REAL FUNCTION HMF2ED(XMAGBR,R,X,XM3)         
C CALCULATES THE PEAK HEIGHT HMF2/KM FOR THE MAGNETIC                           
C LATITUDE XMAGBR/DEG. AND THE SMOOTHED ZUERICH SUNSPOT                         
C NUMBER R USING CCIR-M3000 XM3 AND THE RATIO X=FOF2/FOE.                       
C [REF. D.BILITZA ET AL., TELECOMM.J., 46, 549-553, 1979]                       
C D.BILITZA,1980.     
      F1=(2.32E-3)*R+0.222                         
      F2=1.2-(1.16E-2)*EXP((2.39E-2)*R)            
      F3=0.096*(R-25.0)/150.0                      
      DELM=F1*(1.0-R/150.0*EXP(-XMAGBR*XMAGBR/1600.0))/(X-F2)+F3                
      HMF2ED=1490.0/(XM3+DELM)-176.0               
      RETURN          
      END             
C
C
      REAL FUNCTION FOF1ED(YLATI,R,CHI)
c--------------------------------------------------------------
C CALCULATES THE F1 PEAK PLASMA FREQUENCY (FOF1/MHZ)
C FOR 	DIP-LATITUDE (YLATI/DEGREE)
c	SMOOTHED ZURICH SUNSPOT NUMBER (R)
c	SOLAR ZENITH ANGLE (CHI/DEGREE)
C REFERENCE: 
c	E.D.DUCHARME ET AL., RADIO SCIENCE 6, 369-378, 1971
C 				       AND 8, 837-839, 1973
c	HOWEVER WITH MAGNETIC DIP LATITUDE INSTEAD OF GEOMAGNETIC
c	DIPOLE LATITUDE, EYFRIG, 1979                    
C--------------------------------------------- D. BILITZA, 1988.   
	COMMON/CONST/UMR
	FOF1 = 0.0
	DLA =  YLATI
		CHI0 = 49.84733 + 0.349504 * DLA
		CHI100 = 38.96113 + 0.509932 * DLA
		CHIM = ( CHI0 + ( CHI100 - CHI0 ) * R / 100. )
		IF(CHI.GT.CHIM) GOTO 1 
  	F0 = 4.35 + DLA * ( 0.0058 - 1.2E-4 * DLA ) 
	F100 = 5.348 + DLA * ( 0.011 - 2.3E-4 * DLA )
	FS = F0 + ( F100 - F0 ) * R / 100.0
	XMUE = 0.093 + DLA * ( 0.0046 - 5.4E-5 * DLA ) + 3.0E-4 * R
	FOF1 = FS * COS( CHI * UMR ) ** XMUE
1	FOF1ED = FOF1     
	RETURN
	END             
C
C
      REAL FUNCTION FOEEDI(COV,XHI,XHIM,XLATI)
C-------------------------------------------------------
C CALCULATES FOE/MHZ BY THE EDINBURGH-METHOD.      
C INPUT: MEAN 10.7CM SOLAR RADIO FLUX (COV), GEOGRAPHIC
C LATITUDE (XLATI/DEG), SOLAR ZENITH ANGLE (XHI/DEG AND 
C XHIM/DEG AT NOON).
C REFERENCE: 
C 	KOURIS-MUGGELETON, CCIR DOC. 6/3/07, 1973
C 	TROST, J. GEOPHYS. RES. 84, 2736, 1979 (was used
C		to improve the nighttime varition)
C D.BILITZA--------------------------------- AUGUST 1986.    
      COMMON/CONST/UMR
C variation with solar activity (factor A) ...............
      A=1.0+0.0094*(COV-66.0)                      
C variation with noon solar zenith angle (B) and with latitude (C)
      SL=COS(XLATI*UMR)
	IF(XLATI.LT.32.0) THEN
		SM=-1.93+1.92*SL                             
		C=23.0+116.0*SL                              
 	ELSE
	 	SM=0.11-0.49*SL                              
	 	C=92.0+35.0*SL  
  	ENDIF
	if(XHIM.ge.90.) XHIM=89.999
	B = COS(XHIM*UMR) ** SM
C variation with solar zenith angle (D) ..........................        
 	IF(XLATI.GT.12.0) THEN
		SP=1.2
	ELSE
		SP=1.31         
	ENDIF
C adjusted solar zenith angle during nighttime (XHIC) .............
      XHIC=XHI-3.*ALOG(1.+EXP((XHI-89.98)/3.))   
      D=COS(XHIC*UMR)**SP       
C determine foE**4 ................................................
      R4FOE=A*B*C*D     
C minimum allowable foE (sqrt[SMIN])...............................
      SMIN=0.121+0.0015*(COV-60.)
      SMIN=SMIN*SMIN
      IF(R4FOE.LT.SMIN) R4FOE=SMIN                     
      FOEEDI=R4FOE**0.25                           
      RETURN          
      END   
C
C
      REAL FUNCTION XMDED(XHI,R,YW)                
C D. BILITZA, 1978, CALCULATES ELECTRON DENSITY OF D MAXIMUM.                   
C XHI/DEG. IS SOLAR ZENITH ANGLE, R SMOOTHED ZURICH SUNSPOT NUMBER              
C AND YW/M-3 THE ASSUMED CONSTANT NIGHT VALUE.     
C [REF.: D.BILITZA, WORLD DATA CENTER A REPORT UAG-82,7,                        
C       BOULDER,1981]                              
      COMMON/CONST/UMR
      Y=6.05E8+0.088E8*R 
C      WRITE(*,*)'XMDED  YW=',YW,' Y=',Y,' R=',R
      Z=(-0.1/(ALOG(YW/Y)))**0.3704
	if(abs(z).gt.1.) z=sign(1.,z)
      SUXHI=ACOS(Z)  
      IF (SUXHI.LT.1.0472) SUXHI=1.0472            
      XXHI=XHI*UMR    
      IF (XXHI.GT.SUXHI) GOTO 100                  
      X=COS(XXHI)     
      XMDED=Y*EXP(-0.1/X**2.7)                     
      RETURN          
100   XMDED=YW        
      RETURN          
      END
C
C
      REAL FUNCTION GAMMA1(SMODIP,SLAT,SLONG,HOUR,IHARM,NQ,
     &  			K1,M,MM,M3,SFE)      
C CALCULATES GAMMA1=FOF2 OR M3000 USING CCIR NUMERICAL MAP                      
C COEFFICIENTS SFE(M3) FOR MODIFIED DIP LATITUDE (SMODIP/DEG)
C GEOGRAPHIC LATITUDE (SLAT/DEG) AND LONGITUDE (SLONG/DEG)  
C AND UNIVERSIAL TIME (HOUR/DECIMAL HOURS).
C NQ(K1) IS AN INTEGER ARRAY GIVING THE HIGHEST DEGREES IN 
C LATITUDE FOR EACH LONGITUDE HARMONIC.                  
C M=1+NQ1+2(NQ2+1)+2(NQ3+1)+... .                  
C SHEIKH,4.3.77.      
      REAL*8 C(12),S(12),COEF(100),SUM             
      DIMENSION NQ(K1),XSINX(13),SFE(M3)           
      COMMON/CONST/UMR
      HOU=(15.0*HOUR-180.0)*UMR                    
      S(1)=SIN(HOU)   
      C(1)=COS(HOU)   
      DO 250 I=2,IHARM                             
      C(I)=C(1)*C(I-1)-S(1)*S(I-1)                 
      S(I)=C(1)*S(I-1)+S(1)*C(I-1)                 
250   CONTINUE        
      DO 300 I=1,M    
      MI=(I-1)*MM     
      COEF(I)=SFE(MI+1)                            
      DO 300 J=1,IHARM                             
      COEF(I)=COEF(I)+SFE(MI+2*J)*S(J)+SFE(MI+2*J+1)*C(J)                       
300   CONTINUE        
      SUM=COEF(1)     
      SS=SIN(SMODIP*UMR)                           
      S3=SS           
      XSINX(1)=1.0    
      INDEX=NQ(1)     
      DO 350 J=1,INDEX                             
      SUM=SUM+COEF(1+J)*SS                         
      XSINX(J+1)=SS   
      SS=SS*S3        
350   CONTINUE        
      XSINX(NQ(1)+2)=SS                            
      NP=NQ(1)+1      
      SS=COS(SLAT*UMR)                             
      S3=SS           
      DO 400 J=2,K1   
      S0=SLONG*(J-1.)*UMR                          
      S1=COS(S0)      
      S2=SIN(S0)      
      INDEX=NQ(J)+1   
      DO 450 L=1,INDEX                             
      NP=NP+1         
      SUM=SUM+COEF(NP)*XSINX(L)*SS*S1              
      NP=NP+1         
      SUM=SUM+COEF(NP)*XSINX(L)*SS*S2              
450   CONTINUE        
      SS=SS*S3        
400   CONTINUE        
      GAMMA1=SUM      
      RETURN          
      END             
C
C                     
C************************************************************                   
C*************** EARTH MAGNETIC FIELD ***********************                   
C**************************************************************                 
C
C
      SUBROUTINE GGM(ART,LONG,LATI,MLONG,MLAT)            
C CALCULATES GEOMAGNETIC LONGITUDE (MLONG) AND LATITUDE (MLAT) 
C FROM GEOGRAFIC LONGITUDE (LONG) AND LATITUDE (LATI) FOR ART=0
C AND REVERSE FOR ART=1. ALL ANGLES IN DEGREE.
C LATITUDE:-90 TO 90. LONGITUDE:0 TO 360 EAST.         
      INTEGER ART     
      REAL MLONG,MLAT,LONG,LATI
      COMMON/CONST/FAKTOR
      ZPI=FAKTOR*360.                              
      CBG=11.4*FAKTOR                              
      CI=COS(CBG)     
      SI=SIN(CBG)
      IF(ART.EQ.0) GOTO 10                         
      CBM=COS(MLAT*FAKTOR)                           
      SBM=SIN(MLAT*FAKTOR)                           
      CLM=COS(MLONG*FAKTOR)                          
      SLM=SIN(MLONG*FAKTOR)
      SBG=SBM*CI-CBM*CLM*SI
        IF(ABS(SBG).GT.1.) SBG=SIGN(1.,SBG)
      LATI=ASIN(SBG)
      CBG=COS(LATI)     
      SLG=(CBM*SLM)/CBG  
      CLG=(SBM*SI+CBM*CLM*CI)/CBG
        IF(ABS(CLG).GT.1.) CLG=SIGN(1.,CLG)                  
      LONG=ACOS(CLG)  
      IF(SLG.LT.0.0) LONG=ZPI-LONG
      LATI=LATI/FAKTOR    
      LONG=LONG/FAKTOR  
      LONG=LONG-69.8    
      IF(LONG.LT.0.0) LONG=LONG+360.0                 
      RETURN          
10    YLG=LONG+69.8    
      CBG=COS(LATI*FAKTOR)                           
      SBG=SIN(LATI*FAKTOR)                           
      CLG=COS(YLG*FAKTOR)                          
      SLG=SIN(YLG*FAKTOR)                          
      SBM=SBG*CI+CBG*CLG*SI                        
        IF(ABS(SBM).GT.1.) SBM=SIGN(1.,SBM)
      MLAT=ASIN(SBM)   
      CBM=COS(MLAT)     
      SLM=(CBG*SLG)/CBM                            
      CLM=(-SBG*SI+CBG*CLG*CI)/CBM
        IF(ABS(CLM).GT.1.) CLM=SIGN(1.,CLM) 
      MLONG=ACOS(CLM)
      IF(SLM.LT..0) MLONG=ZPI-MLONG
      MLAT=MLAT/FAKTOR    
      MLONG=MLONG/FAKTOR  
      RETURN          
      END             
C
C
      SUBROUTINE FIELDG(DLAT,DLONG,ALT,X,Y,Z,F,DIP,DEC,SMODIP)                  
C THIS IS A SPECIAL VERSION OF THE POGO 68/10 MAGNETIC FIELD                    
C LEGENDRE MODEL. TRANSFORMATION COEFF. G(144) VALID FOR 1973.                  
C INPUT: DLAT, DLONG=GEOGRAPHIC COORDINATES/DEG.(-90/90,0/360),                 
C        ALT=ALTITUDE/KM.                          
C OUTPUT: F TOTAL FIELD (GAUSS), Z DOWNWARD VERTICAL COMPONENT                  
C        X,Y COMPONENTS IN THE EQUATORIAL PLANE (X TO ZERO LONGITUDE).          
C        DIP INCLINATION ANGLE(DEGREE). SMODIP RAWER'S MODFIED DIP.             
C SHEIK,1977.         
      DIMENSION H(144),XI(3),G(144),FEL1(72),FEL2(72)
      COMMON/CONST/UMR                           
      DATA FEL1/0.0, 0.1506723,0.0101742, -0.0286519, 0.0092606,                
     & -0.0130846, 0.0089594, -0.0136808,-0.0001508, -0.0093977,                
     & 0.0130650, 0.0020520, -0.0121956, -0.0023451, -0.0208555,                
     & 0.0068416,-0.0142659, -0.0093322, -0.0021364, -0.0078910,                
     & 0.0045586,  0.0128904, -0.0002951, -0.0237245,0.0289493,                 
     & 0.0074605, -0.0105741, -0.0005116, -0.0105732, -0.0058542,               
     &0.0033268, 0.0078164,0.0211234, 0.0099309, 0.0362792,                     
     &-0.0201070,-0.0046350,-0.0058722,0.0011147,-0.0013949,                    
     & -0.0108838,  0.0322263, -0.0147390,  0.0031247, 0.0111986,               
     & -0.0109394,0.0058112,  0.2739046, -0.0155682, -0.0253272,                
     &  0.0163782, 0.0205730,  0.0022081, 0.0112749,-0.0098427,                 
     & 0.0072705, 0.0195189, -0.0081132, -0.0071889, -0.0579970,                
     & -0.0856642, 0.1884260,-0.7391512, 0.1210288, -0.0241888,                 
     & -0.0052464, -0.0096312, -0.0044834, 0.0201764,  0.0258343,               
     &0.0083033,  0.0077187/                       
      DATA FEL2/0.0586055,0.0102236,-0.0396107,    
     & -0.0167860, -0.2019911, -0.5810815,0.0379916,  3.7508268,                
     & 1.8133030, -0.0564250, -0.0557352, 0.1335347, -0.0142641,                
     & -0.1024618,0.0970994, -0.0751830,-0.1274948, 0.0402073,                  
     &  0.0386290, 0.1883088,  0.1838960, -0.7848989,0.7591817,                 
     & -0.9302389,-0.8560960, 0.6633250, -4.6363869, -13.2599277,               
     & 0.1002136,  0.0855714,-0.0991981, -0.0765378,-0.0455264,                 
     &  0.1169326, -0.2604067, 0.1800076, -0.2223685, -0.6347679,               
     &0.5334222, -0.3459502,-0.1573697,  0.8589464, 1.7815990,                  
     &-6.3347645, -3.1513653, -9.9927750,13.3327637, -35.4897308,               
     &37.3466339, -0.5257398,  0.0571474, -0.5421217,  0.2404770,               
     & -0.1747774,-0.3433644, 0.4829708,0.3935944, 0.4885033,                   
     &  0.8488121, -0.7640999, -1.8884945, 3.2930784,-7.3497229,                
     & 0.1672821,-0.2306652, 10.5782146, 12.6031065, 8.6579742,                 
     & 215.5209961, -27.1419220,22.3405762,1108.6394043/                        
      K=0             
      DO 10 I=1,72    
      K=K+1           
      G(K)=FEL1(I)    
10    G(72+K)=FEL2(I)                              
      RLAT=DLAT*UMR   
      CT=SIN(RLAT)    
      ST=COS(RLAT)    
      NMAX=11         
      D=SQRT(40680925.0-272336.0*CT*CT)            
      RLONG=DLONG*UMR                              
      CP=COS(RLONG)   
      SP=SIN(RLONG)   
      ZZZ=(ALT+40408589.0/D)*CT/6371.2             
      RHO=(ALT+40680925.0/D)*ST/6371.2             
      XXX=RHO*CP      
      YYY=RHO*SP      
      RQ=1.0/(XXX*XXX+YYY*YYY+ZZZ*ZZZ)             
      XI(1)=XXX*RQ    
      XI(2)=YYY*RQ    
      XI(3)=ZZZ*RQ    
      IHMAX=NMAX*NMAX+1                            
      LAST=IHMAX+NMAX+NMAX                         
      IMAX=NMAX+NMAX-1                             
      DO 100 I=IHMAX,LAST                          
100   H(I)=G(I)       
      DO 200 K=1,3,2  
      I=IMAX          
      IH=IHMAX        
300   IL=IH-I         
      F1=2./(I-K+2.)  
      X1=XI(1)*F1     
      Y1=XI(2)*F1     
      Z1=XI(3)*(F1+F1)                             
      I=I-2           
      IF((I-1).LT.0) GOTO 400                      
      IF((I-1).EQ.0) GOTO 500                      
      DO 600 M=3,I,2  
      H(IL+M+1)=G(IL+M+1)+Z1*H(IH+M+1)+X1*(H(IH+M+3)-H(IH+M-1))-                
     &Y1*(H(IH+M+2)+H(IH+M-2))                     
      H(IL+M)=G(IL+M)+Z1*H(IH+M)+X1*(H(IH+M+2)-H(IH+M-2))+                      
     &Y1*(H(IH+M+3)+H(IH+M-1))                     
600   CONTINUE        
500   H(IL+2)=G(IL+2)+Z1*H(IH+2)+X1*H(IH+4)-Y1*(H(IH+3)+H(IH))                  
      H(IL+1)=G(IL+1)+Z1*H(IH+1)+Y1*H(IH+4)+X1*(H(IH+3)-H(IH))                  
400   H(IL)=G(IL)+Z1*H(IH)+2.0*(X1*H(IH+1)+Y1*H(IH+2))                          
700   IH=IL           
      IF(I.GE.K) GOTO 300                          
200   CONTINUE        
      S=0.5*H(1)+2.0*(H(2)*XI(3)+H(3)*XI(1)+H(4)*XI(2))                         
      XT=(RQ+RQ)*SQRT(RQ)                          
      X=XT*(H(3)-S*XXX)                            
      Y=XT*(H(4)-S*YYY)                            
      Z=XT*(H(2)-S*ZZZ)                            
      F=SQRT(X*X+Y*Y+Z*Z)                          
      BRH0=Y*SP+X*CP  
      Y=Y*CP-X*SP     
      X=Z*ST-BRH0*CT  
      Z=-Z*CT-BRH0*ST 
	zdivf=z/f
        IF(ABS(zdivf).GT.1.) zdivf=SIGN(1.,zdivf)
      DIP=ASIN(zdivf)
	ydivs=y/sqrt(x*x+y*y)  
        IF(ABS(ydivs).GT.1.) ydivs=SIGN(1.,ydivs)
      DEC=ASIN(ydivs)
	dipdiv=DIP/SQRT(DIP*DIP+ST)
        IF(ABS(dipdiv).GT.1.) dipdiv=SIGN(1.,dipdiv)
      SMODIP=ASIN(dipdiv)
      DIP=DIP/UMR     
      DEC=DEC/UMR     
      SMODIP=SMODIP/UMR                            
      RETURN          
      END             
C
C
C************************************************************                   
C*********** INTERPOLATION AND REST ***************************                 
C**************************************************************                 
C
C
      SUBROUTINE REGFA1(X11,X22,FX11,FX22,EPS,FW,F,SCHALT,X) 
C REGULA-FALSI-PROCEDURE TO FIND X WITH F(X)-FW=0. X1,X2 ARE THE                
C STARTING VALUES. THE COMUTATION ENDS WHEN THE X-INTERVAL                      
C HAS BECOME LESS THAN EPS . IF SIGN(F(X1)-FW)= SIGN(F(X2)-FW)                  
C THEN SCHALT=.TRUE.  
      LOGICAL L1,LINKS,K,SCHALT                    
      SCHALT=.FALSE.
      EP=EPS  
      X1=X11          
      X2=X22          
      F1=FX11-FW     
      F2=FX22-FW     
      K=.FALSE.       
      NG=2       
      LFD=0     
      IF(F1*F2.LE.0.0) GOTO 200
   	X=0.0           
    	SCHALT=.TRUE.   
      	RETURN
200   X=(X1*F2-X2*F1)/(F2-F1)                      
      GOTO 400        
300   	L1=LINKS        
	DX=(X2-X1)/NG
     	IF(.NOT.LINKS) DX=DX*(NG-1)
     	X=X1+DX
400   FX=F(X)-FW
      LFD=LFD+1
      IF(LFD.GT.20) THEN
	EP=EP*10.
	LFD=0
      ENDIF 
      LINKS=(F1*FX.GT.0.0)
      K=.NOT.K        
      IF(LINKS) THEN
	X1=X            
 	F1=FX           
      ELSE
	X2=X 
	F2=FX 
      ENDIF   
      IF(ABS(X2-X1).LE.EP) GOTO 800               
      IF(K) GOTO 300  
      IF((LINKS.AND.(.NOT.L1)).OR.(.NOT.LINKS.AND.L1)) NG=2*NG                  
      GOTO 200        
800   RETURN          
      END             
C
C
      SUBROUTINE TAL(SHABR,SDELTA,SHBR,SDTDH0,AUS6,SPT)                         
C CALCULATES THE COEFFICIENTS SPT FOR THE POLYNOMIAL
C Y(X)=1+SPT(1)*X**2+SPT(2)*X**3+SPT(3)*X**4+SPT(4)*X**5               
C TO FIT THE VALLEY IN Y, REPRESENTED BY:                
C Y(X=0)=1, THE X VALUE OF THE DEEPEST VALLEY POINT (SHABR),                    
C THE PRECENTAGE DEPTH (SDELTA), THE WIDTH (SHBR) AND THE                       
C DERIVATIVE DY/DX AT THE UPPER VALLEY BOUNDRY (SDTDH0).                        
C IF THERE IS AN UNWANTED ADDITIONAL EXTREMUM IN THE VALLEY                     
C REGION, THEN AUS6=.TRUE., ELSE AUS6=.FALSE..     
C FOR -SDELTA THE COEFF. ARE CALCULATED FOR THE FUNCTION                        
C Y(X)=EXP(SPT(1)*X**2+...+SPT(4)*X**5).           
      DIMENSION SPT(4)                             
      LOGICAL AUS6    
      Z1=-SDELTA/(100.0*SHABR*SHABR)               
      IF(SDELTA.GT.0.) GOTO 500                    
      SDELTA=-SDELTA  
      Z1=ALOG(1.-SDELTA/100.)/(SHABR*SHABR)        
500   Z3=SDTDH0/(2.*SHBR)                          
      Z4=SHABR-SHBR   
      SPT(4)=2.0*(Z1*(SHBR-2.0*SHABR)*SHBR+Z3*Z4*SHABR)/                        
     &  (SHABR*SHBR*Z4*Z4*Z4)                        
      SPT(3)=Z1*(2.0*SHBR-3.0*SHABR)/(SHABR*Z4*Z4)-
     &  (2.*SHABR+SHBR)*SPT(4)          
      SPT(2)=-2.0*Z1/SHABR-2.0*SHABR*SPT(3)-3.0*SHABR*SHABR*SPT(4)              
      SPT(1)=Z1-SHABR*(SPT(2)+SHABR*(SPT(3)+SHABR*SPT(4)))                      
      AUS6=.FALSE.    
      B=4.*SPT(3)/(5.*SPT(4))+SHABR                
      C=-2.*SPT(1)/(5*SPT(4)*SHABR)                
      Z2=B*B/4.-C     
      IF(Z2.LT.0.0) GOTO 300                       
      Z3=SQRT(Z2)     
      Z1=B/2.         
      Z2=-Z1+Z3       
      IF(Z2.GT.0.0.AND.Z2.LT.SHBR) AUS6=.TRUE.     
      IF (ABS(Z3).GT.1.E-15) GOTO 400              
      Z2=C/Z2         
      IF(Z2.GT.0.0.AND.Z2.LT.SHBR) AUS6=.TRUE.     
      RETURN          
400   Z2=-Z1-Z3       
      IF(Z2.GT.0.0.AND.Z2.LT.SHBR) AUS6=.TRUE.     
300   RETURN          
      END             
C
C
C******************************************************************
C********** ZENITH ANGLE, DAY OF YEAR, TIME ***********************
C******************************************************************
C
C
	subroutine soco (ld,t,flat,Elon,
     &		DECLIN, ZENITH, SUNRSE, SUNSET)
c--------------------------------------------------------------------
c	s/r to calculate the solar declination, zenith angle, and
c	sunrise & sunset times  - based on Newbern Smith's algorithm
c	[leo mcnamara, 1-sep-86, last modified 16-jun-87]
c	{dieter bilitza, 30-oct-89, modified for IRI application}
c
c in:	ld	local day of year
c	t	local hour (decimal)
c	flat	northern latitude in degrees
c	elon	east longitude in degrees
c
c out:	declin      declination of the sun in degrees
c	zenith	    zenith angle of the sun in degrees
c	sunrse	    local time of sunrise in hours 
c	sunset	    local time of sunset in hours 
c-------------------------------------------------------------------
c
	common/const/	dtr
c amplitudes of Fourier coefficients  --  1955 epoch.................
	data  	p1,p2,p3,p4,p6 /
     & 	0.017203534,0.034407068,0.051610602,0.068814136,0.103221204 /
c
c s/r is formulated in terms of WEST longitude.......................
	wlon = 360. - Elon
c
c time of equinox for 1980...........................................
	td = ld + (t + Wlon/15.) / 24.
	te = td + 0.9369
c
c declination of the sun..............................................
	dcl = 23.256 * sin(p1*(te-82.242)) + 0.381 * sin(p2*(te-44.855))
     &      + 0.167 * sin(p3*(te-23.355)) - 0.013 * sin(p4*(te+11.97))
     &      + 0.011 * sin(p6*(te-10.41)) + 0.339137
	DECLIN = dcl
	dc = dcl * dtr
c
c the equation of time................................................
	tf = te - 0.5
	eqt = -7.38*sin(p1*(tf-4.)) - 9.87*sin(p2*(tf+9.))
     &      + 0.27*sin(p3*(tf-53.)) - 0.2*cos(p4*(tf-17.))
	et = eqt * dtr / 4.
c
	fa = flat * dtr
	phi = 0.26179939 * ( t - 12.) + et
c
	a = sin(fa) * sin(dc)
	b = cos(fa) * cos(dc)
	cosx = a + b * cos(phi)
	if(abs(cosx).gt.1.) cosx=sign(1.,cosx)
	zenith = acos(cosx) / dtr
c
c calculate sunrise and sunset times --  at the ground...........
c see Explanatory Supplement to the Ephemeris (1961) pg 401......
c sunrise at height h metres is at...............................
c	chi(h) = 90.83 + 0.0347 * sqrt(h)........................
c this includes corrections for horizontal refraction and........
c semi-diameter of the solar disk................................
	ch = cos(90.83 * dtr)
	cosphi = (ch -a ) / b
c if abs(secphi) > 1., sun does not rise/set.....................
c allow for sun never setting - high latitude summer.............
	secphi = 999999.
	if(cosphi.ne.0.) secphi = 1./cosphi
	sunset = 99.
	sunrse = 99.
	if(secphi.gt.-1.0.and.secphi.le.0.) return
c allow for sun never rising - high latitude winter..............
	sunset = -99.
	sunrse = -99.
	if(secphi.gt.0.0.and.secphi.lt.1.) return
c
	if(cosphi.gt.1.) cosphi=sign(1.,cosphi)
	phi = acos(cosphi)
	et = et / 0.26179939
	phi = phi / 0.26179939
	sunrse = 12. - phi - et
	sunset = 12. + phi - et
	if(sunrse.lt.0.) sunrse = sunrse + 24.
	if(sunset.ge.24.) sunset = sunset - 24.
c
	return
	end
c
C
      FUNCTION HPOL(HOUR,TW,XNW,SA,SU,DSA,DSU)            
C-------------------------------------------------------
C PROCEDURE FOR SMOOTH TIME-INTERPOLATION USING EPSTEIN  
C STEP FUNCTION AT SUNRISE (SA) AND SUNSET (SU). THE 
C STEP-WIDTH FOR SUNRISE IS DSA AND FOR SUNSET DSU.
C TW,NW ARE THE DAY AND NIGHT VALUE OF THE PARAMETER TO 
C BE INTERPOLATED. SA AND SU ARE TIME OF SUNRIES AND 
C SUNSET IN DECIMAL HOURS.
C BILITZA----------------------------------------- 1979.
	IF(ABS(SU).GT.25.) THEN
		IF(SU.GT.0.0) THEN
			HPOL=TW
		ELSE
			HPOL=XNW
		ENDIF
		RETURN
	ENDIF
      HPOL=XNW+(TW-XNW)*EPST(HOUR,DSA,SA)+
     &	(XNW-TW)*EPST(HOUR,DSU,SU) 
      RETURN          
      END       
C      
C
	SUBROUTINE MODA(IN,MONTH,IDAY,IDOY)
C-------------------------------------------------------------------
C CALCULATES DAY OF YEAR (IDOY) FROM MONTH (MONTH) AND DAY (IDAY) 
C IF IN=0, OR MONTH (MONTH) AND DAY (IDAY) FROM DAY OF 
C YEAR (IDOY), IF IN=1. 
C-------------------------------------------------------------------
	DIMENSION	MO(12)
	DATA		MO/0,31,59,90,120,151,181,212,243,273,304,334/
	IMO=0
	MOBE=0
	IF(IN.GT.0) GOTO 5
		IDOY=MO(MONTH)+IDAY
		RETURN
5	IMO=IMO+1
		MOOLD=MOBE
		IF(IMO.GT.12) GOTO 55
		MOBE=MO(IMO)
		IF(MOBE.LT.IDOY) GOTO 5
55		MONTH=IMO-1
		IDAY=IDOY-MOOLD
	RETURN
	END		
c
C
	REAL FUNCTION B0POL ( HOUR, SAX, SUX, ISEASON, R, DELA)
C-----------------------------------------------------------------
C Interpolation procedure for bottomside thickness parameter B0.
C Array B0F(ILT,ISEASON,IR,ILATI) distinguishes between day and
C night (ILT=1,2), four seasons (ISEASON=1 spring), low and high
C solar activity (IR=1,2), and low and middle modified dip
C latitudes (ILATI=1,2). In the DATA statement the first value
C corresponds to B0F(1,1,1,1), the second to B0F(2,1,1,1), the
C third to B0F(1,2,1,1) and so on.
C JUNE 1989 --------------------------------------- Dieter Bilitza
C
      REAL		NITVAL
      DIMENSION 	B0F(2,4,2,2),SIPH(2),SIPL(2)
      DATA 	B0F/114.,64.0,134.,77.0,128.,66.0,75.,73.0,
     &		    113.,115.,150.,116.,138.,123.,94.,132.,
     &		    72.0,84.0,83.0,89.0,75.0,85.0,57.,76.0,
     &		    102.,100.,120.,110.,107.,103.,76.,86.0/

  	DO 7033 ISR=1,2
 		DO 7034 ISL=1,2
			DAYVAL   = B0F(1,ISEASON,ISR,ISL)
			NITVAL = B0F(2,ISEASON,ISR,ISL)

C Interpolation day/night with transitions at SAX (sunrise) and SUX (sunset)
7034  			SIPH(ISL) = HPOL(HOUR,DAYVAL,NITVAL,
     &				SAX,SUX,1.,1.)

C Interpolation low/middle modip with transition at 30 degrees modip
7033		SIPL(ISR) = SIPH(1) + (SIPH(2) - SIPH(1)) / DELA

C Interpolation low/high Rz12: linear from 10 to 100
	B0POL=SIPL(1)+(SIPL(2)-SIPL(1))/90.*(R-10.)
	RETURN
	END
c
C
C *********************************************************************
C ************************ EPSTEIN FUNCTIONS **************************
C *********************************************************************
C REF:	H. G. BOOKER, J. ATMOS. TERR. PHYS. 39, 619-623, 1977
C 	K. RAWER, ADV. SPACE RES. 4, #1, 11-15, 1984
C *********************************************************************
C
C
	REAL FUNCTION  RLAY ( X, XM, SC, HX )
C -------------------------------------------------------- RAWER  LAYER
	Y1  = EPTR ( X , SC, HX )
	Y1M = EPTR ( XM, SC, HX )
	Y2M = EPST ( XM, SC, HX )
	RLAY = Y1 - Y1M - ( X - XM ) * Y2M / SC
	RETURN
	END
C
C
	REAL FUNCTION D1LAY ( X, XM, SC, HX )
C ------------------------------------------------------------ dLAY/dX
	D1LAY = ( EPST(X,SC,HX) - EPST(XM,SC,HX) ) /  SC
	RETURN
	END
C
C
	REAL FUNCTION D2LAY ( X, XM, SC, HX )
C ---------------------------------------------------------- d2LAY/dX2
	D2LAY = EPLA(X,SC,HX) /  (SC * SC)
	RETURN
	END
C
C
	REAL FUNCTION EPTR ( X, SC, HX )
C ------------------------------------------------------------ TRANSITION
	COMMON/ARGEXP/ARGMAX
	D1 = ( X - HX ) / SC
	IF (ABS(D1).LT.ARGMAX) GOTO 1
	IF (D1.GT.0.0) THEN
	  EPTR = D1
	ELSE
	  EPTR = 0.0
	ENDIF
	RETURN
1	EPTR = ALOG ( 1. + EXP( D1 ))
	RETURN
	END
C
C
	REAL FUNCTION EPST ( X, SC, HX )
C -------------------------------------------------------------- STEP
	COMMON/ARGEXP/ARGMAX
	D1 = ( X - HX ) / SC
	IF (ABS(D1).LT.ARGMAX) GOTO 1
	IF (D1.GT.0.0) THEN
	  EPST = 1.
	ELSE
	  EPST = 0.
	ENDIF
	RETURN
1	EPST = 1. / ( 1. + EXP( -D1 ))
	RETURN
	END
C
C
	REAL FUNCTION EPSTEP ( Y2, Y1, SC, HX, X)
C---------------------------------------------- STEP FROM Y1 TO Y2	
	EPSTEP = Y1 + ( Y2 - Y1 ) * EPST ( X, SC, HX)
	RETURN
	END
C
C
	REAL FUNCTION EPLA ( X, SC, HX )
C ------------------------------------------------------------ PEAK 
	COMMON/ARGEXP/ARGMAX
	D1 = ( X - HX ) / SC
	IF (ABS(D1).LT.ARGMAX) GOTO 1
		EPLA = 0
		RETURN	
1	D0 = EXP ( D1 )
	D2 = 1. + D0
	EPLA = D0 / ( D2 * D2 )
	RETURN
	END
c
c
	FUNCTION XE2TO5(H,HMF2,NL,HX,SC,AMP)
C----------------------------------------------------------------------
C NORMALIZED ELECTRON DENSITY (N/NMF2) FOR THE MIDDLE IONOSPHERE FROM 
C HME TO HMF2 USING LAY-FUNCTIONS.
C----------------------------------------------------------------------
	DIMENSION	HX(NL),SC(NL),AMP(NL)
	SUM = 1.0
	DO 1 I=1,NL
	   YLAY = AMP(I) * RLAY( H, HMF2, SC(I), HX(I) )
	   zlay=10.**ylay
1	   sum=sum*zlay
	XE2TO5 = sum
	RETURN
	END
C
C
	REAL FUNCTION XEN(H,HMF2,XNMF2,HME,NL,HX,SC,AMP)
C----------------------------------------------------------------------
C ELECTRON DENSITY WITH NEW MIDDLE IONOSPHERE
C----------------------------------------------------------------------
	DIMENSION	HX(NL),SC(NL),AMP(NL)
C
	IF(H.LT.HMF2) GOTO 100
		XEN = XE1(H)
		RETURN
100	IF(H.LT.HME) GOTO 200
		XEN = XNMF2 * XE2TO5(H,HMF2,NL,HX,SC,AMP)
		RETURN
200	XEN = XE6(H)
	RETURN
	END
C
C
	SUBROUTINE VALGUL(XHI,HVB,VWU,VWA,VDP)
C --------------------------------------------------------------------- 
C   CALCULATES E-F VALLEY PARAMETERS; T.L. GULYAEVA, ADVANCES IN
C   SPACE RESEARCH 7, #6, 39-48, 1987.
C
C	INPUT:	XHI	SOLAR ZENITH ANGLE [DEGREE]
C	
C	OUTPUT:	VDP	VALLEY DEPTH  (NVB/NME)
C		VWU	VALLEY WIDTH  [KM]
C		VWA	VALLEY WIDTH  (SMALLER, CORRECTED BY RAWER)
C		HVB	HEIGHT OF VALLEY BASE [KM]
C -----------------------------------------------------------------------
C
	COMMON	/CONST/UMR
C
	CS = 0.1 + COS(UMR*XHI)
	ABC = ABS(CS)
	VDP = 0.45 * CS / (0.1 + ABC ) + 0.55
	ARL = ( 0.1 + ABC + CS ) / ( 0.1 + ABC - CS)
	ZZZ = ALOG( ARL )
	VWU = 45. - 10. * ZZZ
	VWA = 45. -  5. * ZZZ
	HVB = 1000. / ( 7.024 + 0.224 * CS + 0.966 * ABC )
	RETURN
	END
C
C
	SUBROUTINE ROGUL(IDAY,XHI,SX,GRO)
C --------------------------------------------------------------------- 
C   CALCULATES RATIO H0.5/HMF2 FOR HALF-DENSITY POINT (NE(H0.5)=0.5*NMF2)
C   T.L. GULYAEVA, ADVANCES IN SPACE RESEARCH 7, #6, 39-48, 1987.
C
C	INPUT:	IDAY	DAY OF YEAR
C		XHI	SOLAR ZENITH ANGLE [DEGREE]
C	
C	OUTPUT:	GRO	RATIO OF HALF DENSITY HEIGHT TO F PEAK HEIGHT
C		SX	SMOOTHLY VARYING SEASON PARAMTER (SX=1 FOR 
C			DAY=1; SX=3 FOR DAY=180; SX=2 FOR EQUINOX)
C -----------------------------------------------------------------------
C
	SX = 2. - COS ( IDAY * 0.017214206 )
	XS = ( XHI - 20. * SX) / 15.
	GRO = 0.8 - 0.2 / ( 1. + EXP(XS) )
c same as gro=0.6+0.2/(1+exp(-xs))
	RETURN
	END
C
C
	SUBROUTINE LNGLSN ( N, A, B, AUS)
C --------------------------------------------------------------------
C SOLVES QUADRATIC SYSTEM OF LINEAR EQUATIONS:
C
C	INPUT:	N	NUMBER OF EQUATIONS (= NUMBER OF UNKNOWNS)
C		A(N,N)	MATRIX (LEFT SIDE OF SYSTEM OF EQUATIONS)
C		B(N)	VECTOR (RIGHT SIDE OF SYSTEM)
C
C	OUTPUT:	AUS	=.TRUE.	  NO SOLUTION FOUND
C			=.FALSE.  SOLUTION IS IN  A(N,J) FOR J=1,N
C --------------------------------------------------------------------
C
	DIMENSION	A(5,5), B(5), AZV(10)
	LOGICAL		AUS
C
	NN = N - 1
	AUS = .FALSE.
	DO 1 K=1,N-1
		IMAX = K
		L    = K
		IZG  = 0
		AMAX  = ABS( A(K,K) )
110		L = L + 1
		IF (L.GT.N) GOTO 111
		HSP = ABS( A(L,K) )
		IF (HSP.LT.1.E-8) IZG = IZG + 1
		IF (HSP.LE.AMAX) GOTO 110
111		IF (ABS(AMAX).GE.1.E-10) GOTO 133
			AUS = .TRUE.
			RETURN
133		IF (IMAX.EQ.K) GOTO 112
		DO 2 L=K,N
			AZV(L+1)  = A(IMAX,L)
			A(IMAX,L) = A(K,L)
2			A(K,L)    = AZV(L+1)
		AZV(1)  = B(IMAX)
		B(IMAX) = B(K)
		B(K)    = AZV(1)
112		IF (IZG.EQ.(N-K)) GOTO 1
		AMAX = 1. / A(K,K)
		AZV(1) = B(K) * AMAX
		DO 3 M=K+1,N
3			AZV(M+1) = A(K,M) * AMAX
		DO 4 L=K+1,N
			AMAX = A(L,K)
			IF (ABS(AMAX).LT.1.E-8) GOTO 4
			A(L,K) = 0.0
			B(L) = B(L) - AZV(1) * AMAX
			DO 5 M=K+1,N
5				A(L,M) = A(L,M) - AMAX * AZV(M+1)
4		CONTINUE
1	CONTINUE
	DO 6 K=N,1,-1
		AMAX = 0.0
		IF (K.LT.N) THEN
			DO 7 L=K+1,N
7				AMAX = AMAX + A(K,L) * A(N,L)
			ENDIF
		IF (ABS(A(K,K)).LT.1.E-6) THEN
			A(N,K) = 0.0
		ELSE
			A(N,K) = ( B(K) - AMAX ) / A(K,K)
		ENDIF
6	CONTINUE
	RETURN
	END
C
C
	SUBROUTINE LSKNM ( N, M, M0, M1, HM, SC, HX, W, X, Y, VAR, SING)
C --------------------------------------------------------------------
C   DETERMINES LAY-FUNCTIONS AMPLITUDES FOR A NUMBER OF CONSTRAINTS:
C
C	INPUT:	N	NUMBER OF AMPLITUDES ( LAY-FUNCTIONS)
C		M	NUMBER OF CONSTRAINTS
C		M0	NUMBER OF POINT CONSTRAINTS
C		M1	NUMBER OF FIRST DERIVATIVE CONSTRAINTS
C		HM	F PEAK ALTITUDE  [KM]
C		SC(N)	SCALE PARAMETERS FOR LAY-FUNCTIONS  [KM]
C		HX(N)	HEIGHT PARAMETERS FOR LAY-FUNCTIONS  [KM]
C		W(M)	WEIGHT OF CONSTRAINTS
C		X(M)	ALTITUDES FOR CONSTRAINTS  [KM]
C		Y(M)	LOG(DENSITY/NMF2) FOR CONSTRAINTS
C
C	OUTPUT:	VAR(M)	AMPLITUDES
C		SING	=.TRUE.   NO SOLUTION
C ------------------------------------------------------------------------
C
	LOGICAL		SING
	DIMENSION	VAR(N), HX(N), SC(N), W(M), X(M), Y(M),
     &			BLI(5), ALI(5,5), XLI(5,10)
C
	M01=M0+M1
	SCM=0
	DO 1 J=1,5
		BLI(J) = 0.
		DO 1 I=1,5
1			ALI(J,I) = 0. 
	DO 2 I=1,N
		DO 3 K=1,M0
3			XLI(I,K) = RLAY( X(K), HM, SC(I), HX(I) )
		DO 4 K=M0+1,M01
4			XLI(I,K) = D1LAY( X(K), HM, SC(I), HX(I) )
		DO 5 K=M01+1,M
5			XLI(I,K) = D2LAY( X(K), HM, SC(I), HX(I) )
2	CONTINUE
		DO 7 J=1,N
		DO 6 K=1,M
			BLI(J) = BLI(J) + W(K) * Y(K) * XLI(J,K)
			DO 6 I=1,N
6				ALI(J,I) = ALI(J,I) + W(K) * XLI(I,K) 
     &					* XLI(J,K)
7	CONTINUE
	CALL LNGLSN( N, ALI, BLI, SING )
	IF (.NOT.SING) THEN
		DO 8 I=1,N
8			VAR(I) = ALI(N,I)
		ENDIF
	RETURN
	END
C
C
	SUBROUTINE INILAY(NIGHT,XNMF2,XNMF1,XNME,VNE,HMF2,HMF1, 
     &				HME,HV1,HV2,HHALF,HXL,SCL,AMP,IQUAL)
C-------------------------------------------------------------------
C CALCULATES AMPLITUDES FOR LAY FUNCTIONS
C D. BILITZA, DECEMBER 1988
C
C INPUT:	NIGHT	LOGICAL VARIABLE FOR DAY/NIGHT DISTINCTION
C		XNMF2	F2 PEAK ELECTRON DENSITY [M-3]
C		XNMF1	F1 PEAK ELECTRON DENSITY [M-3]
C		XNME	E  PEAK ELECTRON DENSITY [M-3]
C		VNE	ELECTRON DENSITY AT VALLEY BASE [M-3]
C		HMF2	F2 PEAK ALTITUDE [KM]
C		HMF1	F1 PEAK ALTITUDE [KM]
C		HME	E  PEAK ALTITUDE [KM]
C		HV1	ALTITUDE OF VALLEY TOP [KM]
C		HV2	ALTITUDE OF VALLEY BASE [KM]
C		HHALF	ALTITUDE OF HALF-F2-PEAK-DENSITY [KM]
C
C OUTPUT:	HXL(4)	HEIGHT PARAMETERS FOR LAY FUNCTIONS [KM] 
C		SCL(4)	SCALE PARAMETERS FOR LAY FUNCTIONS [KM]
C		AMP(4)	AMPLITUDES FOR LAY FUNCTIONS
C		IQUAL	=0 ok, =1 ok using second choice for HXL(1)
C                       =2 NO SOLUTION
C---------------------------------------------------------------  
	DIMENSION	XX(8),YY(8),WW(8),AMP(4),HXL(4),SCL(4)
	LOGICAL		SSIN,NIGHT
c
c constants --------------------------------------------------------
		NUMLAY=4
		NC1 = 2
		ALG102=ALOG10(2.)
c
c constraints: xx == height	yy == log(Ne/NmF2)    ww == weights
c -----------------------------------------------------------------
		ALOGF = ALOG10(XNMF2)
		ALOGEF = ALOG10(XNME) - ALOGF
		XHALF=XNMF2/2.
		XX(1) = HHALF
 		XX(2) = HV1
		XX(3) = HV2
		XX(4) = HME
		XX(5) = HME - ( HV2 - HME )
		YY(1) = -ALG102
		YY(2) = ALOGEF
		YY(3) = ALOG10(VNE) - ALOGF
		YY(4) = ALOGEF
		YY(5) = YY(3)
		YY(7) = 0.0
		WW(2) = 1.
		WW(3) = 2.
		WW(4) = 5.
c
c geometric paramters for LAY -------------------------------------
c difference to earlier version:  HXL(3) = HV2 + SCL(3)
c
		SCL0 = 0.7 * ( 0.216 * ( HMF2 - HHALF ) + 56.8 )
		SCL(1) = 0.8 * SCL0
		SCL(2) = 10.
 		SCL(3) = 9.
	  	SCL(4) = 6.
		HXL(3) = HV2
c
C DAY CONDITION--------------------------------------------------
c earlier tested: 	HXL(2) = HMF1 + SCL(2)
c 
	    IF(NIGHT) GOTO 7711
		NUMCON = 8
		HXL(1) = 0.9 * HMF2
		  HXL1T  = HHALF
		HXL(2) = HMF1
		HXL(4) = HME - SCL(4)
		XX(6) = HMF1
 		XX(7) = HV2
		XX(8) = HME
		YY(8) = 0.0
		WW(5) = 1.
	   	WW(7) = 50.
	   	WW(8) = 500.
c without F-region ----------------------------------------------
		IF(XNMF1.GT.0) GOTO 100
			HXL(2)=(HMF2+HHALF)/2.
			YY(6) = 0.
			WW(6) = 0.
			WW(1) = 1.
			GOTO 7722
c with F-region --------------------------------------------
100		YY(6) = ALOG10(XNMF1) - ALOGF
		WW(6) = 3.
		IF((XNMF1-XHALF)*(HMF1-HHALF).LT.0.0) THEN
		  WW(1)=0.5
		ELSE
		  ZET = YY(1) - YY(6)
		  WW(1) = EPST( ZET, 0.1, 0.15)
		ENDIF
		IF(HHALF.GT.HMF1) THEN
		  HFFF=HMF1
		  XFFF=XNMF1
		ELSE
		  HFFF=HHALF
		  XFFF=XHALF
		ENDIF
	        GOTO 7722
c
C NIGHT CONDITION---------------------------------------------------
c different HXL,SCL values were tested including: 
c	SCL(1) = HMF2 * 0.15 - 27.1	HXL(2) = 200.	
c	HXL(2) = HMF1 + SCL(2)		HXL(3) = 140.
c  	SCL(3) = 5.			HXL(4) = HME + SCL(4)
c	HXL(4) = 105.			
c
7711		NUMCON = 7
		HXL(1) = HHALF
		  HXL1T  = 0.4 * HMF2 + 30.
		HXL(2) = ( HMF2 + HV1 ) / 2.
		HXL(4) = HME
		XX(6) = HV2
 		XX(7) = HME
		YY(6) = 0.0
		WW(1) = 1.
		WW(3) = 3.
		WW(5) = 0.5
		WW(6) = 50.
 		WW(7) = 500.
		HFFF=HHALF
		XFFF=XHALF
c
C are valley-top and bottomside point compatible ? -------------
C
7722	IF((HV1-HFFF)*(XNME-XFFF).LT.0.0) WW(2)=0.5
	IF(HV1.LE.HV2+5.0) WW(2)=0.5
c
C DETERMINE AMPLITUDES-----------------------------------------
C
	    NC0=NUMCON-NC1
	    IQUAL=0
2299	    CALL LSKNM(NUMLAY,NUMCON,NC0,NC1,HMF2,SCL,HXL,WW,XX,YY,
     &		AMP,SSIN)
	   	IF(IQUAL.gt.0) GOTO 1937
	    IF((ABS(AMP(1)).GT.10.0).OR.(SSIN)) THEN
		IQUAL=1
		HXL(1)=HXL1T
		GOTO 2299
		ENDIF
1937	    IF(SSIN) IQUAL=2
	    RETURN
	    END
c
c
	subroutine ioncom(h,z,f,fs,t,cn)
c---------------------------------------------------------------
c ion composition model
c A.D. Danilov and A.P. Yaichnikov, A New Model of the Ion
c   Composition at 75 to 1000 km for IRI, Adv. Space Res. 5, #7,
c   75-79, 107-108, 1985
c
c 	h	altitude in km
c	z	solar zenith angle in radians
c	f	latitude in radians
c	fs	10.7cm solar radio flux
c	t	season (month)
c	cn(1)   O+  relative density in percent
c	cn(2)   H+  relative density in percent
c	cn(3)   N+  relative density in percent
c	cn(4)   He+ relative density in percent
c	cn(5)   NO+ relative density in percent
c	cn(6)   O2+ relative density in percent
c	cn(7)   cluster ions  relative density in percent
c---------------------------------------------------------------
c
	dimension	cn(7),cm(7),hm(7),alh(7),all(7),beth(7),
     &			betl(7),p(5,6,7),var(6),po(5,6),ph(5,6),
     &			pn(5,6),phe(5,6),pno(5,6),po2(5,6),pcl(5,6)

	data po/4*0.,98.5,4*0.,320.,4*0.,-2.59E-4,2.79E-4,-3.33E-3,
     &		-3.52E-3,-5.16E-3,-2.47E-2,4*0.,-2.5E-6,1.04E-3,
     &		-1.79E-4,-4.29E-5,1.01E-5,-1.27E-3/
	data ph/-4.97E-7,-1.21E-1,-1.31E-1,0.,98.1,355.,-191.,
     &		-127.,0.,2040.,4*0.,-4.79E-6,-2.E-4,5.67E-4,
     &		2.6E-4,0.,-5.08E-3,10*0./
	data pn/7.6E-1,-5.62,-4.99,0.,5.79,83.,-369.,-324.,0.,593.,
     &		4*0.,-6.3E-5,-6.74E-3,-7.93E-3,-4.65E-3,0.,-3.26E-3,
     &		4*0.,-1.17E-5,4.88E-3,-1.31E-3,-7.03E-4,0.,-2.38E-3/
	data phe/-8.95E-1,6.1,5.39,0.,8.01,4*0.,1200.,4*0.,-1.04E-5,
     &		1.9E-3,9.53E-4,1.06E-3,0.,-3.44E-3,10*0./ 
	data pno/-22.4,17.7,-13.4,-4.88,62.3,32.7,0.,19.8,2.07,115.,
     &		5*0.,3.94E-3,0.,2.48E-3,2.15E-4,6.67E-3,5*0.,
     &		-8.4E-3,0.,-3.64E-3,2.E-3,-2.59E-2/
	data po2/8.,-12.2,9.9,5.8,53.4,-25.2,0.,-28.5,-6.72,120.,
     &		5*0.,-1.4E-2,0.,-9.3E-3,3.3E-3,2.8E-2,5*0.,4.25E-3,
     &		0.,-6.04E-3,3.85E-3,-3.64E-2/
	data pcl/4*0.,100.,4*0.,75.,10*0.,4*0.,-9.04E-3,-7.28E-3,
     &		2*0.,3.46E-3,-2.11E-2/

	DO 8 I=1,5
	DO 8 J=1,6
		p(i,j,1)=po(i,j)
		p(i,j,2)=ph(i,j)
		p(i,j,3)=pn(i,j)
		p(i,j,4)=phe(i,j)
		p(i,j,5)=pno(i,j)
		p(i,j,6)=po2(i,j)
		p(i,j,7)=pcl(i,j)
8	continue

	s=0.
	do 5 i=1,7
	  do 7 j=1,6
		var(j) = p(1,j,i)*cos(z) + p(2,j,i)*cos(f) +
     &  		 p(3,j,i)*cos(0.013*(300.-fs)) +
     &			 p(4,j,i)*cos(0.52*(t-6.)) + p(5,j,i)
7	  continue
	  cm(i)  = var(1)
	  hm(i)  = var(2)
	  all(i) = var(3)
	  betl(i)= var(4)
	  alh(i) = var(5)
	  beth(i)= var(6)
	  hx=h-hm(i)
 	  if(hx) 1,2,3
1		cn(i) = cm(i) * exp( hx * (hx * all(i) + betl(i)) )
		goto 4
2	  	cn(i) = cm(i)
		goto 4
3		cn(i) = cm(i) * exp( hx * (hx * alh(i) + beth(i)) )
4	  continue
	  if(cn(i).LT.0.005*cm(i)) cn(i)=0.
	  if(cn(i).GT.cm(i)) cn(i)=cm(i)
	  s=s+cn(i)
5	continue
	do 6 i=1,7
6		cn(i)=cn(i)/s*100.
	return
	end

