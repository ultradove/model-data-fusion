module obsdrivers
real ::maxt,mint,rad,nit,day,ca,lat,yearday,projectday
integer, parameter :: nday=	!n year run
real, parameter ::  psid=-2.0		!water potential gradient
real, parameter ::  rtot=1.0		!hydraulic resistance
end module obsdrivers


Program DALEC_evergreen
use obsdrivers
implicit none
real :: LAI,Trate,GPP,G
real :: P(11),LMA
real :: Ra,Af,Aw,Ar,Lf,Lw,Lr,Rh1,Rh2,D ! Fluxes
real :: Cf,Cw,Cr,Clit,Csom ! Stocks
integer :: i
open(unit=25,file='/drivers.csv',status='old')	!contains drivers
open(unit=26,file='/output.csv',status='unknown')	!outputs
open(unit=27,file='/initial.csv',status='old')	!inputs

write(26,*)'day,G,Ra,Af,Aw,Ar,Lf,Lw,Lr,Rh1,Rh2,D,Cf,Cw,Cr,Clit,Csom,NEE'

! INPUT PARAMETERS P1-11 AS AN ARRAY P
! INITIALISE C STOCKS AND SITE-SPECIFIC DATA USING TABLE A5 

read(27,*)(p(i),i=1,11)
read(27,*)Cf,Cr,Cw,Clit,Csom,lat,nit,LMA

lat=lat*3.14/180.		!convert to radians

DO i=1,nday
Read(25,*)projectday,mint,maxt,rad,ca,yearday
! Carbon fluxes
	LAI=max(0.1,Cf/LMA)	
	G=GPP(lai,p(11))
	Trate=0.5*exp(p(10)*0.5*(maxt+mint))
	Ra = p(2)*G				
	Af = (G-Ra)*p(3)		
	Ar = (G-Ra-Af)*p(4)	
	Aw = G-Ra-Af-Ar		
	Lf = p(5)*Cf			
	Lw = p(6)*Cw			
	Lr = p(7)*Cr			
	Rh1 = p(8)*Clit*Trate		
	Rh2 = p(9)*Csom*Trate		
	D = p(1)*Clit*Trate		
! Pools:
	Cf = Cf + Af - Lf						
	Cw = Cw+ Aw - Lw							
	Cr =Cr+ Ar - Lr							
	Clit =Clit + Lf + Lr - Rh1 - D		
	Csom = Csom+ D - Rh2 +Lw				
	write (26,'(I6,",",17(F9.3,","))') i,G,Ra,Af,Aw,Ar,Lf,Lw,Lr,Rh1,Rh2,D,Cf,Cw,Cr,Clit,Csom,(Ra+Rh1+Rh2-G)
ENDDO

END

!------------------------------------------------------------------

REAL FUNCTION GPP(lai,p11)	!Aggregated Canopy Model
USE obsdrivers
implicit none
real, intent(in) :: lai, p11
real :: cps,e0,gs,ci,qq,pp,trange,model,dec,mult,dayl,pi,a(10)
pi=3.1416
trange=0.5*(maxt-mint)
! parameter values
a(1)=    p11
a(2)=    0.0156935
a(3)=    4.22273
a(4)=    208.868
a(5)=    0.0453194
a(6)=    0.37836
a(7)=    7.19298
a(8)=    0.011136
a(9)=    2.1001
a(10)=   0.789798

gs=abs(psid)**a(10)/((a(6)*rtot+trange))
pp=LAI*Nit/gs*a(1)*exp(a(8)*maxt)
qq=a(3)-a(4)
ci=0.5*(Ca+qq-pp+((Ca+qq-pp)**2-4*(Ca*qq-pp*a(3)))**0.5)
e0=a(7)*LAI**2/(LAI**2+a(9))	 
dec=-23.4*cos((360.*(yearday+10.)/365.)*pi/180.)*pi/180.	
mult=tan(lat)*tan(dec)
IF(mult.ge.1.)THEN
   dayl=24.0
ELSE IF(mult.le.-1.)THEN
   dayl=0.
ELSE
   dayl=24.*acos(-mult)/pi
ENDIF

cps=e0*rad*gs*(Ca-ci)/(e0*rad+gs*(Ca-ci))
model=cps*(a(2)*dayl+a(5))		
gpp=model
END function GPP

!------------------------------------------------------------------


