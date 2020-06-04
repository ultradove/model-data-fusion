module obsdrivers
real ::maxt,mint,rad,nit,day,ca,lat,yearday,projectday
integer, parameter :: nday=	!n year run
real, parameter ::  psid=-2.0		!water potential gradient
real, parameter ::  rtot=1.0		!hydraulic resistance
end module obsdrivers


Program DALEC_deciduous
use obsdrivers
implicit none
real :: LAI,Trate,GPP,gdd
real :: P(17),LMA,max_fol,multtl,multtf,ralabfrom,ralabto
real :: Ra,Af,Aw,Ar,Lf,Lw,Lr,Rh1,Rh2,D,G,Atolab,Afromlab,npp
real :: Cf,Cw,Cr,Clab,Clit,Csom
integer :: i
open(unit=25,file='/drivers.csv',status='old')	!contains drivers
open(unit=26,file='/output.csv',status='unknown')	!outputs
open(unit=27,file='/initial.csv',status='old')	!inputs

write(26,*)'day,G,Ra,Af,Aw,Ar,Atolab,Afrlab,Lf,Lw,Lr,Rh1,Rh2,D,Cf,Cw,Cr,Clab,Clit,Csom,NEE'

! INPUT PARAMETERS P1-17 AS AN ARRAY P
! INITIALISE C STOCKS AND SITE-SPECIFIC DATA USING TABLE A5 

read(27,*)(p(i),i=1,17)
read(27,*)Cf,Cr,Cw,Clit,Csom,Clab,lat,nit,LMA

lat=lat.*3.14/180.		!convert to radians

DO i=1,nday

!Read in drivers for day
Read(25,*)projectday,mint,maxt,rad,ca,yearday

	IF(yearday.le.100)then
		gdd=0.
		max_fol=1.
	endif
!time switches
	multtf=1.		!defaults
	multtl=0.
	gdd=gdd+0.5*(maxt+mint)	!growing degree day heat sum from day 100
	If(gdd.lt.p(12))THEN	!winter
		multtf=1.	!turnover of foliage on
		multtl=0.	!turnover of labile C off
	ELSE
		IF(max_fol.eq.1)THEN	!spring
			multtl=1.
			multtf=0.
		ELSE			!summer
			multtl=0.
			multtf=0.
		ENDIF
	ENDIF
	IF((Cf.ge.p(17)).or.(yearday.ge.200))THEN	
		max_fol=0.
		multtl=0.
	ENDIF
	If((yearday.ge.200).and.(mint.lt.p(13)))THEN		!drop leaves - N hemisphere
		multtf=1.
	ENDIF
! Carbon fluxes
	LAI=max(0.1,Cf/LMA)	
	G=GPP(lai,p(11))
	Trate=0.5*exp(p(10)*0.5*(maxt+mint))
	ralabfrom=p(15)*Clab*p(16)*multtl*Trate
	ralabto=(1.-p(14))*p(5)*Cf*p(16)*multtf*Trate
	Ra = p(2)*G + ralabfrom + ralabto			
	npp = (1-p(2))*G
	Af= min(p(17)-Cf,npp*p(3))*multtl+Afromlab	
	npp = npp-min(p(17)-Cf,npp*p(3))*multtl			
	Ar= npp*p(4)						
	Aw= (1-p(4))*npp					
	Lf = p(5)*Cf*p(14)*multtf	
	Lw = p(6)*Cw					
	Lr = p(7)*Cr					
	Rh1 = p(8)*Clit*Trate				
	Rh2 = p(9)*Csom*Trate				
	D = p(1)*Clit*Trate				
	Atolab = (1.-p(14))*p(5)*Cf*(1.-p(16))*multtf*Trate	!Atolab: 
	Afromlab = p(15)*Clab*(1.-p(16))*multtl*Trate				

!Pools:
	Cf = Cf + Af - Lf - Atolab - ralabto		
	Cw = Cw+ Aw - Lw								
	Cr =Cr+ Ar - Lr								
	Clit =Clit + Lf + Lr - Rh1 - D		
	Csom = Csom+ D - Rh2 +Lw				
	Clab=Clab+Atolab-Afromlab-ralabfrom					

write (26,'(I6,",",20(F9.3,","))') i,G,Ra,Af,Aw,Ar,Atolab,Afromlab,Lf,Lw,Lr,Rh1,Rh2,D,Cf,Cw,Cr,Clab,Clit,Csom,(Ra+Rh1+Rh2-G)

ENDDO

END

!------------------------------------------------------------------



