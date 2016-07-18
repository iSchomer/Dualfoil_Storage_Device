c***********************************************************************
c     dualfoil.f  Paul Albertus's thesis version  May 11, 2009'

c  Dual lithium ion insertion cell
c  Copyright Marc Doyle, Karen Thomas, Paul Albertus and John Newman 2009.
c  You may make a copy of this program which you may
c  personally and freely use in its unaltered form.
c  You may distribute this program subject to the
c  conditions that it be made freely available and
c  that any duplication of this program must be
c  essentially unaltered and must include this notice.
c
c  We make no warranties, express or implied, that
c  this program is free of errors or that it will
c  meet the requirements of your application.  The
c  author and publisher disclaim all liablility for
c  direct or consequential damages resulting from
c  use of this program.
c
c Based on the original articles:
c Marc Doyle, Thomas F. Fuller, and John Newman,
c J. Electrochem. Soc., 140 (1993), 1526-1533.
c Thomas F. Fuller, Marc Doyle, and John Newman,
c J. Electrochem. Soc., 141 (1994), 1-10.
c Thomas F. Fuller, Marc Doyle, and John Newman,
c J. Electrochem. Soc., 141 (1994), 982-990.
c Paul Albertus, Jake Christensen, and John Newman,
c J. Electrochem. Soc., 155 (2008), A48-A60
c
c     This program was developed for lithium and lithium-ion
c     batteries, with the flexibility that a number of positive
c     and negative active materials of the insertion or intercalation
c     type can be selected or added, in addition to a lithium foil
c     electrode.  Several electrolytes are also provided, and
c     these can be added to by providing properties in subroutine
c     prop.  The nickel/metal hydride battery in a KOH solution is 
c     also included, since it follows the pattern of being a 
c     dual-insertion system.  Primary cells, as well as rechargeable 
c     cells, can be simulated with this program.
c
c To identify a Li foil, look for either n1.ne.0 or n1.eq.0
c
c Major changes to this code have been made by John Newman,
c Karen Thomas, and Paul Albertus relative to the original version.

c***********************************************************************
      implicit real*8(a-h,o-z)
      character *30 filin, filout
      parameter(maxt=900)
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
      common /calc/ ai(maxt,5),ai2(maxt,5),ts(maxt),h,h1,h2,h3,hcn,
     1hcp,rr,rrmax,cuL,sumpa(5,221),Rad1pa(5),Rad3pa(5),area1pa(5)
     &,area3pa(5),mcL
      common/pindiv/ vf1(5),vf3(5),cot1(9),cot3(9),rs1(9),rs3(9)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/power/ ed,Vold,ranode(20),rcathde(20),heat,qlosstot
      common/ssblock/ xp0(16),xx0(16,221),term(221)
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/imp/ vreal(110),vimag(110),phi(110),zmag(110),omega(510),
     1vreala(110),vrealc(110),vimaga(110),vimagc(110),
     1phia(110),phic(110),zmaga(110),zmagc(110)
      common/cprop/ sig3,area3,rka3(9),rka3save,ct3(9),dfs3(9),
	1Rad3,cap1,cap3,sig1,area1,rka1(9),rka1save,ct1(9),dfs1(9),
     1Rad1,tw,dfs1save,dfs3save
      common/tprop/df(221),cd(221),tm(221),ddo2(221),ddh2(221),
     1ddf(221),dcd(221),dtm(221),dfu(221),d2fu(221),do2(221),dh2(221)
      common/temp/ thk,htc,dudt,Cp,dens,tam,g0(9),
     &qq,qloss,residm,ncell,lht
      common/side/rksc1,c1init,c2init,rksa1,term_s1(221),vol,
     &rksa2,rksa3,rksc3,rksc2,UsO2,UsH2,term_s2(221),nside
      common /vdc/ aa(1,1),bb(1,1),cc(1,150),dd(1,3),gg(1),xxx(1,1),
     &yy(1,1),hha(20),hhc(20),cssold(220,150,20),css(220,150,20),
     &utz(9,221),dsold(220,150,20),ds(220,150,20),time,
     &nn,nnj,np,mvdc1,mvdc3,lims
      common/gas/ epg1,epg2,epg3
      common /maxpow/ lcount
      common/RG/ RG,RGn,RGp,RGext
      common/resistances/R_total,R_anode,R_sep,R_cathode
      dimension terms(221),tt(200),cu(200),tot(200),mc(200),taper(200)
      dimension vcutlo(200),vcuthi(200)
      dimension term_s1s(221)
      dimension rad1paf(5),rad3paf(5)
cSP
      logical restart ! logical variable to check for restart
      logical newrun ! logical variable to check for newrun
   44 format(/'            mass  = ',f7.4,' kg/m2')
   45 format(' specific energy whole run  = ',f8.2,' W-h/kg')
   46 format(' specific power whole run  = ',f8.2,' W/kg')
   47 format(' total heat = ', f15.4,' W-h/m2')
   48 format(' specific energy segment = ', f8.2,' W-h/kg')
   49 format(' specific power segment  = ', f8.2,' W/kg')
   57 format(e15.6,', ',e15.6,', ',e15.6)
   89 format(f8.1,',',f8.1,',',f8.1,',',i3,',',g10.5,',',f5.2)
   90 format (g10.3,',')

c *** DEFINE THE INPUT AND OUTPUT FILES *** 

      open(3,file='halfcells.out',status='unknown')
      write (3,*) ' Time (min)    V neg      V pos       Cur (A/m2)   '
     &,' Temp (C)   Heat Gen (W/m3)'
c     print *, 'Enter input file name, press return'
c     read *, filin
      open (1, FILE = 'dualfoil5.in', status = 'old')
c     open (1, FILE = filin, status = 'unknown')
c     print *, 'Enter output file name, press return'
c     read *, filout
      open (2, file = 'dualfoil5.out', status = 'unknown')
c     open (2, file = filout, status = 'unknown')
      open (4,file='profiles.out',status='unknown')  !for cell sandwich profiles
      open (5,file='li-ion-ebar.in', status='old')   !activation energies
      open (6,file='resistances.out',status='unknown') !for cell resistances
      open (7,file='solidprof.out',status='unknown') !for particle profile
cSP
      open (11,file='df_caebat.out',status='unknown') !for CAEBAT output
      open (12,file='df_restart.dat',status='unknown') !for DF restart
      open (14,file='df_sources.dat',status='unknown') !for DF sources
c
      data fc/96487.0d0/, r/8.314d0/, pi/3.141592653589d0/
      data ed/0.d0/, Vold/0.d0/

c *** DEFINE THE VARIABLES ***

cSP
      dt_out = 30 ! variable to write out df_out

      npa=1 ! Number of particle types

      if (npa.ne.1.and.imp.eq.1) print *,'npa must be 1 for imp=1'

c Here is a full variable list. Not all of these will be used.
c     Variable #      Description
c     1               Liquid phase concentration
c     2               Liquid phase potential
c     3 to 2+npa      Solid phase concentrations   
c     3+npa           Liquid phase current
c     4+npa to 3+2npa Butler-Volmer kinetics and pore wall flux
c     4+2npa          Solid phase potential
c     5+2npa          Variable to help with constant load, power, potential

c     Variable and equation 1 are not given a letter name.
c     They are for the liquid-phase concentration.

      ip2=2 ! equation number for Ohm's law in solution.
      kp2=2 ! variable number for PHI_2, solution potential.

c     The variable and equation here are called 2+mpa.
c     They are for the solid-phase material balance.

      ii2=3+npa ! equation number for current density i2
      ki2=3+npa ! variable number for current density i2

c     The variable and equation here are called 3+npa+mpa
c     They are for the pore wall flux and Butler-Volmer kinetics.
c     (formerly i2div and kj)

      ip1=4+2*npa ! equation number for Ohm's law in the matrix.
      kp1=4+2*npa ! variable number for PHI_1, matrix potential.

	imode=5+2*npa ! equation number for mode converge
	kmode=5+2*npa ! variable number for mode converge

      imb1=6+2*npa ! equation number (side reaction 1 matl balance)
      kS1=6+2*npa ! variable number (side reaction 1 matl balance)

      iSk1=7+2*npa ! equation number (side reaction 1 kinetics)
      kj1=7+2*npa ! variable number (side reaction 1 kinetics)

      imb2=8+2*npa ! equation number (side reaction 2 matl balance)
      kS2=8+2*npa ! variable number (side reaction 2 matl balance)

      iSk2=9+2*npa ! equation number (side reaction 2 kinetics)
      kj2=9+2*npa ! variable number (side reaction 2 kinetics)

      iSk3=10+2*npa ! equation number (side reaction 3 kinetics)
      kj3=10+2*npa ! variable number (side reaction 3 kinetics)

c     Define a grid resistance here 
      RG=0.0d0 !total resistance in foils, leads, and contacts, ohm-m2
      RGn=RG/3.0d0 ! resistance affecting negative half cell
      RGp=RG-RGn ! resistance affecting positive half cell
      RGext=0.0d0 ! resistance outside cell
c
c***********************************************************************

c *** READ IN PARAMETERS AND BOUNDARY CONDITIONS ***

      read (1,*) restart ! cSP - logical to say whether this 
c                        !       is a new run or a restart
      if(restart) then
         newrun = .false.
      else
         newrun = .true.
      end if 

      read (1,*) lim !limit on number of iterations
      read (1,*) h1  !thickness of negative electrode (m)
      read (1,*) h2  !thickness of separator (m)
      read (1,*) h3  !thickness of positive electrode (m)
      read (1,*) hcn !thickness of negative electrode current collector (m)
      read (1,*) hcp !thickness of positive electrode current collector (m)
      thk=h1+h2+h3
      read (1,*) n1  !number of nodes in negative electrode
c     If negative electrode is metal foil, let n1 = 0
      read (1,*) n2  !number of nodes in separator
      read (1,*) n3  !number of nodes in positive electrode
      read (1,*) n4  !number of nodes in solid particle
      read (1,*) mvdc1 !flag for variable solid diff coeff, anode
      read (1,*) mvdc3 !flag for variable solid diff coeff, cathode
      read (1,*) lims  !limit on number of iterations in solid phase
cSP
      read (1,*) t   !temperature (K)
      write (2,1101) lim,1.d6*h1,1.d6*h2,1.d6*h3,1.d6*hcn,1.d6*hcp
     &,n1,n2,n3,t
      n2=n2+1
      nj=n1+n2+n3 !total number of nodes

cSP
      dt_out_incr = 0.0

      nnj=n4      !number of nodes for particle radius
c
cSP
      read (1,*) xx(1,n1+2) ! initial concentration (mol/m3)
      read (1,*) csx  !initial stochiometric parameter for negative
      read (1,*) csy  !initial stochiometric parameter for positive
      read (1,*) tmmax !maximum time step size (s)
      read (1,*) dfs1(1) !diffusion coefficient in negative solid (m2/s)
      read (1,*) dfs3(1) !diffusion coefficient in positive solid (m2/s)
      dfs1save=dfs1(1) !save the original values because of temp dependence
      dfs3save=dfs3(1)
      read (1,*) Rad1 !radius of negative particles (m)
      read (1,*) Rad3 !radius of positive particles (m)
      write (2,1102) xx(1,n1+2),csx,csy,tmmax,dfs1(1),dfs3(1),
     &1.d6*Rad1,1.d6*Rad3
c     If negative electrode is metal foil, let ep1=epp1=epf1=0.0
      read (1,*) ep1  !volume fraction of electrolyte in negative electrode
      read (1,*) epp1 !volume fraction of polymer phase in negative electrode
      read (1,*) epf1 !volume fraction of inert filler in negative electrode
      read (1,*) epg1 !volume fraction of gas in negative
      read (1,*) ep2  !volume fraction of electrolyte in separator
      read (1,*) epp2 !volume fraction of polymer phase in separator
      read (1,*) epg2 !volume fraction gas in separator
      read (1,*) ep3  !volume fraction of electrolyte in positive electrode
      read (1,*) epp3 !volume fraction of polymer phase in positive electrode
      read (1,*) epf3 !volume fraction of inert filler in positive electrode
      read (1,*) epg3 !volume fraction of gas in positive
      read (1,*) sig1 !conductivity of solid negative matrix (S/m)
      read (1,*) sig3 !conductivity of solid positive matrix (S/m)
      read (1,*) rka1(1) !reaction rate constant for negative reaction
      read (1,*) rka3(1) !reaction rate constant for positive reaction
      rka1save=rka1(1)
      rka3save=rka3(1)
      read (1,*) ranode(1)  !anode film resistance (out of place)
      read (1,*) rcathde(1) !cathode film resistance (out of place)
      write (2,1103) ep1,epp1,epf1,ep2,epp2,ep3,epp3,epf3,sig1,
     & sig3,rka1(1),rka3(1)
      read (1,*) re  ! density of electrolyte (kg/m3)
      read (1,*) rf  ! density of inert filler (kg/m3)
      read (1,*) rpl ! density of polymer phase (kg/m3)
      read (1,*) rc  ! density of separator material (kg/m3)
      read (1,*) rcn ! density of negative current collector (kg/m3)
      read (1,*) rcp ! density of positive current collector (kg/m3)
      write (2,1104) re,rf,rpl,rc,rcn,rcp
      read (1,*) htc  !heat-transfer coefficient with external medium (W/m2K)
      read (1,*) Cp   !heat capacity of cell (J/kgK)
      read (1,*) Tam  !ambient temperature (K)
      read (1,*) ncell !number of cells in a cell stack
      read (1,*) lht  !0 uses htc, 1 calcs htc, 2 isothermal
      read (1,*) il1   !1 for long print-out 0 for short print-out
      read (1,*) il2   !1/il2 = fraction of nodes in long print-out
      read (1,*) il3   !1/il3 = fraction of time steps in long print-out
      read (1,*) imp  ! 0 for no impedance, 1 for impedance
      read (1,*) capp1 ! capacitance of negative material (F/m2)
      read (1,*) capp3 ! capacitance of positive material (F/m2)
      read (1,*) jsol  ! calculate solid profiles if 1<jsol<nj
      read (1,*) nside ! flag for side reaction
      read (1,*) rksa1 ! rate constant side reaction anode
      read (1,*) rksc1 ! rate constant side reaction cathode
      read (1,*) rksa2 ! rate constant side reaction anode
      read (1,*) rksc2 ! rate constant side reaction cathode
      read (1,*) rksa3 ! rate constant side reaction anode
      read (1,*) rksc3 ! rate constant side reaction cathode
      c1init=.1d0      ! init conc of side reaction species 1 (mol/m^3)
      c2init=3.0d0     ! init conc of side reaction species 2 (mol/m^3)
      residm=0.0d0     ! Residual mass not in cell sandwich (kg/m^2)
      write (2,1105) ranode(1),rcathde(1),htc,dudt,Cp,residm,
     &tam,ncell,lht
      write (2,1116) nside
      if (nside.ge.1) then
      write (2,1111) rksa1,rksc1,rksa2,rksc2,rksa3,rksc3
      endif
      read (1,*) nneg  ! designates negative electrode system
      read (1,*) nprop ! designates electrolyte system
      read (1,*) npos  ! designates positive electrode system
      read (1,*) lcurs ! number of current changes
      write (2,1106) il1,il2,il3,imp,capp1,capp3,lcurs
      read (5,*) EbarS1  !activation energy, anode solid diffusion
      read (5,*) EbarS3  !activiation energy, cathode solid diffusion
      read (5,*) Ebarkap !activation energy electrolyte conductivity
      read (5,*) EbarD   !activation energy electrolyte diffusion
      read (5,*) Ebarka  !activation energy negative kinetics
      read (5,*) Ebarkc  !activation energy positive kinetics
      read (5,*) Ebarks1a  !activation energy O2 side rxn
      read (5,*) Ebarks1c  !activation energy O2 side rxn
      read (5,*) Ebarks2a  !activation energy H2 side rxn
      read (5,*) Ebarks2c  !activation energy H2 side rxn
      read (5,*) Ebarks3a  !activation energy shuttle side rxn
      read (5,*) Ebarks3c  !activation energy shuttle side rxn
      read (5,*) Ebarr1  !activation energy, film resistance anode
      read (5,*) Ebarr3  !activation energy, film resistance cathode

      nn=1 ! used for variable solid-phase diffusion coefficient.
c     n is number of equations, always include equation that 
c     helps converge for load, power, potential
c     side reactions only work with one particle type (npa=1)

c *** SPECIFY THE NUMBER OF VARIABLES TO USE ***

      if (nside.eq.0) n=5+2*npa  !no side reaction, use six eqns
      if (nside.eq.1) n=7+2*npa  ! first side reaction, mass balance & kinetic
      if (nside.eq.2) n=9+2*npa  ! second side reaction, mass balance & kinetic
      if (nside.eq.3) n=10+2*npa  ! third side reaction, only kinetic

      do 555 i = 1, lcurs
      read (1,*) cu(i),tt(i),mc(i),vcutlo(i),vcuthi(i)
  555 continue

c *** WRITE THE INPUT FILE INTO THE OUTPUT FILE ***

      print *, 'Now running DUAL...'
 1101 format (i7,'  lim, limit on number of iterations'
     &/1x,f6.2,'  h1,  thickness of negative electrode (microns)'
     &/1x,f6.2,'  h2,  thickness of separator (microns)'
     &/1x,f6.2,'  h3,  thickness of positive electrode (microns)'
     &/1x,f6.2,'  hcn, ',
     &'thickness of negative electrode current collector (microns)'
     &/1x,f6.2,'  hcp, thickness of positive electrode current'
     &,' collector (microns)'
     &/i7,'  n1,  number of nodes in negative electrode'
     &/i7,'  n2,  number of nodes in separator'
     &/i7,'  n3,  number of nodes in positive electrode'
     &/1x,f6.2, '  T,   temperature (K)')
 1102 format (/1x,f6.1,'  xx(1,n1+2), initial concentration (mol/m3)'
     &/1x,f6.4,'  csx,   initial stoichiometric parameter for negative'
     &/1x,f6.4,'  csy,   initial stoichiometric parameter for positive'
     &/1x,f6.1,'  tmmax, maximum time step size (s)'
     &/1x,e6.1,'  dfs1(1), diffusion coefficient in negative solid 
     &(m2/s)'
     &/1x,e6.1,'  dfs3(1), diffusion coefficient in positive solid 
     &(m2/s)'
     &/1x,f6.2,'  Rad1,  radius of negative particles (microns)'
     &/1x,f6.2,'  Rad3,  radius of positive particles (microns)')
 1103 format (/1x,f6.3,'  ep1,'
     &,'   volume fraction of electrolyte in negative electrode'
     &/1x,f6.3,'  epp1,'
     &,'  volume fraction of polymer phase in negative electrode'
     &/1x,f6.3,'  epf1,'
     &,'  volume fraction of inert filler in negative electrode'
     &/1x,f6.3,'  ep2,   volume fraction of electrolyte in separator'
     &/1x,f6.3,'  epp2,  volume fraction of polymer phase in separator'
     &/1x,f6.3,'  ep3,'
     &,'   volume fraction of electrolyte in positive electrode'
     &/1x,f6.3,'  epp3,'
     &,'  volume fraction of polymer phase in positive electrode'
     &/1x,f6.3,'  epf3,'
     &,'  volume fraction of inert filler in positive electrode'
     &/1x,g8.2,'  sig1,  conductivity of negative matrix (S/m)'
     &/1x,g8.2,'  sig3,  conductivity of positive matrix (S/m)'
     &/1x,e6.1,'  rka1(1),reaction rate constant for negative reaction'
     &/1x,e6.1,'  rka3(1),reaction rate constant for positive reaction')
 1104 format (/1x,f6.1,'  re,   density of electrolyte (kg/m3)'
     &/1x,f6.1,'  rf,   density of inert filler (kg/m3)'
     &/1x,f6.1,'  rpl,  density of polymer phase (kg/m3)'
     &/1x,f6.1,'  rc,   density of separator material (kg/m3)'
     &/1x,f6.1,'  rcn,  density of negative current collector (kg/m3)'
     &/1x,f6.1,'  rcp,  density of positive current collector (kg/m3)')
 1105 format (/1x,f10.6,'  ranode(1),   anode film resistance (ohm-m2)'
     &/1x,f10.6,'  rcathde(1),  cathode film resistance (ohm-m2)'
     &/1x,f6.2,'  htc,   heat-transfer coefficient with'
     &,' external medium (W/m2K)'
     &/1x,f10.6,'  dUdT,  temperature coefficient of EMF (V/K)'
     &/1x,f6.1,'  Cp,    heat capacity of cell (J/kg-K)'
     &/1x,f6.2,'  residm,    residual mass (kg/m2)'
     &/1x,f6.2,'  Tam,   ambient temperature (K)'
     &/i7,'  ncell, number of cells in a cell stack'
     &/i7,'  lht,   0 uses htc,  1 calcs htc,  2 isothermal')
 1106 format (/i7,'  il1,   1 for long print-out  0 for short print-out'
     &/i7,'  il2,   prints every il2 th node in long print-out'
     &/i7,'  il3,   prints every il3 th time step in long print-out'
     &/i7,'  imp, 0 for no impedance, 1 for impedance'
     &/1x,f6.2,'  capp1,  capacitance of negative material'
     &,' (F/m2)'
     &/1x,f6.2,'  capp3,  capacitance of positive material'
     &,' (F/m2)'
     &/i7,'  lcurs, number of current changes')
 1107 format ('   Time     Util N  Util P  Cell Pot   Uocp      Curr',
     &'      pH2     pO2   Total P  Temp   heatgen')
 1108 format ('   (min)       x       y      (V)       (V)      (A/m2)',
     %'   (bar)   (bar)   (bar)   (C)     (W/m2)')
 1109  format ('   Time     Util N  Util P  Cell Pot   Uocp      Curr',
     &'      Temp   heatgen')
 1110 format ('   (min)       x       y      (V)       (V)      (A/m2)',
     %'    (C)    (W/m2)')
 1111 format (
     &g10.3,'  rksa1,   rate constant 1 for negative side reaction'
     &/1x,g10.3,'  rksc1,   rate constant 1 for positive side reaction'
     &/1x,g10.3,'  rksa2,   rate constant 2 for negative side reaction'
     &/1x,g10.3,'  rksc2,   rate constant 2 for positive side reaction'
     &/1x,g10.3,'  rksa3,  rate constant 3 for negative side reaction'
     &/1x,g10.3,'  rksc3r,  rate constant 3 for positive side reaction')
 1114 format ('   Time     Util N  Util P  Cell Pot   Uocp      Curr',
     &'    Temp    Heat Gen')
 1115  format ('   (min)       x       y      (V)       (V)     (A/m2)',
     %'  (C)    (W/m2)')
 1116  format(/i7, '    nside, side reaction flag ')
      write (2,*) ' '

      go to (131,132,133,134,135),nneg
  131 write (2,*) 'Li foil'
      go to 147
  132 write (2,*) 'Carbon (petroleum coke)'
      go to 147
  133 write (2,*) 'MCMB 2528 Graphite (Bellcore)'
      go to 147
  134 write (2,*) 'Metal Hydride'
      go to 147
  135 write (2,*) 'Add your own negative electrode'

  147 go to (101,102,103,104,105),nprop
  101 write (2,*) 'LiPF6 in PC (Sony cell simulation)'
      go to 200
  102 write (2,*) 'LiPF6 in EC:DMC'
      go to 200
  103 write (2,*) 'LiTFSI in PEO at 85 C (data from Ludvig Edman)'
      go to 200
  104 write (2,*) 'KOH in H2O'
      go to 200
  105 write (2,*) 'add your own electrolyte'

  200 go to (201,202,203,204,205,206,207),npos
  201 write (2,*) 'LiCoO2 (Cobalt dioxide)'
      go to 300
  202 write (2,*) 'V2O5 (Vanadium oxide)'
      go to 300
  203 write (2,*) 'Spinel Mn2O4'
	go to 300
  204 write (2,*) 'Ni0.80Co0.15Al0.05O2'
      go to 300
  205 write (2,*) 'LiFePO4'
      go to 300
  206 write (2,*) 'NiOOH'
      go to 300
  207 write (2,*) 'Add your own'
  300 continue


      if(restart) then 
cSP   reading the restart file
      rewind(12)
      read(12,*) rr, time ! time   
      dt_out_incr = time
      read(12,*) t ! temperature   
      read(12,*) k, (ts(i), i = 1,k)
      do j = 1,nj ! number of variables
      do i = 1,n ! number of equations
         read(12,*) xx(i,j)
         read(12,*) (xt(i,j,kk), kk=1,k)
      end do
      do jj = 1,nnj
      do mpa = 1,npa
      read(12,*) css(j,jj,mpa)
      read(12,*) ds(j,jj,mpa)
      end do
      end do
      do i = 1,npa 
         read(12,*) utz(i,j)
      end do
      end do
      csx = utz(1,1)
      ut1 = utz(1,1)
      csy = utz(1,nj)
      ut3 = utz(1,nj)
      end if

      if (mvdc1.eq.1) write (2,*)
     &'Variable solid-phase diff coeff, anode'
      if (mvdc3.eq.1) write (2,*)
     &'Variable solid-phase diff coeff, cathode'


c *** GET THE DENSITY AND CAPACITY FROM LIBRARY IN EKIN, AND SET UP CELL ***

      if(newrun) then
	do mpa=1,npa
        call ekin(1,1,1,csx,mpa,mvdc1,mvdc3)
	  xx(2+mpa,1)=0.0d0
	  call ekin(nj,1,1,csy,mpa,mvdc1,mvdc3)
	  xx(2+mpa,nj)=0.0d0
	enddo !mpa
      else
      mpa=1
      call ekin(1,1,0,ut1,mpa,mvdc1,mvdc3)
      call ekin(nj,1,0,ut3,mpa,mvdc1,mvdc3)
      endif

c      print *,'capacity ratio',(h1*ct1(1)*(1-ep1-epp1-epf1-epg1))/
c     &(h3*ct3(1)*(1-ep3-epp3-epf3-epg3))
c      print *,'Ah/m2, pos',(h3*cot3(1)*(1-ep3-epp3-epf3-epg3)*rs3(1))
c      print *,'Ah/m2, neg',(h1*cot1(1)*(1-ep1-epp1-epf1-epg1)*rs1(1))

c Calculate the amount of gas in the headspace (1.4 factor for 2005 Prius battery)
c This is necessary only for a gas-phase material balance
      vol=(epg1*h1+epg2*h2+epg3*h3)/((epg1*h1+epg2*h2+epg3*h3)+
     &0.5d0*(h1+h2+h3))/1.4d0

      if (nside.ge.1.and.nprop.ne.4) vol=1.0d0 !vol=1 for liquid-phase side rxn
      lcount=1 !counter for max power

c     calculate mass (kg/m2) of the cell
      call mass(re,rs3(1),rs1(1),rf,rpl,rc,rcn,rcp)
      dens=tw/thk

      shape1=3.0d0 ! 3. for spherical, 1. for planar, 2. for cylindrical
      shape3=3.0d0
      if (imp.eq.0) then 
      cap1=capp1 ! F/m2, capacitance for negative
      cap3=capp3 ! F/m2, capacitance for positive
      else ! For impedance set cap1 and cap3 to zero.
      cap1= 0.0d0 ! F/m2, capacitance for negative
      cap3= 0.0d0 ! F/m2, capacitance for positive
      endif

c     Keeps track of tapered discharge/charge mode
      do i=1,lcurs
         taper(i) = 0
         if(mc(i).eq.-1) then
            mc(i) = 1
            taper(i) = 1
         endif
      enddo
c
c     Convert times to seconds and sum up times of mode changes
      if (mc(1).lt.2) then
      tot(1)=6.0d01*tt(1)
      else
      tot(1)=0.0d0
      end if
      do 51 i=2,lcurs
      if (mc(i).lt.2) then
      tot(i)=tot(i-1)+6.0d01*tt(i)
      else
      tot(i)=tot(i-1)
      end if
   51 continue
c
c *** DEFINE THE PARTICLE TYPES ***
c
c     For multiple active materials need to go to ekin,
c     and change the value of npos.
c
c     Specific area calculated from geometry
      area3=shape3*(1.0d0-ep3-epf3-epp3-epg3)/Rad3
      if (n1.ne.0) then
      area1=shape1*(1.0d0-ep1-epf1-epp1-epg1)/Rad1
      else
      area1 = 1.0d0/h1
      endif

c Set up the radius and area of each particle.  The following six lines
c apply to electrodes with a single particle, or electrodes with
c multiple particles all of the same radius and area.
      if (npa.eq.1) then
      do mpa=1,npa
      Rad1pa(mpa)=Rad1 ! start with uniform particles
      area1pa(mpa)=area1/dble(npa)
      Rad3pa(mpa)=Rad3 ! start with uniform particles
      area3pa(mpa)=area3/dble(npa)
      enddo ! mpa
      vf1(1)=1.0d0
      vf3(1)=1.0d0
      else

c For nonuniform particles use this section.
c We use volume fractions to specify our distribution; the user should
c be able to calculate how to turn other distributions, such as number
c or area fraction, into volume fractions.

      vol1=(1.0d0-ep1-epf1-epp1-epg1) ! Solid vol frac in negative
      vol3=(1.0d0-ep3-epf3-epp3-epg3) ! Solid vol frac in positive

c Here input the radii of each particle.  This is set up for a two-particle
c distribution, but can be extended to as many as five particles by
c adding additional particle radii here.

      Rad1pa(1)=1.0d-6
      Rad1pa(2)=1.0d-6
      Rad1pa(3)=1.0d-6
      Rad1pa(4)=1.0d-6
      Rad1pa(5)=1.0d-6

      Rad3pa(1)=1.0d-6
      Rad3pa(2)=1.0d-6
      Rad3pa(3)=1.0d-6
      Rad3pa(4)=1.0d-6
      Rad3pa(5)=1.0d-6

	ranode(1)=0.001d0
	ranode(2)=0.001d0
	ranode(3)=0.001d0
	ranode(4)=0.001d0
	ranode(5)=0.001d0

	rcathde(1)=0.00001d0
	rcathde(2)=0.00001d0
	rcathde(3)=0.00001d0
	rcathde(4)=0.d0
	rcathde(5)=0.d0


c Here input the relative volume fraction of each particle.  Note that
c these need to add to 1.0, not to vol1 or vol3!  For example, a
c relative volume fraction of 0.2 means that 20% of the solid volume of an
c electrode is composed of particles of a given radius.

      vf1(1)=0.5d0
      vf1(2)=0.5d0
      vf1(3)=0.6d0
      vf1(4)=0.2d0
      vf1(5)=0.2d0

      vf3(1)=0.5d0
      vf3(2)=0.5d0
      vf3(3)=0.6d0
      vf3(4)=0.2d0
      vf3(5)=0.2d0

c Need other properties as well.

	dfs1(1)=dfs1(1)
	dfs1(2)=dfs1(1)
	dfs1(3)=dfs1(1)
	dfs1(4)=dfs1(1)
	dfs1(5)=dfs1(1)

	dfs3(1)=dfs3(1)
	dfs3(2)=dfs3(1)
	dfs3(3)=dfs3(1)
	dfs3(4)=dfs3(1)
	dfs3(5)=dfs3(1)

	rka1(1)=rka1(1)
	rka1(2)=rka1(1)
	rka1(3)=rka1(1)
	rka1(4)=rka1(1)
	rka1(5)=rka1(1)

	rka3(1)=rka3(1)
	rka3(2)=rka3(1)
	rka3(3)=rka3(1)
	rka3(4)=rka3(1)
	rka3(5)=rka3(1)

c Reset area1 and area3 for nonuniform particles.
      area1=0.0d0
      area3=0.0d0
c
      do mpa=1,npa
      area1pa(mpa)=vol1*vf1(mpa)/(4.d0/3.d0*pi*Rad1pa(mpa)**3.0d0)*
     &4.d0*pi*Rad1pa(mpa)**2.d0
      area3pa(mpa)=vol3*vf3(mpa)/(4.d0/3.d0*pi*Rad3pa(mpa)**3.0d0)*
     &4.d0*pi*Rad3pa(mpa)**2.d0
      area1=area1+area1pa(mpa)
      area3=area3+area3pa(mpa)
      enddo !mpa
      endif ! end section defining particle types

c     Get effective solid-phase conductivity with Bruggeman
      sig3=sig3*((1.0d0-ep3-epp3-epg3)**1.5d0)
      sig1=sig1*((1.0d0-ep1-epp1-epg3)**1.5d0)
c
      h2=h2/dble(n2-1)
      h3=h3/dble(n3)
      if (n1.gt.1) h1=h1/dble(n1)
      h=h2
      hha=rad1/dble(n4-1)
      hhc=rad3/dble(n4-1)
      frt=fc/(r*t)
c
      mpa=1 ! only one size for initialization
c     Find initial solid-phase potential guesses
c     from initial solid concentrations:
      if(newrun) then
      call ekin(1,1,1,csx,mpa,mvdc1,mvdc3)
      write (2,*) 'open-circuit potential, negative ',g0(mpa)
      write (2,*) 'delta S negative ', dudt
         Ua=g0(mpa)
      call ekin(nj,1,1,csy,mpa,mvdc1,mvdc3)
      write (2,*) 'open-circuit potential, positive ',g0(mpa)
      write (2,*) 'delta S positive ', dudt
      Uc=g0(mpa)
      else 
!     mpa=1
!     call ekin(1,1,0,ut1,mpa,mvdc1,mvdc3)
!     Ua=g0(mpa)
!     call ekin(nj,1,0,ut3,mpa,mvdc1,mvdc3)
!     Uc=g0(mpa)
      end if
      OCP=Uc-Ua ! open-circuit cell potential

      write (2,*) ' '
      write (2,*) '     DUAL INSERTION CELL VERSION 5.1'
      write (2,*) ' '
      if (imp.eq.1) then
      write (2,*) '         IMPEDANCE OPTION   '
      write (2,*) ' '
      write (2,*) '  Omega                Z real         Z imag  '
      write (2,*) ' (Rad/s)              (ohm cm2)      (ohm cm2)  '
      go to 54
      endif ! end of impedance section (output)

        if (nside.ne.0.and.nneg.eq.4) then
        write (2,1107) !This output includes pressures
        write (2,1108)
        else
        write (2,1109) !This output excludes pressures
        write (2,1110)
        endif
      write (2,*) ' '

      write (6,*) 'Resistances in each portion of the cell, from a',
     1' simple (delta V) / I'
      write (6,*) 'Time       Cur Den      R Anode       R Separator'
     &,'      R Cathode        R Total'
      write (6,*) '(min)      (A/m^2)     (ohm-m^2)       (ohm-m^2)'
     &,'       (ohm-m^2)      (ohm-m^2)'
      write (6,*) ' '
   54 continue
c
c***********************************************************************
c
c     initialize time counting variables

      if(newrun) then
      time=0.0d0
      time2=0.0d0
      rr=0.0d0
      L=1
      ts(1)=0.0d0
      k=1
      else ! restart
      time2 = time
      L=1
      go to 411 ! skip the initialization
      end if

*** INITIALIZE CELL-SANDWICH VARIABLES ***

c     n1+1 is the last node in the negative
c     n1+n2 is the first node in the positive
c     nj is the last node in the cell sandwich

c initialize liquid-phase concentration
      do j=1,nj
        xx(1,j)=xx(1,n1+2) ! initial concentration (mol/m3)
      enddo

      call prop(nj,n2,n1) ! get liquid-phase transport properties
      curold=0.d0 ! open circuit
      vvold=OCP
c      cur=1.0d0 ! guess current to get internal resistance
      cur=100.d0 ! If high currents are expected, use a high current
c                  here to get an appropriate resistance
      if(mc(1).ge.1.or.mc(1).eq.-1) cur=cu(1)
      if (cap1.ne.0.0d0.or.cap3.ne.0.0d0) cur=0.0d0

c initialize solid-phase concentration
c need utz() to do energy balance based on local current
      do j=1,n1+1 ! negative electrode
        do mpa=1,npa
          xx(2+mpa,j)=ct1(mpa)*csx
          utz(mpa,j)=csx
        enddo ! mpa
      enddo
      do j=n1+2,n1+n2-1 ! separator
        do mpa=1,npa
          xx(2+mpa,j)=0.d0
          utz(mpa,j)=0.d0
        enddo ! mpa
      enddo
      do j=n1+n2,nj ! positive electrode
        do mpa=1,npa
          xx(2+mpa,j)=ct3(mpa)*csy
          utz(mpa,j)=csy
        enddo ! mpa
      enddo

c initialize liquid-phase potential, current, and pore-wall flux
      xx(kp2,nj)=0.d0 !boundary condition for liquid-phase potential
      do 78 j=nj,n2+n1,-1 ! positive
        xx(ki2,j)=cur*dble(nj-j)/dble(n3) !linear rise in current
	  do mpa=1,npa
          xx(3+npa+mpa,j)=-cur/fc/h3/area3/dble(n3)/dble(npa) !uniform current
        enddo !mpa
	  if(j.ne.nj)
     &  xx(kp2,j)=xx(kp2,j+1)+h3*xx(ki2,j)/cd(j) !linear rise in potential
   78 xx(kp1,j)=Uc+xx(kp2,j)+xx(3+npa+1,j)*fc*rcathde(1) !rough guess

      do j=n1+n2-1,n1+2,-1 ! separator
        xx(ki2,j)=cur
	  do mpa=1,npa
          xx(3+npa+mpa,j)=0.d0
	  enddo !mpa
        xx(kp2,j)=xx(kp2,j+1)+h2*xx(ki2,j)/cd(j) 
        xx(kp1,j)=0.0d0
      enddo

      do 79 j=n1+1,1,-1 ! negative electrode
          if (n1.gt.0) then !porous electrode
          xx(ki2,j)=cur*dble(j-1)/dble(n1)
	    do mpa=1,npa
            xx(3+npa+mpa,j)=cur/fc/h1/area1/dble(n1)/dble(npa)
	    enddo !mpa
          xx(kp2,j)=xx(kp2,j+1)+h1*xx(ki2,j)/cd(j) ! ohmic drop is not accurate
          xx(kp1,j)=Ua+xx(kp2,j)+xx(3+npa+1,j)*fc*ranode(1)
          else !foil anode
          xx(ki2,j)=cur*dble(j-1)/dble(n1+1)
	    do mpa=1,npa
            xx(3+npa+mpa,j)=cur/fc/h1/area1/dble(n1+1)
	    enddo !mpa
          xx(kp2,j)=xx(kp2,j+1)+h1*xx(ki2,j)/cd(j) ! ohmic drop is not accurate
          xx(kp1,j)=Ua+xx(kp2,j)+xx(3+npa+1,j)*fc*ranode(1)
          endif
   79 continue

c initialization for variables kS1,kj1,kS2,kj3 ! side reactions, not impedance
      do j=1,nj
      if (nside.ge.1) then
      xx(kS1,j)=c1init
      xx(kS2,j)=c2init
      endif
      xx(kj1,j)=0.0d0
      xx(kj2,j)=0.0d0
      xx(kj3,j)=0.0d0
      enddo

      do j=1,nj
      do i=1,n
      xt(i,j,1)=xx(i,j)
      enddo
      enddo

c Assumed temperature dependence for solid-state diffusion
      dfs1(1)=dfs1save*dexp((EbarS1)*(t-298.0d0)/(t*298.0d0))
      dfs3(1)=dfs3save*dexp((EbarS3)*(t-298.0d0)/(t*298.0d0))

c Initialize solid-phase concentrations for variable diffusion coefficient
      if (mvdc1.eq.1.or.mvdc3.eq.3) then
        do mpa=1,npa ! more than one size for vdc
          do jj=1,nj
            do jjj=1,nnj
              css(jj,jjj,mpa)=xx(2+mpa,jj)
              cssold(jj,jjj,mpa)=xx(2+mpa,jj)
c For a true variable solid-phase diffusion coefficient put in the
c functional dependence here and in subroutine vardc
              if (jj.le.n1+1) ds(jj,jjj,mpa)=dfs1(1)
              if (jj.ge.n1+n2) ds(jj,jjj,mpa)=dfs3(1)
            enddo
          enddo
        enddo
	endif

!cSP if restart skip the above steps
 411  continue


      if(restart) then
c     do j=1,nj
c     do i=1,n
c     xt(i,j,k+1)=xx(i,j)
c     enddo
c     enddo
      cssold(:,:,:) = css(:,:,:)
      endif


c   *** IMPEDANCE SECTION ***
      if (imp.eq.1) then
c     initialize steady state to make impedance runs.
      imp=0
      call comp(n,lim,k,rr,jcount)

      imp=1
      ji=1
c     omega is the array of perturbation frequencies in Rad/s
      do 61 iii=1, 110

      omega(ji)=10.d6/(10.d0**0.1d0)**iii
c
      call comp(n,lim,k,rr,jcount)
c
   61 continue
      write (2,*)''
      write (2,*)'  Omega            Z real        Z imag  '
      write (2,*)' (Rad/s)          (ohmcm2)      (ohmcm2) '
      do 62 i=1,110
   62 write (2,57)omega(i),vreal(i)*10000,vimag(i)*10000
      write (2,*)''
      write (2,*)'  Omega           Z reala       Z imaga  '
      write (2,*)' (Rad/s)          (ohmcm2)      (ohmcm2) '
      do 63 i=1,110
   63 write (2,57)omega(i),vreala(i)*10000,vimaga(i)*10000
      write (2,*)''
      write (2,*)'  Omega           Z realc       Z imagc  '
      write (2,*)' (Rad/s)          (ohmcm2)      (ohmcm2) '
      do 64 i=1,110
   64 write (2,57)omega(i),vrealc(i)*10000,vimagc(i)*10000
      write (2,*)''
      write (2,*)'  Omega             Z mag         phi  '
      write (2,*)' (Rad/s)           (ohmcm2)      (deg) '
      do 65 i=1,110
   65 write (2,57)omega(i),zmag(i)*10000,phi(i)
      write (2,*)''
      write (2,*)'  Omega            Z maga        phia  '
      write (2,*)' (Rad/s)           (ohmcm2)      (deg) '
      do 66 i=1,110
   66 write (2,57)omega(i),zmaga(i)*10000,phia(i)
      write (2,*)''
      write (2,*)'  Omega             Z magc       phic  '
      write (2,*)' (Rad/s)           (ohmcm2)      (deg) '
      do 67 i=1,110
   67 write (2,57)omega(i),zmagc(i)*10000,phic(i)
      write (2,*)''
      go to 999
      endif ! end of impedance section (general)

      if(restart) then
      cuL = cu(L)
      cuR = cu(L)
      vv=xx(kp1,nj)-xx(kp1,1)
      mcL = 1
!     call cellpot(k,vv,1)
      go to 412 ! skip the initialization of cell for restart
      end if

c *** CALL COMP TO INITIALIZE CELL WITH ZERO TIMESTEP ***

      mcL=1
      if (imp.eq.0) call comp(n,lim,k,rr,jcount)

      if(nconv.eq.1) write (2,*) 'comp 1 not converged' ! initial
         if(nconv.eq.1) go to 533
      vv=xx(kp1,nj)-xx(kp1,1)
      Rint=-(vv-vvold)/(cur-curold)+RG ! internal resistance
      OCP=vv+(Rint-RG)*cur

      cuL=cu(L)
      cur=cu(L) ! good for mc(L)=1, 2, and -1.
c     First guess for current for constant load.
      if(mc(L).eq.-3) cur=OCP/(cu(L)+Rint)
      if(mc(L).eq.0) cur=(OCP-cu(L))/Rint ! constant potential
      if(mc(L).eq.-2) then ! constant power
        cur=cu(L)/OCP
        do i=1,6 ! refine estimate of current for given power
          cur=cur-(cur*OCP-Rint*cur**2-cu(L))/(OCP-2.d0*Rint*cur)
        enddo
      endif
      
	avgeta=0.d0
      if(mc(L).eq.0) avgeta=(OCP-cuL)/2.d0
      if(mc(L).le.-2) avgeta=cur*(Rint-RG)/2.d0

c     Obtain better initial guess by running at constant current
c     which is close to the current for the chosen constraint.
      mcL=1
c     This call of comp initializes the system for non-impedance
      call comp(n,lim,k,rr,jcount)
      call cellpot(k,vv,0)

      mcL=mc(L)
      cuL=cu(L)
      curold=0.d0
      vvold=OCP

      if(mc(L).eq.-2) then ! constant power
      mcL=mc(L)
      call comp(n,lim,k,rr,jcount)
      if(nconv.eq.1) write (2,*) 'comp 6 not converged' ! repeat?
      call cellpot(k,vv,1)
      endif ! treatment of constant power
c
c     rr is the size of a time step.
      if(cap1.eq.0.d0 .and. cap3.eq.0.d0) then
      rr=0.02d0 ! initial timestep value
      else
      rr=1.5d-13 ! If include capacitance, use small initial timestep
      endif

 412  continue !cSP - skip for restart

      ed=0.d0
      heat=0.d0
      qlosstot=0.d0
      iflag=0
      L=0

cSP   writing out the initial state
c     write(11,*) ts(k) ! time
c     do j = 1,nj
c        write(11,*) xx(1,j), xx(6,j), xx(2,j), xx(5,j)*fc, xx(3,j) ! concentration (solute), phi_1, phi_2, jn, cs
c     end do

      icheck = 0 ! check that all the data is read in correctly for a restart
      if(icheck.eq.1.and.restart) then
      rewind(13)
      write(13,*) rr, ts(k) ! time  - sometimes last time-step is small to just roundoff time till tend  
      write(13,*) t ! temperature
      write(13,*) k, (ts(i), i = 1,k)
      do j = 1,nj ! number of variables
      do i = 1,n ! number of equations
         write(13,*) xx(i,j)
         write(13,*) (xt(i,j,kk), kk=1,k)
      end do
      do jj = 1,nnj
      do mpa = 1,npa
      write(13,*) css(j,jj,mpa)
      write(13,*) ds(j,jj,mpa)
      end do
      end do
      do i = 1,npa
         write(13,*) utz(i,j)
      end do
      end do

      call nucamb(k,il2)

      stop
      end if


c *** HERE IS THE LOOP FOR THE LEGS ***
   53 L=L+1 ! new leg
      mcL=mc(L)
      cuL=cu(L)
      if(L.gt.1) rr=0.d0 !run a zero timestep at the start of each leg

c *** HERE IS THE LOOP FOR TIMESTEPS ***

  123 k=k+1 ! new time step
      nt=k-1

c     adjust time step to match time of change in current
      time=ts(k-1)+rr
      if(time .ge. tot(L) .and. mc(L).lt.2) then
      rr=tot(L)-ts(k-1)
      time=tot(L)
      iflag=1
      endif
c
  129 ts(k)=ts(k-1)+rr ! new time step
      write(20,*) 'time, ', k, ts(k), rr, time !cSP
      call calca(k) ! should need only one call at each time step
      dtnow=rr
c
      if(mc(L).eq.-3 .or. mc(L).ge.0) then
c     mc is 1 or 2 so run galvanostatically
c     or mc is 0; run at constant potential
c     or mc is -3; run at constant load
      call comp(n,lim,k,rr,jcount)
      if(nconv.eq.1) write (2,*) 'comp 7 not converged' ! cur,pot,load
        if (nconv.eq.1) go to 533
c
      else 
c     mc is -2 so run at constant power
      do 610 j=1,nj
      term_s1s(j)=term_s1(j)
  610 terms(j)=term(j)

      mcL=mc(L) ! specified power should be possible
      call comp(n,lim,k,rr,jcount)
      if(nconv.eq.1) write (2,*) 'comp 10 not converged' ! power
      
      endif ! end calculation for set mode

      if(rr.lt.0.9999*dtnow) iflag=0 ! time step decreased in comp
      mcL=mc(L)

c *** CALL CELLPOT TO WRITE CONVERGED RESULTS ***
      if(mc(L).eq.2) then
        call cellpot(k,vv,0)
      else
        call cellpot(k,vv,1)
      endif

      if (il1.eq.1.and.mod(k,il3).eq.0) call nucamb(k,il2) !detailed printout

      frt=fc/r/t

c *** SECTION WITH MODE CONTROL ***

      sign=1.d0
      vcut = vcutlo(L)
      IF(VV.LT.VCUT.and.cur.eq.0.d0) then
        do i=L+1,Lcurs
          tot(i) = tot(i) + time - tot(L) !Correct time markers
        enddo
        go to 888
      endif

      if(cu(L).lt.0.d0) then
         sign=-1.d0
         vcut = vcuthi(L)
      endif
      IF(sign*VV.LT.sign*VCUT .and. cu(L).ne.0.d0) then
         if(taper(L).eq.1) then !Taper the current by holding VV at VCUT
      if(mc(L).eq.1) then
            mc(L) = 0
            mcL = 0
            cu(L) = vcut
            cuL=vcut
            k=k+1
            ts(k)=ts(k-1)
      call comp(n,lim,k,0.d0,jcount)
      call cellpot(k,vv,1)
      if(il1.eq.1) call nucamb(k,il2)
      endif
         else                   !End profile segment
            do i=L+1,Lcurs
               tot(i) = tot(i) + time - tot(L) !Correct time markers
            enddo
            go to 888
         endif
      endif

c     check to see if cutoff potential is exceeded if mc is 2
      if (mc(L).eq.2) then
        IF ((VV.LT.TT(L) .AND. CU(L).GT.0.0) .OR.
     &  (VV.GT.TT(L) .AND. CU(L).LT.0.0)) THEN
          if (dabs(vv-tt(l)) .gt. 1.0d-04) then
            rr=rr/2.0d0
            iflag=1
            go to 129
          else
            time2=time2+rr
            call cellpot(k,vv,1)
            frt=fc/r/t
            iflag=1
          endif
        else
          iflag=0
          time2=time2+rr
          call cellpot(k,vv,1)
          frt=fc/r/t
        end if
      end if
c

      iconst_tstep = 0 ! set it to 1 for constant time step

      if(iconst_tstep.eq.1) then
c     do nothing
      else
c     Increasing time steps: comment out for constant time steps
      rrmax=tmmax
      if(k.le.5) rrmax=0.5d0
      jlim=int(lim/5.0d0)
      if (jlim.lt.5) jlim=5
      if(jcount.lt.jlim.and. k.gt.2 .and. rr.lt.rrmax .and.
     1iflag.eq.0.and.lcount.le.1) then
      if(xx(ki2,n1+5).lt.1200.d0) rr=rr*1.1d0
      print *,'next time step increased to ', rr,' (s)'
      end if
      end if

      if(k.GE.maxt-1) then
        write (2,*) 'kmax=',k,' a larger matrix needed for xt'
      endif
c     Here trim the solid concentrations for superposition.
c     Array only needs to store a given number of values.
      if (k.GE.501) then 
      do 92 kk=3,401,2
      kput=(KK+1)/2
      ts(kput)=ts(kk)
      do 92 j=1,nj
      do 92 i=1,n
   92 xt(i,j,kput)=xt(i,j,kk)
      do 93 kk=402,K
      ts(kk-200)=ts(kk)
      do 93 j=1,nj
      do 93 i=1,n
   93 xt(i,j,kk-200)=xt(i,j,kk)
      k=k-200
      endif

      if (iflag.eq.0 .and. rr.eq.0.d0) then
c     rr is the size of a time step.
      if(cap1.eq.0.d0 .and. cap3.eq.0.d0) then
      rr=0.02d0
c      tmmax !for constant time steps
      else
      rr=1.5d-13 !  initial time step is 1.5d-13 second
      endif
      endif

cSP   writing out the time series

      dt_out_incr = dt_out_incr + (ts(k) - ts(k-1))
c     write(14,*) 'debug', dt_out_incr, ts(k), ts(k-1),(ts(k) - ts(k-1)), tot(1)

      if (dt_out_incr.ge.tot(1)) then 

      dt_out_incr = 0.0 
      rewind(11)
      write(11,*) nj
      write(11,*) ts(k) ! time
      do j = 1,nj
         write(11,*) xx(1,j), xx(6,j), xx(2,j), xx(5,j)*fc, xx(3,j) ! concentration (solute), phi_1, phi_2, jn, cs 
      end do

      rewind(14)
      write(14,*) ts(k) ! time
      write(14,*) qq, r_total ! source in J/(m^3.s) and resistance in ohms

      endif

      if(iflag.eq.1) then 
cSP   writing out the restart file
      rewind(12)
      write(12,*) max(rr, ts(k-1)-ts(k-2)), ts(k) ! time  - sometimes last time-step is small to just roundoff time till tend  
      write(12,*) t ! temperature
      write(12,*) k, (ts(i), i = 1,k)
      do j = 1,nj ! number of variables
      do i = 1,n ! number of equations
         write(12,*) xx(i,j)
         write(12,*) (xt(i,j,kk), kk=1,k)
      end do
      do jj = 1,nnj
      do mpa = 1,npa
      write(12,*) css(j,jj,mpa)
      write(12,*) ds(j,jj,mpa)
      end do
      end do
      do i = 1,npa 
         write(12,*) utz(i,j)
      end do
      end do
      end if


      if (iflag .eq. 0) go to 123 ! a new time step
  888 continue
      iflag=0
      if (mc(L).eq.2) then
      do 124 m=L,Lcurs
  124 tot(m)=tot(m)+time2
      time2=0.0d0
      end if

      IF(L.EQ.LCURS .AND. LCURS.GE.100) THEN 
c If Lcurs greater than 100 go into an endless loop
      L=0
      tot(1)=TOT(LCURS)
      if (mc(1).lt.2) TOT(1)=TOT(1)+60.0D0*TT(1)
      do 403 i=2,Lcurs
      if (mc(i).lt.2) then
      tot(i)=tot(i-1)+6.0d01*tt(i)
      else
      tot(i)=tot(i-1)
      end if
  403 continue
      ENDIF

c *** THIS SECTION PREPARES FOR A NEW LEG ***

  533 if(L.lt.Lcurs) then ! prepare for a new leg
      rr=0.d0
c       calculate zero-time solution for change in current
c     Need some initialization here to get started smoothly on
c     a new leg of the calculation.
      cuL=cu(L+1)
c     First guess for current
      cur=cu(L+1) ! constant current
      if(mc(L+1).eq.-3) cur=OCP/(cu(L+1)+Rint) ! constant load
      if(mc(L+1).eq.0) cur=(OCP-cu(L+1))/Rint ! constant potential

      if(mc(L+1).eq.-2) then ! constant power
      cur=cu(L+1)/OCP
      do ii=1,6 ! refine estimate of current for given power
c     could solve quadratic
      cur=cur-(cur*OCP-Rint*cur**2-cu(L+1))/(OCP-2.d0*Rint*cur)
      enddo
      endif ! end of power loop

      avgeta=cur*Rint/2.d0
      if(mc(L+1).eq.0) avgeta=(OCP-cuL)/2.d0

      Ua=xx(kp1,1)
      Uc=xx(kp1,nj)
      Ua=0.d0 ! need better open-circuit potentials
      Uc=4.d0 ! need better open-circuit potentials
      do 73 j=1,(n1+1) ! negative electrode
      do mpa=1,npa
      xt(2+mpa,j,k+0)=xx(2+mpa,j)
      xt(3+npa+mpa,j,k+0)=cur/fc/h1/area1/dble(n1)
      enddo ! mpa
      xt(ki2,j,k+0)=cur*dble(j-1)/dble(n1)
   73 xt(kp1,j,k+0)=xx(kp2,j)+Ua+0.60d0*avgeta

      do j=n1+1,n1+n2 ! separator
      xt(ki2,j,k+0)=cur
      enddo
c
      do 75 j=(n2+n1),nj ! positive electrode
      do mpa=1,npa
      xt(2+mpa,j,k+0)=xx(2+mpa,j)
      xt(3+npa+mpa,j,k+0)=-cur/fc/h3/area3/dble(n3)
      enddo ! mpa
      xt(ki2,j,k+0)=cur*dble(nj-j)/dble(n3)
   75 xt(kp1,j,k+0)=xx(kp2,j)+Uc-1.20d0*avgeta

c     straighten out the initialization
      mcL=1
      call comp(n,lim,k+1,rr,jcount)
      if(nconv.eq.1) write (2,*) 'comp 15 not converged'
      mcL=mc(L+1)

        if(mc(L+1) .gt. 0) then
      if (nconv.eq.0) k=k+1
          ts(k)=ts(k-1)
          rr=0.0d0 !run a zero time step at the start
c     mcL=1
          call comp(n,lim,k,rr,jcount) 
      if(nconv.eq.1) write (2,*) 'comp 16 not converged'
c     write (2,*) 'cellpot 7'
          call cellpot(k,vv,1)
      if (il1.eq.1.and.mod(k,il3).eq.0) call nucamb(k,il2)
        endif
      endif
c      rr = tmmax !uncomment and comment out next 5 lines for constant time steps

      if(cap1.eq.0.d0 .and. cap3.eq.0.d0) then
        rr=0.02d0   !  initial time step is 0.2 seconds
      else
        rr=1.5d-13 !  initial time step is 1.5d-13 second
      endif
      nconv=0

      if(L.le.Lcurs) then
      if (L.eq.1) pow=ed/tw/ts(k)
      if (L.ne.1) pow=ed/tw/(time-timesave)
      timesave=ts(k)


c *** WRITE THE CELL DATA ***
      if (L.eq.Lcurs) write (2,44) tw
      write (2,48) ed/tw/3.6d03
      write (2,49) pow
      write (2,47) heat
      write (2,*) ' '
      ed=0.0d0
      endif
      if(jsol.gt.0 .and. jsol.lt.nj) call sol(k,jsol)

      IF(L.LT.LCURS) GO TO 53 ! new leg


c     Solid-phase concentration profiles at given time and position
      if(jsol.gt.0 .and. jsol.lt.nj) call sol(k,jsol)

      stop
  999 end
c
c***********************************************************************
      subroutine comp(n,lim,kk,tau,jcount)
c     COMP is the main calculation subroutine, going through the
c     equations that are solved through the cell sandwich.
      implicit real*8(a-h,o-z)
      parameter(maxt=900)
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
      common /calc/ ai(maxt,5),ai2(maxt,5),ts(maxt),h,h1,h2,h3,hcn,
     1hcp,rr,rrmax,cuL,sumpa(5,221),Rad1pa(5),Rad3pa(5),area1pa(5)
     &,area3pa(5),mcL
      common/pindiv/ vf1(5),vf3(5),cot1(9),cot3(9),rs1(9),rs3(9)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/gas/ epg1,epg2,epg3
      common/power/ ed,Vold,ranode(20),rcathde(20),heat,qlosstot
      common/ssblock/ xp0(16),xx0(16,221),term(221)
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/imp/ vreal(110),vimag(110),phi(110),zmag(110),omega(510),
     1vreala(110),vrealc(110),vimaga(110),vimagc(110),
     1phia(110),phic(110),zmaga(110),zmagc(110)
      common/cprop/ sig3,area3,rka3(9),rka3save,ct3(9),dfs3(9),
	1Rad3,cap1,cap3,sig1,area1,rka1(9),rka1save,ct1(9),dfs1(9),
     1Rad1,tw,dfs1save,dfs3save
      common/tprop/df(221),cd(221),tm(221),ddo2(221),ddh2(221),
     1ddf(221),dcd(221),dtm(221),dfu(221),d2fu(221),do2(221),dh2(221)
      common/temp/ thk,htc,dudt,Cp,dens,tam,g0(9),
     &qq,qloss,residm,ncell,lht
      common/side/rksc1,c1init,c2init,rksa1,term_s1(221),vol,
     &rksa2,rksa3,rksc3,rksc2,UsO2,UsH2,term_s2(221),nside
      common/mat/ b,d
      common/bnd/ a,c,g,x,y
      common /vdc/ aa(1,1),bb(1,1),cc(1,150),dd(1,3),gg(1),xxx(1,1),
     &yy(1,1),hha(20),hhc(20),cssold(220,150,20),css(220,150,20),
     &utz(9,221),dsold(220,150,20),ds(220,150,20),time,
     &nn,nnj,np,mvdc1,mvdc3,lims
      common /maxpow/ lcount
      common/RG/ RG,RGn,RGp,RGext
      save iijj
      dimension b(17,17),d(17,35),termn(221)
      dimension termn_s1(221), termn_s2(221)
      dimension a(17,17),c(17,221),g(17),x(17,17),y(17,17)
	dimension css_store(220,150,20)
   99 format (1h ,//5x,'this run just did not converge'//)
  103 format(f13.5,'      ',f12.6,'     ',f12.6)
  104 format (i4,' ',g15.5,' ',g15.5,' ',
     &g15.5,' ',g15.5,' ',g15.5,' ',g15.5)

      ntot=n

      if(imp.eq.1) ntot=2*n ! double equations for impedance, real and imag
      nx=ntot
      mcLL=mcL
      Lcount=0 ! counter for maximum power
      curold=cur ! save correctly calculated values at one cur
      vvold=xx(kp1,nj)-xx(kp1,1)
      rrold=rr
      rr=tau

      omi=dble(1-imp)
      exbrug=1.5d0 ! exponent for Bruggeman correction
c
  666 kadd=0

c     sets first guess to last time-step values
      if(kk .eq. 1) kadd = 1
      do 1 j=1,nj
      do 1 i=1,ntot
      if(rr.ne.0.d0) xx(i,j)=xt(i,j,kk-1+kadd)
    1 c(i,j)=xx(i,j)

      do 2 j=1,nj
      do 2 i=1,nnj
	do 2 k=1,mpa
    2 css(j,i,k)=cssold(j,i,k)
c
c     initialize variables to begin each iteration (jcount is iteration #)
    3 Lcount=Lcount+1
      jcount=0
      do 4 i=1,n
    4 xp(i)=0.0d0 !xp stores values of current iteration
c
c *** HERE IS THE MAIN LOOP IN COMP; IF DON'T CONVERGE RETURN HERE ***

    8 j=0
      jcount=jcount+1
      jcountsum=jcountsum+1
      if(mcL.le.-2 .or. mcL.eq.0) cur=xx(ki2,n1+1)
c     calculate physical properties each iteration
      call prop(nj,n2,n1)
c
c     initialize x and y for iteration
      do 9 i=1,ntot
      do 9 k=1,ntot
      x(i,k)=0.0d0
    9 y(i,k)=0.0d0
c
c     store previous iteration of (xp in xp0)  &  (xx in xx0)
      do 6 i=1,n
      xp0(i)=xp(i)
      do 6 jj=1,nj
    6 xx0(i,jj) = xx(i,jj)  

c     for a given iteration, set up governing equations and bc's
c     start at the left interface and move across cell.
c     initialize a,b,d,g arrays for each node
c
c *** MAIN LOOP FOR SETTING UP NODE POINTS ***

   10 j=j+1
      hC=h2
      if(j.le.n1+1)  hC=h1
      if(j.le.n1+1)  area=area1
      if(j.le.n1+1)  capp=capp1
      if(j.le.n1+1)  cap=cap1
      if(j.ge.n1+n2) hC=h3
      if(j.ge.n1+n2) area=area3
      if(j.ge.n1+n2) capp=capp3
      if(j.ge.n1+n2) cap=cap3
      if(j.eq.1 .or. j.eq.n1+1 .or. j.eq.n1+n2 .or. j.eq.nj)
     &area=area/2.d0

c
      do 11 i=1,ntot
      g(i)=0.0d0
      xx(i,j)=c(i,j)
      do 11 k=1,ntot
      a(i,k)=0.0d0
      b(i,k)=0.0d0
   11 d(i,k)=0.0d0

c
      if(imp.eq.1) then
      rr=1.0d0 ! set rr to 1.0 to avoid dividing by 0
      else 
      if(rr.eq.0.0d0) then ! treat as a zero time step
      b(1,1)=1.0d0
      g(1)=xt(1,j,kk-1+kadd)-xx(1,j) ! fix electrolyte concentration
	do mpa=1,npa
	g(2+mpa)=xt(2+mpa,j,kk-1+kadd)-xx(2+mpa,j) !fix solid concentrations
	b(2+mpa,2+mpa)=1.d0
	enddo !mpa
      if (nside.ge.1) then
      b(imb1,kS1)=1.0d0
      g(imb1)=xt(kS1,j,kk-1+kadd)-xx(kS1,j) !fix side1 concentration
      if (nside.ge.2) then
      b(imb2,kS2)=1.0d0
      g(imb2)=xt(kS2,j,kk-1+kadd)-xx(kS2,j) !fix side2 concentration
      endif
      endif
      go to 199
      endif
      endif
c     ________________________________________________
c     Equation 1, mass balance on electrolyte
c     Uses the control volume approach.
c
c     Adsorption behavior is fixed w/ dqn parameter
c     dqn is the dq-/dq derivative equal to either -1 (anion
c     adsorption) or 0 (cation adsorption)
c 111 dq1=0.0d0
c     dq3=0.0d0
  111 dq1=-1.0d0
      dq3=-1.0d0 ! Johnson and Newman, J. Electrochem. Soc., 118 (1971), 510-517.

      termn(j)=0.d0 ! variable to hold flux values

c Set the stoichiometric coefficient according to the nature of the reacting ion.
c For a cation, set so=1.0d0, for an anion, set so=-1.0d0.

      so=1.0d0
      if (nprop.eq.4) so=-1.d0 ! for an anion in the intercalation reaction

      fac=1.d0
      if(j.eq.n1+2 .and.n1.ne.0)
     &fac=((ep2+epp2)/(ep1+epp1))**exbrug
      if(j.eq.n1+n2+1) fac=((ep3+epp3)/(ep2+epp2))**exbrug
      epn=ep2+epp2
      hn=h2
      acn=0.0d0
      dqn=0.0d0
      if(n1.gt.0 .and. j.le.n1+1) then
        epn=ep1+epp1
        hn=h1
        acn=area1*capp1
        dqn=dq1
      elseif(j.gt.n1+n2) then
        epn=ep3+epp3
        hn=h3
        acn=area3*capp3
        dqn=dq3
      endif

      if(j.gt.1) then ! deal with box to left of point.
        termn(j)=-(df(j)+fac*df(j-1))*(xx(1,j)-xx(1,j-1))/hn/2.d0
     &  -so*(1.d0-0.5d0*(tm(j)+tm(j-1)))*xx(ki2,j-1)/fc
        a(1,1)=epn*hn*0.125d0/rr*omi
     &  -(df(j)+fac*df(j-1))/hn/4.d0+fac*ddf(j-1)*
     &  (xx(1,j)-xx(1,j-1))/hn/4.d0-so*dtm(j-1)*xx(ki2,j-1)/4.d0/fc
        b(1,1)=epn*hn*0.375d0/rr*omi
     &  +(df(j)+fac*df(j-1))/hn/4.d0+ddf(j)*(xx(1,j)-xx(1,j-1))/hn/4.d0
     &  -so*dtm(j)*xx(ki2,j-1)/4.d0/fc
        a(1,ki2)=so*(1.d0-0.5d0*(tm(j)+tm(j-1)))/2.d0/fc
        g(1)=-epn*hn*(0.375d0*(xx(1,j)-xt(1,j,kk-1+kadd))
     &  +0.125d0*(xx(1,j-1)-xt(1,j-1,kk-1+kadd)))/rr*omi
        if (imp.eq.1) then
          b(1,1+n)=-epn*hn*0.375d0*omega(ji)  /2.0d0
          a(1,1+n)=-epn*hn*0.125d0*omega(ji)  /2.0d0
          b(1+n,1)=epn*hn*0.375d0*omega(ji)   /2.0d0
          a(1+n,1)=epn*hn*0.125d0*omega(ji)   /2.0d0
        endif ! end of impedance section (Equation 1)
      endif

      fac=1.d0
      if(n1.gt.0 .and. j.eq.n1+1)
     &fac=((ep2+epp2)/(ep1+epp1))**exbrug
      if(j.eq.n1+n2) fac=((ep3+epp3)/(ep2+epp2))**exbrug
      epn=ep2+epp2
      hn=h2
      acn=0.0d0
      dqn=0.0d0
      if(n1.gt.0 .and. j.lt.n1+1) then
        epn=ep1+epp1
        hn=h1
        acn=area1*capp1
        dqn=dq1
      elseif(j.ge.n1+n2) then
        epn=ep3+epp3
        hn=h3
        acn=area3*capp3
        dqn=dq3
      endif
      if(j.ne.nj) then ! deal with box to right of point.
c     At the foil anode, only the box to the right should be used
       termn(j)=termn(j)-(fac*df(j)+df(j+1))*(xx(1,j)-xx(1,j+1))/hn/2.d0
     &  +so*(1.d0-0.5d0*(tm(j)+tm(j+1)))*xx(ki2,j)/fc
        d(1,1)=epn*hn*0.125d0/rr*omi
     & -(fac*df(j)+df(j+1))/hn/4.d0+ddf(j+1)*(xx(1,j)-xx(1,j+1))/hn/4.d0
     &  +so*dtm(j+1)*xx(ki2,j)/4.d0/fc
        b(1,1)=b(1,1)+epn*hn*0.375d0/rr*omi
     &+(fac*df(j)+df(j+1))/hn/4.d0+fac*ddf(j)*
     &(xx(1,j)-xx(1,j+1))/hn/4.d0+so*dtm(j)*xx(ki2,j)/4.d0/fc
        b(1,ki2)=b(1,ki2)-so*(1.d0-0.5d0*(tm(j)+tm(j+1)))/2.d0/fc
        g(1)=g(1)-epn*hn*(0.375d0*(xx(1,j)-xt(1,j,kk-1+kadd))
     &  +0.125d0*(xx(1,j+1)-xt(1,j+1,kk-1+kadd)))/rr*omi
        if (imp.eq.1) then
          b(1,1+n)=b(1,1+n)-epn*hn*0.375d0*omega(ji) /2.0d0
          d(1,1+n)=-epn*hn*0.125d0*omega(ji)/2.0d0
          b(1+n,1)=b(1+n,1)+epn*hn*0.375d0*omega(ji) /2.0d0
          d(1+n,1)=epn*hn*0.125d0*omega(ji)/2.0d0
        endif ! end of impedance section (equation 1)
      endif
      g(1)=g(1)+(termn(j)+term(j))/2.d0

      if (imp.eq.1) g(1)=0.0d0

c     ________________________________________________
c Equation 2+mpa, material balance in solid insertion material.
c This material balance includes the option of having side-reaction
c species 2 (hydrogen in the NiMH case) absorb or desorb from
c the solid phase in addition to the electrochemical intercalation
c or deintercalation of hydrogen. 
c Either superposition or a full discretization can be used.

      if(imp.eq.0) then ! impedance is off

        if(j.le.n1+1) then ! in the anode
          if (n1.eq.0) then  ! foil anode
            mpa=1 ! only one size with foil anode
            g(2+mpa) = -xx(2+mpa,j) !set concentration to zero
            b(2+mpa,2+mpa) = 1.0d0
          else ! nonfoil anode
            if (mvdc1.eq.0) then ! superposition
              if (nside.eq.3) then ! side reaction with hydrogen
                mpa=1 ! only one size with side reactions
c     If side reaction 3 does not involve absorption or desorption,
c     remove xx(kj3,j) and modify the appropriate Jacobian.  This feature
c     is included to treat hydrogen absorption/desorption in the NiMH system.
                b(2+mpa,kj3)=1.0d0/Rad1pa(mpa)
                b(2+mpa,2+mpa)=ai2(1,mpa)/rr*omi
                b(2+mpa,3+npa+mpa)=1.0d0/Rad1pa(mpa)
                g(2+mpa)=ai2(1,mpa)*xt(2+mpa,j,kk-1)/rr*omi-
     &		  sumpa(mpa,j)-ai2(1,mpa)/rr*omi*xx(2+mpa,j)-
     &          (xx(3+npa+mpa,j)+xx(kj3,j))/Rad1pa(mpa)
              else !no side reaction 3
                do mpa=1,npa ! Calculate g, no side reaction
                  b(2+mpa,2+mpa)=ai2(1,mpa)/rr*omi
                  b(2+mpa,3+npa+mpa)=1.0d0/Rad1pa(mpa)
                  g(2+mpa)=ai2(1,mpa)*xt(2+mpa,j,kk-1)/rr*omi-
     &		    sumpa(mpa,j)-ai2(1,mpa)/rr*omi*xx(2+mpa,j)-
     &            xx(3+npa+mpa,j)/Rad1pa(mpa)
                enddo ! mpa
              endif ! end side (and no side) reactions

c Use a variable diffusion coefficient - anode
            elseif (mvdc1.eq.1) then

              do mpa=1,npa ! allow multiple sizes with vdc
		      flux_temp=xx(3+npa+mpa,j)
			  do icount=1,nnj
			  	do icount2=1,nj
				  css_store(icount2,icount,mpa)=css(icount2,icount,
     &			  mpa)
				enddo
			  enddo
	          if(dabs(xx(3+npa+mpa,j)).le.1.d-8) then
                  fdel = 1.d-13
                else
                  fdel = 1.d-5*xx(3+npa+mpa,j)
                endif
                xx(3+npa+mpa,j)=xx(3+npa+mpa,j)-fdel
                call vardc(j,jcount,kk,iflag_conv,mpa)
			  if (iflag_conv.eq.1) then
	            tau=tau/2.0d0
                  rr=tau
                  Lcount=0
                  ts(kk)=ts(kk-1)+tau
                  iflag=0	
			    print *,'time step reduced to',rr
			    go to 666
			  endif
			  conc_temp=css(j,nnj,mpa)
			  do icount=1,nnj
				do icount2=1,nj
					css(icount2,icount,mpa)=css_store(icount2,icount,
     &				mpa)
				enddo
			  enddo
	          xx(3+npa+mpa,j)=flux_temp
                call vardc(j,jcount,kk,iflag_conv,mpa)
			  if (iflag_conv.eq.1) then
	            tau=tau/2.0d0
                  rr=tau
                  Lcount=0
                  ts(kk)=ts(kk-1)+tau
                  iflag=0	
			    print *,'time step reduced to',rr
			    go to 666
			  endif
                g(2+mpa)=css(j,nnj,mpa)-xx(2+mpa,j)
			  b(2+mpa,2+mpa)=1.0D0
			  b(2+mpa,3+npa+mpa)=(conc_temp-css(j,nnj,mpa))/fdel 
			enddo !mpa loop

            endif ! end VDC negative
          endif ! end nonfoil anode

        elseif (j.ge.n1+n2) then !cathode
          if (mvdc3.eq.0) then ! superposition
            if (nside.eq.3) then ! side reaction with hydrogen
              mpa=1 ! only one size with side reactions
c     If side reaction 3 does not involve absorption or desorption,
c     remove xx(kj3,j) and modify the appropriate Jacobian.  This feature
c     is included to treat hydrogen absorption/desorption in the NiMH system.
              b(2+mpa,2+mpa)=ai(1,mpa)/rr*omi
              b(2+mpa,3+npa+mpa)=1.0d0/Rad3pa(mpa)
              b(2+mpa,kj3)=1.0d0/Rad3pa(mpa)
              g(2+mpa)=ai(1,mpa)*xt(2+mpa,j,kk-1)/rr*omi-
     &		sumpa(mpa,j)-ai(1,mpa)/rr*omi*xx(2+mpa,j)-(xx(3+npa+mpa,j)
     &        +xx(kj3,j))/Rad3pa(mpa)
            else ! no side reaction 3
              do mpa=1,npa
                b(2+mpa,2+mpa)=ai(1,mpa)/rr*omi
                b(2+mpa,3+npa+mpa)=1.0d0/Rad3pa(mpa)
                g(2+mpa)=ai(1,mpa)*xt(2+mpa,j,kk-1)/rr*omi-
     &		  sumpa(mpa,j)-ai(1,mpa)/rr*omi*xx(2+mpa,j)-
     &          xx(3+npa+mpa,j)/Rad3pa(mpa)
              enddo !mpa

            endif !end side reactions

c Use a variable diffusion coefficient - cathode
         elseif (mvdc3.eq.1) then !calculate b's
           do mpa=1,npa ! only one size for variable solid-phase diffusion coefficient
		    flux_temp=xx(3+npa+mpa,j) !save the flux
			do icount=1,nnj
				do icount2=1,nj
					css_store(icount2,icount,mpa)=css(icount2,icount,
     &				mpa) !save the solid concentrations
				enddo
			enddo
	        if(dabs(xx(3+npa+mpa,j)).le.1.d-8) then
                fdel = 1.d-13
              else
                fdel = 1.d-5*xx(3+npa+mpa,j)
              endif
              xx(3+npa+mpa,j) = xx(3+npa+mpa,j) - fdel
              call vardc(j,jcount,kk,iflag_conv,mpa)
			if (iflag_conv.eq.1) then
	            tau=tau/2.0d0
                  rr=tau
                  Lcount=0
                  ts(kk)=ts(kk-1)+tau
                  iflag=0	
			    print *,'time step reduced to',rr
			    go to 666
			endif
			conc_temp=css(j,nnj,mpa) !value obtained in run-through
			do icount=1,nnj
				do icount2=1,nj
					css(icount2,icount,mpa)=css_store(icount2,icount,
     &				mpa) !set back
				enddo
			enddo
	        xx(3+npa+mpa,j)=flux_temp !restore the flux
              call vardc(j,jcount,kk,iflag_conv,mpa)
			if (iflag_conv.eq.1) then
	            tau=tau/2.0d0
                  rr=tau
                  Lcount=0
                  ts(kk)=ts(kk-1)+tau
                  iflag=0	
			    print *,'time step reduced to',rr
			    go to 666
			endif
              g(2+mpa+npa)=css(j,nnj,mpa)-xx(2+mpa,j)
			b(2+mpa+npa,2+mpa)=1.0D0
			b(2+mpa+npa,3+mpa+npa)=(conc_temp-css(j,nnj,mpa))/fdel
	     enddo !mpa
         endif ! end calculate vdc

        else  !separator, set solid concentration to zero
	    do mpa=1,npa
            b(2+mpa,2+mpa)=1.0d0
            g(2+mpa)=-xx(2+mpa,j) 
	    enddo !mpa
        endif

      else ! Impedance section

      mpa=1 ! only one size for impedance
        if(j.eq.1 .and. n1.eq.0) then
c No solid concentration for foil electrode:
        b(2+mpa,2+mpa)=1.0d0
        g(2+mpa)=-xx(2+mpa,j)
        b(2+mpa+n,2+mpa+n)=1.0d0
        g(2+mpa+n)=-xx(2+mpa+n,j)
        g(2+mpa)=0.0d0
         elseif(j.gt.n1+1 .and. j.lt.n1+n2) then ! separator
        b(2+mpa,2+mpa)=1.0d0
        b(2+mpa+n,2+mpa+n)=1.0d0
        g(2+mpa)=0.d0
         else ! anode and cathode
        if(j.eq.1) then
        Rad=Rad1
        dfs=dfs1(mpa)
        elseif(j.eq.n1+n2) then
        Rad=Rad3
        dfs=dfs3(mpa)
        endif
c Solution of diffusion equation w/Laplace gives:
        b(2+mpa,3+npa+mpa)=-1.0d0
        b(2+mpa+n,kj3)=-1.0d0
c New section added April 4, 1998, JSN
        arg=Rad*(2.d0*omega(ji)/dfs)**0.5d0
        e1=expg(-arg)
        e2=e1**2
        if(arg.lt.0.22d0) then
        denom=2.d0*e1*arg**2*(1.d0+arg**4/3.6d2+arg**8/1.8144d6)
        diff2=2.d0*e1*(2.d0*arg+arg**5/6.d1+arg**9/1.99584d7*110.d0)
        diff3=2.d0*e1*(arg**3/3.d0+arg**7/2.52d3+arg**11/1.99584d7)
        Ysr=dfs/Rad*(arg**4/1.8d2+arg**8/4.536d5+arg**12/7.2648576d9)
     %  /1.d0/(1.d0+arg**4/3.6d2+arg**8/1.8144d6)
        else
        denom=1.d0+e2-2.d0*e1*dcos(arg)
        diff2=1.d0-e2+2.d0*e1*dsin(arg)
        diff3=1.d0-e2-2.d0*e1*dsin(arg)
        Ysr=-dfs/Rad*(1.d0-arg/2.d0*diff2/denom)
        endif
        Ysi=dfs/Rad*arg/2.d0*diff3/denom
        real1=-Ysr
        comp1=Ysi
        b(2+mpa,2+mpa)=real1
        b(2+mpa+n,2+mpa+n)=real1
        b(2+mpa,2+mpa+n)=comp1
        b(2+mpa+n,2+mpa)=-comp1
        g(2+mpa)=0.0d0
         endif
      g(2+mpa+n)=0.0d0
      endif ! impedance section (equation 2+mpa)

c     ________________________________________________

c Equation imb2 for variable kS2
c Material balance on the second side-reaction species.
c This is set up for a balance on hydrogen for the NiMH system.

      if (nside.ge.2) then

      sf=1.0d0/2.0d0  !stoichiometric factor for H2 reaction

      termn_s2(j)=0.d0

c aream the area for use in mass balance

      if (j.le.n1+1) aream=area1
      if (j.ge.n1+n2) aream=area3

      fac=1.d0
      if(j.eq.n1+2 .and. n1.ne.0)
     &fac=((ep2+epp2)/(ep1+epp1))**exbrug
      if(j.eq.n1+n2+1) fac=((ep3+epp3)/(ep2+epp2))**exbrug
      epn=ep2+epp2

      hn=h2  !box to left of point.
      epn=epg2
      if (j.le.n1+1) then
      hn=h1
      epn=epg1
      elseif (j.gt.n1+n2) then
      hn=h3
      epn=epg3
      endif

      if (j.eq.n1+n2) sf=0.0d0 !half box w/o generation
      if (j.eq.n1+2) sf=0.0d0 !half box w/0 generation

      if (j.gt.1) then !box to left of point
      termn_s2(j)=-(dh2(j)+fac*dh2(j-1))*(xx(kS2,j)-
     &xx(kS2,j-1))/hn/2.0d0-sf*hn*aream*vol*(0.375d0*(xx(kj3,j)
     &+xx(kj2,j))+0.125d0*(xx(kj3,j-1)+xx(kj2,j-1)))
      g(imb2)=-epn*hn*(0.375d0*(xx(kS2,j)-xt(kS2,j,kk-1+kadd))
     &+0.125d0*(xx(kS2,j-1)-xt(kS2,j-1,kk-1+kadd)))/rr
      b(imb2,kS2)=epn*hn*0.375d0/rr+(dh2(j)+
     &fac*dh2(j-1))/hn/4.0d0+ddh2(j)*(xx(kS2,j)-xx(kS2,j-1))/hn/4.0d0
      a(imb2,kS2)=-(dh2(j)+fac*dh2(j-1))/hn/4.0d0+hn*epn*0.125d0/rr
     &+fac*ddh2(j-1)*(xx(kS2,j)-xx(kS2,j-1))/hn/4.0d0
      b(imb2,kj3)=+0.375d0*hn*sf*aream*vol/2.0d0
      b(imb2,kj2)=+0.375d0*hn*sf*aream*vol/2.0d0
      a(imb2,kj3)=+0.125d0*hn*sf*aream*vol/2.0d0
      a(imb2,kj2)=+0.125d0*hn*sf*aream*vol/2.0d0
      endif

      sf=1.0d0/2.0d0

      fac=1.d0
      if(j.eq.n1+1 .and. n1.ne.0)
     &fac=((ep2+epp2)/(ep1+epp1))**exbrug
      if(j.eq.n1+n2) fac=((ep3+epp3)/(ep2+epp2))**exbrug
      epn=ep2+epp2

      hn=h2 !deal with box to right of point.
      epn=epg2
      if (j.lt.n1+1) then
      hn=h1
      epn=epg1
      elseif (j.ge.n1+n2) then
      hn=h3
      epn=epg3
      endif

      if (j.eq.n1+1) sf=0.0d0 !half box w/o generation
      if (j.eq.n1+n2-1) sf=0.0d0 !half box w/o generation

      if (j.lt.nj) then !box to right of point
      termn_s2(j)=termn_s2(j)-(fac*dh2(j)+dh2(j+1))*
     &(xx(kS2,j)-xx(kS2,j+1))/hn/2.0d0-
     &sf*hn*aream*vol*(0.375d0*(xx(kj3,j)+xx(kj2,j))+0.125d0*
     &(xx(kj3,j+1)+xx(kj2,j+1)))
      g(imb2)=g(imb2)-epn*hn*(0.375d0*(xx(kS2,j)-xt(kS2,j,kk-1+kadd))
     &+0.125d0*(xx(kS2,j+1)-xt(kS2,j+1,kk-1+kadd)))/rr

      b(imb2,kS2)=b(imb2,kS2)+epn*hn*0.375d0/rr+(fac*dh2(j)+dh2(j+1))
     &/hn/4.0d0+fac*ddh2(j)*(xx(kS2,j)-xx(kS2,j+1))/hn/4.0d0
      b(imb2,kj3)=b(imb2,kj3)+sf*hn*aream*vol*0.375d0/2.0d0
      b(imb2,kj2)=b(imb2,kj2)+sf*hn*aream*vol*0.375d0/2.0d0
      d(imb2,kS2)=-(fac*dh2(j)+dh2(j+1))/hn/4.0d0+ddh2(j+1)*
     &(xx(kS2,j)-xx(kS2,j+1))/hn/4.0d0+epn*hn*0.125d0/rr
      d(imb2,kj3)=+sf*hn*aream*vol*0.125d0/2.0d0
      d(imb2,kj2)=+sf*hn*aream*vol*0.125d0/2.0d0
      endif

      g(imb2)=g(imb2)+(termn_s2(j)+term_s2(j))/2.0d0

      endif

c     ________________________________________________

c Equation imb1 for variable kS1
c Material balance on the first side-reaction species.
c This is set up for a balance on oxygen for the NiMH system.
c It can be changed to treat a shuttle species in a lithium-ion cell.

      if (nside.ge.1) then
      sf=-1.0d0/4.0d0  !stoichiometric factor for O2 reaction

	if (nprop.ne.4) then ! Need diff. coeff. for Li-ion side reaction
      do2(j)=1.0d-5 !gas-phase oxygen diffusion coefficient
      ddo2(j)=0.0d0  ! first derivative wrt O2 concentration
	endif

      termn_s1(j)=0.d0

      if (j.le.n1+1) aream=area1
      if (j.ge.n1+n2) aream=area3

      fac=1.d0
      if(j.eq.n1+2 .and. n1.ne.0)
     &fac=((ep2+epp2)/(ep1+epp1))**exbrug
      if(j.eq.n1+n2+1) fac=((ep3+epp3)/(ep2+epp2))**exbrug
      epn=ep2+epp2

      hn=h2  !box to left of point.
c For liquid-phase side reaction see User's Manual.
c      epn=ep2
      epn=epg2
      if (j.le.n1+1) then
      hn=h1
c      epn=ep1
      epn=epg1
      elseif (j.gt.n1+n2) then
      hn=h3
c      epn=ep3
      epn=epg3
      endif

      if (j.eq.n1+n2) sf=0.0d0 !half box w/o generation
      if (j.eq.n1+2) sf=0.0d0 !half box w/0 generation
      if (j.gt.1) then !box to left of point
      termn_s1(j)=-(do2(j)+fac*do2(j-1))*(xx(kS1,j)
     &-xx(kS1,j-1))/hn/2.0d0-sf*hn*aream*vol*(0.375d0*xx(kj1,j)
     &+0.125d0*xx(kj1,j-1))
      g(imb1)=-epn*hn*(0.375d0*(xx(kS1,j)-xt(kS1,j,kk-1+kadd))
     &+0.125d0*(xx(kS1,j-1)-xt(kS1,j-1,kk-1+kadd)))/rr
      b(imb1,kS1)=epn*hn*0.375d0/rr+(do2(j)+fac*do2(j-1))/hn/4.0d0
     &+ddo2(j)*(xx(kS1,j)-xx(kS1,j-1))/hn/4.0d0
      a(imb1,kS1)=-(do2(j)+fac*do2(j-1))/hn/4.0d0+epn*hn*0.125d0/rr
     &+fac*ddo2(j-1)*(xx(kS1,j)-xx(kS1,j-1))/hn/4.0d0
      b(imb1,kj1)=+0.375d0*hn*sf*aream*vol/2.0d0
      a(imb1,kj1)=+0.125d0*hn*sf*aream*vol/2.0d0
      endif

      sf=-1.0d0/4.0d0

      fac=1.d0
      if(j.eq.n1+1 .and. n1.ne.0)
     &fac=((ep2+epp2)/(ep1+epp1))**exbrug
      if(j.eq.n1+n2) fac=((ep3+epp3)/(ep2+epp2))**exbrug
      epn=ep2+epp2

      hn=h2 !deal with box to right of point.
c For liquid-phase side reaction see User's Manual.
c     epn=ep2
      epn=epg2
      if (j.lt.n1+1) then
      hn=h1
c     epn=ep1
      epn=epg1
      elseif (j.ge.n1+n2) then
      hn=h3
c     epn=ep3
      epn=epg3
      endif

      if (j.eq.n1+1) sf=0.0d0 !half box w/o generation
      if (j.eq.n1+n2-1) sf=0.0d0 !half box w/o generation

      if (j.lt.nj) then !box to right of point
      termn_s1(j)=termn_s1(j)-(fac*do2(j)+do2(j+1))*(xx(kS1,j)
     &-xx(kS1,j+1))
     &/hn/2.0d0-sf*hn*aream*vol*(0.375d0*xx(kj1,j)+0.125d0*xx(kj1,j+1))
      g(imb1)=g(imb1)-epn*hn*(0.375d0*(xx(kS1,j)-xt(kS1,j,kk-1+kadd))
     &+0.125d0*(xx(kS1,j+1)-xt(kS1,j+1,kk-1+kadd)))/rr

      b(imb1,kS1)=b(imb1,kS1)+epn*hn*0.375d0/rr+(fac*do2(j)+do2(j+1))
     &/hn/4.0d0+fac*ddo2(j)*(xx(kS1,j)-xx(kS1,j+1))/hn/4.0d0
      b(imb1,kj1)=b(imb1,kj1)+sf*hn*aream*vol*0.375d0/2.0d0
      d(imb1,kS1)=-(fac*do2(j)+do2(j+1))/hn/4.0d0+epn*hn*0.125d0/rr-
     &ddo2(j+1)*(xx(kS1,j)-xx(kS1,j+1))/hn/4.0d0
      d(imb1,kj1)=+sf*hn*aream*vol*0.125d0/2.0d0
      endif

      g(imb1)=g(imb1)+(termn_s1(j)+term_s1(j))/2.0d0

      endif

  199 if(imp.eq.1) g(12)=0.0d0

c     ________________________________________________
c     Equation ip2, Ohm's law in the liquid phase.
c     There are separate versions of this equation for a Li-ion
c     system and for the NiMH system.
      if(j.le.n1) then
      h=h1
      else if(j.lt.n1+n2) then
      h=h2
      else
      h=h3
      endif
      fac=1.0d0
      if(j.eq.n1+1 .and. n1.ne.0)
     1fac=((ep2+epp2)/(ep1+epp1))**exbrug
      if(j.eq.n1+n2)
     1fac=((ep3+epp3)/(ep2+epp2))**exbrug
c
         if(j.eq.nj) then
      b(ip2,kp2)=1.0d0
      g(ip2)=-xx(kp2,j) !boundary condition on potential
      if (imp.eq.1) g(ip2)=0.0d0
      go to 12
         endif

      if (nprop.eq.4) then 
c Ohm's law in solution for NiMH different from Lithium
      dcf=(xx(1,j+1)-xx(1,j))/h 
      r1=(xx(1,j+1)+xx(1,j))/2.0d0
      r4=xx(ki2,j)
      p1=1.0d0-(tm(j)+tm(j+1))/2.0d0 !different from used in mass balance
      p2=(fac*cd(j)+cd(j+1))/2.0d0
      p3=(fac*dcd(j)+dcd(j+1))/2.0d0
      H2OMW=18.016D0
      PKOHMW=56.11D0
      c1=r1/1.0d6
      fu= 1.004D0-36.23D0*c1**0.5+1374.3D0*c1-17850.7d0*c1**1.5d0+
     &55406.0D0*c1**2d0+7.16856D05*c1**2.5d0
      d_fu=-18.115D0*c1**(-0.5)+1374.3D0-2.6776D04*c1**0.5
     &+1.10812D05*c1 + 1.7921D06*c1**1.5
      d2_fu=9.0575D0/c1**1.5-13388.0D0/c1**0.5+1.10812D05+
     & 2.6882D06*c1**0.5
      d_fu=d_fu/1.0d6
      d2_fu=d2_fu/1.0d12
      densw=1.001D0+47.57D0*c1-776.22D0*c1**2
      dens_1d = 47.57D0 - 1552.44D0*c1
      dens_2d= -1552.44D0
      densw=densw*1.0d6
      dens_1d=dens_1d*1.0d0
      dens_2d=dens_2d/1.0d6

      c_solv=(densw-xx(1,j)*PKOHMW)/H2OMW

      U1=densw-xx(1,j)*PKOHMW
      U2=dens_1d-PKOHMW
      U3=-dcf*((p1*U1+H2OMW*xx(1,j))*((fu*d2_fu-d_fu**2.0D0)/
     &(fu**2.0D0)-1.0D0/xx(1,j)**2.0D0)+(d_fu/fu+1.0D0/xx(1,j))
     &*(p1*U2+H2OMW))
      U4=-p1*(d_fu*densw/fu +densw/xx(1,j)) -(H2OMW-p1*PKOHMW)*
     &(d_fu*xx(1,j)/fu + 1.0D0)

      d(ip2,1)=-frt*xx(ki2,j)/p2/p2*p3/2.0d0
      d(ip2,1)=d(ip2,1)+(p1+r1/c_solv)*(d_fu/fu+1/r1)*(1.0d0/h)
      d(ip2,1)=d(ip2,1)+(p1+r1/c_solv)*dcf*(-d_fu/fu/fu*d_fu*0.5d0
     &+0.5d0/fu*d2_fu-0.5d0/r1/r1)
      d(ip2,1)=d(ip2,1)+(d_fu/fu+1/r1)*dcf*(-0.5d0*r1*H2OMW/U1**2.d0*U2
     &+0.5*H2OMW/U1)
      d(ip2,1)=-d(ip2,1)

      b(ip2,1)=frt*xx(ki2,j)/p2/p2*p3/2.0d0
      b(ip2,1)=b(ip2,1)+(p1+r1/c_solv)*(d_fu/fu+1/r1)*(1.0d0/h)
      b(ip2,1)=b(ip2,1)+(p1+r1/c_solv)*dcf*(-d_fu/fu/fu*d_fu*0.5d0
     &+0.5d0/fu*d2_fu-0.5d0/r1/r1)
      b(ip2,1)=b(ip2,1)+(d_fu/fu+1/r1)*dcf*(-0.5d0*r1*H2OMW/U1**2.d0*U2
     &+0.5*H2OMW/U1)
      b(ip2,1)=-b(ip2,1)
      d(ip2,kp2)=frt/h
      b(ip2,kp2)=-frt/h
      b(ip2,ki2)=frt/p2
      g(ip2)=-frt*(xx(kp2,j+1)-xx(kp2,j))/h-frt/p2*xx(ki2,j)
     &-(p1+r1/c_solv)*(d_fu*1/fu+1/r1)*dcf
      else ! for Lithium chemistry

      dcf=(xx(1,j+1)-xx(1,j))/h *2.0d0 !factor of 2 added
      r1=(xx(1,j+1)+xx(1,j))/2.0d0
      r4=xx(ki2,j)
      p1=(tm(j)+tm(j+1))/2.0d0
      p2=(fac*cd(j)+cd(j+1))/2.0d0
      p4=(dfu(j)+dfu(j+1))/2.0d0
      d(ip2,1)=(1.0d0-p1)*(1.0d0/r1+p4)/h *2.0d0 !factor of 2 added
      b(ip2,1)=-d(ip2,1)+((1.0d0-p1)*(d2fu(j)-1.0d0/r1/r1)*dcf
     &-(1.0d0/r1+p4)*dcf*dtm(j)+frt*r4*fac*dcd(j)/p2/p2)/2.0d0
      d(ip2,1)=d(ip2,1)+((1.0d0-p1)*(d2fu(j+1)-1.0d0/r1/r1)*dcf
     &-(1.0d0/r1+p4)*dcf*dtm(j+1)+frt*r4*dcd(j+1)/p2/p2)/2.0d0
      d(ip2,kp2)=-frt/h
      b(ip2,kp2)=frt/h
      b(ip2,ki2)=-frt/p2
      g(ip2)=-(1.0d0-p1)*(1.0d0/r1+p4)*dcf+frt*(xx(kp2,j+1)-xx(kp2,j))/h
     &+frt/p2*xx(ki2,j)
      endif

      if (imp.eq.1) g(ip2)=0.0d0
   12 if(imp.eq.1) g(ip2+n)=0.0d0

c     ________________________________________________
c     Equation 3, Butler-Volmer kinetics
c     The g() and b() values in the electrodes are written at the 
c     end of subroutine ekin()

      do mpa=1,npa
        if(j.gt.n1+1 .and. j.lt.n1+n2) then !separator
          if (imp.eq.0) then
            b(3+npa+mpa,3+npa+mpa)=1.0d0
            g(3+npa+mpa)=-xx(3+npa+mpa,j) ! zero transfer current in separator
          else
            b(3+npa+mpa,3+npa+mpa)=1.0d0
            g(3+npa+mpa)=-xx(3+npa+mpa,j)
          endif
        else
          call ekin(j,kk,0,0.0d0,mpa,mvdc1,mvdc3)
          if(j.eq.n1+n2 .and. mpa.eq.1) Uc=g0(mpa)
          if(j.eq.1 .and. mpa.eq.1)     Ua=g0(mpa)
        endif
      enddo ! mpa

      mpa=1 ! only one size for impedance
      if (imp.eq.1) g(3+npa+mpa)=0.0d0
      if(imp.eq.1) g(3+npa+mpa+n)=0.0d0

c ________________________________________________
c    Equation iSk1.  Side reaction 1 kinetics
c    This is set up for oxygen evolution (Tafel) and 
c    recombination (concentration driven) for the NiMH system.
c    We also have a sample side reaction for solvent reduction/oxidation
c    in a Li-ion system.  Tafel kinetics is used at each electrode.
c    For this reaction, we recommend setting rksa1=1.0d-9 and rksac1=1.0d-9
c    in the input file.

      if (nside.ge.1) then

      rksa1_save=rksa1
      rksc1_save=rksc1
      ti0n=dexp((Ebarks1a)*(t-298.0d0)/(t*298.0d0))
      ti0p=dexp((Ebarks1c)*(t-298.0d0)/(t*298.0d0))
      rksa1=rksa1*ti0n
      rksc1=rksc1*ti0p

      alphasO2=1.17d0
      UsO2=0.25d0

      if (j.le.n1+1) then  !recombination at the anode
      g(iSk1)=xx(kj1,j)+rksa1*xx(kS1,j)
      b(iSk1,kj1)=-1.0d0
      b(iSk1,kS1)=-rksa1

c Sample kinetics for side reaction in a Li-ion system: comment out
c the above and uncomment the below.  We use an alpha value of 0.5
c and use a value of 1.0 V for the "equilibrium" potential of
c solvent reduction.

c      g(iSk1)=-xx(kj1,j)-rksa1*expg(-0.5d0*frt*(xx(kp1,j)-xx(kp2,j)-1.d0))
c      b(iSk1,kj1)=1.0d0
c      b(iSk1,kp1)=-rksa1*0.5d0*frt*expg(-0.5d0*frt*(xx(kp1,j)-xx(kp2,j)
c     &-1.d0))
c      b(iSk1,kp2)=-b(iSk1,kp1)

      elseif (j.ge.n1+n2) then !evolution at the cathode
      g(iSk1)=xx(kj1,j)-rksc1*expg(alphasO2*frt*(xx(kp1,j)-xx(kp2,j)
     &-UsO2))
      b(iSk1,kj1)=-1.0d0
      b(iSk1,kp1)=rksc1*alphasO2*frt*expg(alphasO2*frt*
     &(xx(kp1,j)-xx(kp2,j)-UsO2))
      b(iSk1,kp2)=-b(iSk1,kp1)

c Sample kinetics for side reaction in a Li-ion system: comment out
c the above and uncomment the below.  We use an alpha value of 0.5
c and use a value of 4.4 V for the "equilibrium" potential of 
c solvent oxidation

c      g(iSk1)=xx(kj1,j)-rksc1*expg(0.5d0*frt*(xx(kp1,j)-xx(kp2,j)-4.d0))
c      b(iSk1,kj1)=-1.0d0
c      b(iSk1,kp1)=rksc1*0.5d0*frt*expg(0.5d0*frt*(xx(kp1,j)-
c     1xx(kp2,j)-4.d0))
c      b(iSk1,kp2)=-b(iSk1,kp1)

      else !in the separator
      g(iSk1)=xx(kj1,j)
      b(iSk1,kj1)=-1.0d0
      endif

      else
      g(iSk1)=0.0d0
      endif

      rksa1=rksa1_save
      rksc1=rksc1_save

c ________________________________________________
c    Equation iSk2.  Side reaction 2 kinetics 
c    This is set up for hydrogen evolution from the negative electrode
c    of the NiMH system.

      if (nside.ge.2) then
      alphasH2=1.0d0
      UsH2=-0.98d0

      rksa2_save=rksa2
      rksc2_save=rksc2
      ti0n=dexp((Ebarks3a)*(t-298.0d0)/(t*298.0d0))
      ti0p=dexp((Ebarks3c)*(t-298.0d0)/(t*298.0d0))
      rksa2=rksa2*ti0n
      rksc2=rksc2*ti0p

      if (j.le.n1+1) then  
c H2 evolution from the negative electrode only.
      g(iSk2)=-xx(kj2,j)-rksa2*expg(-alphasH2*frt*(xx(kp1,j)
     &-xx(kp2,j)-UsH2))
      b(iSk2,kj2)=1.0d0
      b(iSk2,kp1)=-rksa2*alphasH2*frt*
     &expg(-alphasH2*frt*(xx(kp1,j)-xx(kp2,j)-UsH2))
      b(iSk2,kp2)=-b(iSk2,kp1)
      else !in the separator or the positive
      g(iSk2)=xx(kj2,j) 
      b(iSk2,kj2)=-1.0d0
      endif

      else
      if(imp.eq.1) g(3+n)=0.0d0
      endif

      rksa2=rksa2_save
      rksc2=rksc2_save

c ________________________________________________
c    Equation iSk3.  Side reaction 3 kinetics
c    This is set up for hydrogen absorption/desorption in the
c    negative electrode of the NiMH system.
c    Positive j is H leaving the solid phase

      if (nside.eq.3) then

      rksa3_save=rksa3
      rksc3_save=rksc3
      ti0e=dexp((Ebarks2a)*(t-298.0d0)/(t*298.0d0))
      ti0r=dexp((Ebarks2c)*(t-298.0d0)/(t*298.0d0))
      rksa3=rksa3*ti0e
      rksc3=rksc3*ti0r

      mpa=1 ! only one size with side reactions
      if (j.le.n1+1) then  
c H2 absorption, using an equilibrium isotherm from the literature

      g(iSk3)=-xx(kj3,j)+rksa3*(xx(kS2,j)*t*8.206d-5-
     &(5.d0+4.9999d0*dtanh((xx(2+mpa,j)/ct1(mpa)-1.04d0)/0.11d0)))
      b(iSk3,kS2)=-rksa3*t*8.2d-5
      b(iSk3,2+mpa)=-rksa3*4.9999d0*
     &((dcosh((xx(2+mpa,j)/ct1(mpa)-1.04d0)/0.11d0))**(-2))
     &*1.0d0/0.11d0/ct1(mpa)
      b(iSk3,kj3)=1.0d0

      elseif (j.ge.n1+n2) then !reaction at the positive electrode

      g(iSk3)=xx(kj3,j)
      b(iSk3,kj3)=-1.0d0

      else !in the separator
      g(iSk3)=xx(kj3,j)
      b(iSk3,kj3)=-1.0d0
      endif

      else
      mpa=1 ! only one size for impedance
      if(imp.eq.1) g(5+n)=0.0d0
      endif

      rksa3=rksa3_save
      rksc3=rksc3_save

c     ________________________________________________
c     Equation ip1, Ohm's law in the solid phase.
c
      if(j.eq.1) then
      if (n1.eq.0) then ! foil electrode
	mpa=1 
      g(ip1) = cur-fc*xx(3+npa+mpa,j)
      b(ip1,3+npa+mpa) = fc
      if(imp.eq.1) then
      g(ip1) = 1.d0
c     area1 is mult by L here so aL is 1 in double-layer charging term:
      b(ip1,kp2+n)=omega(ji)*capp1
      b(ip1,kp1+n)=-omega(ji)*capp1
      b(ip1+n,kp2)=-omega(ji)*capp1
      b(ip1+n,kp1)=omega(ji)*capp1
      endif ! impedance section (j=1, a)
      else
      b(ip1,kp1)=-1.0d0/h1 ! not a foil electrode
      d(ip1,kp1)= 1.0d0/h1
      b(ip1,ki2)=-1.0d0/sig1
c     next statement requires that cur be the current in the separator
      g(ip1)=-cur/sig1+xx(ki2,j)/sig1-(xx(kp1,j+1)-xx(kp1,j))/h1
      if(imp.eq.1) g(ip1)=-1.0d0/sig1
      endif
      elseif(j.lt.n1+1) then ! anode
      b(ip1,kp1)=-1.0d0/h1
      d(ip1,kp1)= 1.0d0/h1
      b(ip1,ki2)=-1.0d0/sig1
      g(ip1)=-cur/sig1+xx(ki2,j)/sig1-(xx(kp1,j+1)-xx(kp1,j))/h1
      if(imp.eq.1) g(ip1)=-1.0d0/sig1
      elseif(j.eq.n1+1) then
c     This is the current boundary condition.
            if(mcL.eq.-3) then ! constant load
      g(ip1)=(xx(kmode,j+1)-xx(kmode,j))/xx(ki2,j)-cuL-RG 
      d(ip1,kmode)=-1.d0/xx(ki2,j)
      b(ip1,kmode)=1.d0/xx(ki2,j)
      b(ip1,ki2)=(xx(kmode,j+1)-xx(kmode,j))/xx(ki2,j)**2
            elseif(mcL.eq.0) then ! constant potential
      g(ip1)=xx(kmode,j+1)-xx(kmode,j)-cuL-RG*xx(ki2,j)
      d(ip1,kmode)=-1.d0
      b(ip1,kmode)=1.d0
      b(ip1,ki2)=RG !WHT 1-11-08
            elseif(mcL.eq.-2) then ! constant power
      g(ip1)=(xx(kmode,j+1)-xx(kmode,j))*xx(ki2,j)-cuL-RG*xx(ki2,j)**2
      d(ip1,kmode)=-xx(ki2,j)
      b(ip1,kmode)=xx(ki2,j)
      b(ip1,ki2)=-(xx(kmode,j+1)-xx(kmode,j))+2.d0*RG*xx(ki2,j) 
            else ! constant current
      b(ip1,ki2)=1.0d0
      g(ip1)=cur-xx(ki2,j)
            endif

      if(imp.eq.1) g(ip1)=1.0d0
      elseif(j.lt.n1+n2) then
      b(ip1,kp1)=1.0d0 ! separator, set phi1=0
      g(ip1)=-xx(kp1,j)
      if(imp.eq.1) g(ip1)=0.0d0
      elseif(j.lt.nj) then ! cathode
      d(ip1,kp1)=1.0d0/h3
      b(ip1,kp1)=-1.0d0/h3
      b(ip1,ki2)=-1.0d0/sig3
      g(ip1)=-cur/sig3+xx(ki2,j)/sig3-(xx(kp1,j+1)-xx(kp1,j))/h3
      if(imp.eq.1) g(ip1)=-1.0d0/sig3
      else ! j=nj
      b(ip1,ki2)=1.0d0
      g(ip1)=-xx(ki2,j) !  i2 is no longer used at j=nj
      if(imp.eq.1) g(ip1)=0.d0
      endif
      if(imp.eq.1) g(12)=0.0d0

c     ________________________________________________

c     Equation imode.  Carry end potentials.  This equation is added to
c     permit handling constant load, etc. without an extra loop.
      if (imp.eq.0) then
      if(j.eq.1) then
      g(imode)=xx(kp1,1)-xx(kmode,1) ! pick up negative potential
      b(imode,kp1)=-1.d0
      b(imode,kmode)=1.d0
      elseif(j.eq.nj) then
      g(imode)=xx(kp1,nj)-xx(kmode,nj) ! pick up positive potential
      b(imode,kp1)=-1.d0
      b(imode,kmode)=1.d0
      elseif(j.le.n1+1) then
      g(imode)=xx(kmode,j)-xx(kmode,j-1) ! carry negative potential
      b(imode,kmode)=-1.d0
      a(imode,kmode)=1.d0
      else ! if(j.ge.n1+n2) then
      g(imode)=xx(kmode,j)-xx(kmode,j+1) ! carry positive potential
      b(imode,kmode)=-1.d0
      d(imode,kmode)=1.d0
      endif
      endif

c     ________________________________________________
c
c     Equation ii2.  Current balance that relates the pore-wall flux 
c     values to the current in the liquid phase.

      if(j.gt.n1+1 .and. j.lt.n1+n2) then ! separator
        g(ii2)=xx(ki2,j-1)-xx(ki2,j) !constant current in separator
        if (imp.eq.0) a(ii2,ki2)=-1.d0
        b(ii2,ki2)=1.d0
        if (imp.eq.1) then ! impedance section
c     Set perturbation current = 1 purely real:
          b(ii2,ki2)=1.0d0
          g(ii2)=1.0d0
        endif ! end of impedance section (n1+1<j<n1+n2)
c
      elseif(j.eq.1.and.n1.eq.0) then ! foil electrode
        g(ii2) = cur - xx(ki2,j) !set the current
        b(ii2,ki2) = 1.0d0
        if(imp.eq.1) g(ii2)=1.d0 ! set perturbation current = 1 purely real

      else  !no impedance, no foil
        if(cap.eq.0.d0) then
          if (nside.ge.1) then ! Side reactions, both 1,2,3
	      mpa=1 !one particle for side reactions
            b(ii2,ki2)=-1.0d0/hC
            a(ii2,ki2)=1.0d0/hC
            b(ii2,3+npa+mpa)=area*fc
            b(ii2,kj1)=area*fc
            b(ii2,kj2)=area*fc
            g(ii2)=xx(ki2,j)/hC-area*fc*(xx(3+npa+mpa,j)
     &      +xx(kj1,j)+xx(kj2,j))
c written only for j=1
            if(j.gt.1) g(ii2)=(xx(ki2,j)-xx(ki2,j-1))/hC-
     &    area*fc*(xx(3+npa+mpa,j)+xx(kj1,j)+xx(kj2,j))
            if (nside.eq.1) b(ii2,kj2)=0.0d0

          else ! No side reactions, can be multiple particles
            b(ii2,ki2)=-1.0d0/hC
            g(ii2)=xx(ki2,j)/hC
		  areasave=area
            do mpa=1,npa
	        if (j.le.n1+1) area=area1pa(mpa)
	        if (j.ge.n1+n2) area=area3pa(mpa)
		    g(ii2)=g(ii2)-area*fc*xx(3+npa+mpa,j) 
		    b(ii2,3+mpa+npa)=area*fc
            enddo !mpa
            area=areasave

            if (j.gt.1) then 
              b(ii2,ki2)=-1.0d0/hC
              a(ii2,ki2)=1.0d0/hC
              g(ii2)=(xx(ki2,j)-xx(ki2,j-1))/hC
		    areasave=area
              do mpa=1,npa
	          if (j.le.n1+1) area=area1pa(mpa)
	          if (j.ge.n1+n2) area=area3pa(mpa)
		      g(ii2)=g(ii2)-area*fc*xx(3+npa+mpa,j) 
		      b(ii2,3+mpa+npa)=area*fc
              enddo !mpa
	        area=areasave
	      endif 
          endif

        else !nonzero value of capacitances
        if (rr .eq. 0.d0) then ! no instantaneous change across double layer
          g(ii2)=xx(kp1,j)-xx(kp2,j)-xt(kp1,j,kk-1+kadd)+
     &    xt(kp2,j,kk-1+kadd)
          b(ii2,kp1)=-1.d0
          b(ii2,kp2)= 1.d0
        else !nonzero capacitance and nonzero time step
	    mpa=1
          b(ii2,ki2)=-1.0d0/hC
          a(ii2,ki2)=1.0d0/hC
          b(ii2,3+npa+mpa)=area*fc
          b(ii2,kj1)=area*fc
          b(ii2,kj2)=area*fc
          b(ii2,kp1)=area*cap/rr*omi*2.0d0
          b(ii2,kp2)=-area*cap/rr*omi*2.0d0
          g(ii2)=(xx(ki2,j)+xt(ki2,j,kk-1+kadd))/hC
          if(j.gt.1) g(ii2)=g(ii2)+(-xx(ki2,j-1)-xt(ki2,j-1,
     &  kk-1+kadd))/hC
          g(ii2)=g(ii2)-area*fc*(xx(3+npa+mpa,j)+
     &    xt(3+npa+mpa,j,kk-1+kadd)
     &    -xx(kj1,j)-xt(kj1,j,kk-1+kadd)-xx(kj2,j)-xt(kj2,j,kk-1+kadd))
     &    -area*cap*(xx(kp1,j)-xx(kp2,j)-xt(kp1,j,kk-1+kadd)
     &    +xt(kp2,j,kk-1+kadd))/rr*omi*2.0d0
        endif
      endif
c
      if (imp.eq.1) then ! impedance section
	mpa=1
c     Add double-layer charging:
      b(ii2,kp2+n)=omega(ji)*area*capp
      b(ii2,kp1+n)=-omega(ji)*area*capp
      b(3+npa+mpa+n,kp2)=-omega(ji)*area*capp
      b(3+npa+mpa+n,kp1)=omega(ji)*area*capp
      g(ii2)=0.0d0
      g(3+npa+mpa+n)=0.0d0
      endif ! end of impedance section (Equation ii2)
      endif

c     ________________________________________________
c
  121 if (imp.eq.1) then ! impedance section
c     Copy real matrices into imag matrices:
      do 122 nct1=1, n
      do 122 nct2=1, n
      if(j.eq.1) x(nct1+kp1,nct2+kp1)=x(nct1,nct2)
      if(j.gt.1) a(nct1+kp1,nct2+kp1)=a(nct1,nct2)
      b(nct1+kp1,nct2+kp1)=b(nct1,nct2)
      if(j.eq.nj) y(nct1+kp1,nct2+6)=y(nct1,nct2)
      if(j.lt.nj) d(nct1+6,nct2+6)=d(nct1,nct2)
  122 continue
      endif

c     double the number of variables (real & imaginary parts)
      if(imp.eq.1) n=2*n

c *** THE CALL OF BAND ***

            call band(j)
      if(imp.eq.1) n=n/2

c *** UNLESS AT THE LAST NODE GO SET UP ANOTHER NODE ***
      if(j.lt.nj) go to 10

c *** ADD CHANGE VARIABLES TO CURRENT VARIABLE VALUES ***
      if (imp.eq.0) then
      do 607 jj=1,nj
      do 607 i=1,nx
  607 xx(i,jj)=xx(i,jj)+c(i,jj)  
      endif ! make corrections for imp.ne.1

c     ________________________________________________
c
c     print impedance results
c
      if (imp.eq.1) then
      vreal(ji)=xx(kp1,1)-xx(kp1,nj)+RG !WHT 1-11-08
      vimag(ji)=xx(kp1+n,1)-xx(kp1+n,nj)
c     Half-cell impedance calculations:
      vreala(ji)=xx(kp1,1)-xx(kp2,n1+n2)+RGn !WHT 1-11-08
      vrealc(ji)=xx(kp2,n1+n2)-xx(kp1,nj)+RGp !WHT 1-11-08
      vimaga(ji)=xx(kp1+n,1)-xx(kp2+n,n1+n2)
      vimagc(ji)=xx(kp2+n,n1+n2)-xx(kp1+n,nj)
c     Z magnitude and phase angle calculation:
      zmag(ji)=(vreal(ji)*vreal(ji)+vimag(ji)*vimag(ji))**0.5d0
      zmaga(ji)=(vreala(ji)*vreala(ji)+vimaga(ji)*vimaga(ji))**0.5d0
      zmagc(ji)=(vrealc(ji)*vrealc(ji)+vimagc(ji)*vimagc(ji))**0.5d0
      phi(ji)=180.0d0/pi*datan(- vimag(ji)/vreal(ji))
      phia(ji)=180.0d0/pi*datan(- vimaga(ji)/vreala(ji))
      phic(ji)=180.0d0/pi*datan(- vimagc(ji)/vrealc(ji))

      write (2,103)omega(ji),(10**4)*vreal(ji),(10**4)*vimag(ji)  !Jun29

      rr=0.01 !to compensate for rr=1 set to avoid div by 0
      ji=ji+1
      return
      endif ! end of impedance section (printing)
c
      do 56 i=1,n
   56 xp(i)=(4.0d0*xx(i,2)-3.0d0*xx(i,1)-xx(i,3))/2.0d0/h1
      nerr=0
      nconcflag = 0
      do 25 j=1,nj
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c  *** SHOE HORNS ARE USED TO AID CONVERGENCE ***

      if(xx(1,j).lt.xx(1,j)/100.d0) xx(1,j)=xx(1,j)/100.d0
      if(xx(1,j).gt.xx(1,j)*100.d0) xx(1,j)=xx(1,j)*100.0d0
      if (xx(kp2,j).lt.(xx(kp2,j)-0.02d0)) xx(kp2,j)=xx(kp2,j)-0.02d0
      if (xx(kp2,j).gt.(xx(kp2,j)+0.02d0)) xx(kp2,j)=xx(kp2,j)+0.02d0
      if (xx(kp1,j).lt.(xx(kp1,j)-0.02d0)) xx(kp1,j)=xx(kp1,j)-0.02d0
      if (xx(kp1,j).gt.(xx(kp1,j)+0.02d0)) xx(kp1,j)=xx(kp1,j)+0.02d0
      if (xx(kp2,j).gt. 9.9d0) xx(kp2,j)= 9.9d0
      if (xx(kp2,j).lt.-9.9d0) xx(kp2,j)=-9.9d0
      if (xx(kp1,j).gt. 9.9d0) xx(kp1,j)= 9.9d0
      if (xx(kp1,j).lt.-9.9d0) xx(kp1,j)=-9.9d0
c
      do mpa=1,npa
      if (j .ge. n1+n2) then
      if(xx(2+mpa,j).lt.xx(2+mpa,j)/100.d0) then
      nerr=nerr+1
      xx(2+mpa,j)=xx(2+mpa,j)/100.d0 ! use cs min
      endif
      if(xx(2+mpa,j).gt.xx(2+mpa,j)*100.d0) xx(2+mpa,j)=
     &xx(2+mpa,j)*100.d0 
      if(ct3(mpa)-xx(2+mpa,j).le.(ct3(mpa)-xx(2+mpa,j))/100.d0) then
      nerr=nerr+1
      xx(2+mpa,j)=ct3(mpa)-(ct3(mpa)-xx(2+mpa,j))/100.d0
      endif
      if(xx(2+mpa,j).ge.ct3(mpa)) xx(2+mpa,j)=0.999999d0*ct3(mpa)
c
      else if (j .le. n1+1 .and. n1.ne.0) then

      if(xx(2+mpa,j).lt.xx(2+mpa,j)/100.d0) nerr=nerr+1
      if(xx(2+mpa,j).lt.xx(2+mpa,j)/100.d0) 
     &xx(2+mpa,j)=xx(2+mpa,j)/100.d0 ! use cs min
      if(xx(2+mpa,j).gt.xx(2+mpa,j)*100.d0) 
     &xx(2+mpa,j)=xx(2+mpa,j)*100.d0 
      if(ct1(mpa)-xx(2+mpa,j).le.(ct1(mpa)-xx(2+mpa,j))/100.d0) 
     &nerr=nerr+1
      if(ct1(mpa)-xx(2+mpa,j).le.(ct1(mpa)-xx(2+mpa,j))/100.d0)
     & xx(2+mpa,j)=ct1(mpa)-(ct1(mpa)-xx(2+mpa,j))/100.d0
      if(xx(2+mpa,j).ge.ct1(mpa)) xx(2+mpa,j)=0.999999d0*ct1(mpa)
      endif
c
c     Not shoe-horns, but trips to force smaller time steps
      if (j. ge .n1+n2 .or. (j .le. n1+1 .and. n1.ne.0)) then
      if(xx(2+mpa,j).lt.1.0d-10) xx(2+mpa,j) = 1.d-10
      if(xx(2+mpa,j).lt.1.0d-3) nconcflag = 1
      endif
      enddo ! mpa
c
c Here is a shoe horn for the liquid-phase concentration.
c This will likely violate the material balance.
      if(xx(1,j).lt.0.1d0) then 
      xx(1,j) = 0.1d0
      endif
      if(xx(1,j).lt.1.0d-03) nconcflag = 2
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
      xx(kmode,j)=xx(ip1,nj)
      if(j.le.n1+1) xx(kmode,j)=xx(ip1,1)
      do 25 i=1,n+1
	c(i,j)=xx(i,j)
   25 xx(i,j)=c(i,j) 

      if (jcount .gt. 3*lim .and. rr.eq.0.0d0) then
      write (2,99)

      stop
      endif
c     ________________________________________________
c
c *** DECREASE THE TIME STEP IF COMP() CAN'T CONVERGE ***
      if (jcount .gt. lim .and. rr.gt.1.0d-16) then
        tau=tau/2.0d0
        rr=tau
        Lcount=0
        if(mcLL.eq.-4) cur=curold
        ts(kk)=ts(kk-1)+tau
c     print *,'time step reduced to ', tau, ts(kk),jcount
        if (tau.lt.1.0d-4) then
      nconv=1  !trigger the flag for no convergence
          nt=nt-1
          pow=ed/tw/ts(nt+1)
      write (2,*) 'mass is ',tw
      write (2,*) 'energy is ',ed/tw/3.6d03
      write (2,*) 'power is ',pow
          write (2,*) kk-1,' this time step did not converge'
          if (nconcflag .eq. 1) then
            write (2,*), 'Solid concentration driven to zero.'
            write (2,*),'Consider changing vcut or the current density.'
            if (nconcflag.eq.3) then
              write (2,*) 'The solid utilization is above 1.'
            endif
          endif
          if (nconcflag .eq. 2) then
            write (2,*), 'Salt concentration driven to zero.'
            write (2,*),'Consider changing vcut or the current density.'
      endif
      write (2,*) ' '
      if (kk.gt.5) then
      write (2,*) 'dV/dt at point of failure is (V/s)', 
     &    ((xt(kp1,nj,kk-1)-xt(kp1,1,kk-1))-(xt(kp1,nj,kk-2)
     &    -xt(kp1,1,kk-2)))/(ts(kk-1)-ts(kk-2))
      write (2,*) 'Moving on to next leg if not the last'
      write (2,*) ' '
      endif
      return
          else
            iflag=0
            call calca(kk)
            go to 666
          end if
c
      else
c     ________________________________________________
c
c *** CHECK FOR CONVERGENCE ***
c     The system has converged when either the change between iterations
c     is less than 1.d-9 the variable value OR the change is less
c     than an error limit.
c
      mpa=1
      if(nerr.ne.0) go to 8
      do jj=1,nj
        do ki=1,n
          errlim=1.d-10
          if(ki.eq.3+mpa+npa) errlim=1.d-16 !change to -14 if converge problems
          dxx=dabs( xx(ki,jj) - xx0(ki,jj) )
        enddo
      enddo
      do 55 ki=1,n
        errlim=1.d-10
        if(ki.eq.3+npa+mpa) errlim=1.d-16 !change to -14 if converge problems
        dxp=dabs(xp(ki)-xp0(ki))
        n1hold=1
        if (n1.ge.11) n1hold=n1-10
        dxx=dabs(xx(ki,n1hold)-xx0(ki,n1hold))
        dxx2=dabs(xx(ki,n1+n2+10)-xx0(ki,n1+n2+10))
        dxx3=dabs(xx(ki,2)-xx0(ki,2))
        if(dxx.gt.1.d-9*dabs(xx(ki,n1hold)).and.dxx.gt.errlim) then
          go to 8
        endif
        if(dxx2.gt.1.d-9*dabs(xx(ki,n1+n2+10)).and.dxx2.gt.errlim) then
          go to 8
        endif
        if(dxx3.gt.1.d-9*dabs(xx(ki,2)).and.dxx3.gt.errlim) then
          go to 8
        endif
   55 continue

      if ((xx(1,nj)/xt(1,nj,kk-1)).lt.0.75d0.and.xx(1,nj).lt.100.0d0)
     1rr=rr/2.0d0

c     ________________________________________________
c
c *** SAVE THE PRESENT TIME-STEP RESULTS ***

      do 60 ll=1, nj  
      do 60 lk=1,n
   60 xt(lk,ll,kk)=xx(lk,ll)

      do 61 ll=1,nj
      do 61 lk=1,nnj
	do 61 mpa=1,npa
   61 cssold(ll,lk,mpa)=css(ll,lk,mpa)

c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

c *** DEAL WITH THE MATERIAL BALANCE TERMS ***

      if(rr.ne.0.0d0) then
      do 58 j=1,nj  ! nonzero time step
      term_s2(j)=termn_s2(j)
      term_s1(j)=termn_s1(j)
   58 term(j)=termn(j)
      else ! zero time step
      do 65 j=1,nj
      term(j)=0.d0
      term_s1(j)=0.0d0
      term_s2(j)=0.0d0

      fac=1.d0
      if(j.eq.n1+2 .and. n1.ne.0)
     &fac=((ep2+epp2)/(ep1+epp1))**exbrug
      if(j.eq.n1+n2+1) fac=((ep3+epp3)/(ep2+epp2))**exbrug

      hn=h2 
      if(n1.ne.0 .and. j.le.n1+1) hn=h1
      if(n1.ne.0 .and. j.le.n1+1) area=area1
      if(j.gt.n1+n2) hn=h3
      if(j.gt.n1+n2) area=area3

      if(j.gt.1) term(j)=
     &-(df(j)+fac*df(j-1))*(xx(1,j)-xx(1,j-1))/hn/2.d0
     &-(1.d0-0.5d0*(tm(j)+tm(j-1)))*xx(ki2,j-1)/fc
      if (j.gt.1) term_s1(j)=
     &-(do2(j)+fac*do2(j-1))*(xx(kS1,j)-xx(kS1,j-1))/hn/2.0d0
     &+hn*area*vol*(0.375d0*xx(kj1,j)+0.125d0*xx(kj1,j-1))
      if (j.gt.1) term_s2(j)=
     &-(dh2(j)+fac*dh2(j-1))*(xx(kS2,j)-xx(kS2,j-1))/hn/2.0d0
     &+hn*area*vol*(0.375d0*(xx(kj3,j)+xx(kj2,j))+0.125d0*
     &(xx(kj3,j-1)+xx(kj2,j-1)))

      fac=1.d0
      if(j.eq.n1+1 .and. n1.ne.0)
     &fac=((ep2+epp2)/(ep1+epp1))**exbrug
      if(j.eq.n1+n2) fac=((ep3+epp3)/(ep2+epp2))**exbrug

      hn=h2
      if(n1.ne.0 .and. j.lt.n1+1) hn=h1
      if(n1.ne.0 .and. j.lt.n1+1) area=area1
      if(j.ge.n1+n2) hn=h3
      if(j.ge.n1+n2) area=area3

      if(j.lt.nj) term(j)=term(j)
     &-(fac*df(j)+df(j+1))*(xx(1,j)-xx(1,j+1))/hn/2.d0
     &+(1.d0-0.5d0*(tm(j)+tm(j+1)))*xx(ki2,j)/fc
      if (j.lt.nj) term_s2(j)=term_s2(j)
     &-(fac*dh2(j)+dh2(j+1))*(xx(kS2,j)-xx(kS2,j+1))/hn/2.0d0
     &+hn*area*vol*(0.375d0*(xx(kj3,j)+xx(kj2,j))+0.125d0*
     &(xx(kj3,j+1)+xx(kj2,j+1)))
   65 if (j.lt.nj) term_s1(j)=term_s1(j)
     &-(fac*do2(j)+do2(j+1))*(xx(kS1,j)-xx(kS1,j+1))/hn/2.0d0
     &+hn*area*vol*(0.375d0*xx(kj1,j)+0.125d0*xx(kj1,j+1))
      endif
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
      end if
c
   66 continue

      return
      end
c
c***********************************************************************
      subroutine calca(kk)
c     subroutine to calculate diffusion in solid particles
c     of active material, based on a superposition integral.
c     This only needs to be called once per time step.
      implicit real*8(a-h,o-z)
      parameter(maxt=900)
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
      common /calc/ ai(maxt,5),ai2(maxt,5),ts(maxt),h,h1,h2,h3,hcn,
     1hcp,rr,rrmax,cuL,sumpa(5,221),Rad1pa(5),Rad3pa(5),area1pa(5)
     &,area3pa(5),mcL
      common/pindiv/ vf1(5),vf3(5),cot1(9),cot3(9),rs1(9),rs3(9)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/gas/ epg1,epg2,epg3
      common/cprop/ sig3,area3,rka3(9),rka3save,ct3(9),dfs3(9),
	1Rad3,cap1,cap3,sig1,area1,rka1(9),rka1save,ct1(9),dfs1(9),
     1Rad1,tw,dfs1save,dfs3save
      dimension ar(4,maxt),bz(6)
c
      do mpa=1,npa ! different particle sizes
      do 319 l=1,nt
      ai2(l,mpa)=0.0d0
  319 ai(l,mpa)=0.0d0
c
      do 70 i=1,kk-1
      ar(1,i)=dfs3(mpa)*(ts(kk)-ts(i))/Rad3pa(mpa)/Rad3pa(mpa)
      ar(2,i)=dfs3(mpa)*(ts(kk)-ts(i+1))/Rad3pa(mpa)/Rad3pa(mpa)
      ar(3,i)=dfs1(mpa)*(ts(kk)-ts(i))/Rad1pa(mpa)/Rad1pa(mpa)
      ar(4,i)=dfs1(mpa)*(ts(kk)-ts(i+1))/Rad1pa(mpa)/Rad1pa(mpa)
c
      do 69 m=1,2
      t1=ar(m,i)
      t2=ar((m+2),i)
c
      a1=0.0d0
      a12=0.0d0
c
      s=1.644934066848d0
c
c     Bessel's function zeros:
      bz(1)=2.4048255577d0
      bz(2)=5.5200781103d0
      bz(3)=8.6537281103d0
      bz(4)=11.7915344391d0
      bz(5)=14.9309177086d0
c
      if (shape3.gt.2.0d0) then
c     spherical particles:
      if (t1 .gt. 0.06d0) then
c
      do 59 j=1,5
      y1=dble(j*j)*pi*pi*t1
   59 if (y1 .le. 1.5d02) a1=a1+(expg(-y1))/dble(j*j)
      a1=2.0d0*(s-a1)/pi/pi
c
      else
c
      if (t1.LE.0.0d0) then
      a1=0.0d0
      else
      do 60 j=1,3
      z=dble(j)/dsqrt(t1)
      call erfcg(z,e)
      y2=dble(j*j)/t1
      if(y2 .ge. 1.5d02) then
      da=-dble(j)*dsqrt(pi/t1)*e
      else
      da=expg(-y2)-dble(j)*dsqrt(pi/t1)*e
      end if
   60 a1=a1+da
      a1=-t1 + 2.0d0*dsqrt(t1/pi)*(1.0d0+2.0d0*a1)
      end if
c
      end if
      else
c
      if (shape3.lt.2.0d0) then
c     planar particles:
      if(t1 .gt. 0.06d0) then
c
      do 61 j=1,5
      da=((-1.0d0)**j)*(1.0d0 - expg(-dble(2*j+1)**2
     1*pi*pi*t1))/dble(2*j+1)**2
   61 a1=a1+da
      a1=4.0d0*a1/pi/pi
c
      else
c
      do 62 j=1,3
      z=dble(j)/2.0d0/dsqrt(t1)
      call erfcg(z,e)
      da=((-1.0d0)**j)*(expg(-dble(j*j)/4.0d0/t1)
     &-dble(j)/2.0d0*dsqrt(pi/t1)*e)
   62 a1=a1+da
      a1=2.0d0*dsqrt(t1/pi)*(1.0d0+2.0d0*a1)
c
      end if
      else ! shape = 2
c     cylindrical particles:
      if (t1.gt.0.06d0) then
c
      do 63 j=1,5
      da=(1.0d0-expg(-bz(j)*bz(j)*t1))/bz(j)/bz(j)
   63 a1=a1+da
      a1=2.0d0*a1
c
      else
c
      a1=2.0d0*dsqrt(t1/pi)-t1/4.0d0-5.0d0*(t1**1.5d0)/96.0d0/dsqrt(pi)
     1-31.0d0*t1*t1/2048.0d0
c
      end if
      end if
      end if
c
      if (n1.eq.0) go to 36
c     (skip calculations of Li diffusion in the solid
c     if the negative electrode is metal foil)
c
      if (shape1.gt.2.0d0) then
c     spherical particles:
      if(t2 .gt. 0.06d0) then
c
      do 64 j=1,5
      y2=dble(j*j)*pi*pi*t2
   64 if(y2 .le. 1.5d02) a12=a12+(expg(-y2))/dble(j*j)
      a12=2.0d0*(s-a12)/pi/pi
c
      else
c
      if (t2.eq.0.0d0) then
      a12=0.0d0
      else
      do 65 j=1,3
      z=dble(j)/dsqrt(t2)
      call erfcg(z,e)
      y2=dble(j*j)/t2
      if(y2 .gt. 1.5d02) then
      da=-dble(j)*dsqrt(pi/t2)*e
      else
      da=expg(-y2)-dble(j)*dsqrt(pi/t2)*e
      end if
   65 a12=a12+da
      a12=-t2 + 2.0d0*dsqrt(t2/pi)*(1.0d0+2.0d0*a12)
      end if
      end if
c
      else
      if (shape1.lt.2.0d0) then
c     planar particles:
      if(t2 .gt. 0.06d0) then
c
      do 66 j=1,5
      da=((-1.0d0)**j)*(1.0d0 - expg(-dble(2*j+1)**2
     1*pi*pi*t2))/dble(2*j+1)**2
   66 a12=a12+da
      a12=4.0d0*a12/pi/pi
c
      else
c
      do 67 j=1,3
      z=dble(j)/2.0d0/dsqrt(t2)
      call erfcg(z,e)
      da=((-1.0d0)**j)*(expg(-dble(j*j)/4.0d0/t2)
     &-dble(j)/2.0d0*dsqrt(pi/t2)*e)
   67 a12=a12+da
      a12=2.0d0*dsqrt(t2/pi)*(1.0d0+2.0d0*a12)
c
      end if
      else
c     cylindrical particles:
      if (t2.gt.0.06d0) then
c
      do 68 j=1,5
      da=(1.0d0-expg(-bz(j)*bz(j)*t2))/bz(j)/bz(j)
   68 a12=a12+da
      a12=2.0d0*a12
c
      else
c
      a12=2.0d0*dsqrt(t2/pi)-t2/4.0d0-5.0d0*(t2**1.5d0)/96.0d0
     &/dsqrt(pi)-31.0d0*t2*t2/2048.0d0
c
      end if
      end if
      end if
c
   36 continue
c
      ar(m,i)=a1
   69 ar((m+2),i)=a12
c
      ai(kk-i,mpa)=ar(1,i)-ar(2,i)
   70 ai2(kk-i,mpa)=ar(3,i)-ar(4,i)
c
      do j=1,nj
      sumpa(mpa,j)=0.d0
      enddo
      do j=1,n1+1
      if (kk.gt.2) then  
      do  54 i=1, kk-2
   54 if(ts(i+1)-ts(i).ne.0.0d0) sumpa(mpa,j)=sumpa(mpa,j)
     &+(xt(2+mpa,j,i+1)- xt(2+mpa,j,i))*ai2(kk-i,mpa)/(ts(i+1)-ts(i))
      endif
      enddo
      do j=n1+n2,nj
      if (kk.gt.2) then  
      do 95 i=1, kk-2
   95 if(ts(i+1)-ts(i).ne.0.0d0) sumpa(mpa,j)=sumpa(mpa,j) ! problem
     &+(xt(2+mpa,j,i+1)- xt(2+mpa,j,i))*ai(kk-i,mpa)/(ts(i+1)-ts(i))
      endif
      enddo
      enddo ! mpa
c
      return
      end
c***********************************************************************
      subroutine erfcg(z,e)
c   an error function complement subroutine.
      implicit real*8(a-h,o-z)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/gas/ epg1,epg2,epg3
c
      a1=0.254829592d0
      a2=-0.284496736d0
      a3=1.421413741d0
      a4=-1.453152027d0
      a5=1.061405429d0
      if(z .lt. 2.747192d0) then
      t3=1.0d0/(1.0d0+0.3275911d0*z)
      e=(a1*t3+a2*t3*t3+a3*(t3**3)+a4*(t3**4)
     1+a5*(t3**5))*expg(-z*z)
      else
c
      if(z .gt. 25.0d0) then
      e=0.0d0
      else
c
      sum=0.0d0
      max=z*z + 0.5d0
      fac=-0.5d0/z/z
      sum=fac
      tl=fac
      n=1
   10 n=n+1
      if(n .gt. max) go to 15
      tn=tl*dble(2*n-1)*fac
      sum=sum + tn
      if(tn .lt. 1.0d-06) go to 15
      tl=tn
      go to 10
   15 e=(expg(-z*z))*(1.0d0+sum)/dsqrt(pi)/z
      end if
      end if
c
      return
      end
c***********************************************************************
      subroutine band(j)
c     subroutine to solve coupled, linear differnece equations
      implicit real*8(a-h,o-z)
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/mat/ b,d
      common/bnd/ a,c,g,x,y
      dimension b(17,17),d(17,35)
      dimension a(17,17),c(17,221),g(17),x(17,17),y(17,17)
      dimension e(17,18,221)
      save e, np1
  101 format (15h determ=0 at j=,i4)
      n=nx
      if (j-2)  1,6,8
    1 np1 = n + 1
      do 2 i=1,n
      d(i,2*n+1)= g(i)
      do 2 l=1,n
      lpn= l + n
    2 d(i,lpn)= x(i,l)
      call matinv(n,2*n+1,determ)
      if (determ)  4,3,4
    3 print 101, j
    4 do 5 k=1,n
      e(k,np1,1)= d(k,2*n+1)
      do 5 l=1,n
      e(k,l,1)= - d(k,l)
      lpn= l + n
    5 x(k,l)= - d(k,lpn)
      return
    6 do 7 i=1,n
      do 7 k=1,n
      do 7 l=1,n
    7 d(i,k)= d(i,k) + a(i,l)*x(l,k)
    8 if (j-nj)  11,9,9
    9 do 10 i=1,n
      do 10 l=1,n
      g(i)= g(i) - y(i,l)*e(l,np1,j-2)
      do 10 m=1,n
   10 a(i,l)= a(i,l) + y(i,m)*e(m,l,j-2)
   11 do 12 i=1,n
      d(i,np1)= - g(i)
      do 12 l=1,n
      d(i,np1)= d(i,np1) + a(i,l)*e(l,np1,j-1)
      do 12 k=1,n
   12 b(i,k)= b(i,k) + a(i,l)*e(l,k,j-1)
      call matinv(n,np1,determ)
      if (determ)  14,13,14
   13 print 101,j
   14 do 15 k=1,n
      do 15 m=1,np1
   15 e(k,m,j)= - d(k,m)
      if (j-nj)  20,16,16
   16 do 17 k=1,n
   17 c(k,j)= e(k,np1,j)
      do 18 jj=2,nj
      m= nj - jj + 1
      do 18 k=1,n
      c(k,m)= e(k,np1,m)
      do 18 l=1,n
   18 c(k,m)= c(k,m) + e(k,l,m)*c(l,m+1)
      do 19 l=1,n
      do 19 k=1,n
   19 c(k,1)= c(k,1) + x(k,l)*c(l,3)
   20 return
      end
c***********************************************************************
      subroutine matinv(n,m,determ)
c     matrix inversion program used by BAND(j).
      implicit real*8(a-h,o-z)
      common/mat/ b,d
      dimension b(17,17),d(17,35)
      dimension id(17)
      determ=1.0d0
      do 1 i=1,n
    1 id(i)=0
      do 18 nn=1,n
      bmax=1.1d0
      do 6 i=1,n
      if(id(i).ne.0) go to 6
      bnext=0.0d0
      btry=0.0d0
      do 5 j=1,n
      if(id(j).ne.0) go to 5
      if(dabs(b(i,j)).le.bnext) go to 5
      bnext=dabs(b(i,j))
      if(bnext.le.btry) go to 5
      bnext=btry
      btry=dabs(b(i,j))
      jc=j
    5 continue
      if(bnext.ge.bmax*btry) go to 6
      bmax=bnext/btry
      irow=i
      jcol=jc
    6 continue
      if(id(jc).eq.0) go to 8
      determ=0.0d0
      return
    8 id(jcol)=1
      if(jcol.eq.irow) go to 12
      do 10 j=1,n
      save=b(irow,j)
      b(irow,j)=b(jcol,j)
   10 b(jcol,j)=save
      do 11 k=1,m
      save=d(irow,k)
      d(irow,k)=d(jcol,k)
   11 d(jcol,k)=save
   12 f=1.0d0/b(jcol,jcol)
      do 13 j=1,n
   13 b(jcol,j)=b(jcol,j)*f
      do 14 k=1,m
   14 d(jcol,k)=d(jcol,k)*f
      do 18 i=1,n
      if(i.eq.jcol) go to 18
      f=b(i,jcol)
      do 16 j=1,n
   16 b(i,j)=b(i,j)-f*b(jcol,j)
      do 17 k=1,m
   17 d(i,k)=d(i,k)-f*d(jcol,k)
   18 continue
      return
      end
c***********************************************************************
      subroutine nucamb(kk,il2)
c     subroutine to print detailed profiles across the cell sandwich
      implicit real*8(a-h,o-z)
      parameter(maxt=900)
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
      common /calc/ ai(maxt,5),ai2(maxt,5),ts(maxt),h,h1,h2,h3,hcn,
     1hcp,rr,rrmax,cuL,sumpa(5,221),Rad1pa(5),Rad3pa(5),area1pa(5)
     &,area3pa(5),mcL
      common/pindiv/ vf1(5),vf3(5),cot1(9),cot3(9),rs1(9),rs3(9)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/gas/ epg1,epg2,epg3
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/cprop/ sig3,area3,rka3(9),rka3save,ct3(9),dfs3(9),
	1Rad3,cap1,cap3,sig1,area1,rka1(9),rka1save,ct1(9),dfs1(9),
     1Rad1,tw,dfs1save,dfs3save
      common/tprop/df(221),cd(221),tm(221),ddo2(221),ddh2(221),
     1ddf(221),dcd(221),dtm(221),dfu(221),d2fu(221),do2(221),dh2(221)
      common /vdc/ aa(1,1),bb(1,1),cc(1,150),dd(1,3),gg(1),xxx(1,1),
     &yy(1,1),hha(20),hhc(20),cssold(220,150,20),css(220,150,20),
     &utz(9,221),dsold(220,150,20),ds(220,150,20),time,
     &nn,nnj,np,mvdc1,mvdc3,lims
      dimension zz(221)
  109 format(f6.1,', ',f11.1,', ',f8.4,', ',g10.3,', ',f8.3,', ',g10.4
     1,', ',g10.4,', ',g10.4,', ',g10.4,', ',g10.4)
  310 format('Distance   C Elec    C Sol Surf  Liq Pot    Solid Pot  ',
     &'Liq Cur   j main     j side 1    j side 2    j side 3')
  311 format('(microns)    (mol/m3)   x or y     (V)         (V) ',
     1'    (A/m2)     (A/m2)       (A/m2)      (A/m2)      (A/m2)')
   44 format(' t = ',1pe18.6,' min')
c
      do 5 i=1,n1+1
      w=dble(i-1)-1.d-8 ! helps rounding when diff'ing files from diff comps
    5 zz(i) = w*h1*1.0d06
      do 71 i=n1+2,n2+n1
      w=dble(i-(n1+1))-1.d-8
   71 zz(i)=zz(n1+1)+w*h2*1.0d06
      do 72 i=n1+n2+1,nj
      w=dble(i-(n1+n2))-1.d-8
   72 zz(i)=zz(n2+n1)+w*h3*1.0d06
c
      write (4,*) ' '
      write (4,310) 
      write (4,311)

      write (4,44) ts(kk)/60.0d0
c
      mpa=1 ! only one size for detailed profiles

      do 10 j=1,nj,il2
      if (j .le. n1+1) then
      csol=ct1(mpa)
      else
      csol=ct3(mpa)
      end if
      if(j.le.n1+1) then
      curden_main=area1*fc*xx(3+npa+mpa,j)
      curden_side=area1*fc*xx(kj3,j)
      else if(j.ge.n1+n2) then
      curden_main=area3*fc*xx(3+npa+mpa,j) 
      curden_side=area3*fc*xx(kj3,j)
      else
      curden_main=0.0d0
      curden_side=0.0d0
      endif

   10 write (4,109) zz(j),xx(1,j),xx(2+mpa,j)/csol,xx(kp2,j),xx(kp1,j),
     &xx(ki2,j),xx(3+npa+mpa,j)*fc,xx(kj1,j)*fc,xx(kj2,j)*fc,
     &xx(kj3,j)*fc
      if (kk.eq.1) write (4,*) 'that was the zero-time solution!'
      write (4,*) ' '
      write (4,*) ' '

      return
      end

c***********************************************************************
      subroutine cellpot(kk,v,li)
c     subroutine to calculate the cell potential at the end
c     of a time step, calculate the utilization throughout the electrodes, 
c     check the material balances, and calculate the resistances.
      implicit real*8(a-h,o-z)
      parameter(maxt=900)
      common /vdc/ aa(1,1),bb(1,1),cc(1,150),dd(1,3),gg(1),xxx(1,1),
     &yy(1,1),hha(20),hhc(20),cssold(220,150,20),css(220,150,20),
     &utz(9,221),dsold(220,150,20),ds(220,150,20),time,
     &nn,nnj,np,mvdc1,mvdc3,lims
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
      common /calc/ ai(maxt,5),ai2(maxt,5),ts(maxt),h,h1,h2,h3,hcn,
     1hcp,rr,rrmax,cuL,sumpa(5,221),Rad1pa(5),Rad3pa(5),area1pa(5)
     &,area3pa(5),mcL
      common/pindiv/ vf1(5),vf3(5),cot1(9),cot3(9),rs1(9),rs3(9)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/gas/ epg1,epg2,epg3
      common/power/ ed,Vold,ranode(20),rcathde(20),heat,qlosstot
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/cprop/ sig3,area3,rka3(9),rka3save,ct3(9),dfs3(9),
	1Rad3,cap1,cap3,sig1,area1,rka1(9),rka1save,ct1(9),dfs1(9),
     1Rad1,tw,dfs1save,dfs3save
      common/temp/ thk,htc,dudt,Cp,dens,tam,g0(9),
     &qq,qloss,residm,ncell,lht
      common/side/rksc1,c1init,c2init,rksa1,term_s1(221),vol,
     &rksa2,rksa3,rksc3,rksc2,UsO2,UsH2,term_s2(221),nside
      common/RG/ RG,RGn,RGp,RGext
      common/resistances/R_total,R_anode,R_sep,R_cathode
      save ut1, ut3
  306 format (f8.5,',   ',f6.2,',   ',g12.5,',   ',g12.5,',   '
     &,g12.5,',   ',g12.5)
  307 format(f9.3,', ',f6.4,', ',f6.4,', ',f8.5,', ',f8.5,', ',f8.2,
     &', ', f6.2, ', ', f6.2, ', ', f6.2, ', ', f6.2, ', ', f6.2)
  309 format(f6.3,', ',f8.5,', ',f5.2,', ',f12.3,',',f9.5,', ',f6.2,
     & ',',g10.3, ',', g10.3)

      if (kk.eq.1) then
      ut3=utz(1,nj)
      if (n1.ne.0) ut1=utz(1,1)
      endif

c Need the utilization at each node point to do a rigorous energy balance,
c and to calculate the electrode utilization when have a side reaction.

      if (li.eq.1) then !update utz() for cellpot call after converged timestep
        do j=1,nj
          kadd=0
          do mpa=1,npa
            if (j.le.n1+1) then
              utz(mpa,j)=utz(mpa,j)-xx(3+npa+mpa,j)*rr*area1pa(mpa)
     &        /(1.0d0-ep1-epp1-epf1-epg1)/ct1(mpa)/vf1(mpa)
            elseif (j.ge.n1+n2) then
              utz(mpa,j)=utz(mpa,j)-xx(3+npa+mpa,j)*rr*area3pa(mpa)/
     &        (1.0d0-ep3-epp3-epf3-epg3)/ct3(mpa)/vf3(mpa)
            endif
          enddo ! mpa
        enddo ! j
      endif ! li

c     Material balance tests:
      sum=0.0d0 !mat balance on electrolyte
      sum4=0.0d0 !mat balance oxygen 
      sum5=0.0d0 !mat balance hydrogen

      if (n1 .gt. 2) then
      do 85 j=2,n1
      sum4=sum4+xx(kS1,j)*h1*epg1 !conc O2
      sum5=sum5+xx(kS2,j)*h1*epg1 !conc H2
   85 sum=sum+xx(1,j)*(ep1+epp1)*h1 !conc elec
      endif
      sum=sum+(xx(1,1)+xx(1,n1+1))*(ep1+epp1)*h1/2.0d0
      sum4=sum4+h1*(xx(kS1,1)+xx(kS1,n1+1))/2.0d0*epg1
      sum5=sum5+h1*(xx(kS2,1)+xx(kS2,n1+1))/2.0d0*epg1

      do 86 j=n1+2,n2+n1-1
      sum4=sum4+h2*xx(kS1,j)*epg2
      sum5=sum5+h2*xx(kS2,j)*epg2
   86 sum=sum+xx(1,j)*(ep2+epp2)*h2
      sum=sum+(xx(1,n1+1)+xx(1,n2+n1))*(ep2+epp2)*h2/2.0d0
      sum4=sum4+h2*(xx(kS1,n1+1)+xx(kS1,n1+n2))/2.0d0*epg2
      sum5=sum5+h2*(xx(kS2,n1+1)+xx(kS2,n1+n2))/2.0d0*epg2

      do 87 j=n2+n1+1,nj-1
      sum4=sum4+h3*xx(kS1,j)*epg3
      sum5=sum5+h3*xx(kS2,j)*epg3
   87 sum=sum+xx(1,j)*(ep3+epp3)*h3

      sum=sum+(xx(1,n1+n2)+xx(1,nj))*h3*(ep3+epp3)/2.0d0
      sum4=sum4+h3*(xx(kS1,n1+n2)+xx(kS1,nj))/2.0d0*epg3
      sum5=sum5+h3*(xx(kS2,n1+n2)+xx(kS2,nj))/2.0d0*epg3
      sum4=sum4/thk !adjusts to average concentration
      sum5=sum5/thk

c     calculate total salt in cell from initial profile:
      w=xt(1,n1+2,1)*(dble(n2-1)*(ep2+epp2)*h2+dble(n1)*(ep1+epp1)*h1
     1+dble(n3)*(ep3+epp3)*h3)

c     material-balance parameter should be ca=1.00
      ca=sum/w

c     Calculate cell potential from dif of solid-phase potentials:
      v=xx(kp1,nj)-xx(kp1,1)-RG*cur !extra drop from grid resistance included

c Calculate the resistances in the cell, R=V/I
      mpa=1 ! only one size for resistance calculation
      xxsave=xx(2+mpa,n1+1)
      call ekin(n1+1,kk,0,0.0d0,mpa,mvdc1,mvdc3)
      xx(2+mpa,n1+1)=xxsave
      Ua_end=g0(mpa)
      xxsave=xx(2+mpa,n1+n2)
      call ekin(n1+n2,kk,0,0.0d0,mpa,mvdc1,mvdc3)
      xx(2+mpa,n1+n2)=xxsave
      Uc_end=g0(mpa)
      R_anode=(xt(kp1,1,kk)-xt(kp2,n1+1,kk)-Ua_end)/cur+RGn 
      R_sep=(xt(kp2,n1+2,kk)-xt(kp2,n1+n2,kk))/cur
      R_cathode=RGp-(xt(kp1,nj,kk)-xt(kp2,n1+n2,kk)-Uc_end)/cur
      R_total=R_anode+R_sep+R_cathode
cSP
      R_total = (xt(kp1,nj,kk)-xt(kp1,1,kk))/cur

      write(*,*) 'vc, va, cur', xt(kp1,nj,kk), xt(kp1,1,kk), cur
c
      if(li.eq.1) then ! Li is a flag for whether to write results

c     Calculate energy density by running sum of currentxvoltage:
        ed=ed+((Vold-RG*xx(ki2,n1+1))*xx(ki2,n1+1)
     &+(V-RG*cur)*cur)*rr/2.0d0
        Vold=v

c Calculate the electrode utilization as the average utilization
c from all the node points. 

	temp=0.d0
      do mpa=1,npa
	  do j=n1+n2+1,nj-1
	    temp=temp+utz(mpa,j)
	  enddo
	temp=temp+.5d0*(utz(mpa,n1+n2)+utz(mpa,nj))
        ut3=temp/dble(n3)
      enddo !mpa
	
	temp=0.d0
	do mpa=1,npa
	  do j=2,n1
	    temp=temp+utz(mpa,j)
	  enddo
	  temp=temp+.5d0*(utz(mpa,1)+utz(mpa,n1+1))
        ut1=temp/dble(n1)
	enddo !mpa
      if (n1.eq.0) ut1=0.d0

	ut1=ut1/dble(npa)
	ut3=ut3/dble(npa)

      th=ts(kk)/6.0d01
c
	mpa=1
      xxsave=xx(2+mpa,1)
      call ekin(1,kk,1,ut1,mpa,mvdc1,mvdc3)
      Ua=g0(mpa)
      xx(2+mpa,1)=xxsave
      xxsave=xx(2+mpa,n1+n2)
      call ekin(n1+n2,kk,1,ut3,mpa,mvdc1,mvdc3)
      Uc=g0(mpa)
      xx(2+mpa,n1+n2)=xxsave
      Uoc=Uc-Ua !The equilibrium potential is not well defined for multiple
c     types of active material

c *** THE ENERGY BALANCE IS DONE IN SUBROUTINE TEMPERATURE ***

	call temperature(kk,v,ut3,ut1,Uoc)
      
	tprint=t-273.15d0

c assumed temperature dependence for solid-state diffusion
      dfs1(1)=dfs1save*dexp((EbarS1)*(t-298.0d0)/(t*298.0d0))
      dfs3(1)=dfs3save*dexp((EbarS3)*(t-298.0d0)/(t*298.0d0))

      pressn2=28.0d0*t*8.206d-5 !calculate press in atm, 32.33 for N2
      presso2=sum4*t*8.206d-5 !oxygen pressure
      pressh2=sum5*t*8.206d-5 !hydrogen pressure

      if (nside.ne.0.and.nneg.eq.4) then !NiMH with side reaction
        write (2,307) th,ut1,ut3,v,Uoc,
     1  cur,pressh2*0.9869d0,presso2*0.9869d0,
     1  (pressh2+pressn2+presso2)*0.9869d0,tprint,qq
      else
        write (2,307) th,ut1,ut3,v,Uoc,
     1  cur,tprint,qq
      endif
 
c *** WRITE RESISTANCE VALUES THROUGH CELL SANDWICH ***

      if (cur.ne.0.0d0) write (6,306) th,cur,R_anode,R_sep,
     &R_cathode,R_total

c *** WRITE POTENTIALS OF NEGATIVE AND POSITIVE VS. A REF ELECTRODE ***
      jref=(n1+1+n1+n2)/2 
      write (3,310) th,xt(kp1,1,kk)-xt(kp2,jref,kk)+cur*RG/2.d0
     %,xt(kp1,nj,kk)-xt(kp2,jref,kk)-cur*RG/2.d0,cur,t-273.15d0,qq
  310 format (f11.6,2f12.6,f12.4,f14.6,1pe16.7)

      print 410, th,cur,tprint
  410 format (f9.3,', ',f12.5,', ',f9.5,', ',f6.3,',',f9.3,', ',f6.2,
     & ',',g10.3,',',f8.5)
c     Calculate total heat generated as running sum
      heat=heat+qq*rr/3600.d0 !Wh/m2
      qlosstot=qlosstot+qloss*rr/3600.d0 !Wh/m2
      endif !li flag
c
      return
      end


c***********************************************************************
      subroutine sol(nmax,jj)
      implicit real*8(a-h,o-z)
c     This subroutine calculates the solid-phase concentration profiles,
c     at one position (jj) and at one time.
      parameter(maxt=900)
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
      common /calc/ ai(maxt,5),ai2(maxt,5),ts(maxt),h,h1,h2,h3,hcn,
     1hcp,rr,rrmax,cuL,sumpa(5,221),Rad1pa(5),Rad3pa(5),area1pa(5)
     &,area3pa(5),mcL
      common/pindiv/ vf1(5),vf3(5),cot1(9),cot3(9),rs1(9),rs3(9)
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/gas/ epg1,epg2,epg3
      common/cprop/ sig3,area3,rka3(9),rka3save,ct3(9),dfs3(9),
	1Rad3,cap1,cap3,sig1,area1,rka1(9),rka1save,ct1(9),dfs1(9),
     1Rad1,tw,dfs1save,dfs3save
      dimension cs(50)
  100   format(f8.4,'  ',f8.4)
c
c     set initial value of solid concentration
      mpa=1 ! only one size for solid profiles
      do 88 i=1, 50
   88 cs(i)=xt(2+mpa,jj,1)
c
      if (jj .le. n1) then
      dfs = dfs1(mpa)
      Rad = Rad1
      else if (jj .ge. n1+n2) then
      dfs = dfs3(mpa)
      Rad = Rad3
      else
      print *, 'jsol selected for node in separator'
      return
      endif
c
c     complete calculations for 50 points along radius of particle
      nmax=nmax-1  ! added
      do 10 i=1,50
      y2=0.02d0*dble(i)
c
      sum1=0.0d0
      do 20 kk=1,nmax
      k=nmax+1-kk
c
      t1=(ts(nmax+1)-ts(k))*dfs/Rad/Rad
      sum2=sum1
c
c     calculate c bar (r,t1)
      sum1=0.0d0
      r1=1.0d0
c
      do 89 j=1,15
      r1=-r1
      y1=dble(j*j)*pi*pi*t1
      y3=dble(j)*pi*y2
      if (y1 .gt. 1.50d02) then
      da=0.0d0
      else
      da=expg(-y1)
      end if
   89 sum1=sum1-2.0d0*r1*da*dsin(y3)/dble(j)/pi/y2
      sum1=1.0d0-sum1
c
c     perform superposition
c
      cs(i)=cs(i)+(xt(2+mpa,jj,k+1)+xt(2+mpa,jj,k)-2.0d0*xt(2+mpa,jj,1)
     1)*(sum1-sum2)/2.0d0
   20 continue
c
   10 continue
      nmax=nmax+1  ! added
c
      write (7,*) ' '
      write (7,*) 'time is ',ts(nmax)/60.0d0,' minutes'
      write (7,*) ' '
      do 90 i=1, 50, 1
   90 write (7,100) .02d0*dble(i),cs(i)/ct1(mpa)
c
      return
      end
c***********************************************************************
      subroutine mass(re,rs3a,rs1a,rf,rpl,rc,rcn,rcp)
c     calculate mass (kg/m2) of the cell based on 
c     densities and volume fractions of components.
      implicit real*8(a-h,o-z)
      parameter(maxt=900)
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
      common /calc/ ai(maxt,5),ai2(maxt,5),ts(maxt),h,h1,h2,h3,hcn,
     1hcp,rr,rrmax,cuL,sumpa(5,221),Rad1pa(5),Rad3pa(5),area1pa(5)
     &,area3pa(5),mcL
      common/pindiv/ vf1(5),vf3(5),cot1(9),cot3(9),rs1(9),rs3(9)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/gas/ epg1,epg2,epg3
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/cprop/ sig3,area3,rka3(9),rka3save,ct3(9),dfs3(9),
	1Rad3,cap1,cap3,sig1,area1,rka1(9),rka1save,ct1(9),dfs1(9),
     1Rad1,tw,dfs1save,dfs3save
      common/temp/ thk,htc,dudt,Cp,dens,tam,g0(9),
     &qq,qloss,residm,ncell,lht
c
c     mass of positive electrode
      c1=h3*dble(n3)*(re*ep3+rpl*epp3+rs3a*(1.0d0-ep3-epf3-epp3-epg3)+
     &rf*epf3)
c     mass of separator
      s=(re*ep2+rpl*epp2+rc*(1.0d0-ep2-epp2-epg2))*h2*dble(n2-1)
c
c     mass of negative electrode
      if (n1.ne.0) a1=h1*dble(n1)*(re*ep1+rpl*epp1+rs1a*(1.0d0-ep1-
     1epf1-epp1-epg1)+rf*epf1)
      if (n1.eq.0) a1=h1*rs1a
c
c     mass of current collectors
      cc1=rcn*hcn+rcp*hcp
c
      tw=c1+s+a1+cc1+residm
c
      return
      end
c***********************************************************************
      subroutine temperature(kk,v,ut,ut2,Uoc)
c     subroutine to recompute the cell temperature based 
c     on heat generation, heat capacity, and heat losses.
      implicit real*8(a-h,o-z)
      parameter(maxt=900)
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
      common /calc/ ai(maxt,5),ai2(maxt,5),ts(maxt),h,h1,h2,h3,hcn,
     1hcp,rr,rrmax,cuL,sumpa(5,221),Rad1pa(5),Rad3pa(5),area1pa(5)
     &,area3pa(5),mcL
      common/pindiv/ vf1(5),vf3(5),cot1(9),cot3(9),rs1(9),rs3(9)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/gas/ epg1,epg2,epg3
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/cprop/ sig3,area3,rka3(9),rka3save,ct3(9),dfs3(9),
	1Rad3,cap1,cap3,sig1,area1,rka1(9),rka1save,ct1(9),dfs1(9),
     1Rad1,tw,dfs1save,dfs3save
      common/temp/ thk,htc,dudt,Cp,dens,tam,g0(9),
     &qq,qloss,residm,ncell,lht
      common/side/rksc1,c1init,c2init,rksa1,term_s1(221),vol,
     &rksa2,rksa3,rksc3,rksc2,UsO2,UsH2,term_s2(221),nside
      common /vdc/ aa(1,1),bb(1,1),cc(1,150),dd(1,3),gg(1),xxx(1,1),
     &yy(1,1),hha(20),hhc(20),cssold(220,150,20),css(220,150,20),
     &utz(9,221),dsold(220,150,20),ds(220,150,20),time,
     &nn,nnj,np,mvdc1,mvdc3,lims
      common/RG/ RG,RGn,RGp,RGext
      common/resistances/R_total,R_anode,R_sep,R_cathode
c
c     The entropy and equilibrium potential for each electrode
c     should be given in ekin with respect to a reference electrode.
c     Heat generation is positive if exothermic.
c
c     The energy balance is done according to the method given
c     in Rao and Newman, J. Electrochem. Soc., 144 (1997), 2697-2704.
c     Cp(dT/dt)-Q=-Integral(sum over reactions (a*in,i*Uh,i))dx-IV

c     The heat-transfer coefficient is for heat transferred out of
c     one side of the cell; it is defined based on cell area.
c     htcc is a per-cell heat-transfer coefficient.
c
      htcc=htc/dble(Ncell)

      sum_main1=0.0d0
      sum_main3=0.0d0
      sum_s11=0.0d0 !side reaction 1 in negative (region 1)
      sum_s13=0.0d0 !side reaction 1 in positive (region 3)
      sum_s21=0.0d0 !side reaction 2 in negative (region 1)
      sum_s31=0.0d0 !side reaction 3 in negative (region 1)

      dudtS1=-0.00168d0 
      dudtS1=0.0d0 !reversible heat for side reaction 1
      dudtS2=0.0d0 !reversible heat for side reaction 2
      dHread=100.0d0 !units here J/mole of H2 readsorbed. all reversible.


! q = ∑_j a_sj i_nj (ϕ_s-ϕ_e-U_j ) +∑_j a_sj i_nj T (∂U_n)/∂T 
!     + σ^eff ∇ϕ_s⋅∇ϕ_s + κ^eff ∇ϕ_e⋅∇ϕ_e + κ_D^eff ∇lnc_e⋅∇ϕ_e

      do j=2,n1 !in the negative electrode
      do mpa=1,npa
      utzs=utz(mpa,j)
      xxsave=xx(2+mpa,j)
      call ekin(j,kk,1,utzs,mpa,mvdc1,mvdc3)
      xx(2+mpa,j)=xxsave
      Umain1=g0(mpa) !equilibrium potential
      dUdTmain1=dudt !reversible energy
      sum_main1=sum_main1+area1pa(mpa)*fc*xx(3+npa+mpa,j)*h1
     &*(Umain1-t*dUdTmain1) !using the local current
      if (nside.ge.1 .and. nprop.eq.4) then
      sum_s11=sum_s11+area1*fc*xx(kj1,j)*h1*(UsO2+r*t/fc*dlog(xx(kS1,j)*
     &8.206d-5*t)-t*dudtS1)
      sum_s21=sum_s21+area1*fc*xx(kj3,j)*h1*(UsH2-r*t/fc/2.0d0*
     &dlog(xx(kS2,j)*8.206d-5*t)-t*dudtS2)
      sum_s31=sum_s31+area1*xx(kj2,j)*h1*dHread
      endif
      enddo !mpa
      enddo !j, negative electrode

c for nodepoint j=1
      do mpa=1,npa
      utzs=utz(mpa,1)
      xxsave=xx(2+mpa,1)
      call ekin(1,kk,1,utzs,mpa,mvdc1,mvdc3)
      xx(2+mpa,1)=xxsave
      Umain1=g0(mpa)
      dUdTmain1=dudt
      sum_main1=sum_main1+area1pa(mpa)*fc*xx(3+npa+mpa,1)*h1*
     &(Umain1-t*dUdTmain1)/2.d0
      if (nside.ge.1 .and. nprop.eq.4) then
      sum_s11=sum_s11+area1*fc*xx(kj1,1)*h1*(UsO2+r*t/fc*dlog(xx(kS1,1)*
     &8.206d-5*t)-t*dudtS1)
      sum_s21=sum_s21+area1*fc*xx(kj3,1)*h1*(UsH2-r*t/fc/2.0d0*
     &dlog(xx(kS2,1)*8.206d-5*t)-t*dudtS2)
      sum_s31=sum_s31+area1*xx(kj2,1)*h1*dHread
      endif
      enddo !mpa

c for nodepoint j=n1+1
      do mpa=1,npa
      utzs=utz(mpa,n1+1)
      xxsave=xx(2+mpa,n1+1)
      call ekin(n1+1,kk,1,utzs,mpa,mvdc1,mvdc3)
      xx(2+mpa,n1+1)=xxsave
      Umain1=g0(mpa)
      dUdTmain1=dudt
      sum_main1=sum_main1+area1pa(mpa)*fc*xx(3+npa+mpa,n1+1)*h1*
     &(Umain1-t*dUdTmain1)/2.0d0
      if (nside.ge.1 .and. nprop.eq.4) then
      sum_s11=sum_s11+area1*fc*xx(kj1,n1+1)*h1*(UsO2+r*t/fc*
     &dlog(xx(kS1,n1+1)*8.206d-5*t)-t*dudtS1)
      sum_s21=sum_s21+area1*fc*xx(kj3,n1+1)*h1*(UsH2-r*t/fc/2.0d0*
     &dlog(xx(kS2,n1+1)*8.206d-5*t)-t*dudtS2)
      sum_s31=sum_s31+area1*xx(kj2,n1+1)*h1*dHread
      endif
      enddo !mpa

      do j=n1+n2+1,nj-1 !in the positive electrode
      do mpa=1,npa
      utzs=utz(mpa,j)
      xxsave=xx(2+mpa,j)
      call ekin(j,kk,1,utzs,mpa,mvdc1,mvdc3)
      xx(2+mpa,j)=xxsave
      Umain3=g0(mpa)
      dUdTmain3=dudt
      sum_main3=sum_main3+area3pa(mpa)*fc*xx(3+npa+mpa,j)
     &*h3*(Umain3-t*dUdTmain3)
      if (nside.ge.1 .and. nprop.eq.4) then
      sum_s13=sum_s13+area3*fc*xx(kj1,j)*h1*(UsO2+r*t/fc*
     &dlog(xx(kS1,j)*8.206d-5*t)-t*dudtS1)
      endif
      enddo !mpa
      enddo !j, positive electrode

      do mpa=1,npa !point n1+n2
      utzs=utz(mpa,n1+n2)
      xxsave=xx(2+mpa,n1+n2)
      call ekin(n1+n2,kk,1,utzs,mpa,mvdc1,mvdc3)
      xx(2+mpa,n1+n2)=xxsave
      Umain3=g0(mpa)
      dUdTmain3=dudt
      sum_main3=sum_main3+area3pa(mpa)*fc*xx(3+npa+mpa,n1+n2)*h3*
     &(Umain3-t*dUdTmain3)/2.0d0
      if (nside.ge.1 .and. nprop.eq.4) then
      sum_s13=sum_s13+area3*fc*xx(kj1,n1+n2)*h1*(UsO2+r*t/fc*
     &dlog(xx(kS1,n1+n2)*8.206d-5*t)-t*dudtS1)
      endif
      enddo !mpa

      do mpa=1,npa !point nj
      utzs=utz(mpa,nj)
      xxsave=xx(2+mpa,nj)
      call ekin(nj,kk,1,utzs,mpa,mvdc1,mvdc3)
      xx(2+mpa,nj)=xxsave
      Umain3=g0(mpa)
      dUdTmain3=dudt
      sum_main3=sum_main3+area3pa(mpa)*fc*xx(3+npa+mpa,nj)*h3*
     &(Umain3-t*dUdTmain3)/2.0d0
      if (nside.ge.1 .and. nprop.eq.4) then
      sum_s13=sum_s13+area3*fc*xx(kj1,nj)*h1*(UsO2+r*t/fc*
     &dlog(xx(kS1,nj)*8.206d-5*t)-t*dudtS1)
      endif
      enddo !mpa

      sumtotal=sum_main1+sum_main3+sum_s11+sum_s13+sum_s21+sum_s31

*** HERE CALCULATE THE TOTAL HEAT GENERATION ***

      if (kk.eq.1) then
      qq=cur*(Uoc-v)-cur**2*RGext !Zero time-step, use a simple energy balance
      else
      qq=-cur*v-sumtotal-cur**2*RGext !The main calculation
!SP

!      qq = -cur*v-sumtotal+cur**2*RGext + cur**2*R_total! adding ohmic term
!      qq = -cur*v-sumtotal+cur**2*RGext 
!        qq=cur*(Uoc-v)-cur**2*RGext !SK
      write(21,*) ts(kk), qq, -cur*v-sumtotal, cur**2*R_total
      qq = qq/thk
      endif

c     This energy balance includes a residual mass.
      if(lht.eq.0)
     &t=t+(rr/(Cp*(dens*thk+residm)))*(htcc*(tam-t)+qq)

      if (lht.eq.1) then
      print *,'lht.eq.1 is NOT WORKING'
        if (t.ne.tam) then
c         htc=dble(Ncell)*cur*(Uoc-v-t*Soc)/(t-tam) ! fixed 6-30-01
        else
          htc=0.0d0
        endif
         htcc=htc/dble(Ncell)
      endif
c
      return
      end
c***********************************************************************
      double precision function expg(x)
      implicit real*8 (a-h,o-z)
      expg=0.d0
      if(x.gt.-700.d0) expg=dexp(x)
      return
      end

c***********************************************************************
      subroutine ekin(j,kk,lag,utz,mpa,mvdc1,mvdc3)
      implicit real*8(a-h,o-z)
c     This subroutine evaluates the Butler-Volmer equations.
c     It also provides a library of data for various positive
c     and negative active materials, including the equilibrium potentials,
c     specific capacities, and densities.
      parameter(maxt=900)
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/gas/ epg1,epg2,epg3
      common/power/ ed,Vold,ranode(20),rcathde(20),heat,qlosstot
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/cprop/ sig3,area3,rka3(9),rka3save,ct3(9),dfs3(9),
	1Rad3,cap1,cap3,sig1,area1,rka1(9),rka1save,ct1(9),dfs1(9),
     1Rad1,tw,dfs1save,dfs3save
      common/temp/ thk,htc,dudt,Cp,dens,tam,g0(9),
     &qq,qloss,residm,ncell,lht
      common/mat/ b,d
      common/bnd/ a,c,g,x,y
      common/side/rksc1,c1init,c2init,rksa1,term_s1(221),vol,
     &rksa2,rksa3,rksc3,rksc2,UsO2,UsH2,term_s2(221),nside
      common/pindiv/ vf1(5),vf3(5),cot1(9),cot3(9),rs1(9),rs3(9)
      common/RG/ RG,RGn,RGp,RGext
	dimension g1(9)
      dimension b(17,17),d(17,35)
      dimension a(17,17),c(17,221),g(17),x(17,17),y(17,17)
c
c     Calculate average equilibrium potential in either
c     electrode if lag=1, otherwise lag=0

c Section for Paxton kinetic data
c NOTE: always set rate constants rka1, rka3 to 1.0 if using NiMH

      if (nprop.eq.4) then !Ni/MH system
      RTF=r*t/fc ! change danger
      ATCN=0.65d0
      CTCN=0.35d0
      ATCP=0.35d0
      CTCP=0.65d0
      H2OMW=18.016D0
      PKOHMW=56.11D0
      Pcon=xx(1,j)/1.0d6
      Pcs=xx(2+mpa,j)/1.0d6
      CNGMAX=ct1(mpa)/1.0d6
      CPSMAX=ct3(mpa)/1.0d6

      EXCDN= 7.85D-04 /(0.012644D0**CTCN*0.046814D0**ATCN*
     1        (0.5D0*CNGMAX)**(CTCN+ATCN))
      EXCDP= 1.04D-04 /(0.012644D0**CTCP*0.046814D0**ATCP*
     1         (0.5D0*CPSMAX)**(CTCP+ATCP))     

      DN = 1.001D0 + 47.57D0*Pcon - 776.22D0*Pcon**2
      DN1D = 47.57D0 - 1552.44D0*Pcon
      DN2D = -1552.44D0

      AC = 1.004D0 - 36.23D0*Pcon**0.5 + 1374.3D0*Pcon
     1 - 17850.7*Pcon**1.5 + 55406.0D0*Pcon**2
     1 +7.16856D05*Pcon**2.5
      AC1D=-18.115D0*Pcon**(-0.5)+1374.3D0-2.6776D04*Pcon**0.5
     1 +1.10812D05*Pcon + 1.7921D06*Pcon**1.5
      AC2D = 9.0575D0/Pcon**1.5 - 13388.0D0/Pcon**0.5
     1 + 1.10812D05 + 2.6882D06*Pcon**0.5
      WAC = 1.0002D0 - 21.238D0*Pcon - 4.1312D03*Pcon**2.0D0
      WAC1D= -21.238D0 -2.0d0*4.1312D03*Pcon    
      endif


c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c     EQUILIBRIUM POTENTIAL FUNCTIONS:
c
c     g0(mpa) is the equilibrium potential in terms of the solid
c     concentration, xx(2+mpa,j), with respect to a lithium metal
c     electrode
c     g1(mpa) is the derivative of the equilibrium potential wrt
c     the solid concentration
c
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c     FOR THE NEGATIVE ELECTRODE
c
      if (j .le. n1+1) then
c
c *** FOR MULTIPLE TYPES OF ACTIVE MATERIALS ***
	if (mpa.eq.1) nneg=nneg
	if (mpa.eq.2) nneg=nneg
	if (mpa.eq.3) nneg=nneg
	if (mpa.eq.4) nneg=nneg
	if (mpa.eq.5) nneg=nneg

c
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
      go to (51,52,53,54,55),nneg
      write (2,*) 'Please enter data for negative electrode
     & #5 in subroutine ekin'
      stop
   51 go to 111  ! Li foil
   52 go to 112  ! Carbon (petroleum coke)
   53 go to 113  ! MCMB 2528 graphite (Bellcore)
   54 go to 114  ! Metal Hydride (from Paxton)
   55 go to 115  ! add your own

c
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     Li foil (works only if n1 = 0)
c
  111 cot1(mpa)=3860.d0
	rs1(mpa)=540.d0
      ct1(mpa)=3.6d03*cot1(mpa)*rs1(mpa)/fc
      if (lag.eq.1) xx(2+mpa,j)=utz*ct1(mpa)

      g0(mpa) = 0.0d0
      g1(mpa) = 0.0d0
c assumed temperature dependence for exchange current
      ti0n=dexp((Ebarka)*(t-298.0d0)/(t*298.0d0))
      rka1(mpa)=rka1save*ti0n
      dudt = 0.0d0
      go to 97
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     Carbon (petroleum coke):  Bellcore
c
  112 cot1(mpa)=372.d0
      rs1(mpa)=1900.d0 !from 1996 JES paper Doyle, Newman,Tarascon
      ct1(mpa)=3.6d03*cot1(mpa)*rs1(mpa)/fc
      if (lag.eq.1) xx(2+mpa,j)=utz*ct1(mpa)
	c1=-0.160d0
      c2=1.32d0
      c3=-3.0d0
      g0(mpa)=c1+c2*expg(c3*xx(2+mpa,j)/ct1(mpa))
      g0(mpa)=g0(mpa)+10.d0*expg(-2000.d0*xx(2+mpa,j)/ct1(mpa))
      g1(mpa)=c2*c3*expg(c3*xx(2+mpa,j)/ct1(mpa))/ct1(mpa)
      g1(mpa)=g1(mpa)-10.d0*2000.d0/ct1(mpa)*
	1expg(-2000.d0*xx(2+mpa,j)/ct1(mpa))

c assumed temperature dependence for exchange current
      ti0n=dexp((Ebarka)*(t-298.0d0)/(t*298.0d0))
      rka1(mpa)=rka1save*ti0n
      dudt = 0.0d0
      go to 97
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c
  113 cot1(mpa)=372.d0
      rs1(mpa)=2250.d0
      ct1(mpa)=3.6d03*cot1(mpa)*rs1(mpa)/fc
      if (lag.eq.1) xx(2+mpa,j)=utz*ct1(mpa)

c     MCMB 2528 graphite measured by Chris Bogatu 2000, 
c     Telcordia and PolyStor materials.
c     Modified May 2003 to match data from Joongpyo Shim
c     for 0.01 < x < 0.99
      sto = xx(2+mpa,j)/ct1(mpa)
      g0(mpa) = 0.124d0+1.5d0*expg(-150.0d0*sto)
     & +0.0351d0*dtanh((sto-0.286d0)/0.083d0)
     & - 0.0045d0*dtanh((sto-0.9d0)/0.119d0)
     & - 0.035d0*dtanh((sto-0.99d0)/0.05d0)
     & - 0.0147d0*dtanh((sto-0.5d0)/0.034d0)
     & - 0.102d0*dtanh((sto-0.194d0)/0.142d0)
     & - 0.022d0*dtanh((sto-0.98d0)/0.0164d0)
     & - 0.011d0*dtanh((sto-0.124d0)/0.0226d0)
     & + 0.0155d0*dtanh((sto-0.105d0)/0.029d0)
      g1(mpa) = -1.5d0*(150.0d0/ct1(mpa))*expg(-150.0d0*sto)
     &+(0.0351d0/(0.083d0*ct1(mpa)))*((dcosh((sto-0.286d0)/0.083d0))
     &**(-2))
     &-(0.0045d0/(ct1(mpa)*0.119d0))*((dcosh((sto-0.9d0)/0.119d0))
     &**(-2))
     &-(0.035d0/(ct1(mpa)*0.05d0))*((dcosh((sto-0.99d0)/0.05d0))**(-2))
     &-(0.0147d0/(ct1(mpa)*0.034d0))*((dcosh((sto-0.5d0)/0.034d0))
     &**(-2))
     &-(0.102d0/(ct1(mpa)*0.142d0))*((dcosh((sto-0.194d0)/0.142d0))
     &**(-2))
     &-(0.022d0/(ct1(mpa)*0.0164d0))*((dcosh((sto-0.98d0)/0.0164d0))
     &**(-2))
     &-(0.011d0/(ct1(mpa)*0.0226d0))*((dcosh((sto-0.124d0)/0.0226d0))
     &**(-2))
     &+(0.0155d0/(ct1(mpa)*0.029d0))*((dcosh((sto-0.105d0)/0.029d0))
     &**(-2))
c assumed temperature dependence for exchange current
      ti0n=dexp((Ebarka)*(t-298.0d0)/(t*298.0d0))
      rka1(mpa)=rka1save*ti0n
      dudt = 0.0d0
      go to 97

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  114 cot1(mpa)=372.d0
      rs1(mpa)=7500.d0
      ct1(mpa)=3.6d03*cot1(mpa)*rs1(mpa)/fc
      if (lag.eq.1) xx(2+mpa,j)=utz*ct1(mpa)
c     Metal hydride (based on Blaine Paxtons masters thesis)
      CNORM=xx(2+mpa,j)/ct1(mpa)
      c1=40.0d0
      c2=50.0d0
      c3=5000.0d0
      c4=0.8d0
      c5=0.2d0
      c6=-0.91d0
      g0(mpa)=(expg(c2*(c5-cnorm))-expg(c1*(cnorm-c4)))/c3+c6
      g1(mpa)=-1.0d0/c3*(c2*expg(c2*(c5-cnorm))+c1*expg(c1*(cnorm-c4)))
      g1(mpa)=g1(mpa)/ct1(mpa)

      dudt=0.0d0 
c kinetic expressions for MH here

      if (lag.eq.1) go to 99
      RTF=r*t/fc
      ATCN=0.58d0
      CTCN=0.35d0
      ATCP=0.36d0
      CTCP=0.58d0
      H2OMW=18.016D0
      PKOHMW=56.11D0
      Pcon=xx(1,j)/1.0d6
      Pcs=xx(2+mpa,j)/1.0d6
      CNGMAX=ct1(mpa)/1.0d6
      CPSMAX=ct3(mpa)/1.0d6

      EXCDN= 7.85D-04 /(0.012644D0**CTCN*0.046814D0**ATCN*
     1         (0.5D0*CNGMAX)**(CTCN+ATCN))
      EXCDP= 1.04D-04 /(0.012644D0**CTCP*0.046814D0**ATCP*
     1         (0.5D0*CPSMAX)**(CTCP+ATCP))

      DN = 1.001D0 + 47.57D0*Pcon - 776.22D0*Pcon**2
      DN1D = 47.57D0 - 1552.44D0*Pcon
      DN2D = -1552.44D0

        AC = 1.004D0 - 36.23D0*Pcon**0.5 + 1374.3D0*Pcon
     1 - 17850.7*Pcon**1.5 + 55406.0D0*Pcon**2
     1 +7.16856D05*Pcon**2.5
        AC1D=-18.115D0*Pcon**(-0.5)+1374.3D0-2.6776D04*Pcon**0.5
     1 +1.10812D05*Pcon + 1.7921D06*Pcon**1.5
      AC2D = 9.0575D0/Pcon**1.5 - 13388.0D0/Pcon**0.5
     1 + 1.10812D05 + 2.6882D06*Pcon**0.5
      WAC = 1.0002D0 - 21.238D0*Pcon - 4.1312D03*Pcon**2.0D0
      WAC1D= -21.238D0 -2.0d0*4.1312D03*Pcon  

c assumed temperature dependence for exchange current
      ti0n=dexp((Ebarka)*(t-298.0d0)/(t*298.0d0))
      rka1(mpa)=rka1save*ti0n
      if(cngmax.le.pcs) print *, 'large H ',cngmax,pcs

      U0=rka1(mpa)*EXCDN/H2OMW**ATCN
      U1=DN-Pcon*PKOHMW
      U2=DN1D-PKOHMW
      U3= (AC*Pcon)**CTCN*(WAC*U1)**ATCN
      U4= PCS**CTCN*(CNGMAX-Pcs)**ATCN
      U6= (AC*Pcon)**CTCN*ATCN*(WAC*U1)**(ATCN-1.0D0)*
     1(WAC*U2+WAC1D*U1)+(WAC*U1)**ATCN*CTCN*
     1(AC*PCON)**(CTCN-1.0D0)*(AC+Pcon*AC1D)
      U7= CTCN*(CNGMAX-Pcs)**ATCN*PCS**(CTCN-1.0D0)-
     1ATCN*Pcs**CTCN*(CNGMAX-Pcs)**(ATCN-1.0D0)

      h0=U0*U3*U4/fc*1.0d4
      h1=U0*U3*U7/fc/1.0d2
      h2=U0*U6*U4/fc/1.0d2
      r1a=ATCN*frt
      r1c=CTCN*frt
      r2a=r1a*(xx(kp1,j)-xx(kp2,j)-g0(mpa))
      r2c=r1c*(xx(kp1,j)-xx(kp2,j)-g0(mpa))

      de=expg(-r2c)-expg(r2a)
      pe=expg(-r2c)+expg(r2a)

      b(3+npa+mpa,1)=h2*de
      b(3+npa+mpa,kp2)=h0*(r1c*expg(-r2c)+r1a*expg(r2a))
      b(3+npa+mpa,kp1)=-b(3+mpa+npa,kp2)
      b(3+npa+mpa,2+mpa)=h1*de+h0*(r1c*g1(mpa)*expg(-r2c)+r1a*g1(mpa)
     &*expg(r2a))
      b(3+npa+mpa,3+npa+mpa)=1.0d0
      g(3+npa+mpa)=-h0*de-xx(3+npa+mpa,j)

      go to 99 ! go to 99 not 97 to avoid other kinetics!

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  115 continue
c *** INPUT YOUR OWN ELECTRODE DATA HERE ***
      write (2,*) 'Please enter data for negative electrode
     & #5 in subroutine ekin'
      stop

c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c     KINETIC EXPRESSIONS FOR NEGATIVE ELECTRODE
c
c     h0 is the exchange current density (A/m2)
c     h1 is the derivative of io wrt solid concentration, xx(2+mpa,j)
c     h2 is the derivative of io wrt electrolyte concen., xx(1,j)
c
c
c     NONAQUEOUS LIQUIDS
c
   97 if (lag.eq.1) go to 99
      alpha=0.5d0
      alphc=0.5d0
      if (n1.eq.0) then
      h0=rka1(mpa)*dsqrt(xx(1,j))
      h1 = 0.0d0
      h2 = rka1(mpa)/dsqrt(xx(1,j))/2.0d0
      else
      h0=rka1(mpa)*dsqrt(xx(1,j))*dsqrt(ct1(mpa)-xx(2+mpa,j))
     &*dsqrt(xx(2+mpa,j))
      h1=rka1(mpa)*dsqrt(xx(1,j))*dsqrt(ct1(mpa)-xx(2+mpa,j))
     &*dsqrt(xx(2+mpa,j))
     &*ct1(mpa)/(ct1(mpa)-xx(2+mpa,j))/xx(2+mpa,j)/2.0d0
      h2=rka1(mpa)*dsqrt(ct1(mpa)-xx(2+mpa,j))*dsqrt(xx(2+mpa,j))
     &/dsqrt(xx(1,j))/2.0d0
      endif
 
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c     POLYMER
c
c     cmax=3.92d03
c     alpha=0.5d0
c     alphc=0.5d0
c     h0=rka1(mpa)*dsqrt(xx(1,j))*dsqrt(cmax-xx(1,j))*dsqrt(ct1(mpa)-xx(2+mpa,j))
c    1*dsqrt(xx(2+mpa,j))
c     h1=-rka1(mpa)*dsqrt(xx(1,j))*dsqrt(cmax-xx(1,j))*dsqrt(ct1(mpa)-xx(2+mpa,j))
c    1*dsqrt(xx(2+mpa,j))*(1.0d0/(ct1(mpa)-xx(2+mpa,j))-1.0d0/xx(2+mpa,j))/2.0d0
c     h2=-rka1(mpa)*dsqrt(xx(2+mpa,j))*dsqrt(ct1(mpa)-xx(2+mpa,j))*dsqrt(cmax-xx(1,j))
c    1*dsqrt(xx(1,j))*(1.0d0/(cmax-xx(1,j))-1.0d0/xx(1,j))/2.0d0

      end if
c
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c     FOR THE POSITIVE ELECTRODE
c
      if (j .ge. n1+n2) then
c
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c *** FOR MULTIPLE ACTIVE MATERIALS ***
	if (mpa.eq.1) npos=npos
	if (mpa.eq.2) npos=npos
	if (mpa.eq.3) npos=npos
	if (mpa.eq.4) npos=npos
	if (mpa.eq.5) npos=npos

      go to (1,2,3,4,5,6,7),npos
      write (2,*) 'Please enter data for positive electrode
     & #7 in subroutine ekin'
      stop

    1 go to 201  ! LiCoO2 (Cobalt dioxide)
    2 go to 202  ! V2O5 (Vanadium oxide)
    3 go to 203  ! Spinel LiMn2O4 
    4 go to 204  ! LiNi0.80Co0.15Al0.05O2
    5 go to 205  ! LiFePO4
    6 go to 206  ! Ni(OH)2 from Albertus and Newman
    7 go to 207  ! add your own
c
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     CoO2 (Cobalt dioxide)
c
  201 cot3(mpa)=274.d0
      rs3(mpa)=5100.d0
      ct3(mpa)=3.6d03*cot3(mpa)*rs3(mpa)/fc
      if (lag.eq.1) xx(2+mpa,j)=utz*ct3(mpa)
c     Measured by Oscar Garcia 2001 using Quallion electrodes for
c     0.5 < y < 0.99.  Fit revised by Karen Thomas in May 2003 to
c     match Doyle's fit for y < 0.4 and Garcia's data at larger y.
c     Valid for 0 < y < 0.99. Note that capacity fade is found to
c     occur experimentally if y goes below 0.5; this is not included
c     in the model.
      sto = xx(2+mpa,j)/ct3(mpa)
      g0(mpa) = 2.16216d0+0.07645d0*dtanh(30.834d0-54.4806d0*sto)
     & + 2.1581d0*dtanh(52.294d0-50.294d0*sto)
     & - 0.14169d0*dtanh(11.0923d0-19.8543d0*sto)
     & + 0.2051d0*dtanh(1.4684d0-5.4888d0*sto)
     & + 0.2531d0*dtanh((-sto+0.56478d0)/0.1316d0)
     & - 0.02167d0*dtanh((sto-0.525d0)/0.006d0)
      g1(mpa) = 0.07645d0*(-54.4806d0/ct3(mpa))*
     &((1.0d0/dcosh(30.834d0-54.4806d0*sto))**2)
     &+2.1581d0*(-50.294d0/ct3(mpa))*((dcosh(52.294d0-50.294d0*sto))
     &**(-2))
     &+0.14169d0*(19.854d0/ct3(mpa))*((dcosh(11.0923d0-19.8543d0*sto))
     &**(-2))
     &-0.2051d0*(5.4888d0/ct3(mpa))*((dcosh(1.4684d0-5.4888d0*sto))
     &**(-2))
     &-0.2531d0/0.1316d0/ct3(mpa)*((dcosh((-sto+0.56478d0)/0.1316d0))
     &**(-2))
     &-0.02167d0/0.006d0/ct3(mpa)*((dcosh((sto-0.525d0)/0.006d0))
     &**(-2))
c assumed temperature dependence for exchange current
      ti0p=dexp((Ebarkc)*(t-298.0d0)/(t*298.0d0))
      rka3(mpa)=rka3save*ti0p
      go to 98
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     V2O5 (Vanadium oxide 0<y<0.95)
c
  202 cot3(mpa)=417.d0 !double check
	rs3(mpa)=3900.d0 !double check
      ct3(mpa)=3.6d03*cot3(mpa)*rs3(mpa)/fc
      if (lag.eq.1) xx(2+mpa,j)=utz*ct3(mpa)

      r1=3.3059d0
      r2=0.092769d0
      r3=14.362d0
      r4=6.6874d0
      r5=0.034252d0
      r6=100.0d0
      r7=0.96d0
      r8=0.00724d0
      r9=80.0d0
      r10=0.01d0
      a2=xx(2+mpa,j)/ct3(mpa)
      g0(mpa)=r1+r2*dtanh(-r3*a2+r4)-r5*expg(r6*(a2-r7))+r8*expg(r9*
     1(r10-a2))
      g1(mpa)=-r2*r3/dcosh(-r3*a2+r4)/dcosh(-r3*a2-r4)/ct3(mpa)-r5*r6
     &*expg(r6*(a2-r7))/ct3(mpa)-r8*r9*expg(r9*(r10-a2))/ct3(mpa)
c assumed temperature dependence for exchange current
      ti0p=dexp((Ebarkc)*(t-298.0d0)/(t*298.0d0))
      rka3(mpa)=rka3save*ti0p
      go to 98
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c	Spinel Mn2O4, from Doyle, Newman, Tarascon
  203 cot3(mpa)=148.d0
	rs3(mpa)=4140.d0
      ct3(mpa)=3.6d03*cot3(mpa)*rs3(mpa)/fc
      if (lag.eq.1) xx(2+mpa,j)=utz*ct3(mpa)

	a1=4.19829d0
      a2=0.0565661d0
      a3=14.5546d0
      a4=8.60942d0
      a5=0.0275479d0
      a6=0.998432d0  ! would prefer this to be >=1
      a7=-0.492465d0
      a8=1.901110d0
      a9=0.157123d0
      a10=0.04738d0
      a11=0.810239d0
      a12=40.0d0
      a13=0.133875d0

      g0(mpa)=a1+a2*dtanh(-a3*xx(2+mpa,j)/ct3(mpa)+a4)
     1-a9*expg(-a10*((xx(2+mpa,j)/ct3(mpa))**8))+a11
     1*expg(-a12*(xx(2+mpa,j)/ct3(mpa)-a13))+a5*a8
      if(xx(2+mpa,j).lt.a6*ct3(mpa)) 
	1g0=g0-a5*((a6-xx(2+mpa,j)/ct3(mpa))**a7)

      g1=(1.0d0/ct3(mpa))*(-a2*a3/dcosh(-a3*xx(2+mpa,j)/ct3(mpa)
	1+a4)/dcosh(-a3
     1*xx(2+mpa,j)/ct3(mpa)+a4)
     1+a9*a10*8.0d0*((xx(2+mpa,j)/ct3(mpa))**7)*expg(-a10*
     1(xx(2+mpa,j)/ct3(mpa))**8))-a11*a12/ct3(mpa)*
     1expg(-a12*(xx(2+mpa,j)
     &/ct3(mpa)-a13))
      if(xx(2+mpa,j).lt.a6*ct3(mpa)) g1=g1+a5*a7*(a6-xx(2+mpa,j)
	1/ct3(mpa))
     &**(-1.d0+a7)/ct3(mpa)

      ti0p=dexp((Ebarkc)*(t-298.0d0)/(t*298.0d0))
      rka3(mpa)=rka3save*ti0p
      go to 98
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c	Ni0.80Co0.15Al0.05O2, measured by Paul Albertus

  204 cot3(mpa)=279.d0
      rs3(mpa)=4710.d0
      ct3(mpa)=3.6d03*cot3(mpa)*rs3(mpa)/fc
      if (lag.eq.1) xx(2+mpa,j)=utz*ct3(mpa)

	b2 = xx(2+mpa,j)/ct3(mpa)
      sto = xx(2+mpa,j)/ct3(mpa)
      dac = 45.d0*(b2-1.012d0)
      dbc = (0.04d0-b2)/.085d0
	dcc=100.d0*(b2-1.005d0)
      g0(mpa)=0.99d0*b2**2.d0-2.35d0*b2+4.9d0-dexp(dac)+dexp(dbc)-
	1dexp(dcc)
      g1(mpa)=2.d0*0.99d0*b2-2.35d0-dexp(dac)*45.d0-dexp(dbc)
	1/0.085d0-100.d0*dexp(dcc)
      g1(mpa)=g1(mpa)/ct3(mpa)
      ti0p=dexp((Ebarkc)*(t-298.0d0)/(t*298.0d0))
      rka3(mpa)=rka3save*ti0p
      go to 98

c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c	FePO4, from Venkat Srinivasan

  205	cot3(mpa)=170.d0
	rs3(mpa)=3430.d0
      ct3(mpa)=3.6d03*cot3(mpa)*rs3(mpa)/fc
      if (lag.eq.1) xx(2+mpa,j)=utz*ct3(mpa)

	g0(mpa)=3.56d0+0.55163D0*atan(0.2354D2-0.40895D3*
     &xx(2+mpa,j)/ct3(mpa))
     &+0.46309D0*atan(-0.20336D3*xx(2+mpa,j)/ct3(mpa)+0.20243D3)

	g0(mpa)=g0(mpa)-dexp(70.d0*(xx(2+mpa,j)/ct3(mpa)-.99d0))

      g1(mpa)= -0.2255891D3/ct3(mpa)/(1.D0+(-0.40895D3*xx(2+mpa,j)/
     &ct3(mpa)+0.2354D2)
     &**2.D0)-0.0941740D3/ct3(mpa)/(1.D0+(-0.20336D3*xx(2+mpa,j)/
     &ct3(mpa)+0.20243D3
     &)**2.D0)

      g1(mpa)=g1(mpa)-70.d0/ct3(mpa)*
     &dexp(70.d0*(xx(2+mpa,j)/ct3(mpa)-.99d0))

      ti0p=dexp((Ebarkc)*(t-298.0d0)/(t*298.0d0))
      rka3(mpa)=rka3save*ti0p
      go to 98

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  206 cot3(mpa)=279.d0
	rs3(mpa)=3550.d0
      ct3(mpa)=3.6d03*cot3(mpa)*rs3(mpa)/fc
      if (lag.eq.1) xx(2+mpa,j)=utz*ct3(mpa)

c NiOOHHy positive electrode

c These numbers are for discharging
      c1=0.41d0
      c2=0.1d0
      c3=20.0d0
      c4=50.0d0
      c5=2.0d0
      c6=-0.016d0
      c7=2.0d0
      c8=5.0d0
      c9=0.015d0
      c10=1.07d0
      c11=-0.01d0
      c12=.01d0
      c13=2.7d0
      c14=0.9d0
      c15=0.01d0
      c16=0.65d0
      c17=50.0d0

c These numbers are for charging
      ca1=0.41d0
      ca2=2.0d0
      ca3=11.0d0
      ca4=20.0d0
      ca5=2.0d0
      ca6=-.008d0
      ca7=2.0d0
      ca8=0.1d0
      ca9=0.065d0
      ca10=1.00d0
      ca11=-0.3d0
      ca12=.001d0
      ca13=10.0d0
      ca14=0.7d0

      CNORM=xx(2+mpa,j)/ct3(mpa)
      if (cur.ge.0.0d0) then !discharge
      g0(mpa)=c1+c2*expg(c3*(-cnorm+c11))-c8*expg(c4*(cnorm-c10))
     &+c6*dlog(dabs((cnorm/(1.0d0-cnorm))))-c9-c12*expg(c13*cnorm-c14)
     &+c15*expg(-c17*((cnorm-c16)**2))
      g1(mpa)=-c3*c2*expg(c3*(-cnorm+c11))-c8*c4*expg(c4*(cnorm-c10))
     &+c6/cnorm/(1.0d0-cnorm)-c12*c13*expg(c13*cnorm-c14)
     &+c15*expg(-c17*((cnorm-c16)**2))*(-c17)*2.0d0*(cnorm-c16)
      else !charge
      g0(mpa)=ca1+ca2*expg(ca3*(-cnorm+ca11))-ca8*expg(ca4*(cnorm-ca10))
     &+ca6*dlog(dabs((cnorm/(1.0d0-cnorm))))+ca9-
     &ca12*expg(ca13*(cnorm-ca14))
      g1(mpa)=-ca3*ca2*expg(ca3*(-cnorm+ca11))-ca8*ca4*expg(ca4*(cnorm
     &-ca10))+ca6/cnorm/(1.0d0-cnorm)-ca12*ca13*expg(ca13*(cnorm-ca14))
      endif
      g1(mpa)=g1(mpa)/ct3(mpa)

      dudt=0.0d0

c kinetic expressions here for positive

      if (lag.eq.1) go to 99

      RTF=r*t/fc
      ATCN=0.58d0
      CTCN=0.35d0
      ATCP=0.36d0
      CTCP=0.58d0
      H2OMW=18.016D0
      PKOHMW=56.11D0
      Pcon=xx(1,j)/1.0d6
      Pcs=xx(2+mpa,j)/1.0d6
      CNGMAX=ct1(mpa)/1.0d6
      CPSMAX=ct3(mpa)/1.0d6

      EXCDN= 7.85D-04 /(0.012644D0**CTCN*0.046814D0**ATCN*
     1         (0.5D0*CNGMAX)**(CTCN+ATCN))
      EXCDP= 1.04D-04 /(0.012644D0**CTCP*0.046814D0**ATCP*
     1         (0.5D0*CPSMAX)**(CTCP+ATCP))     

      DN = 1.001D0 + 47.57D0*Pcon - 776.22D0*Pcon**2
      DN1D = 47.57D0 - 1552.44D0*Pcon
      DN2D = -1552.44D0

        AC = 1.004D0 - 36.23D0*Pcon**0.5 + 1374.3D0*Pcon
     1 - 17850.7*Pcon**1.5 + 55406.0D0*Pcon**2
     1 +7.16856D05*Pcon**2.5
        AC1D=-18.115D0*Pcon**(-0.5)+1374.3D0-2.6776D04*Pcon**0.5
     1 +1.10812D05*Pcon + 1.7921D06*Pcon**1.5
      AC2D = 9.0575D0/Pcon**1.5 - 13388.0D0/Pcon**0.5
     1 + 1.10812D05 + 2.6882D06*Pcon**0.5
      WAC = 1.0002D0 - 21.238D0*Pcon - 4.1312D03*Pcon**2.0D0
      WAC1D= -21.238D0 -2.0d0*4.1312D03*Pcon  

c assumed temperature dependence for exchange current
      ti0p=dexp((Ebarkc)*(t-298.0d0)/(t*298.0d0))
      rka3(mpa)=rka3save*ti0p

      U0=rka3(mpa)*EXCDP/H2OMW**ATCP
      U1=DN-Pcon*PKOHMW
      U2=DN1D-PKOHMW
      U3= (AC*Pcon)**CTCP*(WAC*U1)**ATCP
      U4= Pcs**CTCP*(CPSMAX-Pcs)**ATCP
      U6= (AC*Pcon)**CTCP*ATCP*(WAC*U1)**(ATCP-1.0D0)*
     1(WAC*U2+WAC1D*U1)+(WAC*U1)**ATCP*CTCP*
     1(AC*Pcon)**(CTCP-1.0D0)*(AC+Pcon*AC1D)
      U7= CTCP*(CPSMAX-Pcs)**ATCP*Pcs**(CTCP-1.0D0)-
     1ATCP*Pcs**CTCP*(CPSMAX-Pcs)**(ATCP-1.0D0)

      h0=U0*U3*U4/fc*1.0d4
      h1=U0*U3*U7/fc/1.0d2
      h2=U0*U6*U4/fc/1.0d2
      r1a=ATCP*frt
      r1c=CTCP*frt
      r2a=r1a*(xx(kp1,j)-xx(kp2,j)-g0(mpa))
      r2c=r1c*(xx(kp1,j)-xx(kp2,j)-g0(mpa))

      de=expg(-r2c)-expg(r2a)
      pe=expg(-r2c)+expg(r2a)

      b(3+npa+mpa,1)=h2*de
      b(3+npa+mpa,kp2)=h0*(r1c*expg(-r2c)+r1a*expg(r2a))
      b(3+npa+mpa,kp1)=-b(3+npa+mpa,kp2)
      b(3+npa+mpa,2+mpa)=h1*de+h0*(r1c*g1(mpa)*expg(-r2c)+r1a*g1(mpa)
     &*expg(r2a))
      b(3+npa+mpa,3+npa+mpa)=1.0d0
      g(3+npa+mpa)=-h0*de-xx(3+npa+mpa,j)

      go to 99 ! go to 99 not 97 to avoid other kinetics

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  207 continue

      write (2,*) 'Please enter data for positive electrode
     & #7 in subroutine ekin'
      stop

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c     KINETIC EXPRESSIONS FOR THE POSITIVE ELECTRODE
c
c     h0 is the exchange current density (A/m2)
c     h1 is the derivative of io wrt solid concentration, xx(2+mpa,j)
c     h2 is the derivative of io wrt electrolyte concen., xx(1,j)
c
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c     NONAQUEOUS LIQUIDS
c
   98 if (lag.eq.1) go to 99
      alpha=0.5d0
      alphc=0.5d0
      h0=rka3(mpa)*dsqrt(xx(1,j))*dsqrt(ct3(mpa)-xx(2+mpa,j))
     &*dsqrt(xx(2+mpa,j))
      h1=rka3(mpa)*dsqrt(xx(1,j))*dsqrt(ct3(mpa)-xx(2+mpa,j))
     &*dsqrt(xx(2+mpa,j))
     &*ct3(mpa)/(ct3(mpa)-xx(2+mpa,j))/xx(2+mpa,j)/2.0d0
      h2=rka3(mpa)*dsqrt(ct3(mpa)-xx(2+mpa,j))*dsqrt(xx(2+mpa,j))
     &/dsqrt(xx(1,j))/2.0d0
c
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c     POLYMER
c
c     alpha=0.5d0
c     alphc=0.5d0
c     h0=rka3(mpa)*dsqrt(xx(1,j))*dsqrt(cmax-xx(1,j))*dsqrt(ct3(mpa)-xx(2+mpa,j))
c    1*dsqrt(xx(2+mpa,j))
c     h1=-rka3(mpa)*dsqrt(xx(1,j))*dsqrt(cmax-xx(1,j))*dsqrt(ct3(mpa)-xx(2+mpa,j))
c    1*dsqrt(xx(2+mpa,j))*(1.0d0/(ct3(mpa)-xx(2+mpa,j))-1.0d0/xx(2+mpa,j))/2.0d0
c     h2=-rka3(mpa)*dsqrt(xx(2+mpa,j))*dsqrt(ct3(mpa)-xx(2+mpa,j))*dsqrt(cmax-xx(1,j))
c    1*dsqrt(xx(1,j))*(1.0d0/(cmax-xx(1,j))-1.0d0/xx(1,j))/2.0d0
c
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
      end if
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      if (lag.eq.1) go to 99
c *** HERE DEFINE THE b() AND g() FOR THIS EQUATION ***
      r1=alpha*frt
      if(j.le.n1+1) then
           an1=1.0d0
           an2=0.0d0
         else
           an1=0.0d0
           an2=1.0d0
      endif

      r2=r1*(xx(kp1,j)-xx(kp2,j)-g0(mpa)-fc*xx(3+npa+mpa,j)
     &*(an1*ranode(mpa)+an2*rcathde(mpa)))

      de=-2.d0*r2-r2**3/3.d0-r2**5/60.d0
      if(dabs(r2).gt.200.d0) then
      if(r2.gt.200.d0) de=7.0d86
      if(r2.lt.-200.d0) de=-7.0d86
      pe=7.0d86
      else
      if(dabs(r2).gt.1.0d-7) de=expg(-r2)-expg(r2)
      pe=expg(-r2)+expg(r2)
      endif
      b(3+npa+mpa,1)=h2*de
      b(3+npa+mpa,kp2)=h0*r1*pe
      b(3+npa+mpa,kp1)=-b(3+npa+mpa,kp2)
      b(3+npa+mpa,2+mpa)=h1*de+h0*r1*g1(mpa)*pe 
	b(3+npa+mpa,3+npa+mpa)=1.d0+fc*b(3+npa+mpa,kp2)*
     &(an1*ranode(mpa)+an2*rcathde(mpa))
      g(3+npa+mpa)=-h0*de-xx(3+npa+mpa,j)

   99 return
      end

c***********************************************************************
      subroutine prop(nj,n2,n1)
c     subroutine to create library of properties of various
c     electrolytes.
      implicit real*8(a-h,o-z)
      parameter(maxt=900)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/gas/ epg1,epg2,epg3
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/tprop/df(221),cd(221),tm(221),ddo2(221),ddh2(221),
     1ddf(221),dcd(221),dtm(221),dfu(221),d2fu(221),do2(221),dh2(221)
      common/temp/ thk,htc,dudt,Cp,dens,tam,g0(9),
     &qq,qloss,residm,ncell,lht
      common/side/rksc1,c1init,c2init,rksa1,term_s1(221),vol,
     &rksa2,rksa3,rksc3,rksc2,UsO2,UsH2,term_s2(221),nside
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
c
      do 99 j=1,nj
      ee=ep2+epp2
      if(j .lt. n1+2 .and. n1.ne.0) ee=ep1+epp1
      if(j .gt. n2+n1) ee=ep3+epp3 

      go to (1,2,3,4,5),nprop
    1 go to 101  ! LiPF6 in PC (Sony cell simulation)
    2 go to 102  ! LiPF6 in EC:DMC
    3 go to 103  ! LiTFSI in PEO at 85 C (from Ludvig Edman, 2000)
    4 go to 104  ! 30% KOH in H20 (from Paxton)  
    5 go to 105  ! add your own
c
c  Key to labeling of transport properties:
c  As written salt must be a binary electrolyte with v+=v-=1
c  df(j) - Diffusion coefficient of the salt (m2/s)
c  ddf(j) - First derivative of df(j) wrt electrolyte concentration
c  cd(j) - Conductivity of the salt (S/m)
c  dcd(j) - First derivative of cd(j) wrt electrolyte concentration
c  tm(j) - Transference number. For system in which cation reacts use
c          t+ (e.g. Li systems), for systems in which anion reacts use
c          t- (e.g. NiMH system).
c  dtm(j) - First derivative of tm(j) wrt electrolyte concentration
c  dfu(j) - Activity factor for salt (dlnf/dc) 
c  d2fu(j) - Derivative of dfj(j) wrt electrolyte concentration (d2lnf/dc2)
c 
c  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     LiPF6 in PC (Sony cell simulation used in JES vol. 141, 1994, p. 982 paper)
c
c     this is actually the diff coeff for perchlorate
c     diffusion coefficient of the salt (m2/s)
  101 df(j)=(ee**1.5d0)*2.58d-10
      ddf(j)=0.0d0
      tdd=dexp((EbarD)*(t-298.0d0)/(t*298.0d0))
      df(j)=df(j)*tdd
      ddf(j)=ddf(j)*tdd
c     conductivity of the salt (S/m) from Barthel et al., Ber. Bunsenges. Phys. Chem. vol 83, 1979, p. 911
c     pmax=0.5409d0 !Marcs fit to Sony cell data
      pmax=0.035d0 !from Barthel et al.
      pu=0.857d0
      aa=1.093d0
      bb=0.04d0
      rho=1.2041d03
      fun=pmax*((1.0d0/rho/pu)**aa)*expg(bb*((xx(1,j)/rho-pu)**2)
     1-(aa/pu)*(xx(1,j)/rho-pu))
      fun2=2.0d0*(bb/rho)*(xx(1,j)/rho-pu)-aa/pu/rho
      cd(j)=0.0001d0+(ee**1.5d0)*((xx(1,j))**aa)*fun
      dcd(j)=(ee**1.5d0)*fun*(aa*(xx(1,j)**(aa-1.0d0))+(xx(1,j)**aa)
     1*fun2)
      tdkap=dexp((Ebarkap)*(t-298.0d0)/(t*298.0d0))
      cd(j)=cd(j)*tdkap
      dcd(j)=dcd(j)*tdkap
c     transference number of lithium
      tm(j)=0.20d0
      dtm(j)=0.0d0
c
c     activity factor for the salt (dlnf/dc and d2lnf/dc2)
      dfu(j)=0.0d0
      d2fu(j)=0.0d0
      go to 99

c &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c LiPF6 in EC:DMC.
c D and t+ are fit to data from J. Power Sources, vol. 82, 1999, p. 859
c for LiPF6 in EC:EMC.
c cd is from measurements made at
c Bellcore, as reported in Marc Doyles dissertation.
c
  102 continue
      df(j) = (ee**1.5d0)*5.34d-10*expg(-0.65d0*xx(1,j)/1000.0d0)
      ddf(j) = -0.65d0*df(j)/1000.d0
      tdd=dexp((EbarD)*(t-298.0d0)/(t*298.0d0))
      df(j)=df(j)*tdd
      ddf(j)=ddf(j)*tdd
      cd(j) = (ee**1.5d0)*(0.0911d0+1.9101d0*xx(1,j)/1000.0d0 -
     & 1.052d0*((xx(1,j)/1000.0d0)**2) +
     & 0.1554d0*((xx(1,j)/1000.0d0)**3))
      dcd(j) = (ee**1.5d0)*(1.9101d0/1000.0d0 -
     & 2.0d0*1.052d0*xx(1,j)/1000.0d0/1000.0d0
     & + 0.1554d0*3.0d0*((xx(1,j)/1000.0d0)**2)/1000.0d0)
      tdkap=dexp((Ebarkap)*(t-298.0d0)/(t*298.0d0))
      cd(j)=cd(j)*tdkap
      dcd(j)=dcd(j)*tdkap
      tm(j) =0.4d0
      dtm(j) = 0.0d0
      dfu(j) = 0.0d0
      d2fu(j) = 0.0d0
      go to 99
c
c &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c LiTFSI in PEO at 85 C, from Ludvig Edman (LBL) July 2000
  103 continue
      df(j) = (ee**1.5d0)*(-6.73d-19*xx(1,j)*xx(1,j)+
     & 4.92d-16*xx(1,j) +5.5d-12)
      ddf(j) = (ee**1.5d0)*(-13.4d-19*xx(1,j)+4.92d-16)
      tdd=dexp((EbarD)*(t-298.0d0)/(t*298.0d0))
      df(j)=df(j)*tdd
      ddf(j)=ddf(j)*tdd
      cd(j)=(ee**1.5d0)*(6.53d-14*(xx(1,j)**3)-5.73d-10*xx(1,j)*
     & xx(1,j) +1.2d-06*xx(1,j)+4.25d-05)
      dcd(j) = (ee**1.5d0)*(19.59d-14*xx(1,j)*xx(1,j)-11.4d-10*xx(1,j)
     & + 1.2d-06)
      tdkap=dexp((Ebarkap)*(t-298.0d0)/(t*298.0d0))
      cd(j)=cd(j)*tdkap
      dcd(j)=dcd(j)*tdkap
      tm(j) = -5.05d-08*xx(1,j)*xx(1,j) + 3.77d-04*xx(1,j)-0.0834d0
      dtm(j) = -10.1d-08*xx(1,j) + 3.77d-04
      dfu(j) = 0.0d0
      d2fu(j) = 0.0d0
      go to 99
c
c &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c 30% KOH in H2O.  Data from Blaine Paxtons masters thesis
c note the unit conversions

  104 continue

      do2(j)=(ee**1.5d0)*1.0d-5 !gas-phase oxygen diffusion coefficient
      ddo2(j)=0.0d0  ! first derivative wrt O2 concentration
      dh2(j)=(ee**1.5d0)*1.0d-5
      ddh2(j)=0.0d0  ! first derivative wrt H2 concentration

      c1=xx(1,j)/1.0d6
      DF(J) = (ee**1.5d0)*(2.8509D-5 -2.9659D-4*c1**0.5D0
     1+1.3768D-2*c1-0.14199D0*c1**1.5D0+0.42661D0*c1**2.0D0)
      DF(J)=DF(J)/1.0d4
      DDF(J) = (-7.4148D-05*C1**(-0.5D0)+1.3768D-2 -0.212985D0
     1*C1**0.5D0+0.85322D0*C1)*(ee**1.5d0)
      DDF(J) = DDF(J)/1.0d10
      tdd=dexp((EbarD)*(t-298.0d0)/(t*298.0d0))
      df(j)=df(j)*tdd
      ddf(j)=ddf(j)*tdd

      CD(J) = 2.325D-02 + 210.95D0*C1 -2.2077D04*C1**2.0D0
     1  +6.2907D05*C1**3.0D0
      CD(J) = CD(J)*ee**1.5D0*1.0d2
      DCD(J) = 210.95D0 -4.4154D04*C1+1.8872D06*C1**2.0D0
      DCD(J) = DCD(J)*ee**1.5D0/1.0d4
      tdkap=dexp((Ebarkap)*(t-298.0d0)/(t*298.0d0))
      cd(j)=cd(j)*tdkap
      dcd(j)=dcd(j)*tdkap


c For an anion involved in the electrode reaction put the 
c transference number of the anion here.  
c For KOH solution, t+=0.23 and t-=0.77
      tm(j)=0.77d0  
      dtm(j)=0.0d0

c U1=f, U2=df/dc (in Paxton units), U3=d2f/dc2 (in Paxton units)

      U1 = 1.004D0 - 36.23D0*C1**0.5d0 + 1374.3D0*C1
     &- 17850.7d0*C1**1.5d0 + 55406.0D0*C1**2.0d0
     &+7.16856D5*C1**2.5d0   
      U2=-18.115*C1**(-0.5d0)+1374.4d0-2.6776d4*C1**0.5d0+110812.0d0*C1
     &+1.7921d6*C1**1.5d0
      U3=9.0575*C1**(-1.5d0)-13388.0d0*C1**(-0.5d0)+110812.0d0+2.6881d6
     &*C1**0.5d0
      dfu(j)=1/U1*U2/1.0d6
      d2fu(j)=(1/U1*U3-U2*U2/(U1**2.0d0))/1.0d12

      go to 99

c &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c add your own
  105 continue
	c1=xx(1,j)/1000.d0
      df(j)=(ee**1.5d0)* 2.58d-10
      ddf(j) = 0.0d0
      cd(j)=(ee**3.3d0)*(1.0793d0+5.007d-3*c1-
     &4.7212d-3*c1**2+1.5094d-3*c1**3-1.6018d-4*c1**4)
      dcd(j)= (ee**3.3d0)*(5.007d-3+2.d0*4.7212d-3*c1+1.5094d-3*
     &3.d0*c1**2-1.6018d-4*4.d0**c1**3)
	dcd(j)=dcd(j)/1000.d0
      tm(j) = 0.2d0
      dtm(j) = 0.0d0
      dfu(j) = 0.0d0
      d2fu(j) = 0.0d0
      go to 99
c &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
   99 continue
c
      return
      end

c***********************************************************************

      subroutine vardc(j,jcount,kk,iflag_conv,mpa)
c     This subrountine allows the calculation of the solid-phase profiles
c     for a variable solid-phase diffusion coefficient.
      implicit real*8(a-h,o-z)
      parameter(maxt=900)
      common /vdc/ aa(1,1),bb(1,1),cc(1,150),dd(1,3),gg(1),xxx(1,1),
     &yy(1,1),hha(20),hhc(20),cssold(220,150,20),css(220,150,20),
     &utz(9,221),dsold(220,150,20),ds(220,150,20),time,
     &nn,nnj,np,mvdc1,mvdc3,lims
      common /calc/ ai(maxt,5),ai2(maxt,5),ts(maxt),h,h1,h2,h3,hcn,
     1hcp,rr,rrmax,cuL,sumpa(5,221),Rad1pa(5),Rad3pa(5),area1pa(5)
     &,area3pa(5),mcL
      common/pindiv/ vf1(5),vf3(5),cot1(9),cot3(9),rs1(9),rs3(9)
      common/const/ fc,r,t,frt,cur,ep3,ep2,pi,ep1,epf3,epf1,
     &epp1,epp2,epp3,shape3,shape1,capp1,capp3,nneg,nprop,npos
      common/gas/ epg1,epg2,epg3
      common/var/ xp(16),xx(17,221),xt(16,221,maxt)
      common/activ/ EbarD,Ebarkap,Ebarka,Ebarkc,Ebarr1,Ebarr3,
     &Ebars1,Ebars3,Ebarks1a,Ebarks1c,Ebarks2a,Ebarks2c,
     &Ebarks3a,Ebarks3c
      common /n/ tmmax,imp,ji,nx,nt,n1,n2,nj,n3,nconv,npa,iSk3,kj3
     &,ii2,ki2,kj,ip2,kp2,ip1,kp1,imb2,kS2,imb1,kS1,iSk1,kj1
     &,iSk2,kj2,imode,kmode
      common/cprop/ sig3,area3,rka3(9),rka3save,ct3(9),dfs3(9),
	1Rad3,cap1,cap3,sig1,area1,rka1(9),rka1save,ct1(9),dfs1(9),
     1Rad1,tw,dfs1save,dfs3save
      dimension ds1d(150),oflx(221),ocsufs(221)
      save

      errlimit2=1.0d-10 !max error for convergence

	iflag_conv=0 !a flag is needed if don't converge in solid phase
	
      do 376 jj=1,nnj
      cc(1,jj)=0.0D0
  376 continue

c *** CONVERGENCE LOOP STARTS HERE ***
      do 112 mm=1,lims  !go up to lims tries on convergence

c *** SAVE OLD VALUES AND DEFINE THE DIFFUSION COEFF ***
      do 114 jj=1,nnj
      if (jcount.eq.1.and.mm.eq.1) then 
      cssold(j,jj,mpa)=css(j,jj,mpa)
      dsold(j,jj,mpa)=ds(j,jj,mpa)
      endif
c For a true variable solid-phase diffusion coefficient put in the 
c functional dependence here
      if (j.le.n1+1) then
        ds(j,jj,mpa)=dfs1(1)
        ds1d(jj)=0.0d0
      elseif (j.ge.n1+n2) then
	  ds(j,jj,mpa)=dfs3(1)
	  ds1d(jj)=0.0d0
      endif
  114 continue

	err2=0.0D0
      do 888 jj=1,nnj
      if (j.le.n1+1) hh=hha(mpa)
      if (j.ge.n1+n2) hh=hhc(mpa)
      dist=hh*(jj-1)
      dp=dist+hh/2.0d0
      dm=dist-hh/2.0d0
      if (jj.eq.1) then  !jj=1 is at r=0
      bb(1,1)=hh**3.0d0/rr/6.0d0+0.25d0*hh*(ds(j,jj,mpa)+
     &ds(j,jj+1,mpa)-(css(j,jj+1,mpa)-css(j,jj,mpa))*ds1d(jj))
      dd(1,1)=-0.25d0*hh*(ds(j,jj,mpa)+ds(j,jj+1,mpa)
     &+(css(j,jj+1,mpa)-css(j,jj,mpa))*ds1d(jj+1))
      gg(1)=-hh**3.0d0*(css(j,jj,mpa)-cssold(j,jj,mpa))/6.0d0
     &/rr+0.25d0*hh*(ds(j,jj+1,mpa)+ds(j,jj,mpa))
     &*(css(j,jj+1,mpa)-css(j,jj,mpa))+0.25d0*hh*(dsold(j,jj+1,mpa)
     &+dsold(j,jj,mpa))*(cssold(j,jj+1,mpa)-cssold(j,jj,mpa))
      elseif (jj.eq.nnj) then !jj=nnj is at particle surface
      aa(1,1)=0.25d0*dm**2.0d0*(-ds(j,jj,mpa)-ds(j,jj-1,mpa)+
     &(css(j,jj,mpa)-css(j,jj-1,mpa))*ds1d(jj-1))/hh
      bb(1,1)=0.5d0*dm**2.0d0*hh/rr+0.25d0*dm**2.0d0*
     &(ds(j,jj,mpa)+ds(j,jj-1,mpa)+(css(j,jj,mpa)-css(j,jj-1,mpa))*
     &ds1d(jj))/hh
      gg(1)=-0.5d0*dm**2.0d0*hh*(css(j,jj,mpa)-cssold(j,jj,mpa))/
     &rr-0.25d0*dm**2.0d0*(ds(j,jj,mpa)+ds(j,jj-1,mpa))*
     &(css(j,jj,mpa)-css(j,jj-1,mpa))/hh-0.25d0*dm**2.0D0*
     &(dsold(j,jj,mpa)+dsold(j,jj-1,mpa))*(cssold(j,jj,mpa)
     &-cssold(j,jj-1,mpa))/hh-((xx(3+mpa+npa,j)+xt(3+mpa+npa,j,kk-1))
     &/2.0d0
     &+(xx(kj2,j)+xt(kj2,j,kk-1))/2.0d0)*dist**2.0D0
      else
      aa(1,1)=0.25d0*dm**2.0d0*(-ds(j,jj,mpa)-ds(j,jj-1,mpa)
     &+(css(j,jj,mpa)-css(j,jj-1,mpa))*ds1d(jj-1))/hh
      bb(1,1)=hh*dist**2.0d0/rr+0.25d0*dm**2.0d0*
     &(ds(j,jj,mpa)+ds(j,jj-1,mpa)+(css(j,jj,mpa)-css(j,jj-1,mpa))*
     &ds1d(jj))/hh+0.25d0*dp**2.0d0*(ds(j,jj,mpa)+ds(j,jj+1,mpa)
     &-(css(j,jj+1,mpa)-css(j,jj,mpa))*ds1d(JJ))/hh
      dd(1,1)=-0.25d0*dp**2.0d0*(ds(j,jj,mpa)+ds(j,jj+1,mpa)
     &+(css(j,jj+1,mpa)-css(j,jj,mpa))*ds1d(JJ+1))/hh
      gg(1)=-hh*dist**2.0d0*(css(j,jj,mpa)-cssold(j,jj,mpa))/rr
     &+0.25d0*(-dm**2.0d0*(ds(j,jj,mpa)+ds(j,jj-1,mpa))*(css(j,jj,mpa)
     &-css(j,jj-1,mpa))+DP**2.0d0*(ds(j,jj+1,mpa)+ds(j,jj,mpa))*
     &(css(j,jj+1,mpa)-css(j,jj,mpa)))/hh
     &+0.25d0*(-dm**2.0d0*(dsold(j,jj,mpa)+dsold(j,jj-1,mpa))*
     &(cssold(j,jj,mpa)-cssold(j,jj-1,mpa))+dp**2.0d0*
     &(dsold(j,jj+1,mpa)+dsold(j,jj,mpa))*(cssold(j,jj+1,mpa)
     &-cssold(j,jj,mpa)))/hh

      endif

      err2=err2+dabs(gg(1))
      call band2(jj)

888   continue !run until hit nnj

      do 322 jj=1,nnj
      css(j,jj,mpa)=css(j,jj,mpa)+cc(1,jj)
  322 continue

c SET SHOE-HORNS FOR THE SOLID CONCENTRATIONS ***
      do l=1,nj
        do ll=1,nnj
          if (l.le.n1+1) then
            if (css(l,ll,mpa).lt.cssold(l,ll,mpa)/1.d2) css(l,ll,mpa)=
     &      cssold(l,ll,mpa)/1.d2
            if (css(l,ll,mpa).gt.ct1(mpa)) css(l,ll,mpa)=
     &      ct1(mpa)*0.999999d0
          elseif (l.ge.n1+n2) then

            if (css(l,ll,mpa).lt.cssold(l,ll,mpa)/1.d2) then
	        css(l,ll,mpa)=cssold(l,ll,mpa)/1.d2
	        print *,'in shoe horn 1',css(l,ll,mpa),cssold(l,ll,mpa),
     &        xx(2+mpa+npa,j),mpa
c decrease time step, restart in main code	
	        iflag_conv=1
	        return
	      endif
	
            if (css(l,ll,mpa).gt.ct3(mpa)) then
	       css(l,ll,mpa)=ct3(mpa)*0.99999d0
	       print *,'in shoe horn 2',xx(2+mpa+npa,j),mpa
c decrease time step, restart in main code	
	       iflag_conv=1
	       return
	      endif

            if ((ct3(mpa)-css(l,ll,mpa)).lt.
     &      (ct3(mpa)-cssold(l,ll,mpa))/1.d2) then
	        css(l,ll,mpa)=ct3(mpa)-100.d0*(ct3(mpa)-cssold(l,ll,mpa))
	        print *,'in shoe horn 3',xx(2+npa+mpa,j)
              ! decrease time step, restart in main code	
	        iflag_conv=1
	        return
	      endif
          endif !position
        enddo !ll
      enddo !l

c *** CHECK FOR CONVERGENCE ***
      err2=err2/nnj

      if (err2.lt.errlimit2) goto 345

  112 continue
      print *,'SOLID PHASE DIFF NOT CONVERGE',kk,j,css(j,nnj,mpa),
     &xx(2+mpa,j)
      stop
  345 continue

      return 
      end

c***********************************************************************

      subroutine band2(j)
      implicit real*8(a-h,o-z)
      common /vdc/ aa(1,1),bb(1,1),cc(1,150),dd(1,3),gg(1),xxx(1,1),
     &yy(1,1),hha(20),hhc(20),cssold(220,150,20),css(220,150,20),
     &utz(9,221),dsold(220,150,20),ds(220,150,20),time,
     &nn,nnj,np,mvdc1,mvdc3,lims
      dimension e(4,5,200)
  101 format (15h determ=0 at j=,i4)
      if (j-2)  1,6,8
    1 np1= nn + 1
      do 2 i=1,nn
      dd(i,2*nn+1)= gg(i)
      do 2 l=1,nn
      lpn= l + nn
    2 dd(i,lpn)= xxx(i,l)
      call matinv2(nn,2*nn+1,determ)
      if (determ)  4,3,4
    3 print 101, j
    4 do 5 k=1,nn
      e(k,np1,1)= dd(k,2*nn+1)
      do 5 l=1,nn
      e(k,l,1)= - dd(k,l)
      lpn= l + nn
    5 xxx(k,l)= - dd(k,lpn)
      return
    6 do 7 i=1,nn
      do 7 k=1,nn
      do 7 l=1,nn
    7 dd(i,k)= dd(i,k) + aa(i,l)*xxx(l,k)
    8 if (j-nnj)  11,9,9
    9 do 10 i=1,nn
      do 10 l=1,nn
      gg(i)= gg(i) - yy(i,l)*e(l,np1,j-2)
      do 10 m=1,nn
   10 aa(i,l)= aa(i,l) + yy(i,m)*e(m,l,j-2)
   11 do 12 i=1,nn
      dd(i,np1)= - gg(i)
      do 12 l=1,nn
      dd(i,np1)= dd(i,np1) + aa(i,l)*e(l,np1,j-1)
      do 12 k=1,nn
   12 bb(i,k)= bb(i,k) + aa(i,l)*e(l,k,j-1)
      call matinv2(nn,np1,determ)
      if (determ)  14,13,14
   13 print 101, j
   14 do 15 k=1,nn
      do 15 m=1,np1
   15 e(k,m,j)= - dd(k,m)
      if (j-nnj)  20,16,16
   16 do 17 k=1,nn
   17 cc(k,j)= e(k,np1,j)
      do 18 jj=2,nnj
      m= nnj - jj + 1
      do 18 k=1,nn
      cc(k,m)= e(k,np1,m)
      do 18 l=1,nn
   18 cc(k,m)= cc(k,m) + e(k,l,m)*cc(l,m+1)
      do 19 l=1,nn
      do 19 k=1,nn
   19 cc(k,1)= cc(k,1) + xxx(k,l)*cc(l,3)
   20 return
      end
      
c***********************************************************************

      subroutine matinv2(nn,m,determ)
      implicit real*8(a-h,o-z)
      common /vdc/ aa(1,1),bb(1,1),cc(1,150),dd(1,3),gg(1),xxx(1,1),
     &yy(1,1),hha(20),hhc(20),cssold(220,150,20),css(220,150,20),
     &utz(9,221),dsold(220,150,20),ds(220,150,20),time,
     &nx,nnj,np,mvdc1,mvdc3,lims
      dimension id(4)
      determ=1.0
      do 1 i=1,nn
   1  id(i)=0
      do 18 nm=1,nn
      bmax=1.1
      do 6 i=1,nn
      if(id(i).ne.0) go to 6
      bnext=0.0
      btry=0.0
      do 5 j=1,nn
      if(id(j).ne.0) go to 5
      if(dabs(bb(i,j)).le.bnext) go to 5
      bnext=dabs(bb(i,j))
      if(bnext.le.btry) go to 5
      bnext=btry
      btry=dabs(bb(i,j))
      jc=j
   5  continue
      if(bnext.ge.bmax*btry) go to 6
      bmax=bnext/btry
      irow=i
      jcol=jc
   6  continue
      if(id(jc).eq.0) go to 8
      determ=0.0
      return
   8  id(jcol)=1
      if(jcol.eq.irow) go to 12
      do 10 j=1,nn
      save=bb(irow,j)
      bb(irow,j)=bb(jcol,j)
  10  bb(jcol,j)=save
      do 11 k=1,m
      save=dd(irow,k)
      dd(irow,k)=dd(jcol,k)
  11  dd(jcol,k)=save
  12  f=1.0/bb(jcol,jcol)
      do 13 j=1,nn
  13  bb(jcol,j)=bb(jcol,j)*f
      do 14 k=1,m
  14  dd(jcol,k)=dd(jcol,k)*f
      do 18 i=1,nn
      if(i.eq.jcol) go to 18
      f=bb(i,jcol)
      do 16 j=1,nn
  16  bb(i,j)=bb(i,j)-f*bb(jcol,j)
      do 17 k=1,m
  17  dd(i,k)=dd(i,k)-f*dd(jcol,k)
  18  continue
      return
      end

c***********************************************************************
c     Exuent...
c***********************************************************************
