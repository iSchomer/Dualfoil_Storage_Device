#Discrepencies between 5.2(Newman) and 5.1
###(Default reference to Newman code)

##Input files

1. added `restart` boolean 
2. removed `cot1, cot3`, coulumbic capacity
3. removed `rs1, rs2`, density of insertion material
4. removed `lflag` and `lpow` flags
5. `nneg, nprop, npos` reference metals have been changed significantly

##Main Code

###Variables
1. added `i2div` in `common /n/`
2. completely altered variables inside `common /pindiv/`
3. deleted xbrug variables in `common /var/`
4. added `common /resistances/`
5. added `newrun` and `restart` logicals
6. many other minor name changes / functional alterations

###Comp Subroutine

####Code unique to Newman

before the main loop:

```fortran
c     The exbrug exponent is being set independently for the separator and
c     positive and negative electrodes as shown in the following three lines...

      exbrug1=1.5d0 !EX for the negative active material.
      exbrug2=1.5d0 !EX for the separator material.
      exbrug3=1.5d0 !EX for the positive active material.
```

and

```fortran
      if(time.eq.0.d0) then
      totLiold=0.d0
      do j=1,nj
      fh=h3*ep3
      if(j.le.n1+n2) fh=h2*ep2
      if(j.le.n1+1) fh=h1*ep1
      if(j.eq.1) fh=0.5*h1*ep1
      if(j.eq.n1+1) fh=0.5d0*(h1*ep1+h2*ep2)
      if(j.eq.n1+n2) fh=0.5d0*(h3*ep3+h2*ep2)
      if(j.eq.nj) fh=0.5d0*h3*ep3
      totLiold=totLiold + fh*c(1,j)
      enddo
      print *, totLiold
      write (3,*) 'totLiold ', totLiold
	endif
      totLio=0.d0
      do j=1,nj
      fh=h3*ep3
      if(j.le.n1+n2) fh=h2*ep2
      if(j.le.n1+1) fh=h1*ep1
      if(j.eq.1) fh=0.5*h1*ep1
      if(j.eq.n1+1) fh=0.5d0*(h1*ep1+h2*ep2)
      if(j.eq.n1+n2) fh=0.5d0*(h3*ep3+h2*ep2)
      if(j.eq.nj) fh=0.5d0*h3*ep3
      totLio=totLio + fh*c(1,j)
      enddo
	print *, totLiold,totLio
```

####Code unique to 5.1

inside of main loop:

```fortran
	do mpa=1,npa
	g(2+mpa)=xt(2+mpa,j,kk-1+kadd)-xx(2+mpa,j) !fix solid concentrations
	b(2+mpa,2+mpa)=1.d0
	enddo !mpa
```

##Equations 

1. 5.1 allows for multiple sizes of variable solid-phase diffusion coefficient (Newman allows only one)
2. Newman Equation 3 (Butler-volmer kinetics) has large blocks of code for pore-wall fluxes that 5.1 lacks
