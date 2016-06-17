#Discrepencies between 5.2(Newman) and 5.1
###(Default reference to Newman code)

##Input files

1. added `restart` boolean 
2. removed `cot1, cot3`, coulumbic capacity
3. removed `rs1, rs2`, density of insertion material
4. removed `lflag` and `lpow` flags
5. `nneg, nprop, npos` reference metals have been altered and reordered

##Main Code

###Variables
1. added `i2div` in `common /n/`
2. completely altered variables inside `common /pindiv/`
3. deleted xbrug variables in `common /var/`
4. added `common /resistances/`
5. added `newrun` and `restart` logicals
6. many other minor name changes / functional alterations

###Timestep loop and Restart
+ Before timestep loop, 5.1 performs a unique initialization if it is a restart (shown below):

line 882:

```fortran
      if(restart) then
c     do j=1,nj
c     do i=1,n
c     xt(i,j,k+1)=xx(i,j)
c     enddo
c     enddo
      cssold(:,:,:) = css(:,:,:)
      endif
```

and line 942:

```fortran
      if(restart) then
      cuL = cu(L)
      cuR = cu(L)
      vv=xx(kp1,nj)-xx(kp1,1)
      mcL = 1
!     call cellpot(k,vv,1)
      go to 412 ! skip the initialization of cell for restart
      end if
```

lastly line 1010:

```fortran
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
```

+ In  process marked 610 within timestep loop, Newman calls `comp` 4 times whereas 5.1 calls it only once
+ 5.1 includes code (line 1220) at end of loop that prepares for a possible restart in next simulation (shown below) 

```fortran
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
```

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

####Solver Equations 

1. In equation dealing with material balance in solid insertion metal, 5.1 allows for multiple sizes of variable solid-phase diffusion coefficient (Newman allows only one)
2. Newman Equation 3 (Butler-volmer kinetics) has large blocks of code for pore-wall fluxes that 5.1 lacks
