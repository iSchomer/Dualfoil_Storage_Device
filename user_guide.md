#Reference Information for Dualfoil 5.1 and 5.2
Note: sections labeled in *italics* are unique to 5.1 code

##Input

Dualfoil reads in all required information from the following two files:
* dualfoil5.in (named "li-ion.in" for 5.2):
  1. includes all the main data read in for simulation, with a discription beside each variable
  2. *includes* `restart`, *the usage of which is described below*
* li-ion-ebar.in which includes data for activation energies


##Output

Dual generates the following output files:
* dualfoil5.out: provides main data such as potential and current for each timestep
* profiles.out: includes detailed profiles accross the length of the cell for each timestep
  +increasing the number of nodes along the length of the cell is controlled with `il2` in main input
  +increasing the number of profiles printed from the timesteps is controlled with `il3` in main input
* resistances.out: tracks resistance within each compartment of the cell over time
* halfcells.out: breaks up potential into `V neg` and `V pos`
* *df_caebat.out: for generating CAEBAT output; four columns of unlabeled values*
* *df_restart.dat and df_sources.dat: used in 5.1 to initialize variables when beginning from a restart*

##Important information for main code

* Variable storage
  + most functional data is stored in the `xx` 2D array for each timestep
  + each instance of `xx` is stored within the 3D array `xt`
* Basic Organization
  + main `comp` subroutine updates `xx` using a temporary array: `c`
  + `cellpot` calculates and writes the data found in dualfoil5.out
  + `nucamb` calculates and writes the data for profiles.out

###The Equations

The main equations are found within the `comp` subroutine. Here is the logic for that section:
  + Its main loop iterates for the number of specified nodes
  + Before solving equations, several 2D arrays (# of equations by # of equations) are initialized
  + Solve each equation, storing the updated values in the temporary arrays
  + Update permanent arrays and repeat until each node has been calculated from left to right

Below is the list of equations; each name corresponds with what it is solving for
  1. Liquid phase concentration
    + uses constant volume approach
  2. Ohm's Law in solution
    + solves for solution potential
  3. Solid phase material balance
  4. Current density
  5. Porewall flux and Butler-Volmer kinetics
  6. Ohm's Law in the matrix
    + solves for matrix potential
  7. Mode converge
  8. Material Balance and kinetics of side reactions 1 and 2
  9. Kinetics only for side reactions 3

###Table of important locations 

| 5.1 Line(s) | 5.2 Line(s) | Description                                                                                        |
|:-----------:|:-----------:|----------------------------------------------------------------------------------------------------|
|  191-316    |  251-368    | Read in input data                                                                                 |
|  __1379__   | __1361__    | `comp` subroutine; solves main equations and updates `xx` values                                   |
|    2832     |   2892      | `calca` sub; calculates diffusion in solid particles                                               |
|    3055     |   3114      | `erfcg` sub; error function compliment                                                             |
|    3097     |   3156      | `band` sub; solves coupled, linear differential equations                                          |
|    3165     |   3224      | `matinv` sub; matrix inversion program for `band`                                                  |
|  __3223__   | __3282__    | `nucamb` sub; calculates and prints detailed profiles                                              |
|    DNE      |   3361      | `peak` sub; calculates peak power per timestep in discharge                                        |
|  __3305__   | __3541__    | `cellpot` sub; calculates and prints main output data per timestep                                 |
|    3526     |   3821      | `sol` sub; calculates solid-phase concentration files                                              |
|    3613     |   3908      | `mass` sub; calculates mass from densities and volume fraction                                     |
|    3657     |   3952      | `temperature` sub; recomputes cell temperature                                                     |
|    3867     |   4167      | `ekin` sub; evaluates Butler-Volmer equations and provides data for pos. and neg. active materials |
|    4595     |   5231      | `prop` sub; creates a library of electrolyte properties                                            |
|    4800     |   5834      | `vardc` sub; unclear utility                                                                       |
|    4970     |   5981      | `band2` sub; unclear purpose distinguishable from `band`                                           |
|    5034     |   6044      | `matinv2` sub; matrix inversion program for `band2`                                                |
| 1046-1369   | 1016-1342   | loop for each simulation step                                                                      |
| 1280-1369   | 1222-1342   | portion of above loop that prepares for new simulation step                                        |
| 1054-1255   | 1020-1200   | portion of loop that iterates through each timestep                                                |
|    1093     |   1128      | portion of loop where profiles are called to be generated                                          |
|  __1163__   | __1161__    | can comment out this block for a constant timestep                                                 |
|    3493     |   3779      | block of code where main output list is printed within `cellpot`                                   |

---

##Running Successful Simulations

###Special cases within the main input 
* Some variables should not be changed without also changing others
  + Ex: in 5.2 when changing insertion materials,  `cot1` and `cot3` must be manually changed to match that material's property
* The last line of input contains the specifications for the run, which are highly dependent on each other
  + the meaning of the first two numbers--`cu(i)` and `tt(i)`--are dependent upon the value of `mc(i)`, as described in the main input file

###*Using the* `restart` *mode*

Basic idea: When beginning a new simulation, `restart` should be set to False in order to initialize properties based on the input data. After this run, any additional runs that are a continuation should have `restart` set to True.

+ Include only the current leg that will be started / continued, 
+ For updating `tt(i)`:
  + When running in terms of time, `tt(i)` will represent desired endtime, not additional length of simulation runtime
  + When runing in terms of cutoff voltage, set `tt(i)` to the new desired cuttoff voltage 
  + If an input file has a two or more legs, `tt(i)` for the second leg and beyond will represent additional length of runtim, not endtime
+ After a simulation, the total runtime can be found as the second number on the first line of `df_restart.dat`  
---

##Problems with running

###Unique to 5.2

* Any attempts to run a multi-leg simulation with similar format to 5.1 have failed
* If multi-leg simulations do work, the required input is less obvious than that of 5.1 
* When running to a cutoff potential, the main output is littered with occasional rows with a time marker of 0.000

###Unique to 5.1

* When running to a cutoff potential, the program usually has to slightly exceed the cutoff before it stops the simulation
  + This is the contributing factor for the timestep difference between straight runs and restarts

###Shared issues

* Running a simulation on constant voltage mode sometimes never finishes because the timesteps seemingly *decrease* rapidly toward 0
  + This has only occured when it is the first leg of a simulation; charging/discharging to a constant voltage works consistently
* Given the same input, 5.1 and 5.2 yield significantly different main output
  + This might be due to differences in the initialization process as well as within `comp`, the main compoutational subroutine
  + Another possible source for the error is `cellpot`, a subroutine tasked with updating and printing the main output data
  + These changes are provided below


---

##Differences between 5.1 and 5.2
Note: change is expressed as change from 5.2 to 5.1

###Input files

* added `restart` boolean to line 1 of 5.1 input
* removed `cot1, cot3`, coulumbic capacity from lines 44-45 of 5.2 input
* removed `rs1, rs2`, density of insertion material form lines 47-48 of 5.2 input
* removed `lflag` and `lpow` flags from lines 62 and 66 of 5.2 input, respectively
* `nneg, nprop, npos` lists of reference metals have been altered and rearranged (5.1 lines 66-68, 5.2 lines 75-77)

###Main Code

####Variables
* added `i2div` in `common /n/`
* completely altered variables inside `common /pindiv/`
* merged 5.2's xbrug variables into a single variable in `common /var/`
* added `common /resistances/`
* added `newrun` and `restart` logicals
* many other minor name changes / functional alterations

####Reading in variables
If a restart is called in 5.1, certain variables are read in instead of initialized (line 463) (see below):

```fortran
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
```

####Timestep loop and Restart
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
Note: only location where file 13 is written to or read from; unclear purpose

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

+ In  process marked 610 within timestep loop, 5.2 calls `comp` 4 times whereas 5.1 calls it only once

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
####After main loop

* 5.1 does not include the peak power optional mode that 5.2 does
  * As a result, the `peak` subroutine does not exist in 5.1

---

###Comp Subroutine

####Code unique to 5.2

before the main subroutine loop:

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

inside of main subroutine loop:

```fortran
	do mpa=1,npa
	g(2+mpa)=xt(2+mpa,j,kk-1+kadd)-xx(2+mpa,j) !fix solid concentrations
	b(2+mpa,2+mpa)=1.d0
	enddo !mpa
```

####Solver Equations 

These are the 6 (sometimes 7) equations that the program solves in order to initialize and update its functional variables

* In equation dealing with material balance in solid insertion metal, 5.1 allows for multiple sizes of variable solid-phase diffusion coefficient (5.2 allows only one)
* 5.2 Equation 3 (Butler-volmer kinetics) has large blocks of code for pore-wall fluxes that 5.1 lacks
