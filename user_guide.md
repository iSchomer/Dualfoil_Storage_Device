#Reference Information for Dualfoil 5.1 and 5.2
Note: sections labeled in *italics* are unique to 5.1 code

##Input

Dualfoil reads in all required information from the following two files:
* dualfoil5.in (named "li-ion.in" for 5.2):
  1. includes all the main data read in for simulation, with a discription beside each variable
  2. *includes `restart`, the usage of which is described below*
* li-ion-ebar.in which includes data for activation energies

---

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

---

##Running Successful Simulations

###Special cases within the main input 
* Some variables should not be changed without also changing others
  + Ex: in 5.2 when changing insertion materials,  `cot1` and `cot3` must be manually changed to match that material's property
* The last line of input contains the specifications for the run, which are highly dependent on each other
  + the meaning of the first two numbers (`cu(i)` and `tt(i)`) are dependent upon the value of `mc(i)`, as described in the main input file

###*Using the `restart` mode*

Basic idea: When beginning a new simulation, `restart` should be set to False in order to initialize properties  
  based on the input data. After this run, any additional runs that are a continuation should have `restart` set to True.

1. Restarting at the beginning of a new leg:
  + Keep the last leg that just ran in the input file and include the new leg. Program will only run the new leg
  + When running in terms of time, change the previous `tt(i)` from the duration of that leg to the last recorded timestep
    * Note: When running a multi-leg straight run, each `tt(i)` represents duration for that leg, not the desired endtime
  + Restarting from a cutoff voltage instead of time will slightly throw off the timesteps relative to a multi-leg straight run

2. Restarting within a leg:
  + Include only the current leg that will be continued, updating `tt(i)` to the new desired cutoff/endtime.
  + When running in terms of time, `tt(i)` will represent desired endtime, not additional length of simulation runtime
  + When runing in terms of cutoff voltage, set `tt(i)` to the new desired cuttoff voltage 

3. Determining which type of restart to use:
  + If changing the type of mode through `mc(i)`, restart from a new leg
  + If changing value of `cu(i)`, restart from a new leg
  + Changing the value of `tt(i)` suggests restarting from the same leg __unless__ working with cutoff voltages, and the new  
    desired cutoff voltage would require a shift form charging to discharging, or vice versa

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

1. added `restart` boolean 
2. removed `cot1, cot3`, coulumbic capacity
3. removed `rs1, rs2`, density of insertion material
4. removed `lflag` and `lpow` flags
5. `nneg, nprop, npos` lists of reference metals have been altered and rearranged

###Main Code

####Variables
1. added `i2div` in `common /n/`
2. completely altered variables inside `common /pindiv/`
3. merged 5.2's xbrug variables into a single variable in `common /var/`
4. added `common /resistances/`
5. added `newrun` and `restart` logicals
6. many other minor name changes / functional alterations

####Reading in variables
If a restart is called, certain variables are read in instead of initialized (line 463) (see below):

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
_note: only location where file 13 is written to or read from; unclear purpose_

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

