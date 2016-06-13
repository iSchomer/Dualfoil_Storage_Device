# Notes on DualFoil code

* Variable storage
  + most functional data is stored in the `xx` 2D array
  + information found in profiles.out is not stored in `xx`
* Basic Organization
  + main `comp` subroutine updates `xx` using another array: `c`
  + `cellpot` calculates and writes the data found in dualfoil5.out, but the information is not stored in `xx`
  + `nucamb` calculates and writes the data for profiles.out (not stored in `xx`)
* Discrepencies
  + `comp` is called 6 times during initialization of the functional data

## Table of known variable types within the `xx` array

| `xx(i, *)` | Description                                       |
|-----------:|---------------------------------------------------|
|          1 | Concentration of electrolyte                      |
|          2 | Solution Potential                                |
|          3 | `ct1*csx` and `ct3*csy` for respective electrodes |
|          4 | `ct1*csx` and `ct3*csy` for respective electrodes |
|          5 | Current Density                                   |
|          6 | Transfer current (flux density)                   |
|          7 | Matrix potential                                  |
|          8 | Side reaction 1 matl balance                      |
|          9 | Side reaction 1 kinetics                          |
|         10 | Side reaction 2 kinetics                          |
|         11 | Side reaction 2 matl balance                      |
|         12 | Side reaction 3 kinetics                          |
|         13 | Unknown                                           |
|         14 | Unknown                                           |
|         15 | Unknown                                           |
|         16 | Unknown                                           |
|         17 | Unknown                                           |

_Note_: 3 and 4 are most likely current and its derivative

## Table of important locations 

| Line(s)  | Description                                                                                        |
|:--------:|----------------------------------------------------------------------------------------------------|
|  251-368 | Read in input data                                                                                 |
| __1361__ | `comp` subroutine; solves main equations and updates `xx` values                                   |
|   2892   | `calca` sub; calculates diffusion in solid particles                                               |
|   3114   | `erfcg` sub; error function compliment                                                             |
|   3156   | `band` sub; solves coupled, linear differential equations                                          |
|   3224   | `matinv` sub; matrix inversion program for `band`                                                  |
| __3282__ | `nucamb` sub; calculates and prints detailed profiles                                              |
|   3361   | `peak` sub; calculates peak power per timestep in discharge                                        |
| __3541__ | `cellpot` sub; calculates and prints main output data per timestep                                 |
|   3821   | `sol` sub; calculates solid-phase concentration files                                              |
|   3908   | `mass` sub; calculates mass from densities and volume fraction                                     |
|   3952   | `temperature` sub; recomputes cell temperature                                                     |
|   4167   | `ekin` sub; evaluates Butler-Volmer equations and provides data for pos. and neg. active materials |
|   5231   | `prop` sub; creates a library of electrolyte properties                                            |
|   5834   | `vardc` sub; unclear utility                                                                       |
|   5981   | `band2` sub; unclear purpose distinguishable from `band`                                           |
|   6044   | `matinv2` sub; matrix inversion program for `band2`                                                |
|1016-1342 | loop for each simulation step                                                                      |
|1222-1342 | portion of above loop that prepares for new simulation step                                        |
|1020-1200 | portion of loop that iterates through each timestep                                                |
|   1092   | end of timestep calculation for set mode                                                           |
|   1128   | portion of loop where profiles are called to be generated                                          |
|   3779   | block of code where main output list is printed within `cellpot`                                   |


