					Notes on marsh erosion
			Written by Tarandeep S. Kalra, Neil K. Ganju, John C. Warner 

Introducing new CPP flags
#WAVE_MARSH_THRUST
#WAVE_MARSH_SEDBEDLOAD

The vegetation test case present in COAWST/Projects/Veg_test has been modified to test the wave effect on marsh boundary and marsh erosion. Details of the test case setup can be found in section 4.3.9 in the COAWST manual. 

Test case has been modified to have the marsh boundary close to the Southern boundary. The marsh extends from the Southern boundary to 2000 meters North (covering 20 grid cells).  

#WAVE_MARSH_THRUST
The presence of marsh is felt through a marsh masking. This is an input to the model. It can get modified if the marsh is considered fully eroded. The thrust on the the marsh is reduced based on the depth of the cell. This is output in variable name “mask_thrust”. The output thrust in the history file is the net thrust added from all the four adjacent cell faces (Thrust_tonelli).

#WAVE_MARSH_SEDBEDLOAD
Only one sediment class has been used in the test case and is a cohesive class of sediment. The marsh  contains sediment bed and the properties of this bed can be modified through a combination of initial files (input netcdf file and sediment.in). When the thrust acts on the marsh boundary, the marsh erosion takes place that leads to accretion of sediment bedload on the adjacent cell face that causes the thrust. This sediment that moves to adjacent cells is available for resuspension. 

COAWST files edited for setup: 
a. ROMS/Nonlinear/Vegetation/marsh_wave_thrust.f
b. ROMS/Nonlinear/Vegetation/marsh_sed_bedload.f
c. ROMS/Nonlinear/Sediment/sed_bedload.f

Other modified files include declarations of some arrays for I/O but that would not alter the algorithmic component of the code. 

 
a. ROMS/Nonlinear/Vegetation/marsh_wave_thrust.f
- Calculated wave thrust based on Nicoletta's routines.
- Can be separately used to compute wave thrust on marsh boundary.  
- Inputs wave climate from coupled SWAN model and water level from ROMS.

Output variable names in history file from this subroutine:
a) marsh_mask 
– Marsh mask is used as an input mask indicating the presence of marsh.
– It can be altered when a certain amount of sediment erodes over marsh (The logic for that is in ROMS/Nonlinear/Sediment/marsh_sed_bedload.f.

b)  mask_thrust                   
– Reduction in the value of thrust that occurs due to the depth of the marsh cell. 


c) Thrust_tonelli
– Thrust values computed from adjacent four cells and added at the cell center of the marsh cell. 

d) Thrust_n, Thrust_s, Thrust_w, Thrust_e
– Thrust at all cell faces from different direction that get transferred to marsh_sed_bedload.f.

b. ROMS/Nonlinear/Vegetation/marsh_sed_bedload.f

Output variable names:
a) Marsh erosion occurs when thrust is acted upon at cell faces. he marsh sediment is available as sediment bed. Thrust obtained at four different cell faces acts upon each cell face. The marsh is assumed to be available as sediment bed. Units of bed mass are kg/m2. Th erosion is dependent on the cohesive properties of the sediment, grid cell size, time step size, and the wave thrust on marsh. 

Pseudo code: 
            Effective_thrust=Wave_thrust-Thrust_crit 
            bed_mass_eroded=Effective_thrust*kfac*grid_cell_width*time_step*bed_mass
            bed_mass_adjacent_cell=bed_mass_adjacent_cell+bed_mass_eroded
         
          -------------------------------------------------------------------------------------------------------------
	 If effective thrust > 0 then use the equation used to update bed mass in the cell from where 	marsh got eroded after the wave thrust on marsh is applied, making sure bedmass is not going 	to zero
	------------------------------------------------------------------------------------------------------------
            If (Eff_thrust>0.0)
              bed_mass_wavethrust=bed_mass_wavethrust-bed_mass_eroded

where

a. kfac - May depend on sediment cohesive properties, Units of kfac have to be kg-m/sec 

b. Crit_thrust – critical thrust over which the marsh starts to erode in kN/m

4. marsh_mask  
– marsh mask = 1 for the marsh cells but when the sedbed mass is half of the original mass, the marsh mask = 0. This rule can be changed.


c. ROMS/Nonlinear/Sediment/sed_bedload.f
If marsh_mask =1 then, the sed_bedload routine does not compute any bed load that can lead to suspension of sediment. If marsh_mask=0, then the bed load mass can be available for resuspension in the water column. This is being achieved through the use of marsh mask in sed_bedload.f. 
 




Outstanding issues:
a) Improve the parameterization of kfac, Crit_thrust in marsh_sed_bedload.
b) Improve the criterion when the marsh is considered to be fully eroded (Currently set to 50% of initial bed mass.   
