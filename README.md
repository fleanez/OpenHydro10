# OpenHydro10
This is the main repostory for the OpenHydro OpenPLEXOS assembly to be used with Coordinador's monthly schedule dataset <a href="https://www.coordinador.cl" target="_blank">Coordinador.cl</a>. 
This OpenPLEXOS utitily only works with PLEXOS 10 version. For older PLEXOS versions, please check other OpenHydro repositories [here](https://github.com/fleanez).

This utility has been tested in PLEXOS versions P10 R07 and P10 R08.

## Filtration

Linear "Leaks" constraints adjusted to the linear equation based on Laja's initial volume. Fixed Constraints:
 + CIPRESESleaks
 + ELTOROleaks
 + COLBUNleaks

Note: Otherwise filtrations will be null

<a id="laja_section"></a>
## Laja Agreements

Affects the behavoir (dynamically changes the RHS) of the following constraints:
 + Qmin_Saltos
 + Qmin_Tucapel
 + Qmin_Zanartu-Collao

**IMPORTANT**: This automated behavior will only be included if the RHS of the aforementioned constraints are equal to zero. Otherwise, this assembly will skip and leave the constraints unmodified.

## Maule Agreements
Affects the behavoir (dynamically changes the RHS) of the following constraints:
 + ExtractDef_Maule
 + Maule_Irrigation_Deficit
 + Cipreses_IfDeficitMaule
 + Cipreses_IfDeficitMaule_NoDeficitInv_2
 + Cipreses_IfDeficitMaule_IfDeficitInv_1
 + DeficitMaulexEcoRiego
 + Inv_CotaFija_ub
 + Inv_CotaFija_lb
 + HoyaIntermedia
 + Maule_Irrigation_Sup
 + Maule_Irrigation_Low

This assembly also modifies the hydro-balance constraint of the following storages:

+ Invernada_Eco: Introduce Decision Variable "Inflow_MauleLow_Eco_Gen" (as an inflow)
+ MauleLow_Eco_Gen: Introduce Decision Variable "Inflow_MauleLow_Eco_Gen" (as an inflow)
+ MauleLow_Eco_Riego: Introduce Decision Variable "Inflow_MauleLow_Eco_Riego" (as an inflow)
+ MauleSup_Eco_Gen: Introduce Decision Variable "Inflow_MauleSup_Eco_Gen" (as an inflow)
+ MauleSup_Eco_Riego: Introduce Decision Variable "Inflow_MauleSup_Eco_Riego" (as an inflow)

Recent changes also include the precalculation of the moving average of the expected unserved waterloads (shortage) considering levels of RIego fictitious storages. In practical terms, this assembly dynamically changes the RHS of constraint "ShortageDef_CuencaMaule"

## FCF Modification

This assembly is designed to identify FCF custom Constraints and modify in such a way that they stay active **only** in the very last period of the simulation step, including look-ahead period. Procedure is as follows:

1. If current phase is ST Schedule do:
2. Identify the "FCF_" named constraint from the list of active Constraints in model
  1. While is not the last period in step (including look-ahead)
     1. Change RHS of "FCF_" to zero

## Conditions

Several conditions should be met for this OpenPLEXOS assembly to change the execution flow modifications. Some of the most relevant are listed below:

1. **ST Schedule** This assembly is designed to work exclusively for ST Scehdule executions phases.
2. **FCF Constraints**. Constraints must be named with prefix "FCF_"
3. Filtration (storage leakage constraints): Only condition is the right name (case insensitive)
4. Maule Irrigation: Several Variables, Decision Variables and Constraints are required for the assembly to modify the PLEXOS execution flow
5. Laja Irrigation: Modifications are performed only if Laja Irrigation constraints RHS=0. See Laja irrigation constraint names in [Laja](#Laja_section)
6. TEMP constraint: To avoid PLEXOS discarding unused Variables, it is required to be used in "dummy" constraints. Original dataset suggested TEMP_MauleVarActivate, but other Constraints and names can be defined. This assembly will **not** modify these constraints
