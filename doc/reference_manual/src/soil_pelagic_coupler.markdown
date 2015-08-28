# MOSSCO generic Mediator
# Documentation of the soil pelagic prototype v 1.0

@section Concept
The generic Coupler delivers basic coupler functionality to all modules working with substances.
It's primary function is to identify substances in the given import and export states and then automatically search and copy the fields required by the export state. 

A database is running in the background which contains the names that are considered, as well as the rules for equivalent names according to chosen rulesets.  In each step the coupler calls a routine which allows the implementation of user code.  By this the automatic process can be controlled. 
In the end a report is created, which gives detailed information about the items found in the states, the identified substances, as well as the result of the automatic search process.

@subsection How to use it
A good start to use the Coupler is to implement the coupler with the empty template only, then run the coupler and analyse the report. The first adjustment could then be, to add entries to the database to identify additional substances, use or create additional equivalents and rulesets. Further customisation can be done by implementing user code.  This documentation will explain how the processes of the generic mediator are working, can be influenced and can be further developed.

@subsection Mediator Template
The mediator template consists of generic coupler routines which handle the automation process and of user routines which allow the execution of individual code. The working files of the generic routines (ESMF states called 'data base arrays': dba_import, dba_export) are stored within the import and export State and therefore are available for modification within the user routines. The section “Controlling the automation” will explain this in more detail.

@subsubsection Coupler Routines 
@li SetServices (cplComp, rc) 
@li InitP0 (cplComp, ImportState, ExportState, parentClock, rc) 
@li InitP1 (cplComp, ImportState, ExportState, externalClock, rc) 
@li Run (cplComp, ImportState, ExportState, externalClock, rc) 
@li Finalize (cplComp, ImportState, ExportState, externalClock, rc)

@subsubsection User Routines
@li Config 
@li mcpl_InitP0 (cplComp, ImportState, ExportState, parentClock, rc) @n Untouched states
@li mcpl_InitP1 (cplComp, ImportState, ExportState, externalClock, rc) @n Untouched states
@li mcpl_run_pre_recipe (cplComp, ImportState, ExportState, externalClock, rc) @n States with inventory list for import and required list for export
@li mcpl_run_pre_log (cplComp, ImportState, ExportState, externalClock, rc) @n States with updated inventory list for import and required list for export and automatically copied fields
@li mcpl_finalize (cplComp, ImportState, ExportState, externalClock, rc) 


@subsection Database
The database contains tables to identify relations between substances.
The primary substance can be connected with equivalents, which is valid only within a given rulesets. The user can then define the rulesets to be used by the mediator. By the use of the database module the mediator then searches within the activated substance-equvalent relations to find substances in the import and export state and copy the related fields to the export state.

@subsubsection Database table structure

@b tblSubstance
@li ID
@li SubstanceName
Table with primary names of chemical substances 
e.g.: O_2

@b tblAppendix
ID
@li SubstanceID
@li Unit
@li [Condition]
@li [Location]
Table with appendix additions to each substance 
e.g.: O_2 [] [], O_2 [] [_at_soil_surface],  O_2 [_upward_flux] [_at_soil_surface]

@b tblEquivalents
@li ID
@li EquivalentName
Table with equivalent names
e.g.: dissolved_oxygen

@b tblRulesets
@li ID
@li RulesetName
Table with rulesets to consider specific equivalents
e.g.: General

@b tblSubstancesEquivalents
@li RulesetID
@li SubstanceID
@li EquivalentID
@li [Rule]
Table that connects equivalents to substances for the defined rulesets
e.g.: Ruleset 'General': dissolved_oxygen=O_2


@subsection Mossco Database Module
The database module allows easy access to the SQLite database for MOSSO purposes.
It contains general SQL routines, which use the fsqlite module to run database queries, and the substance table routines, which hold mossco specific functions to query specific tables for specific results and return them in a usable form, which at the current state is an 2D character array.

Here is a list of the currently implemented functions. For more information on the single routines please refer to the routine annotations in the reference manual.

@subsubsection Basic SQL Routines
@li load_session
@li finalize_session (hold_con,abort)
@li sql_select_state (sql,col,columns,search_list,replace_list,dba)

@subsubsection Substance tables routines
@li get_equivalent_name (equivalent,rulesets,nameout)
@li get_equivalent_appendix_name (alias,rulesets,nameout)
@li get_substances_list (dbaout)
@li get_substance_aliases_list (name, rulesets, dbaout)
@li get_substance_appendices_list (name, dbaout)
@li Interface get_substance_appendix_aliases_list
@li get_substance_appendix_aliases_list_1 (SubstanceName, apdxID, rulesets, dbaout)
@li get_substance_appendix_aliases_list_2 (SubstanceAppendix, rulesets, dbaout)

@subsubsection Rulesets
The rulesets are used to tell the database which substance-equivalents should be used.
The database functions expect a character argument in the form:
@example character(len=ESMF_MAXSTR) :: active_rulesets = “'General','Fallback'”

To allow more control of the generic routines the mediator stores the rulesets in an array:
@example character(len=ESMF_MAXSTR), dimension(2) :: rulesets ="(/'General  ', 'Fallback '/)

The automation then uses the rulesets in the priority determined by the order in the rulesets array, which means that the substance-equivalents in the first ruleset are all checked before the following rulesets are used and so on. All you need to do is to have an array in the config section (see below) with the rulesets you want to use.


@section Controlling the automation
In the @b <config> section at the top of the mediator basic settings can be made.
By now this is basically the definition of the rulesets that should be used by the database.

Before anything is done by the generic routines the @b <mcpl_InitializeP0> routine allows the execution of user code, e.g. to add fields to the states.

During the @b <phase 1> init a dba_import and dba_export state are created, which contain attributes for each identified substance in import/export state and are then added to those states to be available in all routines. The attribute values of dba_import tells how often this substance is used, the attribute value of the dba_export tells if the substance was found. 

In the @b <mcpl_run_pre_recipe> user-code routines this attributes can be edited before the automation is started. By setting the “found” attribute value of a substance in dba_export, the substance will be considered as found and will no longer be searched by the automation. 
Note that if the field is connected manually this should be communicated to the import side as well, by increasing the “used” attribute of all used substances in dba_import. Of cause the attributes can also be removed from the database arrays (dba) to be ignored by the automation completely.

In the following @b <run phase> the import and export attributes are iteratively read into an array.
In the beginning all export substances with a “found” attribute value of zero are directly, meaning by their name in the export state, searched in the import state. 

Subsequently the export state is iteratively looped, to search all export substances by all their possible names received from the database. If a substance is found the export attributes “found” value is set to 1 and the import “used” value is increased by 1. This is repeated wit increasing “used level”, which means that in the first loop only unused substances are considered and in the last loop all substances are be taken into account. The automation thereby tries to keep the used number of import substances as low as possible. As described above this whole process is repeated until all configured rulesets have been checked.

@subsection Advanced modification
It would also be possible to modify a field as required and add the modified field to the import state in the @b <mcpl_InitP1> user code routine. To make the modification visible a customised name can be used, e.g. by splitting oxygen_at_soil_surface into mod_dissolved_oxygen_01_at_soil_surface and  mod_dissolved_oxygen_02_at_soil_surface. Now the database just needs to be teached the two names mod_dissolved_oxygen_01 and  mod_dissolved_oxygen_02, so it can find the substances and an equivalent needs to be set, so the substances are considered for the automation.
This equivalents can be stored in an individual modification ruleset, e.g. “halved oxygen”, which gets a high priority so it is considered before the general rules are applied.

Example
One working example of the modification process are nutrients used in the soil-pelagic-mediator prototype. The first run showed that nutrients where required and ammonium and nitrate are available, so the user code in mcpl_Run_pre_recipe adds the import substances to get nutrients.

1) Get the data base arrays with the inventory and required list
@example    call ESMF_StateGet(importState, "dba_import", dba_import, rc=localrc)
@example    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
@example       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
@example    call ESMF_StateGet(exportState, "dba_export", dba_export, rc=localrc)
@example    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
@example       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
2) Get the import and export field and add them
@example    call mossco_state_get(exportState,	(/'nutrients_upward_flux_at_soil_surface'/),DINflux,verbose=verbose)
@example    call mossco_state_get(importState, 	(/'mole_concentration_of_nitrate_upward_flux_at_soil_surface'/), &
@example        val1_f2, verbose=verbose)
@example    call mossco_state_get(importState, 	(/'mole_concentration_of_ammonium_upward_flux_at_soil_surface'/), &
@example        val2_f2, verbose=verbose, rc=localrc)
@example    DINflux = val1_f2 + val2_f2
3) Now tell the coupler that the export substance has been found ("found" value=1)
@example    dba_value=1
@example    call ESMF_AttributeSet(state=dba_export, 	name='nutrients_upward_flux_at_soil_surface', value=dba_value, rc=localrc)
@example
4) Now tell the coupler that the two import substances have been used
@example    if (localrc==ESMF_SUCCESS) then
@example      call ESMF_AttributeGet(state=dba_import, 		name='mole_concentration_of_nitrate_upward_flux_at_soil_surface', value=dba_value)
@example      dba_value=dba_value+1
@example      call ESMF_AttributeSet(state=dba_import, 	name='mole_concentration_of_nitrate_upward_flux_at_soil_surface', value=dba_value)
@example
@example      call ESMF_AttributeGet(state=dba_import, 	name='mole_concentration_of_ammonium_upward_flux_at_soil_surface',value=dba_value)
@example        dba_value=dba_value+1
@example      call ESMF_AttributeSet(state=dba_import, 	name='mole_concentration_of_ammonium_upward_flux_at_soil_surface',value=dba_value)
@example    end if
@example
@example    call MOSSCO_CompExit(cplComp, rc=localrc)
@example    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
@example      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

@subsection How to modify the database
In the current prototype the database is stored in the file: 
$MOSSCO_DIR/src/test/mossco.db.
The structure as well as the data is stored in the creation script: $MOSSCO_DIR/src/utilities/scripts/mossco_substances_createTables.sql.

To enter a new substance that can be used by the automation the following things are required:
1. Substance
@li Primary Substance name in tblSubstances
@example INSERT INTO "tblSubstances" (SubstanceName) VALUES ("O_2");
2. Appendices
@li Location and condition of the substance
@example INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) 
@example VALUES((SELECT ID FROM tblSubstances 
@example WHERE SubstanceName="O_2"),"_at_soil_surface", “umol/L”);
@example INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) 
@example VALUES ((SELECT ID FROM tblSubstances 
@example WHERE SubstanceName="O_2"), "_upward_flux", "_at_soil_surface", “umol/L”);	
3. Rulesets
@li Name of the ruleset that should contain @n (of cause only required if no existing ruleset can be used)
@example INSERT INTO "tblRulesets" (RulesetName) 
@example VALUES ("General");
4. Equivalents
@li Equivalent name for the substance, substance names of the fields in import/export state
@example INSERT INTO "tblEquivalents" (EquivalentName) 
@example VALUES ("dissolved_oxygen");
@example INSERT INTO "tblEquivalents" (EquivalentName) 
@example VALUES ("oxygen");
5. Connection of Substance and Equivalents
@li ID of the concerned Substance
@li ID of the connected Equivalent
@li Ruleset which activates the connection
@example INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID)
@example VALUES ( 
@example  (SELECT ID FROM tblRulesets WHERE RulesetName="General"),
@example  (SELECT ID FROM tblSubstances WHERE SubstanceName="O_2"),
@example  (SELECT ID FROM tblEquivalents WHERE EquivalentName="oxygen"));

So if a substance with set appendices and an appropriate ruleset already exist only step 4 and 5 are required to add a new equivalent.

The commands must be added in the sql script, which then must be executed.
This can be done by command:
@example cd $MOSSCO_DIR/src/test
@example sqlite3 mossco.db < ../utilities/scripts/mossco_substances_createTables.sql
or with an appropriate SQLite tool, such as sqliteman (sudo apt-get install sqliteman).



@section Future plans
There are a lot of features that did not make it in the prototype, but are to be implemented later on.
Therefore all features were kept in mind during the development of the prototype.

@subsection Database Routines
To handle the database with more comfort there are routines planned to edit the database in the desired way. This methods, however, desire the database file to be dumped as plain sql text, so that it can be read by git. Right now changes to the data are made within the creation script, which creates the database with all entries. Thereupon the database file is synced binary with the repository. 

A first workaround would be to configure git to dump the database (see here) and use a tool like SQLite Database Browser (sudo apt-get install sqlitebrowser). 

@subsection Units
The unit is not yet considered by the automation but is already available.
The unit is stored in the tblAppendix table, so that multiple instances of one substances with different units can be stored. The unit is retreived from the database in the second column of the dba returned by the routines get_substance_aliases_list and  get_substance_appendix_aliases_list.

@subsection Automatic Recipe Search
If a substance can not be found by the equivalent name search it may be possible to get it from other substances. Thereby an advanced database should contain formula to connect different substances.
The formula is received from the database and then executed within the mediator.

A further development of the automatic recipe search could loop the possible results for the best result (routing) which fills the most / most relevant fields.

@subsection Config
These are some ideas for optional settings that can be implemented in the mediator:
@li Don't allow multiple use of import substance / define a maximum number
@li Define criteria to abort the process
@li Ruleset Loops @n If the search does not find all substances repeat it with additional rulesets
@li Priority @n Set a priority for specific substances @n Always serves those substances first @n (relevant i.e. if the loop number is limited)

