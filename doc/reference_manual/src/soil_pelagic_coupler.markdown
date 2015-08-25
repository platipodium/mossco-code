# MOSSCO generic Mediator
# Documentation of the soil pelagic prototype v 1.0

@section Concept
The generic Coupler delivers basic coupler functionality to all modules working with substances.
It's primary function is to identify substances in the given import and export states and then automatically search and copy the fields required by the export state. 

A database is running in the background which contains the names that are considered, as well as the rules for equivalent names according to chosen rulesets.  In each step the coupler calls an empty routine which allows the implementation of user code.  By this the automatic process can be controlled. In the end a report is created, which gives detailed information about the items found in the states, the identified substances as well as the result of the automatic search process.

@subsection How to use it
A good start to use the Coupler is, to implement the coupler with the empty template only, then run the coupler and analyse the report. The first adjustment could then be, to add entries to the database to identify additional substances, use or create additional equivalents and rulesets.
This documentation will explain how to to this within the protoype.

@subsection Mediator Template
The mediator template consists of generic coupler routines which handle the auomation process and of user routines, which allow the execution of individual code. By editing the inventory / required list, the fields within the states or the database during the user-code the automatic process can be controlled.

@subsubsection Coupler Routines 
SetServices (cplComp, rc) 
InitP0 (cplComp, ImportState, ExportState, parentClock, rc) 
InitP1 (cplComp, ImportState, ExportState, externalClock, rc) 
Run (cplComp, ImportState, ExportState, externalClock, rc) 
Finalize (cplComp, ImportState, ExportState, externalClock, rc)

@subsubsection User Routines
Config 
mcpl_InitP0 (cplComp, ImportState, ExportState, parentClock, rc) 
Untouched states
mcpl_InitP1 (cplComp, ImportState, ExportState, externalClock, rc) 
States with inventory list for import and required list for export
mcpl_run_pre_recipe (cplComp, ImportState, ExportState, externalClock, rc) 
States with inventory list for import and required list for export
mcpl_run_pre_log (cplComp, ImportState, ExportState, externalClock, rc) 
States with updated inventory list for import and required list for export
and automatically copied fields
mcpl_finalize (cplComp, ImportState, ExportState, externalClock, rc) 

@subsubsection Controlling the automation
In the config section at the top of the mediator basic settings can be made.
By now this is basically the definition of the rulesets that should be used by the database.
The rulesets allow the individual definition of the valid equivalents for different users.
The database concept and the modifications of the database to control the automation will be described in the following section. However, the rulesets must be listed in a string in the form:     character(len=ESMF_MAXSTR) :: rulesets ="'General', 'Fallback', … "

During the phase 1 init a dba_import and dba_export state are created, which contain attributes for each identified substance in import/export state and are then added to those states to be available in all routines. The attribute values of dba_import tells how often this substance is used, the attribute value of the dba_export tells if the substance was found. 

In the mcpl_InitP1 and mcpl_run_pre_recipe user-code routines this attributes can be edited before the automation is started. By setting the dba_export “found” attribute value of a substance, the substance will be considered as found and will no longer be searched by the automation. Note that if the field is connected manually this should be communicated to the import side as well, by increasing the dba_import “used” attribute by one. Of cause the attributes can also be removed from the dba_import or dba_export states to be ignored by the automation  completely.

In the following run phase the import and export attributes are iteratively read into an array.
In the beginning all export substances with an attribute “found” value of zero are directly, meaning by their name in the export state, searched in the import state.
Subsequently the export state is iteratively looped, to search all export substances by all their possible names received from the database. If a substance is found the export attributes “found” value is set to 1 and the import “used” value is increased by 1. This is repeated until either all substances in the export list are found or there are no more hits. The automation thereby tries to keep the used number of import substances as low as possible.

@subsection Mossco Database Module
The database module allows easy access to the SQLite database for MOSSO purposes.
It contains the Basic SQL Routines, which use the fsqlite module to run database queries, and the substance table routines, which hold mossco specific functions to query specific tables for specific results.

@subsubsection Basic SQL Routines
load_session
finalize_session (hold_con,abort)
sql_select_state (sql,col,columns,search_list,replace_list,dba)

@subsubsection Substance tables routines
get_equivalent_name (equivalent,rulesets,nameout)
get_equivalent_appendix_name (alias,rulesets,nameout)
get_substances_list (dbaout)
get_substance_aliases_list (name, rulesets, dbaout)
get_substance_appendices_list (name, dbaout)
Interface get_substance_appendix_aliases_list
get_substance_appendix_aliases_list_1 (SubstanceName, apdxID, rulesets, dbaout)
get_substance_appendix_aliases_list_2 (SubstanceAppendix, rulesets, dbaout)


@subsection Database
The database contains tables to identify relations between substances.
The primary substance can be connected with equivalents, which are valid within rulesets.
The user can then define the rulesets to be used by the mediator. By the use of the database module the mediator then uses the activated substance-equvalent relations to search substances in the import and export state and copy the related fields to the export state.

@subsubsection Database table structure

tblSubstance
ID
SubstanceName
[DefaultUnit]
Table with primary names of
chemical substances,  e.g.: O_2

tblAppendix
ID
SubstanceID
[Condition]
[Location]
[Unit]
Table with appendix additions to each substance 
e.g.: O_2 [] [], O_2 [] [_at_soil_surface],  O_2 [_upward_flux] [_at_soil_surface]

tblEquivalents
ID
EquivalentName
Table with equivalent names
Bsp: dissolved_oxygen

tblRulesets
ID
RulesetName
Table with rulesets to consider specific equivalents
e.g.: General

tblSubstancesEquivalents
RulesetID
SubstanceID
EquivalentID
[Rule]
Table that connects equivalents to substances for the defined rulesets
e.g.: Ruleset 'General': dissolved_oxygen=O_2

@subsection How to modify the database
In the current prototype the database is stored in the file: 
$MOSSCO_DIR/src/test/mossco.db.
The structure as well as the data is stored in the creation script: $MOSSCO_DIR/src/utilities/scripts/mossco_substances_createTables.sql.

To enter a new substance that can be used by the automation the following things are required:
1. Substance
Primary Substance name in tblSubstances
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("O_2");
2. Appendices
Location and condition of the substance
INSERT INTO "tblAppendix" (Substance_ID,	Location) 
VALUES((SELECT ID FROM tblSubstances 
WHERE SubstanceName="O_2"),"_at_soil_surface");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location) 
VALUES ((SELECT ID FROM tblSubstances 
WHERE SubstanceName="O_2"), "_upward_flux", "_at_soil_surface");	
3. Rulesets
Name of the ruleset that should contain 
(of cause only required if no existing ruleset can be used)
INSERT INTO "tblRulesets" (RulesetName) 
VALUES ("General");
4. Equivalents
Equivalent name for the substance, substance names of the fields in import/export state
INSERT INTO "tblEquivalents" (EquivalentName) 
VALUES ("dissolved_oxygen");
INSERT INTO "tblEquivalents" (EquivalentName) 
VALUES ("oxygen");
5. Connection of Substance and Equivalents
ID of the concerned Substance
ID of the connected Equivalent
Ruleset which activates the connection
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID)
VALUES ( 
 (SELECT ID FROM tblRulesets WHERE RulesetName="General"),
 (SELECT ID FROM tblSubstances WHERE SubstanceName="O_2"),
 (SELECT ID FROM tblEquivalents WHERE EquivalentName="oxygen"));

So if a substance with set appendices and an appropriate ruleset already exist only step 4 and 5 are required to add a new equivalent.

The commands must be added in the sql script, which then must be executed.
This can be done by command:
cd $MOSSCO_DIR/src/test
sqlite3 mossco.db < ../utilities/scripts/mossco_substances_createTables.sql
or with an appropriate SQLite tool, such as sqliteman (sudo apt-get install sqliteman).

@section Future plans
There are a lot of features that did not make it in the prototype, but are to be implemented later on.
Therefore all features were kept in mind during the development of the prototype.
Database Routines
To handle the database with more comfort there are routines planned to edit the database in the desired way. This methods, however, desire the database file to be dumped as plain sql text, so that it can be read by git. Right now changes to the data are made within the creation script, which creates the database with all entries. Thereupon the database file is synced binary with the repository. 

A first workaround would be to configure git to dump the database (see here) and use a tool like SQLite Database Browser (sudo apt-get install sqlitebrowser). 
Automatic Recipe Search
If a substance can not be found by the equivalent name search it may be possible to get it from other substances. Thereby an advanced database should contain formula to connect different substances.
The formula is received from the database and then executed within the mediator.

A further development of the automatic recipe search could loop the possible results for the best result (routing) which fills the most / most relevant fields.
Config
These are some ideas for optional settings that can be implemented in the mediator:
Don't allow multiple use of import substance
/ define a maximum number
Define criteria to abort the process
Ruleset Loops
If the search does not find all substances repeat it with additional rulesets
Priority
Set a priority for specific substances
Always serves those substances first 
(relevant i.e. if the loop number is limited)


