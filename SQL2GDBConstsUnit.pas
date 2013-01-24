
{*******************************************************}
{                                                       }
{       SQL 2 InterBase Shared Constants Unit           }
{                                                       }
{       This unit holds interesting decalarations.      }
{                                                       }
{*******************************************************}

unit SQL2GDBConstsUnit;

interface

uses
  ConvertInfoUnit;

const
{ ADO property names. }
  SAutoincrementing = 'Autoincrement';

{ Name mangling constants. }  
  SColSuffix = 'Col';
  SUDFTitle = 'UDF: %s - %d';  { Takes library name and number. }
  SGeneratorName = 'G_%s%sGen%d'; { Takes table and field names. }
  SIdentifierPrefix = 'A';
  SInitGenerator = 'SET GENERATOR %s TO %d'; { Takes generator name and value. }
  SKeyName = '%sKey%d'; { Takes table name and key index. }
  STableSuffix = 'Table';
  SReservedWordsMissing = 'Reserved words could not be found in '#13#10'%s'#13#10#13#10'Keyword collissions will not be checked.';
  SDefaultValuesMissing = 'Default values could not be found in '#13#10'%s'#13#10#13#10'Default values will not be applied to null fields.';
  SSourceColumnNameInvalid = 'Source column ''%s'' contains an unaceptable ''='' character.'#13#10#13#10'Please rename this column in the source data before re-running SQL2GDB';

{ Wizard dialog messages. }
  SCannotDeleteAndUse = 'Cannot "Delete existing" and "Use existing". Select one or none.';
  SConfirmRecreateScripts = 'You already have scripts. Do you want to recreate them?';
  SInterBaseFileMissing = 'You must select an InterBase database to continue';
  SNoValidMethod = 'No valid method';
  SOutputFileMissing = 'You must select an output file to continue';
  SQuotesRequireDialect3 = 'Use of quotes requires dialect 3';
  STriggerSkippingImplied = 'Trigger skipping implied by metadata skipping';

{ Provider names. }
  SJetProviderName = 'Jet.OLEDB';
  SSQLServerProviderName = 'SQLOLEDB';

{ Maximum identifier lengths. }
  GenNameMaxLen = 31;
  IndexNameMaxLen = 31;
  TrigNameMaxLen = 31;

{ Library management. }  
  LibraryEndMask = '_setup.sql';
  SIncorrectUDFTermination = 'Incorrect UDF termination: ";" expected';
  SNoLibrariesToSelect = 'There are no libraries to select';

{ Defaults. }
  MapMethodIB56 = [mmUnderscores];

{ Scripting constants. }
  SCharSet = 'CharSet';
  SCreateGUIDDomain = 'CreateGUIDDomain';
  SDefaultValues = 'DefaultValues';
  SDeleteExisting = 'DeleteExisting';
  SIBFileName = 'IBFileName';
  SIBPassword = 'IBPassword';
  SIBUserName = 'IBUserName';
  SInterruptOnErrors = 'InterruptOnErrors';
  SLibraries = 'Libraries';
  SNameMap = 'NameMap';
  SMapMethods = 'MapMethods';
  SMigrateAutoNumber = 'MigrateAutoNumber';
  SOutputFileName = 'OutputFileName';
  SOutputType = 'OutputType';
  SRestrictToTables = 'RestrictToTables';
  SSkipMetadata = 'SkipMetadata';
  SSkipTriggers = 'SkipTriggers';
  SSourceConnectionString = 'SourceConnectionString';
  SUseDialect3 = 'UseDialect3';
  SUseExisting = 'UseExisting';
  
  SWizardInfoStart = '/* WIZARD INFORMATION STARTS HERE';
  SWizardInfoStop = 'WIZARD INFORMATION STOPS HERE */';
  
  SNoWizardStartMarker = 'No wizard start marker found';
  SUnknownWizardMarker = 'Unknown wizard marker in line:'#13#10'%s';

implementation

end.
