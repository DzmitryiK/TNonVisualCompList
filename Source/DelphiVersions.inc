// to make it easier to find out which version of Delphi we are using
{$IFDEF VER80}
{$DEFINE Delphi1}
{$DEFINE Delphi1_up}
{$ENDIF}

{$IFDEF VER90}
{$DEFINE Delphi2}
{$DEFINE Delphi2_up}
{$ENDIF}

{$IFDEF VER100}
{$DEFINE Delphi3}
{$DEFINE Delphi3_up}
{$ENDIF}

{$IFDEF VER120}
{$DEFINE Delphi4}
{$DEFINE Delphi4_up}
{$ENDIF}

{$IFDEF VER130}
{$DEFINE Delphi5}
{$DEFINE Delphi5_up}
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS}
// Delphi 6 and up support the CompilerVersion constant and the IF conditional define
// This has the advantage that new Delphi versions will automatically be supported by this
// include file. Of course this won't help with fatures newly introduced by these versions

{$IF CompilerVersion >= 14}
{$DEFINE Delphi6}
{$DEFINE Delphi6_up}
{$IFEND}

{$IF CompilerVersion >= 15}
{$DEFINE Delphi7}
{$DEFINE Delphi7_up}
{$IFEND}

// VER160 was Delphi 8 for .NET only

{$IF CompilerVersion >= 17}
{$DEFINE Delphi2005}
{$DEFINE Delphi2005_up}
{$IFEND}

{$IF CompilerVersion >= 18}
{$DEFINE Delphi2006}
{$DEFINE Delphi2006_up}
{$IFEND}

{$IFDEF VER185}
{$DEFINE Delphi2007}
{$DEFINE Delphi2007_up}
{$ENDIF}

{$IF CompilerVersion >= 20}
{$DEFINE Delphi2009}
{$DEFINE Delphi2009_up}
{$IFEND}

{$IF CompilerVersion >= 21}
{$DEFINE Delphi2010}
{$DEFINE Delphi2010_up}
{$IFEND}

{$IF CompilerVersion >= 22}
{$DEFINE DelphiXE}
{$DEFINE DelphiXE_up}
{$IFEND}

{$IF CompilerVersion >= 23}
{$DEFINE DelphiXE2}
{$DEFINE DelphiXE2_up}
{$IFEND}

{$IF CompilerVersion >= 24}
{$DEFINE DelphiXE3}
{$DEFINE DelphiXE3_up}
{$IFEND}

{$IF CompilerVersion >= 25}
{$DEFINE DelphiXE4}
{$DEFINE DelphiXE4_up}
{$IFEND}

{$IF CompilerVersion >= 26}
{$DEFINE DelphiXE5}
{$DEFINE DelphiXE5_up}
{$IFEND}

//{$IFDEF VER275}
//{$DEFINE APPMETHOD}
//{$ENDIF}

{$IF CompilerVersion >= 27}
{$DEFINE DelphiXE6}
{$DEFINE DelphiXE6_up}
{$IFEND}

{$IF CompilerVersion >= 28}
{$DEFINE DelphiXE7}
{$DEFINE DelphiXE7_up}
{$IFEND}

{$IF CompilerVersion >= 29}
{$DEFINE DelphiXE8}
{$DEFINE DelphiXE8_up}
{$IFEND}

// Embarcadero changed the naming scheme again.
// It's Delphi 10 Seattle now
// We assume that they will increment that number
// for now and declare a DelphiXnn define for this.
// Let's see what the next version will be called...
{$IF CompilerVersion >= 30}
{$DEFINE DelphiX10}
{$DEFINE DelphiX10_up}
{$DEFINE DelphiX100}
{$DEFINE DelphiX100_up}
{$IFEND}

// It's Delphi 10.1 Berlin now
{$IF CompilerVersion >= 31}
{$DEFINE DelphiX101}
{$DEFINE DelphiX101_up}
{$IFEND}

{$IF CompilerVersion >= 32}
{$DEFINE DelphiX102}
{$DEFINE DelphiX102_up}
{$IFEND}

{$IF CompilerVersion >= 33}
{$DEFINE DelphiX103}
{$DEFINE DelphiX103_up}
{$IFEND}

{$IF CompilerVersion >= 34}
{$DEFINE DelphiX104}
{$DEFINE DelphiX104_up}
{$IFEND}

// now add DelphiXx_Up constants for all previous versions based on the highest version found

{$IFDEF DelphiX104_up}
{$DEFINE DelphiX103_up}
{$DEFINE DelphiX103up}
{$ENDIF}

{$IFDEF DelphiX103_up}
{$DEFINE DelphiX102_up}
{$DEFINE DelphiX102up}
{$ENDIF}

{$IFDEF DelphiX102_up}
{$DEFINE DelphiX101_up}
{$DEFINE DelphiX101up}
{$ENDIF}

{$IFDEF DelphiX101_up}
{$DEFINE DelphiX10_up}
{$DEFINE DelphiX10up}
{$ENDIF}

{$IFDEF DelphiX10_up}
{$DEFINE DelphiXE8_up}
{$DEFINE DelphiXE8up}
{$ENDIF}

{$IFDEF DelphiXE8_up}
{$DEFINE DelphiXE7_up}
{$DEFINE DelphiXE7up}
{$ENDIF}

{$IFDEF DelphiXE7_up}
{$DEFINE DelphiXE6_up}
{$DEFINE DelphiXE6up}
{$ENDIF}

{$IFDEF DelphiXE6_up}
{$DEFINE DelphiXE5_up}
{$DEFINE DelphiXE5up}
{$ENDIF}

{$IFDEF DelphiXE5_up}
{$DEFINE DelphiXE4_up}
{$DEFINE DelphiXE4up}
{$ENDIF}

{$IFDEF DelphiXE4_up}
{$DEFINE DelphiXE3_up}
{$DEFINE DelphiXE3up}
{$ENDIF}

{$IFDEF DelphiXE3_up}
{$DEFINE DelphiXE2_up}
{$DEFINE DelphiXE2up}
{$ENDIF}

{$IFDEF DelphiXE2_up}
{$DEFINE DelphiXE_up}
{$DEFINE DelphiXEup}
{$ENDIF}

{$IFDEF DelphiXE_up}
{$DEFINE Delphi2010_up}
{$DEFINE Delphi2010up}
{$ENDIF}

{$IFDEF Delphi2010_up}
{$DEFINE Delphi2009_up}
{$DEFINE Delphi2009up}
{$ENDIF}

{$IFDEF Delphi2009_up}
{$DEFINE Delphi2007_up}
{$DEFINE Delphi2007up}
{$ENDIF}

{$IFDEF Delphi2007_up}
{$DEFINE Delphi2006_up}
{$DEFINE Delphi2006up}
{$ENDIF}

{$IFDEF Delphi2006_up}
{$DEFINE Delphi2005_up}
{$DEFINE Delphi2005up}
{$ENDIF}

{$IFDEF Delphi2005_up}
{$DEFINE Delphi7_up}
{$DEFINE Delphi7up}
{$ENDIF}

{$IFDEF Delphi7_up}
{$DEFINE Delphi6_up}
{$DEFINE Delphi6up}
{$ENDIF}

{$IFDEF Delphi6_up}
{$DEFINE Delphi5_up}
{$DEFINE Delphi5up}
{$ENDIF}

{$IFDEF Delphi5_up}
{$DEFINE Delphi4_up}
{$DEFINE Delphi4up}
{$ENDIF}

{$IFDEF Delphi4_up}
{$DEFINE Delphi3_up}
{$DEFINE Delphi3up}
{$ENDIF}

{$IFDEF Delphi3_up}
{$DEFINE Delphi2_up}
{$DEFINE Delphi2up}
{$ENDIF}

{$IFDEF Delphi2_up}
{$DEFINE Delphi1_up}
{$DEFINE Delphi1up}
{$ENDIF}

{$ENDIF CONDITIONALEXPRESSIONS}