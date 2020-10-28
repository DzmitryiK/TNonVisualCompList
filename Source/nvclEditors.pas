unit nvclEditors;

interface

uses SysUtils, Classes, DesignEditors, DesignIntf, ToolsAPI, Windows,
     STREDIT, ComCtrls, Controls, nvclComponent, frmCList;

type
  TNVCompListEditor=class(TComponentEditor)
  //Used to creating custom popupmenu item and handling it and doubleclick
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TNVCompListStringsProperty = class(TStringListProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function CurrentRootComponent: IOTAComponent;
function CurrentFileName: string;
function CurrentEditor: IOTAEditor;
function GetFormEditor: IOTAFormEditor;
function GetFormDesigner: IDesigner;
procedure MarkModified;

procedure Register;

implementation

{$IFDEF Delphi2007_up}
{$REGION 'Designer methods'}
{$ENDIF}		
function OTAMsgServices: IOTAMessageServices;
begin
  Result := (BorlandIDEServices as IOTAMessageServices);
  if Result=nil then
    raise Exception.Create('IOTAMessageServices is not available. Please, contact the developer.');
end;

function QuerySvcs(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean;
begin
  Result := (Instance<>nil)and Supports(Instance, Intf, Inst);
end;

function GetCurrentModule: IOTAModule;
var
  iModuleServices: IOTAModuleServices;
begin
  Result := nil;
  QuerySvcs(BorlandIDEServices, IOTAModuleServices, iModuleServices);
  if iModuleServices<>nil then
    Result := iModuleServices.CurrentModule;
end;

function GetCurrentDFMModule: IOTAModule;
var
  FileName: string;
  iEditor: IOTAEditor;
begin
  FileName := '';
  Result := GetCurrentModule;
  if Result<>nil then
  begin
    iEditor := Result.GetCurrentEditor;
    if iEditor<>nil then
      FileName := iEditor.FileName;
    if not AnsiSameText(ExtractFileExt(FileName), '.DFM') then
    begin
      if (Result.ModuleFileEditors[1] <> nil)
        and (AnsiSameText(ExtractFileExt(Result.ModuleFileEditors[1].FileName), '.DFM')) then
        Result := Result.ModuleFileEditors[1].Module
      else
        Result := nil;
    end;
  end;
end;

function CurrentRootComponent: IOTAComponent;
var
  FIOTAFormEditor: IOTAFormEditor;
  Module: IOTAModule;
begin
  Result := nil;
  Module := GetCurrentDFMModule;
  if Module=nil then
    Exit;
  FIOTAFormEditor := GetFormEditor;
  if FIOTAFormEditor=nil then
    Exit;
  Result := FIOTAFormEditor.GetRootComponent;
end;

function CurrentFileName: string;
var
  iEditor: IOTAEditor;
  md: IOTAModule;
begin
  Result := '';
  md := GetCurrentModule;
  if md<>nil then
  begin
    iEditor := md.GetCurrentEditor;
    if iEditor<>nil then
      Result := iEditor.FileName;
    if not AnsiSameText(ExtractFileExt(Result), '.DFM') then
      Result := '';
  end;
end;

function CurrentEditor: IOTAEditor;
var md: IOTAModule;
begin
  Result := nil;
  md := GetCurrentModule;
  if md<>nil then
  begin
    Result := md.GetCurrentEditor;
  end;
end;

function GetFormEditor: IOTAFormEditor;
var
  i: integer;
  Module: IOTAModule;
begin
  Result := nil;
  Module := GetCurrentDFMModule;
  if not Assigned(Module) then Exit;
  for i := 0 to Module.GetModuleFileCount-1 do
    if Supports(Module.GetModuleFileEditor(i), IOTAFormEditor, Result) then Break;
end;

function GetFormDesigner: IDesigner;
var
  NTAFormEditor: INTAFormEditor;
  FormEditor: IOTAFormEditor;
begin
  FormEditor := GetFormEditor;
  Supports(FormEditor, INTAFormEditor, NTAFormEditor);
  if NTAFormEditor<>nil then
  begin
    Result := NTAFormEditor.GetFormDesigner;
    Exit;
  end;
  Result := nil;
end;

procedure MarkModified;
var
  md: IOTAModule;
begin
  md := GetCurrentDFMModule;
  if MD<>nil then
    md.MarkModified;
end;
{$IFDEF Delphi2007_up}
{$ENDREGION}
{$ENDIF}	

{$IFDEF Delphi2007_up}
{$REGION 'TNVCompListEditor'}
{$ENDIF}		
constructor TNVCompListEditor.Create(AComponent: TComponent;
  ADesigner: IDesigner);
begin
  inherited;
end;

procedure TNVCompListEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    //Click on custom popupmenu item
    0: begin
        if not Assigned(TNonVisualCompList(self.Component).CListFormP) then
          TNonVisualCompList(self.Component).CListFormP := TCListForm.Create(nil);
        if not Assigned(TNonVisualCompList(self.Component).CListFormP) then
          Exit;
        TNonVisualCompList(self.Component).CListFormP.Show;
       end;
  end;
end;

procedure TNVCompListEditor.Edit;
begin
  if not Assigned(TNonVisualCompList(self.Component).CListFormP) then
    TNonVisualCompList(self.Component).CListFormP := TCListForm.Create(nil);
  if not Assigned(TNonVisualCompList(self.Component).CListFormP) then
    Exit;
  TNonVisualCompList(self.Component).CListFormP.Show;
end;

function TNVCompListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := _MenuText;   //Custom popupmenu item
  end;
end;

function TNVCompListEditor.GetVerbCount: Integer;
begin
  Result := 1; //Number of custom popupmenu items
end;
{$IFDEF Delphi2007_up}
{$ENDREGION}
{$ENDIF}	

{$IFDEF Delphi2007_up}
{$REGION 'TNVCompListStringsProperty'}
{$ENDIF}	
procedure TNVCompListStringsProperty.Edit;
var i,j:integer;
    Dlg: TStrEditDlg;
begin
    //Opening default TStrings editor in read only mode
    dlg := EditDialog;
    try
      Dlg.Caption := Dlg.Caption + ' (read only)';
      Dlg.Lines := TStrings(GetOrdValue);
      for i  := 0 to dlg.ControlCount-1 do
      begin
        for j := 0 to TWinControl(dlg.Controls[i]).ControlCount - 1 do
        begin
          if TWinControl(dlg.Controls[i]).controls[j].ClassNameIs('TRichEdit') then
             TRichEdit(TWinControl(dlg.Controls[i]).controls[j]).ReadOnly := true;
        end;
      end;
      dlg.PopupMenu.Items.Clear;
      dlg.CodeWndBtn.Visible := false;
      dlg.HelpButton.Visible := false;
      dlg.OKButton.Left := dlg.CancelButton.Left;
      dlg.CancelButton.Left := dlg.HelpButton.Left;
      Dlg.ShowModal;
    finally
      Dlg.Free;
    end;
end;

function TNVCompListStringsProperty.GetAttributes: TPropertyAttributes;
begin
  // Editing property in editor
  Result := [paDialog];
end;
{$IFDEF Delphi2007_up}
{$ENDREGION}
{$ENDIF}	

{$IFDEF Delphi2007_up}				  
procedure RegisterSplashScreen;
var
  AboutSvcs: IOTAAboutBoxServices;
  ProductImage: HBITMAP;
begin
  ProductImage := LoadBitmap(HInstance, 'TNonVisualCompList');
  if Assigned(SplashScreenServices) then
    SplashScreenServices.AddPluginBitmap(_TCompListExpert_GetName, ProductImage, false,'Free ' );
  if  (BorlandIDEServices <> nil) and
      Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutSvcs) then
    AboutSvcs.AddPluginInfo(_TCompListExpert_GetName, '', ProductImage,False, '','Free');
end;
{$ENDIF}		

procedure Register;
begin
  RegisterComponents('Nonvisual Component List', [TNonVisualCompList]);
  RegisterComponentEditor(TNonVisualCompList, TNVCompListEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TNonVisualCompList, 'Stored', TNVCompListStringsProperty);
end;

{$IFDEF Delphi2007_up}					  
initialization
 RegisterSplashScreen;
{$ENDIF}

end.
