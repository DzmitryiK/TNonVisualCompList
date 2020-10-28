unit nvclComponent;

interface

uses
  SysUtils, Classes, IniFiles, StrUtils, Windows, Controls, Dialogs, ClipBrd,
  Forms, Messages, {$IFDEF Delphi2007_up}vcl.{$ENDIF}stdctrls;

type

  TNonVisualCompList=class(TComponent)
  private
    { Private declarations }
    FMemIniFile: TMemIniFile;     // Used for easier work with FStored
    FclGroups,                    // List of groups
      FStored: TStringList;       // List of hided components
    FOnAddInGroup: TNotifyEvent;
    FOnSetDesignInfoXY: TNotifyEvent;
    FOnDeleteFromGroup: TNotifyEvent;
    function GetStored: TStrings;
    procedure SetStored(const Value: TStrings);
    procedure SetVisible;
    procedure SetVisibleGroup(const GroupName: string);
    procedure DeleteComponent(const CmpName: string);
    function GetGroupElementCount(Item: string): integer;
    function GetDesignForm: TWinControl;  //Returns components's owner form as TWinControl
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetName(const NewName: TComponentName);override;
  public
    { Public declarations }
    CListFormP: TForm{TDesignWindow};
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadGroups;
    function GetComponentXY(sCompName: string): integer;
    procedure Loaded; override;
    procedure SetDesignInfoXY(c: TComponent; x: integer; isXTop:boolean = false);
    procedure DeleteFromGroup(const GroupName: string; Cmp: TComponent);
    procedure AddInGroup(const GroupName: string; Cmp: TComponent);
    procedure ReadGroup(const GroupName: string; sl: TStrings; fullread:boolean=false);
    procedure WriteGroup(const GroupName: string; sl: TStrings);
    procedure AddGroup(const GroupName: string);
    procedure RenameGroup(OldGroupName, GroupName: string);
    procedure DeleteGroup(const GroupName: string);
    function IsComponentInGroup(GroupName, CompName:string):boolean;
    property Groups: TStringList read FclGroups;
    property OnAddInGroup: TNotifyEvent read FOnAddInGroup write FOnAddInGroup;    // Notifier of component added to group
    property OnSetDesignInfoXY: TNotifyEvent read FOnSetDesignInfoXY write FOnSetDesignInfoXY;   //Notifier of component changed it's coordinates
    property OnDeleteFromGroup: TNotifyEvent read FOnDeleteFromGroup write FOnDeleteFromGroup;  //Notifier of component deleted from group
    property GroupElementCount[Item: string]: integer read GetGroupElementCount;
  published
    { Published declarations }
    property Stored: TStrings read GetStored write SetStored;
  end;

const
  //Used for moving/hiding component
  csNonVisualSize=28;        //Default non-visual component size (width and height) on form
  csNonVisualCaptionSize=14; //Default non-visual component caption's height on form
  csNonVisualCaptionV=30;    //Default offset between non-visual component top and it's caption's top on form
  _Hide=10000;               //defines the default left offset for hiding

  _MenuText='Nonvisual component list';
  _TCompListExpert_GetName='Nonvisual component list';
  _NoGroup='(No group)';
  _HidedGroupName='(Hided)';

function HWndIsNonvisualComponent(wHandle: THandle): Boolean;
function ObjectIsInheritedFromClass(AObj: TObject; const AClassName: string): Boolean;
procedure SetNonVisualPos(Form: TWinControl; Component: TComponent; X, Y: Integer);

implementation

{$IFDEF Delphi2007_up}
{$REGION 'Designer methods'}
{$ENDIF}		

{Return True if wHandle is of TContainer}
function HWndIsNonvisualComponent(wHandle: THandle): Boolean;
const
  NonvisualClassNamePattern='TContainer';
var
  cln: array[0..MAX_PATH] of char;
begin
  GetClassName(wHandle, cln, MAX_PATH);
  Result := string(cln)=NonvisualClassNamePattern;
end;

function ObjectIsInheritedFromClass(AObj: TObject; const AClassName: string): Boolean;
var
  AClass: TClass;
begin
  Result := False;
  AClass := AObj.ClassType;
  while AClass<>nil do
  begin
    if AClass.ClassNameIs(AClassName) then
    begin
      Result := True;
      Exit;
    end;
    AClass := AClass.ClassParent;
  end;
end;

//Changing position of Component on Form
procedure SetNonVisualPos(Form: TWinControl; Component: TComponent; X, Y: Integer);
var
  P: TSmallPoint;
  H1, H2: HWND;
  Offset: TPoint;

  {H1 - Container handle for component
   H2 - Container handle for component's caption
   Offset - x and y for positioning component's caption}
  procedure GetComponentContainerHandle(AForm: TWinControl; L, T: Integer;
    var H1, H2: HWND; var Offset: TPoint);
  var
    R1, R2: TRect;
    P: TPoint;
    ParentHandle: HWND;
    AControl: TWinControl;
    i, hwndI, cI: Integer;
    found:boolean;
  begin
    ParentHandle := AForm.Handle;
    AControl := AForm;

    //Handling TDataModuleForm - something from CnPack, a package where original
    // version of GetComponentContainerHandle was found
    if AForm.ClassNameIs('TDataModuleForm') then
    begin
      for I := 0 to AForm.ControlCount-1 do
        if AForm.Controls[I].ClassNameIs('TComponentContainer')
          and(AForm.Controls[I]is TWinControl) then
        begin
          AControl := AForm.Controls[I]as TWinControl;
          ParentHandle := AControl.Handle;
          Break;
        end;
    end;
    H2 := 0;
    H1 := GetWindow(ParentHandle, GW_CHILD);
    H1 := GetWindow(H1, GW_HWNDLAST);

    hwndI := -1;
    found := false;

    // Getting index including visual and child (like actions or dataset fields) components
    cI := COMPONENT.ComponentIndex;

    // Correcting index - getting component's non-visual z-order position
    for I := 0 to COMPONENT.Owner.ComponentCount - 1 do
      if ((COMPONENT.Owner.Components[i].ClassType.InheritsFrom(TControl)) or (COMPONENT.Owner.Components[i].HasParent))
        and (i < COMPONENT.ComponentIndex) then
          cI := cI - 1;  // removing visual and child components

    while H1<>0 do
    begin
      //Searching for non-visual component's containers and getting their sizes to R1
      if HWndIsNonvisualComponent(H1)and GetWindowRect(H1, R1) then
      begin
        P.x := R1.Left;
        P.y := R1.Top;
        P := AControl.ScreenToClient(P);

        //Getting HWND z-order position according to creation order
        if (R1.Right-R1.Left=csNonVisualSize)and
          (R1.Bottom-R1.Top=csNonVisualSize) then inc(hwndI);

        //Searching for component's TContainer by it's size, position and z-order
        if (P.x=L)and(P.y=T)and(R1.Right-R1.Left=csNonVisualSize)and
          (R1.Bottom-R1.Top=csNonVisualSize) and
          (cI = hwndI) then
        begin
          //Searching for component's caption
          H2 := GetWindow(ParentHandle, GW_CHILD);
          H2 := GetWindow(H2, GW_HWNDLAST);
          hwndI := -1;
          while H2<>0 do
          begin
            //Searching for non-visual component's captions containers and getting their sizes to R2
            if HWndIsNonvisualComponent(H2)and GetWindowRect(H2, R2) then
            begin
              //Getting HWND z-order position according to creation order
              if (R2.Bottom-R2.Top=csNonVisualCaptionSize) then inc(hwndI);

              //Searching for component's caption TContainer by it's size, position and z-order
              if (R2.Top-R1.Top=csNonVisualCaptionV)and
                (Abs(R2.Left+R2.Right-R1.Left-R1.Right)<=1)and
                (R2.Bottom-R2.Top=csNonVisualCaptionSize) and
                (cI = hwndI) then
              begin
                Offset.x := R2.Left-R1.Left;
                Offset.y := R2.Top-R1.Top;
                Break;
              end;
            end;
            H2 := GetWindow(H2, GW_HWNDPREV);
          end;
          Exit;
        end;
      end;

      H1 := GetWindow(H1, GW_HWNDPREV);
    end;

    if not found then
      ShowMessage('Can''t find component '+COMPONENT.Name+' on the form. Please, contact the developer.');

  end;
begin
  if ObjectIsInheritedFromClass(Form, 'TWidgetControl') then  //CLX projects
  begin
    ShowMessage('This type of form (TWidgetControl) is not supported. Please, contact the developer.');
    Exit;
  end;

  //Setting form z-order to default
  TControl(Form).BringToFront;

  P := TSmallPoint(Component.DesignInfo);
  GetComponentContainerHandle(Form, P.x, P.y, H1, H2, Offset);

  //Setting Component's Left and Top in dfm (for saving)
  Component.DesignInfo := Integer(PointToSmallPoint(Point(X, Y)));

  //Setting Component's Left and Top on form visually
  if H1<>0 then
    SetWindowPos(H1, 0, X, Y, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
  //Setting Component's caption Left and Top on form visually
  if H2<>0 then
    SetWindowPos(H2, 0, X+Offset.x, Y+Offset.y, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
end;

       {
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
end;          }

{$IFDEF Delphi2007_up}
{$ENDREGION}
{$ENDIF}	

{$IFDEF Delphi2007_up}
{$REGION 'TNonVisualCompList'}
{$ENDIF}		

procedure TNonVisualCompList.AddInGroup(const GroupName: string; Cmp: TComponent);
var
  l, x: integer;
begin
with FMemIniFile do
  begin
    SetStrings(FStored);
    l := Cmp.DesignInfo;
    x := TSmallPoint(L).x;

    if GroupName = _HidedGroupName then
    begin
      if x=_Hide then
        x := GetComponentXY(Cmp.Name)
      else
        SetDesignInfoXY(Cmp, _Hide);
      WriteString(GroupName, Cmp.Name, Format('%d', [x]));
    end
    else
    begin
      WriteString(GroupName, Cmp.Name, '');
    end;

    FStored.Clear;
    if Assigned(OnAddInGroup) then
      OnAddInGroup(Self);
    GetStrings(FStored);
  end;
end;

procedure TNonVisualCompList.DeleteFromGroup(const GroupName: string; Cmp: TComponent);
var
  s: string;
begin
  with FMemIniFile do
  begin
    SetStrings(FStored);
    s := ReadString(GroupName, Cmp.Name, '');
    DeleteKey(GroupName, Cmp.Name);
    FStored.Clear;
    GetStrings(FStored);
    if GroupName = _HidedGroupName then
    begin
      if GetComponentXY(Cmp.Name)<0 then
        SetDesignInfoXY(Cmp, StrToIntDef(s, 0));
    end;
    if Assigned(OnDeleteFromGroup) then
      OnDeleteFromGroup(Self);
  end;
end;

procedure TNonVisualCompList.DeleteComponent(const CmpName: string);
var
  sSec: TStringList;
  i: integer;
begin
  sSec := TStringList.Create;
  with FMemIniFile do
  begin
    SetStrings(FStored);
    ReadSections(sSec);
    for I := 0 to sSec.Count-1 do
      DeleteKey(sSec[i], CmpName);
    FStored.Clear;
    GetStrings(FStored);
  end;
end;

function TNonVisualCompList.GetDesignForm: TWinControl;
var
  cmp: TComponent;
  i: integer;
begin
  Result := nil;
  cmp := TWinControl(Owner);
  if not Owner.InheritsFrom(TWinControl) then
    while (cmp<>nil)and not cmp.InheritsFrom(TWinControl) do
      cmp := Owner.owner;
  if cmp=nil then
    Exit;
  Result := TWinControl(cmp);

  //Handling TComponentContainer - something from CnPack, a package where original
  // version of GetDesignForm was found
  for I := 0 to TWinControl(cmp).ControlCount-1 do
    if TWinControl(cmp).Controls[I].ClassNameIs('TComponentContainer')and
      (TWinControl(cmp).Controls[I]is TWinControl) then
    begin
      Result := TWinControl(cmp).Controls[I] as TWinControl;
      Break;
    end;
end;

//Changing Left or Top of c to x
procedure TNonVisualCompList.SetDesignInfoXY(c: TComponent; x: integer; isXTop:boolean = false);
var
  n: TSmallPoint;
  AControl: TWinControl;
begin
  AControl := GetDesignForm;
  n := TSmallPoint(c.DesignInfo);
  if isXTop then
    SetNonVisualPos(AControl, c, n.x, x)
  else
    SetNonVisualPos(AControl, c, x, n.y);
  if Assigned(OnSetDesignInfoXY) then
    OnSetDesignInfoXY(Self);
end;

constructor TNonVisualCompList.Create(AOwner: TComponent);
var
  i: integer;
begin
  FMemIniFile := TMemIniFile.Create('');
  with AOwner do
    for I := 0 to ComponentCount-1 do
      if Components[i]is TNonVisualCompList then
        raise Exception.Create('TNonVisualCompList is already on the form!');

  if ObjectIsInheritedFromClass(AOwner, 'TCustomFrame') then
  begin
    raise Exception.Create('You can''t use TNonVisualCompList in frames because '
      +'it can cause errors in forms this frame nested in!');
    Exit;
  end;

  inherited;
  FclGroups := TStringList.Create;
  FStored := TStringList.Create;
end;

destructor TNonVisualCompList.Destroy;
begin
  // If component deleted - return all hided components back on their positions
  if Assigned(Owner) then
  begin
    SetVisible;
  end;

  if Assigned(CListFormP) then
    Freeandnil(CListFormP);

  FclGroups.Free;
  FMemIniFile.Free;
  FStored.Free;
  inherited;
end;

procedure TNonVisualCompList.SetName(const NewName: TComponentName);
begin
  if (Trim(NewName) = '') then
  begin
    raise Exception.Create('TNonVisualCompList: Name can''t be null');
  end;
  inherited;
end;

//Returning sCompName component's Left value from FStored
function TNonVisualCompList.GetComponentXY(sCompName: string): integer;
var
  i, n: integer;
  s: string;
begin
  Result := -1;
  with FStored do
    for i := 0 to Count-1 do
      if AnsiStartsText(sCompName+'=', Strings[i]) then
      begin
        s := Copy(Strings[i], length(sCompName)+2, maxint);
        n := StrToIntDef(s, -1);
        if n<_Hide then
        begin
          Result := n;
          Exit;
        end;
      end;
end;

function TNonVisualCompList.GetGroupElementCount(Item: string): integer;
var
  sl: TStringList;
begin
  result := 0;
  if not FMemIniFile.SectionExists(Item) then
    Exit;
  sl := TStringList.Create;
  try
    FMemIniFile.ReadSection(Item, sl);
    Result := sl.Count;
  finally
    sl.Free;
  end;
end;

function TNonVisualCompList.GetStored: TStrings;
begin
  Result := FStored;
end;

procedure TNonVisualCompList.Loaded;

function CheckClipboard:boolean;
begin
 try
  if (pos('object', Clipboard.AsText) <> 0)
    and (pos('object', Clipboard.AsText) < pos('TNonVisualCompList', Clipboard.AsText))
    and (pos('TNonVisualCompList', Clipboard.AsText) < pos(#13#10, Clipboard.AsText))
    and (pos(#13#10, Clipboard.AsText) < pos('end', Clipboard.AsText))
    and (TForm(Owner).Showing) then
       Result:=true
  else
    Result := false;
 except
  on E : Exception do
  begin
   MessageDlg('TNonVisualCompList, Clipboard error: '+E.Message, mtError, mbOKCancel, 0);
   Result := false;
  end;
 end;
end;

begin
  inherited;
  ReadGroups;

  //Clearing Stored if this component is a copy from another form
  if CheckClipboard then
    FStored.Clear;
end;

procedure TNonVisualCompList.ReadGroup(const GroupName: string; sl: TStrings; fullread:boolean=false);
var
  i: integer;
begin
  with FMemIniFile do
  begin
    SetStrings(FStored);
    sl.Clear;
    if GroupName<>_NoGroup then
    begin
      if fullread then
        ReadSectionValues(GroupName, sl)
      else
        ReadSection(GroupName, sl);
    end
    else
    begin
       for i := 0 to FStored.Count-1 do
        if (Copy(FStored[i], 1, 1)<>'[')and(Trim(FStored[i])<>'') then
          if (FStored.ValueFromIndex[i] = '') then
            sl.Add(FStored.Names[i]);
    end;
  end;
end;

procedure TNonVisualCompList.AddGroup(const GroupName: string);
begin
  with FMemIniFile do
  begin
    Clear;
    SetStrings(FStored);
    WriteString(GroupName, 'xx', '');
    DeleteKey(GroupName, 'xx');
    FStored.Clear;
    GetStrings(FStored);
    ReadGroups;
  end;
end;

procedure TNonVisualCompList.RenameGroup(OldGroupName, GroupName: string);
var i : integer;
begin
  for I := 0 to FStored.Count - 1 do
  begin
    if FStored[i] = Format('[%s]',[OldGroupName]) then
    begin
      FStored[i] := Format('[%s]',[GroupName]);
      break;
    end;
  end;

end;

procedure TNonVisualCompList.DeleteGroup(const GroupName: string);
begin
  SetVisibleGroup(GroupName);
  with FMemIniFile do
  begin
    SetStrings(FStored);
    EraseSection(GroupName);
    FStored.Clear;
    GetStrings(FStored);
    ReadGroups;
  end;
end;

procedure TNonVisualCompList.WriteGroup(const GroupName: string; sl: TStrings);
var
  I: integer;
begin
  with FMemIniFile do
  begin
    SetStrings(FStored);
    EraseSection(GroupName);
    for I := 0 to sl.Count-1 do
      WriteString(GroupName, sl[i], sl[i]);
    FStored.Clear;
    GetStrings(FStored);
  end;
end;

procedure TNonVisualCompList.SetStored(const Value: TStrings);
begin
  FStored.Assign(Value);
  ReadGroups;
end;

{ Sets all hided components visible - turns them back on their old places and clears list of hided components }
procedure TNonVisualCompList.SetVisible;
var
  sSec, sL: TStringList;
  i, j: integer;
  c: TComponent;
begin
  sSec := TStringList.Create;
  sL := TStringList.Create;
  with FMemIniFile do
  try
    SetStrings(FStored);
    ReadSections(sSec);
    for I := 0 to sSec.Count-1 do
    begin
      ReadSection(sSec[i], sL);
      for j := 0 to sL.Count-1 do
      begin
        c := Owner.FindComponent(sL[j]);
        if c<>nil then
        begin
          if (TSmallPoint(C.DesignInfo).x = _Hide) then
            SetDesignInfoXY(c, StrToIntDef(ReadString(sSec[i], sL[j], '0'), 0));
        end
        else
          DeleteKey(sSec[i], sL[j]);
      end;
    end;
    FStored.Clear;
    GetStrings(FStored);
    for I := 0 to Owner.ComponentCount-1 do
      if TSmallPoint(Owner.Components[i].DesignInfo).x=_Hide then
          SetDesignInfoXY(Owner.Components[i], 0);
  finally
    sSec.Free;
    sL.Free;
  end;
end;

procedure TNonVisualCompList.SetVisibleGroup(const GroupName: string);
var
  sL: TStringList;
  j: integer;
  c: TComponent;
begin
  sL := TStringList.Create;
  with FMemIniFile do
  try
    SetStrings(FStored);
    ReadSection(GroupName, sL);
    for j := 0 to sL.Count-1 do
    begin
      c := Owner.FindComponent(sL[j]);
      if c<>nil then
      begin
        if (TSmallPoint(C.DesignInfo).x=_Hide) then
          SetDesignInfoXY(c, StrToIntDef(ReadString(GroupName, sL[j], '0'), 0));
      end
      else
        DeleteKey(GroupName, sL[j]);
    end;
    FStored.Clear;
    GetStrings(FStored);
  finally
    sL.Free;
  end;
end;

function TNonVisualCompList.IsComponentInGroup(GroupName, CompName:string):boolean;
begin
  Result := false;
  with FMemIniFile do
  begin
    SetStrings(FStored);
    if ReadString(GroupName, CompName, '') <> '' then
      Result := true;
  end;
end;

procedure TNonVisualCompList.ReadGroups;
var
  I: integer;
begin
  with FMemIniFile do
    if FStored<>nil then
    begin
      SetStrings(FStored);
      ReadSections(FclGroups);
      //Storing group's element count into objects of FclGroups
      for I := 0 to FclGroups.Count-1 do
        FclGroups.Objects[i] := TObject(GroupElementCount[FclGroups[i]]);
    end;
end;

//Deleting AComponent from Stored when AComponent is deleted
procedure TNonVisualCompList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove)and(AComponent<>Self)
    and (AComponent.name <> '') then
  begin
    DeleteComponent(AComponent.name);
  end;
end;

{$IFDEF Delphi2007_up}
{$ENDREGION}
{$ENDIF}		

end.

