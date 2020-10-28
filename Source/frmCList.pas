unit frmCList;

interface

uses
  SysUtils, Classes, Controls, Forms, StrUtils, Windows, Graphics,
  ExtCtrls, ToolsAPI, StdCtrls, Registry, ComCtrls, ActnList, Dialogs,
  Menus, DesignEditors, DesignIntf, typinfo, ImgList, ToolWin,
  nvclComponent, {$IFDEF Delphi2007_up}System.Actions,{$ENDIF} DesignWindows;

type
  TCListForm=class(TDesignWindow, IDesignNotification)
    ilMain: TImageList;
    lstGroups: TListBox;
    spl1: TSplitter;
    edtSearch: TEdit;
    ilVST: TImageList;
    ilVST_alt: TImageList;
    pmTop: TPopupMenu;
    acToSetInteval1: TMenuItem;
    mniToLeft: TMenuItem;
    mniToTopInterval: TMenuItem;
    N2: TMenuItem;
    pmLeft: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    tlb1: TToolBar;
    btnAddGroup: TToolButton;
    btnDelGroup: TToolButton;
    btn1: TToolButton;
    tbtnVisible: TToolButton;
    btn3: TToolButton;
    btnRefresh: TToolButton;
    tvCList: TTreeView;
    cbeCompFilter: TComboBoxEx;
    pnFilter: TPanel;
    imgFilter: TImage;
    ActionList1: TActionList;
    acAddGroup: TAction;
    acDelGroup: TAction;
    acRefresh: TAction;
    acCListClick: TAction;
    acCListDbClick: TAction;
    acHideComp: TAction;
    acDelFromGroup: TAction;
    tbtnDelFromGroup: TToolButton;
    btnRenameGroup: TToolButton;
    acRenameGroup: TAction;
    acAlignToLeft: TAction;
    tbtnAlignToLeft: TToolButton;
    tbtnAlignToTop: TToolButton;
    acAlignToTop: TAction;
    tbtnDeleteComponent: TToolButton;
    ToolButton4: TToolButton;
    acDeleteCmp: TAction;
    tmrScrollGroups: TTimer;
    tbtnSort: TToolButton;
    acSort: TAction;
    StatusBar1: TStatusBar;
	Panel1: TPanel;		   
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure lstGroupsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lstGroupsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lstGroupsClick(Sender: TObject);
    procedure acAddGroupExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure acDelGroupExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvCListClick(Sender: TObject);
    procedure tvCListDblClick(Sender: TObject);
    procedure tvCListGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure cbeCompFilterSelect(Sender: TObject);
    procedure acCListClickExecute(Sender: TObject);
    procedure acCListDbClickExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure acHideCompExecute(Sender: TObject);
    procedure acDelFromGroupExecute(Sender: TObject);
    procedure spl1Moved(Sender: TObject);
    procedure acRenameGroupExecute(Sender: TObject);
    procedure acAlignToLeftExecute(Sender: TObject);
    procedure acAlignToTopExecute(Sender: TObject);
    procedure acDeleteCmpExecute(Sender: TObject);
    procedure tmrScrollGroupsTimer(Sender: TObject);
    procedure tvCListAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure tvCListAdvancedCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure tvCListCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure acSortExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
  private
    sGroupFilter,
      FFileName, SmallFileName: string;
    slCompName: TStringList;   //storing class names for searching their icons
    NonVisualCompList: TNonVisualCompList;
    RootForm: IOTAComponent;
    RootDesigner: IDesigner;
    RootEditor: IOTAEditor;
    SortA:boolean;
    function FindClassTVNode(const clname: string; n: integer; SearchAll:boolean = false): TTreeNode;
    function IndexOfGroup(const s: string): integer;
    function GetlstGroups_Itemindex: integer;
    procedure SetlstGroups_Itemindex(const Value: integer);
    procedure SetCompEvents;
    procedure LoadBitmapFromPackage;
    procedure SetFileName(const Value: string);
    property lstGroups_Itemindex: integer read GetlstGroups_Itemindex write SetlstGroups_Itemindex;
    procedure SetMarked(Sender: TObject);
    procedure DeleteSelectedTVNodes(Nodes:TTreeNodes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FileName: string read FFileName write SetFileName; //current file name
    procedure FindNonVisualCompList;

//  IDesignNotification
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);  override;
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);  override;
    procedure ItemsModified(const ADesigner: IDesigner); override;
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections); override;
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean); override;
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); override;
  end;

var
  tvCanv: TCanvas; // Used in AdvancedCustomDrawItem

type
  TCListFormClass=class of TCListForm;

  {$IFDEF Delphi2007_up}								  
  TString_Helper=class helper for TStrings
  public
    function IndexOfText(const s: string): integer;
  end;
  {$ENDIF}												   

  TAddTreeNode=class(TTreeNode)
  private
    FAddText: string;
    procedure SetAddText(const S: string);
  public
    //Additional property for TTreeNode for storing
    // components captions or similar properties
    property AddText: string read FAddText write SetAddText;
  end;


implementation
{$R *.dfm}

uses nvclEditors;

constructor TCListForm.Create(AOwner: TComponent);
begin
  inherited;
  sGroupFilter := '';
end;

destructor TCListForm.Destroy;
begin
  inherited;
end;

procedure TCListForm.FormCreate(Sender: TObject);
begin
  RegisterDesignNotification(Self);

  slCompName := TStringList.Create;
  slCompName.Add('TComponent');
  LoadBitmapFromPackage;

  cbeCompFilter.ItemIndex := 0;
  SortA := false;
end;

procedure TCListForm.FormDestroy(Sender: TObject);
begin
  slCompName.Free;
  UnRegisterDesignNotification(Self);
end;

procedure TCListForm.FormShow(Sender: TObject);
begin
  RootForm := CurrentRootComponent;
  RootDesigner := GetFormDesigner;
  FileName := CurrentFileName;
  RootEditor := CurrentEditor;
end;

procedure TCListForm.FormResize(Sender: TObject);
begin
  pnFilter.left := lstGroups.Width+spl1.Width;
end;

//Refreshing TreeView on FormActivate
procedure TCListForm.FormActivate(Sender: TObject);
var
  i, j, nItemIndex, compCount: integer;
  TTV: TTreeNode;
  IC, RootComp: IOTAComponent;
  C : TComponent;
  slGroupFilter, SelectedList: TStringList;
  sCaption: string;

  function GetObjectProperty(Sender: TObject; prop: string; f: integer): string;
  var
    p: pointer;

    function HasProperty(Obj: TObject; Prop: string): PPropInfo;
    begin
      Result := GetPropInfo(Obj.ClassInfo, Prop);
    end;

  begin
    Result := '';
    p := HasProperty(Sender, prop);
    if p<>nil then
    try
      case PPropInfo(p)^.PropType^.Kind of
        tkInteger,
          tkClass: Result := IntToStr(GetOrdProp(Sender, p));
        tkLString,
          tkWString, {$IFDEF Delphi2007_up} tkUString, {$ENDIF}
          tkString: Result := GetStrProp(Sender, p);
        tkEnumeration:
          Result := IntToStr(GETENUMValue(PPropInfo(p)^.PropType^,
            GETENUMName(PPropInfo(p)^.PropType^,
            GetOrdProp(Sender, p))));
        tkFloat: Result := format('%.*f', [f, GetFloatProp(Sender, p)]);
      end;
    except
    end;
  end;

  function GetCaption(C: TComponent): string;
  const
    a: array[0..3] of string=('Caption', 'Text', 'DisplayLabel', 'FieldName');
  var
    I: integer;
  begin
    for I := 0 to High(a) do
    begin
      Result := GetObjectProperty(c, a[i], 0);
      if Result<>'' then Exit;
    end;
  end;

  function GetImageIndex(AComponent: TComponent): integer;
  begin
    Result := 0;
    if AComponent=nil then Exit;

    Result := slCompName.IndexOf(AComponent.ClassName);//slCompName.IndexOfText(AComponent.ClassName);
    if Result>=0 then
      Result := integer(slCompName.Objects[Result])
    else
      Result := 0;
  end;

  //Partially filters types of components in TreeView
  function IncludeType(C: TComponent): Boolean;
  begin
    Result := false;

    if (not c.HasParent)
      and (c.Name <> '') and (cbeCompFilter.ItemIndex = 0)
        then Result := true
    else if (cbeCompFilter.ItemIndex in [1,2]) then
        Result := true;
  end;

  // Adds node in TreeView
  function AddNode(const ClassName, Name, sCaption: string; IC: IOTAComponent): TTreeNode;
  var
    c: tComponent;
    n: integer;
  begin
    if IC=nil then
    begin
      Result := tvCList.Items.Add(nil, sCaption);
      Result.SelectedIndex := Result.ImageIndex;
    end
    else
    begin
      c := IC.GetComponentHandle;
      n := GetImageIndex(C);

      if tvCList.Items.Count = 0 then
      begin
        Result := tvCList.Items.Add(nil, Name);
        Result.ImageIndex := n;
        Result.SelectedIndex := n;
      end
      else
      begin
        if cbeCompFilter.ItemIndex in [1,2] then
          Result := tvCList.Items.AddChild(FindClassTVNode(ClassName, n, true), Name)
        else
          Result := tvCList.Items.AddChild(FindClassTVNode(ClassName, n), Name);

        if (NonVisualCompList <> nil)
          and (NonVisualCompList.IsComponentInGroup(_HidedGroupName, Name)) then
            n := 1;
        Result.ImageIndex := n;
        Result.SelectedIndex := n;
        TAddTreeNode(Result).AddText := sCaption;
      end;// count=0
    end;
  end;

begin
  inherited;
  RootComp := RootForm;
  nItemIndex := 0;
  compCount := 0;

  if (RootComp=nil) then
  begin
    Exit;
  end;

  if AnsiSameText(ExtractFileExt(RootEditor.FileName), '.DFM') then
    FileName := RootEditor.FileName
  else FileName := ' ';

  slGroupFilter := TStringList.Create;
  SelectedList := TStringList.Create;
  try
    Screen.Cursor := crHourGlass;

    //Saving selection by name
    if not Sender.ClassNameIs('TListBox') then
    begin
      for I := 0 to tvCList.SelectionCount - 1 do
      begin
        SelectedList.Add(tvCList.Selections[i].Text);
      end;
    end;

    tvCList.Items.BeginUpdate;
    tvCList.Items.Clear;

    //Adding node for form
    AddNode('', SmallFileName, '', RootComp);

    //Refreshing NonVisualCompList and groups
    nItemIndex := lstGroups_ItemIndex;
    if Sender<>lstGroups then
      lstGroups.Items.Clear;
    NonVisualCompList := nil;
    for I := 0 to RootComp.GetComponentCount-1 do
    begin
      ic := RootComp.GetComponent(i);
      c := ic.GetComponentHandle;
      if c is TNonVisualCompList then
      begin
        NonVisualCompList := TNonVisualCompList(c);
        NonVisualCompList.ReadGroups;
        SetCompEvents;
        if Sender<>lstGroups then
        begin
          lstGroups.Items.Assign(TNonVisualCompList(c).Groups);
          for j := 0 to lstGroups.Items.Count-1 do
          begin
            if CompareText(lstGroups.Items[j], _HidedGroupName) = 0 then
            begin
              lstGroups.Items.Delete(j);  
              break;
            end;
          end;
          lstGroups.Items.Insert(0, '(All)');
          lstGroups.Items.Insert(1, _HidedGroupName);
          lstGroups.Items.Insert(2, _NoGroup);

        end;
        if sGroupFilter<>'' then
          TNonVisualCompList(c).ReadGroup(sGroupFilter, slGroupFilter)
        else
          slGroupFilter.Clear;
        Break;
      end;
    end;

    //Adding components to TreeView according to filters (group,view option,search)
    for I := 0 to RootComp.GetComponentCount-1 do
    begin
      ic := RootComp.GetComponent(i);
      c := ic.GetComponentHandle;
      if (not ic.IsTControl) or (cbeCompFilter.ItemIndex = 2) then
      begin
        edtSearch.Text := Trim(edtSearch.Text);
        sCaption := GetCaption(c);
        if not (c is TNonVisualCompList) then
          if (C<>nil)and (C.Name <> '') and
             //filtering by search
             ((edtSearch.Text='')or
               AnsiContainsText(c.Name, edtSearch.Text)or
               AnsiContainsText(sCaption, edtSearch.Text)
             )and
             //filtering by group
             ((sGroupFilter='')or
              (slGroupFilter.IndexOf(C.Name)>=0)and(sGroupFilter<>_NoGroup)or
              (slGroupFilter.IndexOf(C.Name)<0)and(sGroupFilter=_NoGroup)
             )and
             //other filters
             IncludeType(C) then
          begin
            if C.HasParent
              and (not C.InheritsFrom(TControl))
              and (C.GetParentComponent.Name <> SmallFileName)
              and (cbeCompFilter.ItemIndex in [1,2]) then
              if ((cbeCompFilter.ItemIndex = 1) and (not C.GetParentComponent.InheritsFrom(TControl)))
                or (cbeCompFilter.ItemIndex = 2) then
                //Adding >2 level nodes (for example, dataset fields)
                TTV := AddNode(C.GetParentComponent.Name, C.Name, sCaption, IC)
              else
                TTV := AddNode(C.ClassName, C.Name, sCaption, IC)
            else
              TTV := AddNode(C.ClassName, C.Name, sCaption, IC);

            //Adding link to component
            if c.name <> '' then TTV.Data := Pointer(i);

            tvCList.Items[TTV.Parent.index].Expanded := True;
            inc(compCount);
          end;
      end;
    end;
  finally
    if nItemIndex<lstGroups.count then
    begin
      if nItemIndex<0 then
        nItemIndex := 0;
      lstGroups.ItemIndex := nItemIndex;
    end;
    slGroupFilter.Free;

    StatusBar1.Panels[0].Text := 'Components: '+IntToStr(compCount);

    tvCList.Items.EndUpdate;
    tvCList.FullExpand;

    //Restoring selection by name
    if SelectedList.Count > 0 then
      for I := 0 to tvCList.Items.Count - 1 do
        if SelectedList.IndexOf(tvCList.Items[i].Text) >= 0 then
        begin
          tvCList.Select(tvCList.Items[i],[ssCtrl]);
        end;

    if SortA then
      tvCList.AlphaSort(true);

    Screen.Cursor := crDefault;
  end;
end;

procedure TCListForm.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
var
  IOTAEdir: IOTAFormEditor;
  IOTAComp: IOTAComponent;
  IOTADes :  IDesigner;

  CanHideComp, DefaultEnabled:boolean;
  i:integer;
begin
  IOTAEdir := GetFormEditor;
  IOTAComp := CurrentRootComponent;
  IOTADes := GetFormDesigner;

  i:=0;CanHideComp:=false;
  while not (i = Integer(tvCList.SelectionCount)) do
  begin
    if (tvCList.Selections[i].Level = 2) then
    begin
      CanHideComp:=true;
      break;
      inc(i);
    end;
    inc(i);
  end;

  DefaultEnabled := (IOTAEdir<>nil)and(IOTAComp<>nil)and(IOTADes<>nil)and(NonVisualCompList<>nil);

  acHideComp.Enabled := DefaultEnabled and CanHideComp and (cbeCompFilter.ItemIndex = 0);
  acClistClick.Enabled := DefaultEnabled;
  acCListDbClick.Enabled := DefaultEnabled;
  lstGroups.Enabled := DefaultEnabled and (cbeCompFilter.ItemIndex = 0);
  acAddGroup.Enabled := (NonVisualCompList<>nil) and (cbeCompFilter.ItemIndex = 0);
  acDelGroup.Enabled := DefaultEnabled and(lstGroups.ItemIndex>2);
  acRenameGroup.Enabled := DefaultEnabled and(lstGroups.ItemIndex>2);
  acDelFromGroup.Enabled := DefaultEnabled and(lstGroups.ItemIndex>2);
  pnFilter.Visible := Trim(edtSearch.Text)<>'';
  acAlignToLeft.Enabled := acHideComp.Enabled and (cbeCompFilter.ItemIndex = 0);
  acAlignToTop.Enabled := acHideComp.Enabled and (cbeCompFilter.ItemIndex = 0);
  acDeleteCmp.Enabled := acHideComp.Enabled and (tvCList.SelectionCount = 1) and (cbeCompFilter.ItemIndex = 0);
end;

procedure TCListForm.acAddGroupExecute(Sender: TObject);
var
  Value: string;
  n: integer;
begin
  if NonVisualCompList=nil then Exit;
  Value := '';
  if InputQuery('New group', 'Name', Value) then
  begin
    Value := Trim(Value);
    if (Value = '') then
    begin
      MessageDlg('Can''t create a group with empty name. Creation cancelled.',
        mtError, [mbOk], 0);
      exit;
    end;

    if (Value = _HidedGroupName) or (Value = _NoGroup)
      or (Value = '(All)') then
    begin
      MessageDlg('Name "'+Value+'" is reserved by component. Creation cancelled.',
        mtError, [mbOk], 0);
      exit;
    end;

    if NonVisualCompList.Groups.IndexOf(Value) >= 0 then
    begin
      MessageDlg('Group named "'+Value+'" already exists.',
        mtError, [mbOk], 0);
      exit;
    end;

    n := IndexOfGroup(Value);
    if n>=0 then
      lstGroups_Itemindex := n
    else
    begin
      NonVisualCompList.AddGroup(Value);
      FormActivate(Sender);
      MarkModified;
    end;
  end;
end;

procedure TCListForm.acRenameGroupExecute(Sender: TObject);
var
  Value: string;
begin
  if NonVisualCompList=nil then Exit;
  Value := '';

  if InputQuery(Format('Rename group %s to', [lstGroups.Items[lstGroups.ItemIndex]]),
      'Name', Value) then
  begin
    Value := Trim(Value);
    if (Value = '') then
    begin
      MessageDlg('Can''t set group name empty. Renaiming cancelled.',
        mtError, [mbOk], 0);
      exit;
    end;

    if (Value = _HidedGroupName) or (Value = _NoGroup)
      or (Value = '(All)') then
    begin
      MessageDlg('Name '+Value+' is reserved by component. Renaiming cancelled.',
        mtError, [mbOk], 0);
      exit;
    end;

    NonVisualCompList.RenameGroup(lstGroups.Items[lstGroups.ItemIndex], Value);
   // FormActivate(Sender);
    if Value<>sGroupFilter then
    begin
      sGroupFilter := Value;
      FormActivate(Sender);
    end;
    MarkModified;

  end;
end;

//Deletes group and moving group component back on their positions
procedure TCListForm.acDelGroupExecute(Sender: TObject);
begin
  if MessageDlg(Format('Delete group %s?', [lstGroups.Items[lstGroups.ItemIndex]]),
    mtConfirmation, [mbYes, mbNo], 0)=mrYes then
  begin
    NonVisualCompList.DeleteGroup(lstGroups.Items[lstGroups.ItemIndex]);
    lstGroups_Itemindex := lstGroups.ItemIndex - 1;
    FormActivate(Sender);
    MarkModified;
  end;
end;

procedure TCListForm.acHideCompExecute(Sender: TObject);
var i:integer;
    cmp: TComponent;
begin
  i := 0;
  while not (i = Integer(tvCList.SelectionCount)) do
  begin
    if (tvCList.Selections[i].Level = 2) then
    begin
      Cmp := RootForm.GetComponent(Integer(tvCList.selections[i].Data)).GetComponentHandle;
      if (tvCList.Selections[i].ImageIndex = 1) then
        NonVisualCompList.DeleteFromGroup(_HidedGroupName, Cmp)
      else
        NonVisualCompList.AddInGroup(_HidedGroupName, Cmp);
      tvCList.Selections[i].ImageIndex := 1;
      tvCList.Selections[i].SelectedIndex := 1;
    end;
    inc(i);
  end;

  self.BringToFront;
end;

procedure TCListForm.acDelFromGroupExecute(Sender: TObject);
var ind: integer;
    cmp: TComponent;
begin
  try
    for ind := 0 to tvCList.SelectionCount - 1 do
    begin
      if tvCList.selections[ind].Level = 2 then
      begin
        Cmp := RootForm.GetComponent(Integer(tvCList.selections[ind].Data)).GetComponentHandle;

        if Cmp <> nil then
            NonVisualCompList.DeleteFromGroup(lstGroups.Items[lstGroups.Itemindex], Cmp);
      end;
    end;
  finally
    FormActivate(Sender);
  end;
end;

procedure TCListForm.acSortExecute(Sender: TObject);
begin
  if SortA then
  begin
    SortA := false;
    tvCList.SortType := stNone;
    acSort.Hint := 'Sorted by creation order (click to sort alphabetically)';
  end
  else
  begin
    SortA := true;
    tvCList.SortType := stText;
    acSort.Hint := 'Sorted alphabetically (click to sort by creation order)';
  end;

  FormActivate(Sender);
end;

procedure TCListForm.acRefreshExecute(Sender: TObject);
begin
  FormActivate(Sender);
end;

procedure TCListForm.acAlignToLeftExecute(Sender: TObject);
var
  Designer: IDesigner;
  i, SLeft : integer;
  FSelectionList: IDesignerSelections;
  cmp: TComponent;

  function CheckHided:boolean;
  var j:integer;
  begin
    Result := false;
    for j := 0 to tvCList.SelectionCount - 1 do
      begin
        if tvCList.Selections[j].ImageIndex = 1 then
        begin
          Result := true;
          break;
        end;
      end;
  end;
begin
  if NonVisualCompList=nil then Exit;

  if not CheckHided then
  begin
    Designer := RootDesigner;
    FSelectionList := TDesignerSelections.Create;
    try
      Designer.GetSelections(FSelectionList);
      with FSelectionList do
      begin
          cmp := TComponent(Items[0]);
          SLeft := TSmallPoint(cmp.DesignInfo).x;
          for I := 1 to Count-1 do
            if not Items[i].InheritsFrom(TControl) then
            begin
              cmp := TComponent(Items[i]);
              NonVisualCompList.SetDesignInfoXY(Cmp, SLeft);
            end;// if not TControl
      end;
    finally
      FSelectionList := nil;
    end;
  end
  else
    MessageDlg('Can''t align hided components. Deselect them and try again',
        mtError, [mbOk], 0);
end;

procedure TCListForm.acAlignToTopExecute(Sender: TObject);
var
  Designer: IDesigner;
  i, STop : integer;
  FSelectionList: IDesignerSelections;
  cmp: TComponent;

  function CheckHided:boolean;
  var j:integer;
  begin
    Result := false;
    for j := 0 to tvCList.SelectionCount - 1 do
      begin
        if tvCList.Selections[j].ImageIndex = 1 then
        begin
          Result := true;
          break;
        end;
      end;
  end;
begin
  if NonVisualCompList=nil then Exit;

  if not CheckHided then
  begin
    Designer := RootDesigner;
    FSelectionList := TDesignerSelections.Create;
    try
      Designer.GetSelections(FSelectionList);
      with FSelectionList do
      begin
          cmp := TComponent(Items[0]);
          STop := TSmallPoint(cmp.DesignInfo).y;
          for I := 1 to Count-1 do
            if not Items[i].InheritsFrom(TControl) then
            begin
              cmp := TComponent(Items[i]);
              NonVisualCompList.SetDesignInfoXY(Cmp, STop, true);
            end;// if not TControl
      end;
    finally
      FSelectionList := nil;
    end;
  end
  else
    MessageDlg('Can''t align hided components. Deselect them and try again',
        mtError, [mbOk], 0);
end;

procedure TCListForm.acDeleteCmpExecute(Sender: TObject);
begin
  if MessageDlg(Format('Delete component %s?', [tvCList.Selected.Text]),
    mtConfirmation, [mbYes, mbNo], 0)=mrYes then
  begin
      RootForm.GetComponent(Integer(tvCList.Selected.Data)).Delete;
      FormActivate(Sender);
  end;
end;

procedure TCListForm.acCListClickExecute(Sender: TObject);
  var i,f:integer;
      FSelectionList, FSelectionList2: IDesignerSelections;
      ANode:TTreeNode;

  function CountSelectedComponents:integer;
  var j:integer;
  begin
    Result := 0;
    for j := 0 to tvCList.SelectionCount - 1 do
    begin
      if tvCList.Selections[j].Level = 2 then
        inc(Result);
    end;
  end;

  function IsCmpIntvSelected(CmpName:string):boolean;
  var ind:integer;
  begin
    Result := false;
    for ind := 0 to tvCList.SelectionCount - 1 do
    begin
      if (tvCList.Selections[ind].Level = 2)
        and (tvCList.Selections[ind].Text = CmpName) then
      begin
        Result := true;
        break;
      end;
    end;
  end;

  function IsCmpInSelections(CmpName:string; FSelList: IDesignerSelections):boolean;
  var ind:integer;
  begin
    Result := false;
    for ind := 0 to FSelList.Count-1 do
      if (not FSelList.Items[ind].InheritsFrom(TControl))
        and (Tcomponent(FSelList.Items[ind]).Name = CmpName) then
      begin
        Result := true;
        break;
      end;
  end;

begin
  if (tvCList.Selected.Level = 0)  then
  begin
    // if form selected - clear selection and select form
    RootDesigner.Activate;
    RootForm.Select(false);
    tvCList.ClearSelection(false);
    Application.ProcessMessages;
    tvCList.Items[0].Selected := true;
  end
  else if (tvCList.Selected.Level = 1) and (GetKeyState(VK_MENU) < 0)  then
  begin
    //Selecting all component of class when ALt pressed
    RootDesigner.Activate;
    tvCList.ClearSelection(true);
    Application.ProcessMessages;
    ANode := tvCList.Selections[0].GetFirstChild;
    RootForm.GetComponent(Integer(ANode.Data)).Select(false);
    while ANode <> nil do
    begin
      tvCList.Select(ANode, [ssCtrl]);
      RootForm.GetComponent(Integer(ANode.Data)).Select(true);
      ANode := ANode.getNextSibling;
    end;
  end
  else if (tvCList.Selected.Level = 2)  then
  begin
    RootDesigner.Activate;
    Application.ProcessMessages;
    if CountSelectedComponents > 1 then
    begin
        if (GetKeyState(VK_SHIFT) >= 0) and (GetKeyState(VK_CONTROL) >= 0)
          and (GetKeyState(VK_LBUTTON) >= 0) then
        begin
          //not multiselecting - drop selection and select current item
          tvCList.ClearSelection(true);
          Application.ProcessMessages;
          if (tvCList.Selected.Level = 2) then
            RootForm.GetComponent(Integer(tvCList.Selected.Data)).Select(false);
        end
        else
        if (GetKeyState(VK_CONTROL) < 0) then
        begin
          //Ctrl handling
          Application.ProcessMessages;
          if (tvCList.Selected.Level = 2) then
            if (tvCList.Selected.Selected) then
              //Adding to selection
              RootForm.GetComponent(Integer(tvCList.Selected.Data)).Select(true)
            else
            begin
              //Removing from selection
              FSelectionList := TDesignerSelections.Create;
              FSelectionList2 := TDesignerSelections.Create;
              try
                RootDesigner.GetSelections(FSelectionList);
                with FSelectionList do
                  for i := 0 to Count-1 do
                    if (not Items[i].InheritsFrom(TControl)) and
                        (TComponent(Items[i]).Name <> tvCList.Selected.Text) then
                    begin
                      FSelectionList2.Add(Items[i]);
                    end;
                RootDesigner.SetSelections(FSelectionList2);
              finally
                FSelectionList := nil;
                FSelectionList2 := nil;
              end;
            end;
        end  //if (GetKeyState(VK_CONTROL)
        else
        if (GetKeyState(VK_SHIFT) < 0) then
        begin
          //Shift handling
          Application.ProcessMessages;
          if (tvCList.Selected.Level = 2) then
          begin
            FSelectionList := TDesignerSelections.Create;
            FSelectionList2 := TDesignerSelections.Create;
            try
              RootDesigner.GetSelections(FSelectionList);

                //Adding exisitng selections, that selected in tv
                for i := 0 to FSelectionList.Count-1 do
                begin
                  if (not FSelectionList.Items[i].InheritsFrom(TControl)) then
                    if IsCmpIntvSelected(TComponent(FSelectionList.Items[i]).Name) then
                      FSelectionList2.Add(FSelectionList.Items[i]);
                end;

                //Adding additional selected in tv
                for f := 0 to tvCList.SelectionCount - 1 do
                begin
                  if (tvCList.Selections[f].Level = 2) then
                    if not IsCmpInSelections(tvCList.Selections[f].Text, FSelectionList2) then
                      FSelectionList2.Add(
                        RootForm.GetComponent(Integer(tvCList.Selections[f].Data)).GetComponentHandle);
                end;
              RootDesigner.SetSelections(FSelectionList2);
            finally
              FSelectionList := nil;
              FSelectionList2 := nil;
            end;
          end;
        end;  //if (GetKeyState(VK_SHIFT)
    end
    else
      RootForm.GetComponent(Integer(tvCList.Selected.Data)).Select(false);
  end
  else if (tvCList.Selected.Level > 2) and (Integer(tvCList.Selected.Data) <> 0)  then
  begin
    tvCList.ClearSelection(true);
    Application.ProcessMessages;
    RootForm.GetComponent(Integer(tvCList.Selected.Data)).Select(false);
  end;
end;

procedure TCListForm.acCListDbClickExecute(Sender: TObject);
var ice: TComponentEditor;
  EditorClass: TComponentEditorClass;
  cmp: TComponent;
  Designer: IDesigner;
  IC:IOTAComponent;
begin
  //Searching for selected component editor and executing it's Edit method
  if tvCList.Selected.Level in [2,3] then
    IC:= RootForm.GetComponent(Integer(tvCList.Selected.Data));

  if tvCList.SelectionCount > 1 then
  begin
    tvCList.ClearSelection(true);
  end;

  if ic <> nil then
  begin
    Designer := RootDesigner;
    cmp := TComponent(ic.GetComponentHandle);
    ice := Pointer(Integer(GetComponentEditor(cmp, Designer))-20);
    if (ice<>nil) and (Designer <> nil) then
    begin
      EditorClass := TComponentEditorClass(Ice.ClassType);
      with EditorClass.Create(Cmp, Designer)as TComponentEditor do
      try
        Edit;
      finally
        Free;
      end;
    end;
  end;
end;

procedure TCListForm.tvCListAdvancedCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  // Used in AdvancedCustomDrawItem
  // Assigning here, because if assign in AdvancedCustomDrawItem - items will scale their font (bug?)
  tvCanv := Sender.Canvas;
end;

procedure TCListForm.tvCListAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var txtrect : TRect;
begin
  //Printing components captions or similar properties
  if (Node.Level in [0,2,3]) then
  begin
    txtRect := Node.DisplayRect(true);
    txtRect.Left := txtRect.Left +  txtRect.Right - txtRect.Left + 10;
    txtRect.Top := txtRect.Top + 1;

    tvCanv.Font.Style := tvCanv.Font.Style + [fsItalic];
    tvCanv.TextOut(txtRect.Left, txtRect.Top, TAddTreeNode(Node).AddText);
    tvCanv.Font.Style := tvCanv.Font.Style - [fsItalic];
  end
  else
  begin
    //D7 bug - when not doing that nodes with other levels will be with larger font
    tvCanv.Font.Style := tvCanv.Font.Style + [fsItalic];
    tvCanv.Font.Style := tvCanv.Font.Style - [fsItalic];
  end;
end;

procedure TCListForm.tvCListClick(Sender: TObject);
begin
  acClistClickExecute(acClistClick);
end;

procedure TCListForm.tvCListCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TAddTreeNode;
end;

procedure TCListForm.tvCListDblClick(Sender: TObject);
begin
  acClistDbClickExecute(acClistDbClick);
end;

procedure TCListForm.tvCListGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  inherited;
  if Node.Level = 0 then   //Form icon
  begin
    Node.ImageIndex := ilVST.count-1;
    Node.SelectedIndex := ilVST.count-1;
  end;
end;

procedure TCListForm.lstGroupsClick(Sender: TObject);
var
  s: string;
begin
  s := '';
  if lstGroups.ItemIndex>0 then
    s := lstGroups.Items[lstGroups.ItemIndex];

  if lstGroups.ItemIndex = 0 then
    cbeCompFilter.Enabled := true
  else
    cbeCompFilter.Enabled := false;

  if s<>sGroupFilter then
  begin
    sGroupFilter := s;
    FormActivate(lstGroups);
  end;
end;

procedure TCListForm.lstGroupsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  n, ind, NewGrPos: integer;
  cmp: TComponent;
  grSL, newStored:TStrings;

  procedure AddCmp(cmp: TComponent);
  begin
    if cmp<>nil then
      if (n=2) then
      begin
        with TListBox(Sender) do
          NonVisualCompList.DeleteFromGroup(Items[lstGroups.Itemindex], Cmp);
      end
      else
      begin
        if (n > 2) and (lstGroups.Itemindex > 2) then
         with TListBox(Sender) do
          NonVisualCompList.DeleteFromGroup(Items[lstGroups.Itemindex], Cmp);
        NonVisualCompList.AddInGroup(TListBox(Sender).Items[n], Cmp);
      end;
  end;

begin
  if Source is TTreeView then
  begin
    n := TListBox(Sender).ItemAtPos(Point(x, y), true);
    if (n = 0) then Exit;

    for ind := 0 to tvCList.SelectionCount - 1 do
    begin
      if not ((n = 1) and (tvCList.selections[ind].ImageIndex = 1)) then
      begin
        if tvCList.selections[ind].Level = 2 then
        begin
          Cmp := RootForm.GetComponent(Integer(tvCList.selections[ind].Data)).GetComponentHandle;
          AddCmp(Cmp);

          //After moving component NonVisualCompList is set to nil
          FindNonVisualCompList;
        end;
      end;
    end;
    if (lstGroups.Itemindex<>0) then
        DeleteSelectedTVNodes(tvclist.Items);

    FormActivate(Sender);
  end;

  if Source is TListBox then
  begin
    with Sender as TListBox do
    begin
      NewGrPos := ItemAtPos(Point(X, Y), False);;
      Items.Move(ItemIndex, NewGrPos);

      //Resaving Stored according to new order
      grSL := TStringList.Create;
      newStored := TStringList.Create;
      for n := 0 to Count - 1 do
      begin
        if not (n in [0,2]) then
        begin
          NonVisualCompList.ReadGroup(Items[n],grSL, true);
          if (grSL.Count > 0) or (Items[n] <> _HidedGroupName)  then
            newStored.Add('['+Items[n]+']');
          for Ind := 0 to grSL.Count - 1 do
          begin
            newStored.Add(grSL[ind]);
          end;
          grSL.Clear;
        end;
      end;
      NonVisualCompList.Stored := newStored;

      //Selecting
      ItemIndex := NewGrPos;
    end;
  end;
end;

procedure TCListForm.lstGroupsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  n: integer;
begin
  Accept := (Source is TTreeView) and TControl(Sender).Enabled;
  if Accept then
  begin
    // for dragging components
    n := TListBox(Sender).ItemAtPos(Point(x, y), true);
    with TTreeView(Source).Selected do
      Accept := (n>0)and(Level>1)and
        not((n=2)and(TListBox(Sender).ItemIndex=0));

    tmrScrollGroups.Enabled := False;
  end
  else
  begin
    // for moving groups
    Accept := (Sender = Source)
      and (TListBox(Sender).ItemAtPos(Point(x, y), true) > 2)
      and (lstGroups.ItemIndex > 2);

    tmrScrollGroups.Enabled := True
  end;
end;

procedure TCListForm.tmrScrollGroupsTimer(Sender: TObject);
var
  Pt: TPoint;
begin
  if not lstGroups.Dragging then begin
    tmrScrollGroups.Enabled := False;
    Exit;
  end;

  Pt := lstGroups.ScreenToClient(Mouse.CursorPos);
  if Pt.Y < 0 then
    lstGroups.TopIndex := lstGroups.TopIndex - 1
  else
    if Pt.Y > lstGroups.ClientHeight then
      lstGroups.TopIndex := lstGroups.TopIndex + 1
    else
      tmrScrollGroups.Enabled := False;
end;

//Returns ListBox item index by name. If not - returns -1
function TCListForm.IndexOfGroup(const s: string): integer;
var
  i: integer;
begin
  Result := -1;
  with lstGroups do
    for I := 0 to Items.Count-1 do
      if AnsiSameText(lstGroups.Items.Text, s) then
      begin
        Result := i;
        Exit;
      end;
end;

procedure TCListForm.SetlstGroups_Itemindex(const Value: integer);
begin
  lstGroups.Itemindex := Value;
  lstGroupsClick(lstGroups);
end;

function TCListForm.GetlstGroups_Itemindex: integer;
begin
  Result := lstGroups.Itemindex;
end;

procedure TCListForm.spl1Moved(Sender: TObject);
begin
  pnFilter.left := lstGroups.Width+spl1.Width;
end;

procedure TCListForm.DeleteSelectedTVNodes(Nodes:TTreeNodes);
begin
  while not (tvCList.SelectionCount = 0) do
  begin
    tvCList.Selections[0].Delete;
  end;
end;

procedure TCListForm.cbeCompFilterSelect(Sender: TObject);
begin
  lstGroups.ItemIndex := 0;
  FormActivate(Sender);
  tvCList.SetFocus;
end;

procedure TCListForm.edtSearchChange(Sender: TObject);
begin
  FormActivate(Sender);
end;

// Marks form modified
procedure TCListForm.SetMarked(Sender: TObject);
begin
  MarkModified;
end;

// Searching for this component on current form
procedure TCListForm.FindNonVisualCompList;
var
  i: integer;
  RootComp: IOTAComponent;
  c: TComponent;
begin
  RootComp := RootForm;
  for I := 0 to RootComp.GetComponentCount-1 do
    begin
      c := RootComp.GetComponent(i).GetComponentHandle;
      if c is TNonVisualCompList then
      begin
        NonVisualCompList := TNonVisualCompList(c);
        SetCompEvents;
        Break;
      end;
    end;
end;

procedure TCListForm.SetFileName(const Value: string);
begin
  FFileName := Value;
  SmallFileName := ExtractFileName(Value);
end;

//Searching for TreeView item on 1st level(class names) by name. If not found - creates one
function TCListForm.FindClassTVNode(const clname: string; n: integer; SearchAll:boolean = false): TTreeNode;
var
  R, Rt: TTreeNode;
begin
  Result := nil;
  with tvCList.Items do
  begin
    Rt := tvCList.Items[0]; //tvCList.TopItem;  //TopItem is buggy, when many items can return not first item
    R := Rt.GetFirstChild;
    while R<>nil do
    begin
      if AnsiSameText(R.Text, clname) then
      begin
        Result := R;
        Break;
      end;
      R := R.GetNextSibling;
    end;

    if SearchAll then
    begin
      R := tvCList.TopItem;
      while R<>nil do
      begin
        if AnsiSameText(R.Text, clname) then
        begin
          Result := R;
          Break;
        end;
        R := R.GetNext;
      end;
    end;

    if Result=nil then
    begin
      Result := tvCList.Items.AddChild(Rt, clname);
      Result.ImageIndex := n;
      Result.SelectedIndex := n;
    end;
    tvCList.Items[Result.Parent.index].Expanded := True;
  end;
end;

// Setting form marked edited when one of the events occurs
procedure TCListForm.SetCompEvents();
begin
  if NonVisualCompList=nil then Exit;
  with NonVisualCompList do
  begin
    OnAddInGroup := SetMarked;
    OnSetDesignInfoXY := SetMarked;
    OnDeleteFromGroup := SetMarked;
  end;
end;

procedure TCListForm.LoadBitmapFromPackage;
var
  n, i, j: integer;
  PackageHandle: Thandle;
  Bitmap, bmp16: TBitmap;
			  
  s: string;
  ps: IOTAPackageServices;
  CL: TClass;
  ISL: TSTringList;
  imALL: TImageList;

  //Redrawing icon background(color MaskColor) to NewMaskColor
  procedure ChangeBMPbackground(abmp:TBitmap; MaskColor, NewMaskColor:Integer);
    var y, x:integer;
    scanline: PRGBTriple;
    Color: Longint; 
    r, g, b: Byte;
  begin
    abmp.PixelFormat := pf24bit ;
    Color := ColorToRGB(MaskColor);
    r := Color;
    g := Color shr 8;
    b := Color shr 16;
    for y := 0 to abmp.Height - 1 do
    begin
      scanline := abmp.ScanLine[y];
      for x := 0 to abmp.Width - 1 do
      begin
        with scanline^ do
        begin
          if (rgbtBlue = b) and (rgbtGreen = g) and (rgbtRed = r) then
            FillChar(scanline^, sizeof(TRGBTriple), NewMaskColor);
        end;
        inc(scanline);
      end;
    end;
  end;

  //Getting all resource names from hModule to list
  function EnumResNameProc(hModule: THandle; lpszType: PAnsiChar; lpszName: Pointer;
            list: TStrings): BOOL; stdcall;
  var k:integer;
      ss:string;
  begin
    ss := PChar(lpszName);
    k := list.IndexOf(ss);//slCompName.IndexOfText(s);
    if k<0 then
    begin
      list.Add(PChar(lpszName));
    end;
    Result := True;
  end;

begin
  ps := BorlandIDEServices as IOTAPackageServices;

  ISL := TStringList.Create;  

  imALL := TImageList.Create(self);
  imALL.Width := 16; imALL.Height := 16;
  imALL.AllocBy := 4;

  //Getting all bitmaps from all packages for coponents into temporary imagelist
  for i := 0 to Pred(ps.PackageCount) do
    if ps.ComponentCount[i]>0 then
    begin
      PackageHandle := GetModuleHandle(PChar(Uppercase(ps.PackageNames[i]{$IF CompilerVersion <= 18}+'.bpl'{$IFEND})));
      if PackageHandle<>0 then
      begin
        n := ISL.count;
        //Getting all Bitmap names from package
        EnumResourceNames(PackageHandle,RT_BITMAP,@EnumResNameProc,Integer(ISL));
        for j := n to ISL.count -1 do
        begin
          Bitmap := TBitmap.Create;
          bmp16 := TBitmap.Create;
          bmp16.Width := 16; bmp16.height := 16;

          //Loading icon from package, changing BG to white, stretching to 16x16,
          // adding into temp ImageList
          Bitmap.LoadFromResourceName(PackageHandle, ISL[j]);

          ChangeBMPbackground(Bitmap, Bitmap.Canvas.Pixels[0, Bitmap.Height-1], clWhite);
          bmp16.PixelFormat:=pf24bit;
          SetStretchBltMode(bmp16.Canvas.Handle,HALFTONE);
          StretchBlt(bmp16.Canvas.Handle,
                     0, 0,
                     16, 16,
                     Bitmap.Canvas.Handle,
                     0, 0,
                     Bitmap.Width, Bitmap.Height,
                     SRCCOPY );
          //bmp16.SaveToFile('C:\icons\'+ISL[j]+'.bmp');
          imALL.Add(bmp16, nil);

          Bitmap.Free;
          bmp16.Free;
        end;
      end;
    end;

  //Selecting images for components and loading into ilVST
  for i := 0 to Pred(ps.PackageCount) do
    if ps.ComponentCount[i]>0 then
    begin
      PackageHandle := GetModuleHandle(PChar(Uppercase(ps.PackageNames[i]{$IF CompilerVersion <= 18}+'.bpl'{$IFEND})));
      if PackageHandle<>0 then
        for j := 0 to Pred(ps.ComponentCount[i]) do
        begin
          CL := Getclass(ps.ComponentNames[i, j]);
          if (cl<>nil) then
          begin
            s := ps.ComponentNames[i, j];

            bmp16 := TBitmap.Create;
            bmp16.Width := 16;
            bmp16.height := 16;
            while True do
            try
              n := slCompName.IndexOf(s);//slCompName.IndexOfText(s);
              //if class is already loaded - breaking
              if n>=0 then
              begin
                slCompName.AddObject(ps.ComponentNames[i, j], slCompName.Objects[n]);
                Break;
              end;

              //Sometimes components use icons with not the same names as component
              //For now only one exclusion found
              if s = 'TIDMESSAGEDECODERMIME' then
                s := 'TIDMESSAGEDECODER';

              //Searching icon for component, adding into ilVST
              n := ISL.IndexOf(s);
              if n < 0 then break;
              imAll.GetBitmap(n, bmp16);

										  
						  
												 
							   
              ilVST.AddMasked(bmp16, bmp16.Canvas.Pixels[0, bmp16.Height-1]);
              slCompName.AddObject(ps.ComponentNames[i, j], TObject(ilVST.Count-1));
              Break;
            except
              on E: EResNotFound do
              begin
                cl := CL.ClassParent;
                if CL=nil then
                  Break
                else
                  s := CL.ClassName;
              end
            else
              Break;
            end;
         //   Bitmap.Free;
            bmp16.Free;
          end;
        end;
    end;
  ilVST.AddImages(ilVST_alt);
end;


{$IFDEF Delphi2007_up}
{$REGION 'IDesignNotification'}
{$ENDIF}					  

procedure TCListForm.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
begin
   //Closes form when designer closed or changed (for example, to DFM view)
   Close;
end;

procedure TCListForm.DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
begin

end;

procedure TCListForm.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin
 //Clears NonVisualCompList when component deleted
 if AItem=NonVisualCompList then
 begin
   NonVisualCompList:=nil;
 end;
 inherited;
end;

procedure TCListForm.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
begin

end;

procedure TCListForm.ItemsModified(const ADesigner: IDesigner);
begin
  if (NonVisualCompList=nil)or(ADesigner=nil) then Exit;
end;

procedure TCListForm.SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
begin

end;

{$IFDEF Delphi2007_up}
{$ENDREGION}
{$ENDIF}					  

{ TStringList_Helper }
// Search for index of specific string in TStringList, currently not used
{$IFDEF Delphi2007_up}					  
function TString_Helper.IndexOfText(const s: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if AnsiSameText(Trim(s), Strings[i]) then
    begin
      Result := i;
      Exit;
    end;
end;
{$ENDIF}		

{ TAddTreeNode }
procedure TAddTreeNode.SetAddText(const S: string);
begin
  if not Deleting and (S <> Text) then
  begin
    FAddText := S;
  end;
end;

end.

