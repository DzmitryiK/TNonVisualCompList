# TNonVisualCompList
Delphi VCL component to view, edit, move and hide components on VCL form.

## Usage
In order to use the component, you should install TNonVisualCompList package. Then choose the component from the palette and drop it on the form.
Double-click on the component or right-click and choosing the **Nonvisual component list** will open editor window.

## Main functions
### Editor
Editor window consists of a tree with all nonvisual components placed on the form, groups box, search bar and buttons bar.
By clicking on the component you can select it and edit it's properties in default IDE Object Inspector. Tree supports multiselection - when you select several components the IDE will select them too.

### Hiding components
One of the main functions of this component is hiding nonvisual components. The mechanism of it is very simple - component Left property setting to 10000 when original Left value is saving in Stored property.

To hide certain components, you need to select the component (or several of them) and press **Hide component** button (icon with a crossed eye). The component will be hided, and its icon will be changed to eye with a warning mark.
To make it visible again - repeat operation to the hided component.

You can also move the component to default Hided group (which stored all hided components) by drag&dropping it to group.

### Grouping components
The component allows grouping components by creating groups and moving components in them by drag&dropping. 

You can create, rename and delete a group by using corresponding buttons above the groups box. You can move groups by drag&dropping them in the groups box.
You can also delete a component from group by clicking on "Delete from group" button.

There are three default groups: 
- All - contains all nonvisual components of the form
- Hided - contains all hided nonvisual components of the form
- No group - contains all nonvisual components, that are not in a group (excluding Hided)
These groups can't be edited or removed.

### Aligning components
The component allows aligning components by Left and Top properties.
Select several components (the first selected will be an example to align others) and press **Align to Left** or **Align to Top**.

### Other functions
The tree with components is sorted by creation order by default. You can sort components alphabetically by pressing **Sort** button.

There is also a dropbox with a following view options:
- Nonvisual with hiding - default mode, showing only components that are visible in design time on form but not visible on runtime
- All nonvisual - showing all nonvisual components, including, for example, dataset fields or menu items.
- All components - showing all components from the form
When second or third option is selected most of the functions of the component including hiding are disabled - these modes are only for reference.

TNonVisualCompList Stored property stores groups and original Left property values for hided components. You can view it through IDE Object Inspector or DFM text view. 
**Be careful:** editing this property may cause errors with groups and viewing hided components!

### Limitations
- This component can only work with VCL forms
- You can't drop two TNonVisualCompList components on the form 
- TNonVisualCompList don't work with frames (probably will be available in future)
- The component tree starts to refresh slowly with 500+ components to show (not all)
- For now the component has only Win32 version

### Currently working on:
- [ ] Delphi 7 package testing
- [ ] Frames handling
- [ ] Other Delphi and C++ Builder packages
- [ ] Speeding up refreshing with 500+ components to show
- [ ] More aligning options

### License