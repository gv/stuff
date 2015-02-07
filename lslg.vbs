strComputer = Wscript.Arguments.Unnamed.Item(0)
Set colGroups = GetObject("WinNT://" & strComputer & "")
colGroups.Filter = Array("group")
For Each objGroup In colGroups
    Wscript.Echo objGroup.Name 
    For Each objUser in objGroup.Members
        Wscript.Echo vbTab & objUser.Name
    Next
Next
