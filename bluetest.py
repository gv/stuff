import dbus

b = dbus.SystemBus();
o = b.get_object('org.bluez', '/org/bluez/hci0')
i = dbus.Interface(o, dbus_interface = 'org.bluez.Adapter')
print i.GetProperties()
