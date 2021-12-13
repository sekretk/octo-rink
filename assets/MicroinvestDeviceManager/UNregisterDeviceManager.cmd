if exist "%ProgramFiles(x86)%" regsvr32 /u "C:\Program Files (x86)\Microinvest\Warehouse Pro Light\DeviceManager.dll"
if not exist "%ProgramFiles(x86)%" regsvr32 /u "C:\Program Files\Microinvest\Warehouse Pro Light\DeviceManager.dll"
