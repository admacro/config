cd "C:\Users\xn94353\.IntelliJIdea*\config"
rmdir "eval" /s /q

reg delete "HKEY_CURRENT_USER\Software\JavaSoft\Prefs\jetbrains\idea" /f

@echo off
cd "C:\Users\xn94353\.IntelliJIdea*\config\options"
ren options.xml options.xml.old
findstr /v /c:"evlsprt" options.xml.old > options.xml
del options.xml.old
