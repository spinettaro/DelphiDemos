#Automate Save State Feature

##Brief Description 
One of the many exciting new features of Delphi XE7 is the Save State in Firemonkey.
The FireMonkey Save State feature allows you to save data describing the state of the application before it closes. The Save State data can be used to recover the state after the application is restarted.
This is a very cool feature but the way in which you save and restore the data is not so refined (for more info see this http://docwiki.embarcadero.com/RADStudio/XE7/en/FireMonkey_Save_State ).
What happen if I need to save 10,15 or more components? I need to specify the save and the restore for each one.
So I decided to write a class helper to automate this process!
Using JSON serialization all the components of type TFMXObject are saved. In particular is stored the Data property.
SaveStateHelper.pas file contains the process described above. 
For more info visit the post on my blog http://www.danielespinetti.it/2014/10/automate-save-state-feature-in.html

##Language
- Object Pascal

##Delphi Supported Versions
- XE7 and major

##Platforms supported 
- Windows, Android, iOS, OSX