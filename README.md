# Dark Mode Everything (DME)

DME is a collection of modifications, stylesheets and FrontEnd files which aims to give a coherent dark mode theme across all Mathematica GUI elements.

## Install

I have yet to find a way to replace system files programmatically through WL, which DME needs to do, so in the meantime all files should be copies over **manually**. I'm working on automating this whole process.

1. Backup your `$InstallationDirectory/SystemFiles` folder.
2. Open a notebook and run `PacletInstall["user:tonya/PacletSites/DME/DME"]`.
3. Copy `DME/SystemFiles/` inside your Mathematica `$InstallationDirectory`, replacing all existing files.
4. Open a new notebook and and run ```DME`SetInitialization[True]```.
5. Restart your Mathematica session completely.

## Uninstall

1. Run ```DME`SetInitialization[False]``` inside a notebook.
2. Copy your back up of `$InstallationDirectory/SystemFiles/` inside your Mathematica `$InstallationDirectory`, replacing all existing DME files.
3. Restart Mathematica completely.

## Demo

For a showcase of all the DME features, check out [the demo notebook](/Demos/DME-Demo-Notebook.nb).
You can open this with any notebook by running ```HelpDME[]```.