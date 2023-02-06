# MMDarkMode

## Documentation

* To change documantation to Dark mode the default "Reference.nb" file needs to be replaced with the darkened one. 

* This can be typically be found at:  
	
		"C:\Program Files\Wolfram Research\Mathematica\13.2\SystemFiles\FrontEnd\StyleSheets\Wolfram\Reference.nb"

* **(WIP)** This can be also automatically done through the DocumentationDarkModeGUI.
	
	Note:

	*You might need to gain ownership of "Reference.nb" depending on User Account privileges.*

## Portability

* To make StyleDefinitions portable in a given Notebook , evaluate the following:

		SetOptions[EvaluationNotebook[], StyleDefinitions -> Get["directory/to/StyleSheet"]]

## Documentation

* To make the Messages window background Dark by adding the following to your init.m file:
	
		SetOptions[EvaluationNotebook[], Background->GrayLevel[0]]

# TO DO:
	
* `Attributes`, `Full Name` and Documentation Link inside of Symbol Information too dark.

	