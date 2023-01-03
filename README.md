# MMDarkMode

To change documantation to Dark mode the "Reference.nb" file 
needs to be replaced manually. This can be usually found at: 

"C:\Program Files\Wolfram Research\Mathematica\13.2\SystemFiles\FrontEnd\StyleSheets\Wolfram"

*BACKUP A CLEAN COPY OF "Reference.nb" ELSEWHERE* 

(TODO:Automate backup and replace)

Note that you might need to gain ownership of "Reference.nb" depending 
on User Account privileges.


To make the Messages window background Black by 
adding the following to your init.m file:
	
	`SetOptions[EvaluationNotebook[], Background->GrayLevel[0]];`