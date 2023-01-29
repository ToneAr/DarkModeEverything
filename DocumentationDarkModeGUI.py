from tkinter import *
from tkinter import messagebox
from shutil import *
import os

class GUI:   
    def __init__(self):
        self.root = Tk()
        self.root.title("DocumentationDarkMode")
        self.root.geometry("250x110")

        self.cd = os.getcwd()
        self.progfilesdir = os.environ["ProgramFiles"]

        self.versionLabel = Label(self.root, text="Mathimatica Version:")
        self.versionLabel.pack(side=TOP,padx=50)
        self.version = Entry(self.root)
        self.version.pack(side=TOP,padx=50)

        self.buttonFrame = Frame(self.root, pady=10)
        self.buttonFrame.columnconfigure(0,weight=1)
        self.buttonFrame.columnconfigure(1,weight=1)
        self.buttonFrame.columnconfigure(2,weight=1)

        self.onButton = Button(self.buttonFrame, text="Toggle DocumentationDarkMode ON", command=lambda : toggleOn(self))
        self.onButton.grid(row=1, column=0, sticky=W+E)
        self.offButton = Button(self.buttonFrame, text="Toggle DocumentationDarkMode OFF", command=lambda : toggleOff(self))
        self.offButton.grid(row=2, column=0, sticky=W+E)

        self.buttonFrame.pack()

        self.root.mainloop()

def toggleOn(self):
    self.backupCheck = os.listdir(f"{self.cd}/DocumentationDarkMode/Backup")
    self.darkRefLoc = f"{self.cd}/DocumentationDarkMode/Dark/Reference.nb"
    if self.backupCheck != ['Reference.nb']:
        backup(self)
    copy2(self.darkRefLoc, f"{self.progfilesdir}/Wolfram Research/Mathematica/{self.version.get()}/SystemFiles/FrontEnd/StyleSheets/Wolfram")

def toggleOff(self):
    self.backupCheck = os.listdir(f"{self.cd}/DocumentationDarkMode/Backup")
    self.refCleanLoc = f"{self.cd}/DocumentationDarkMode/Backup/Reference.nb"
    if self.backupCheck != ['Reference.nb']:
        messagebox.showerror(
            title="Error",
            message="Not decided what to say yet"
        )
    else:
        copy2(self.refCleanLoc, f"{self.progfilesdir}/Wolfram Research/Mathematica/{self.version.get()}/SystemFiles/FrontEnd/StyleSheets/Wolfram")

def backup(self):
    if self.version.get() == "13.2" | "13.1" | "13.0":
        self.refDir = f"{self.progfilesdir}/Wolfram Research/Mathematica/{self.version.get()}/SystemFiles/FrontEnd/StyleSheets/Wolfram"
        copy2(f"{self.refLoc}/Reference.nb", f"{self.cd}/DocumentationDarkMode/Backup")

    elif self.version.get() == "":
        messagebox.showerror(
            title="Error",
            message="Please enter the version of your Mathematica install before proceeding"
        )
    else:
        messagebox.showerror(
            title="Error",
            message="Mathematica version of 13.0 or newer required"
        )

if __name__ == '__main__': 
    GUI()