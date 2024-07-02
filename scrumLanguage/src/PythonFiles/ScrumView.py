from TeamMember import TeamMember
from ScrumMaster import ScrumMaster
from ProductOwner import ProductOwner
from UserStory import UserStory
import random
import tkinter as tk
from Manager import Manager

manager = Manager()

sm = ScrumMaster("Santiago Caballero") 
manager.setScrumMaster(sm)
po = ProductOwner("Samuel Escalera") 
manager.setProductOwner(sm)
manager.addTeamMember(TeamMember("Jeffer Son"))
manager.addTeamMember(TeamMember("Bryant Mayers"))
manager.addTeamMember(TeamMember("Cristiano Ronaldo"))
manager.addTeamMember(TeamMember("Ronaldiño Gaucho"))


def assign_user_stories():
    if manager.getTeamMembers():
        manager.assignUserStories()
        display_assigned_stories()
    else:
        result_label.config(text="Agrega miembros al equipo primero")

def display_assigned_stories():
    if manager.getTeamMembers():
        result_text = ""
        for story in manager.user_stories:
            result_text += str(story) + "\n"

        result_label.config(text=result_text)
    else:
        result_label.config(text="Agrega miembros al equipo primero")

root = tk.Tk()
root.title("Scrum US Distribution")
root.geometry("650x300")

label = tk.Label(root, text="Presiona el botón para asignar historias de usuario")
label.pack(pady=10)

button = tk.Button(root, text="Asignar Historias", command=assign_user_stories)
button.pack(pady=10)

result_label = tk.Label(root, text="")
result_label.pack(pady=10)

manager.setUserStories([
    UserStory("Historia 1", "Descripción de la historia 1"),
    UserStory("Historia 2", "Descripción de la historia 2"),
    UserStory("Historia 3", "Descripción de la historia 3"),
])

root.mainloop()
