import tkinter as tk

class ScrumView:

    def __init__(self, manager):
        self.manager = manager
        self.result_label = None

    def assign_user_stories(self):
        if self.manager.getTeamMembers():
            self.manager.assignUserStories()
            self.display_assigned_stories()
        else:
            self.set_label_text("Agrega miembros al equipo primero")

    def display_assigned_stories(self):
        if self.manager.getTeamMembers():
            result_text = ""
            for story in self.manager.user_stories:
                result_text += str(story) + "\n"
            self.set_label_text(result_text)    
        else:
            self.set_label_text("Agrega miembros al equipo primero") 

    def set_label_text(self, message_text):
        return self.result_label.config(text=message_text)
    def show_view(self):
        root = tk.Tk()
        root.title("Scrum US Distribution")
        root.geometry("650x300")

        label = tk.Label(root, text="Presiona el botón para asignar historias de usuario")
        label.pack(pady=10)

        button = tk.Button(root, text="Asignar Historias", command=self.assign_user_stories)
        button.pack(pady=10)

        self.result_label = tk.Label(root, text="")
        self.result_label.pack(pady=10)

        root.mainloop()
